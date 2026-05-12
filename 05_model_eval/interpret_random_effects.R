library(tidyverse)
library(coda)

data_path <- "00_data/cleaned_smaller_subsets/train_50k_subset.csv"
summary_path <- "05_model_eval/summary_random.csv"
samples_path <- "04_model_fit/saved_samples/x_random_0511.rds"

output_dir <- "05_model_eval"

posterior_summary <- function(samples) {
  sample_summary <- summary(samples)

  as_tibble(sample_summary$statistics, rownames = "parameter") %>%
    left_join(
      as_tibble(sample_summary$quantiles, rownames = "parameter"),
      by = "parameter"
    )
}

random_summary <- if (file.exists(summary_path)) {
  read_csv(summary_path, show_col_types = FALSE)
} else {
  posterior_summary(readRDS(samples_path))
}

data <- read_csv(data_path, show_col_types = FALSE)

bank_groups <- data %>%
  count(Bank, name = "n_loans") %>%
  arrange(desc(n_loans), Bank) %>%
  mutate(
    bank_rank = row_number(),
    Bank_group = case_when(
      bank_rank <= 8 ~ bank_rank,
      bank_rank <= 40 ~ 9L,
      TRUE ~ 10L
    )
  )

bank_lookup <- bank_groups %>%
  group_by(level_index = Bank_group) %>%
  summarise(
    effect_group = "bank",
    level_label = if_else(
      n() == 1L,
      paste0("Bank rank ", first(bank_rank), ": ", first(Bank)),
      paste0("Bank ranks ", min(bank_rank), "-", max(bank_rank), " (", n(), " banks)")
    ),
    n_loans = sum(n_loans),
    n_original_levels = n(),
    .groups = "drop"
  )

state_levels <- levels(factor(data$State))
state_lookup <- data %>%
  mutate(level_index = as.integer(factor(State, levels = state_levels))) %>%
  group_by(level_index) %>%
  summarise(
    effect_group = "state",
    level_label = first(State),
    n_loans = n(),
    n_original_levels = 1L,
    .groups = "drop"
  )

naics_sector_name <- function(code) {
  recode(
    as.character(code),
    "11" = "Agriculture, forestry, fishing and hunting",
    "21" = "Mining, quarrying, and oil and gas extraction",
    "22" = "Utilities",
    "23" = "Construction",
    "31" = "Manufacturing",
    "32" = "Manufacturing",
    "33" = "Manufacturing",
    "42" = "Wholesale trade",
    "44" = "Retail trade",
    "45" = "Retail trade",
    "48" = "Transportation and warehousing",
    "49" = "Transportation and warehousing",
    "51" = "Information",
    "52" = "Finance and insurance",
    "53" = "Real estate and rental and leasing",
    "54" = "Professional, scientific, and technical services",
    "55" = "Management of companies and enterprises",
    "56" = "Administrative and support and waste management",
    "61" = "Educational services",
    "62" = "Health care and social assistance",
    "71" = "Arts, entertainment, and recreation",
    "72" = "Accommodation and food services",
    "81" = "Other services",
    "92" = "Public administration",
    .default = "Unknown sector"
  )
}

sector_levels <- levels(factor(data$NAICS_sector))
sector_lookup <- data %>%
  mutate(
    sector_code = as.character(NAICS_sector),
    level_index = as.integer(factor(NAICS_sector, levels = sector_levels))
  ) %>%
  group_by(level_index, sector_code) %>%
  summarise(
    effect_group = "sector",
    level_label = paste0("NAICS ", first(sector_code), ": ", naics_sector_name(first(sector_code))),
    n_loans = n(),
    n_original_levels = 1L,
    .groups = "drop"
  ) %>%
  select(-sector_code)

lookup <- bind_rows(bank_lookup, state_lookup, sector_lookup)

observed_rates <- bind_rows(
  data %>%
    left_join(select(bank_groups, Bank, Bank_group), by = "Bank") %>%
    transmute(effect_group = "bank", level_index = Bank_group, PaidInFull),
  data %>%
    transmute(
      effect_group = "state",
      level_index = as.integer(factor(State, levels = state_levels)),
      PaidInFull
    ),
  data %>%
    transmute(
      effect_group = "sector",
      level_index = as.integer(factor(NAICS_sector, levels = sector_levels)),
      PaidInFull
    )
) %>%
  group_by(effect_group, level_index) %>%
  summarise(
    observed_paid_in_full_rate = mean(PaidInFull),
    .groups = "drop"
  )

random_effect_summary_labeled <- random_summary %>%
  mutate(
    parameter_type = str_match(parameter, "^alpha_([^\\[]+)\\[(\\d+)\\]$")[, 2],
    level_index = as.integer(str_match(parameter, "^alpha_([^\\[]+)\\[(\\d+)\\]$")[, 3])
  ) %>%
  filter(!is.na(parameter_type)) %>%
  rename(
    effect_group = parameter_type,
    mean_log_odds_change = Mean,
    sd_log_odds_change = SD,
    lower_95_log_odds_change = `2.5%`,
    median_log_odds_change = `50%`,
    upper_95_log_odds_change = `97.5%`
  ) %>%
  left_join(lookup, by = c("effect_group", "level_index")) %>%
  left_join(observed_rates, by = c("effect_group", "level_index")) %>%
  mutate(
    odds_multiplier = exp(mean_log_odds_change),
    lower_95_odds_multiplier = exp(lower_95_log_odds_change),
    upper_95_odds_multiplier = exp(upper_95_log_odds_change),
    direction = case_when(
      mean_log_odds_change > 0 ~ "higher log odds of PaidInFull than the overall baseline, conditional on the other random effects",
      mean_log_odds_change < 0 ~ "lower log odds of PaidInFull than the overall baseline, conditional on the other random effects",
      TRUE ~ "approximately no log-odds shift from the overall baseline"
    )
  ) %>%
  select(
    effect_group, parameter, level_index, level_label,
    mean_log_odds_change, lower_95_log_odds_change, upper_95_log_odds_change,
    odds_multiplier, lower_95_odds_multiplier, upper_95_odds_multiplier,
    sd_log_odds_change, observed_paid_in_full_rate, n_loans, n_original_levels,
    direction, everything()
  )

largest_random_effects <- random_effect_summary_labeled %>%
  group_by(effect_group) %>%
  slice_max(mean_log_odds_change, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(extreme = "largest positive log-odds shift")

smallest_random_effects <- random_effect_summary_labeled %>%
  group_by(effect_group) %>%
  slice_min(mean_log_odds_change, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(extreme = "largest negative log-odds shift")

modeled_data <- data %>%
  left_join(select(bank_groups, Bank, Bank_group), by = "Bank") %>%
  transmute(
    PaidInFull,
    bank = Bank_group,
    state = as.integer(factor(State, levels = state_levels)),
    sector = as.integer(factor(NAICS_sector, levels = sector_levels))
  )

combination_counts <- modeled_data %>%
  group_by(bank, state, sector) %>%
  summarise(
    n_loans = n(),
    observed_paid_in_full_rate = mean(PaidInFull),
    .groups = "drop"
  )

bank_effects <- random_effect_summary_labeled %>%
  filter(effect_group == "bank") %>%
  transmute(
    bank = level_index,
    bank_parameter = parameter,
    bank_label = level_label,
    bank_log_odds_change = mean_log_odds_change,
    bank_n_original_levels = n_original_levels
  )

state_effects <- random_effect_summary_labeled %>%
  filter(effect_group == "state") %>%
  transmute(
    state = level_index,
    state_parameter = parameter,
    state_label = level_label,
    state_log_odds_change = mean_log_odds_change
  )

sector_effects <- random_effect_summary_labeled %>%
  filter(effect_group == "sector") %>%
  transmute(
    sector = level_index,
    sector_parameter = parameter,
    sector_label = level_label,
    sector_log_odds_change = mean_log_odds_change
  )

combination_effects <- combination_counts %>%
  left_join(bank_effects, by = "bank") %>%
  left_join(state_effects, by = "state") %>%
  left_join(sector_effects, by = "sector") %>%
  mutate(
    extreme = NA_character_,
    effect_group = "combination",
    parameter = paste(bank_parameter, state_parameter, sector_parameter, sep = " + "),
    level_index = NA_integer_,
    level_label = paste0(
      "Bank: ", bank_label,
      " | State: ", state_label,
      " | Sector: ", sector_label
    ),
    mean_log_odds_change = bank_log_odds_change + state_log_odds_change + sector_log_odds_change,
    lower_95_log_odds_change = NA_real_,
    upper_95_log_odds_change = NA_real_,
    odds_multiplier = exp(mean_log_odds_change),
    lower_95_odds_multiplier = NA_real_,
    upper_95_odds_multiplier = NA_real_,
    sd_log_odds_change = NA_real_,
    n_original_levels = bank_n_original_levels,
    direction = case_when(
      mean_log_odds_change > 0 ~ "higher log odds of PaidInFull from the combined bank, state, and sector random-effect deviations",
      mean_log_odds_change < 0 ~ "lower log odds of PaidInFull from the combined bank, state, and sector random-effect deviations",
      TRUE ~ "approximately no combined bank, state, and sector log-odds shift"
    )
  )

largest_combination_effect <- combination_effects %>%
  slice_max(mean_log_odds_change, n = 1, with_ties = FALSE) %>%
  mutate(extreme = "largest positive combined log-odds shift")

smallest_combination_effect <- combination_effects %>%
  slice_min(mean_log_odds_change, n = 1, with_ties = FALSE) %>%
  mutate(extreme = "largest negative combined log-odds shift")

interesting_random_effects <- bind_rows(
  largest_random_effects,
  smallest_random_effects,
  largest_combination_effect,
  smallest_combination_effect
) %>%
  arrange(effect_group, desc(mean_log_odds_change)) %>%
  relocate(extreme, effect_group, parameter, level_label, mean_log_odds_change, odds_multiplier)

write_csv(
  random_effect_summary_labeled,
  file.path(output_dir, "random_effect_summary_labeled.csv")
)

write_csv(
  interesting_random_effects,
  file.path(output_dir, "interesting_random_effects.csv")
)

cat("\nInterpretation notes:\n")
cat("- The outcome in the current aggregate model is PaidInFull, so positive values mean higher log odds of being paid in full, not higher default risk.\n")
cat("- Each random effect is a conditional deviation from the model baseline after accounting for the other random-effect groups.\n")
cat("- Combination rows sum posterior mean alpha_bank + alpha_state + alpha_sector for observed combinations only; they are not separate fitted coefficients.\n")
cat("- Combination-row intervals are left blank because summing marginal 95% intervals would ignore posterior dependence among random effects.\n")
cat("- These are not causal effects, and the bank groups 9 and 10 are pooled groups of many banks rather than individual banks.\n")
cat("- The odds multiplier is exp(log-odds change). For example, 1.20 means about 20% higher odds, while 0.80 means about 20% lower odds.\n\n")

print(
  interesting_random_effects %>%
    select(
      extreme, effect_group, parameter, level_label,
      mean_log_odds_change, odds_multiplier,
      lower_95_log_odds_change, upper_95_log_odds_change,
      observed_paid_in_full_rate, n_loans,
      any_of(c("bank_label", "state_label", "sector_label"))
    )
)
