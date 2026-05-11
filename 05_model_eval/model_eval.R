library(rjags)
library(tidyverse)
library(coda)

x_random <- readRDS("04_model_fit/saved_samples/x_random_0508.rds")
x_fixed <- readRDS("04_model_fit/saved_samples/x_fixed_0508.rds")

fixed_effect_terms <- c(
  "NewExist_f", "UrbanRural_f", "RevLineCr_f",
  "LowDoc_f", "IsFranchise", "GFC_Period"
)

data <- read_csv("00_data/sba_clean_50k.csv", show_col_types = FALSE) %>%
  slice_head(n = 10000)

bank_groups <- data %>%
  count(Bank, name = "n_loans") %>%
  arrange(desc(n_loans), Bank) %>%
  mutate(
    Bank_group = case_when(
      row_number() <= 20 ~ 1L,
      row_number() <= 500 ~ 2L,
      TRUE ~ 3L
    )
  ) %>%
  select(Bank, Bank_group)

df_model <- data %>%
  select(MIS_Status, Bank, State, NAICS_sector, all_of(fixed_effect_terms)) %>%
  left_join(bank_groups, by = "Bank") %>%
  transmute(
    MIS_Status = as.integer(MIS_Status == "CHGOFF"),
    Bank_group,
    State = as.integer(factor(State)),
    NAICS_sector = as.integer(factor(NAICS_sector)),
    across(all_of(fixed_effect_terms), as.factor)
  )

df_agg_fixed <- df_model %>%
  group_by(Bank_group, State, NAICS_sector, across(all_of(fixed_effect_terms))) %>%
  summarise(y = sum(MIS_Status), n = n(), .groups = "drop")

X_fixed <- model.matrix(reformulate(fixed_effect_terms), data = df_agg_fixed)

fixed_effect_lookup <- tibble(
  parameter = paste0("beta[", seq_along(colnames(X_fixed)), "]"),
  model_matrix_column = colnames(X_fixed),
  term = c("(Intercept)", fixed_effect_terms[attr(X_fixed, "assign")[-1]]),
  level = c(NA_character_, str_remove(colnames(X_fixed)[-1], fixed_effect_terms[attr(X_fixed, "assign")[-1]])),
  reference_level = c(
    paste(
      fixed_effect_terms,
      map_chr(df_agg_fixed[fixed_effect_terms], ~ levels(.x)[1]),
      sep = "=",
      collapse = "; "
    ),
    map2_chr(
      fixed_effect_terms[attr(X_fixed, "assign")[-1]],
      colnames(X_fixed)[-1],
      ~ levels(df_agg_fixed[[.x]])[1]
    )
  )
)

beta_statistics_fixed <- as_tibble(summary(x_fixed)$statistics, rownames = "parameter")
beta_quantiles_fixed <- as_tibble(summary(x_fixed)$quantiles, rownames = "parameter")

beta_summary_fixed <- beta_statistics_fixed %>%
  filter(str_detect(parameter, "^beta(\\[|$)")) %>%
  left_join(beta_quantiles_fixed, by = "parameter") %>%
  left_join(fixed_effect_lookup, by = "parameter") %>%
  relocate(parameter, term, level, reference_level, model_matrix_column)

beta_summary_fixed

plot(x_fixed[, c("beta[1]", "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector")], ask = TRUE)

summary(x_fixed)

## Eval 1 DIC values

dic.samples(model_random, 10000, type="pD")
# dic.samples(model_fixed, 10000, type="pD")

## Eval 2: Chi-Square discrepancy

# Pearson chi-square discrepancy
T_obs <- sum((y - n * p_hat)^2 / (n * p_hat * (1 - p_hat)))
T_rep <- apply(ytilde, 1, function(yrep) {
  sum((yrep - n * p_hat)^2 / (n * p_hat * (1 - p_hat)))
})

mean(T_rep > T_obs)  # posterior predictive p-value

