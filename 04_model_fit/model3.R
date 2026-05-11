library(rjags)
load.module("glm")
library(tidyverse)
setwd("~/Desktop/STAT-431-Final-Project")

fixed_effect_terms <- c(
  # "NewExist_f", "RevLineCr_f",
  # "LowDoc_f", "IsFranchise", 
  "GFC_Period", "UrbanRural_f"
)

data <- read_csv("00_data/cleaned_smaller_subsets/train_50k_subset.csv", show_col_types = FALSE)

bank_groups <- data %>%
  count(Bank, name = "n_loans") %>%
  arrange(desc(n_loans), Bank) %>%
  mutate(
    Bank_group = case_when(
      row_number() <= 8 ~ row_number(),
      row_number() <= 40 ~ 9L,
      TRUE ~ 10L
    ),
  ) %>%
  select(Bank, Bank_group, n_loans)

df_model <- data %>%
  select(PaidInFull, Bank, State, NAICS_sector, all_of(fixed_effect_terms)) %>%
  left_join(bank_groups, by = "Bank") %>%
  transmute(
    # MIS_Status = as.integer(MIS_Status == "CHGOFF"),
    PaidInFull = as.integer(PaidInFull),
    Bank_group,
    State = as.integer(factor(State)),
    NAICS_sector = as.integer(factor(NAICS_sector)),
    across(all_of(fixed_effect_terms), as.factor)
  )

df_agg_random <- df_model %>%
  group_by(Bank_group, State, NAICS_sector) %>%
  summarise(y = sum(PaidInFull), n = n(), .groups = "drop")

df_agg_fixed <- df_model %>%
  group_by(Bank_group, State, NAICS_sector, across(all_of(fixed_effect_terms))) %>%
  summarise(y = sum(PaidInFull), n = n(), .groups = "drop")

X_fixed <- model.matrix(reformulate(fixed_effect_terms), data = df_agg_fixed)

fixed_effect_names <- colnames(X_fixed)
beta_lookup_fixed <- tibble(
  parameter = paste0("beta[", seq_along(fixed_effect_names), "]"),
  fixed_effect = fixed_effect_names
)

make_data <- function(df_agg) {
  list(
    y = df_agg$y,
    n = df_agg$n,
    bank = df_agg$Bank_group,
    state = df_agg$State,
    sector = df_agg$NAICS_sector,
    N = nrow(df_agg),
    B = n_distinct(df_agg$Bank_group),
    S = n_distinct(df_agg$State),
    C = n_distinct(df_agg$NAICS_sector)
  )
}

# Since model.txt does not utilize the X matrix, we only need to pass the basic aggregated data
d_model <- make_data(df_agg_random)

# 2. Setup Inits to exactly match the parameter nodes in model.txt
# Sensible inits centered on logit of mean default rate
b0 <- qlogis(mean(df_model$PaidInFull))   

make_inits <- function(d, b_start, eff_start, seed) {
  list(
    beta = c(b_start),                    
    # Put NA in the 1st position, and initialize the rest
    beta_bank = c(NA, rep(eff_start, d$B - 1)),      
    beta_state = c(NA, rep(eff_start, d$S - 1)),     
    beta_sector = c(NA, rep(eff_start, d$C - 1)),    
    .RNG.name = "base::Wichmann-Hill",
    .RNG.seed = seed
  )
}

make_inits_list <- function(d) {
  list(
    make_inits(d, b0, 0, 123),
    make_inits(d, b0 + 1, 0.1, 431),
    make_inits(d, b0 - 1, -0.1, 6769)
  )
}

inits_model <- make_inits_list(d_model)

# 3. Monitor ONLY the variables explicitly defined in your model.txt
monitored_params <- c(
  "beta", "beta_bank", "beta_state", "beta_sector", "ytilde"
)

# 4. Compile and run the model
system.time({
  model_fit <- jags.model(
    "04_model_fit/model3.txt", 
    data = d_model,
    inits = inits_model,
    n.chains = 3
  )
})

system.time({
  x_fixed_only3 <- coda.samples(
    model_fit, 
    variable.names = monitored_params, 
    n.iter = 20000
  )
})

saveRDS(x_fixed_only, "04_model_fit/saved_samples/x_fixed_only_0510.rds")
saveRDS(x_fixed_only2, "04_model_fit/saved_samples/x_fixed_only2_0510.rds")
saveRDS(x_fixed_only3, "04_model_fit/saved_samples/x_fixed_only3_0510.rds")

params_to_check <- function(mcmc_list) {
  varnames(mcmc_list)[grepl("beta|beta_bank|beta_state|beta_sector", varnames(mcmc_list))]
}

# get trace plots only
plot(x_fixed_only2[, params_to_check(x_fixed_only2)], trace = TRUE, ask = TRUE)

gelman.diag(x_fixed_only2[, params_to_check(x_fixed_only2)], autoburnin = FALSE, multivariate = FALSE)

gelman.plot(x_fixed_only2[, params_to_check(x_fixed_only2)], autoburnin = FALSE, ask = TRUE)

# Autocorrelation check
autocorr.plot(x_fixed_only2[, params_to_check(x_fixed_only2)], ask = TRUE)

# Effective Sample Size
# Check that Time-series SE is 1/20th or less of the SD (indicating good mixing)
eff_size <- effectiveSize(x_fixed_only2[, params_to_check(x_fixed_only2)])
eff_size

# what is the smallest effective sample size among the parameters of interest?
min(eff_size)

summary <- as_tibble(summary(x_fixed_only3[, params_to_check(x_fixed_only3)]))


# Calculate the DIC value
dic <- dic.samples(model_fit, n.iter = 10000)
dic

# find the bank, state, and sector that increased and decreased the log odds the most

best_finder <- function(mcmc_list, param_prefix) {
  param_names <- varnames(mcmc_list)
  target_params <- param_names[grepl(param_prefix, param_names)]
  
  summary_df <- as_tibble(summary(mcmc_list[, target_params])$statistics, rownames = "parameter") %>%
    mutate(
      mean = `Mean`,
      lower = `Mean` - 2 * `SD`,
      upper = `Mean` + 2 * `SD`
    ) %>%
    select(parameter, mean, lower, upper) %>%
    arrange(desc(mean))
  
  summary_df
}

worst_finder <- function(mcmc_list, param_prefix) {
  param_names <- varnames(mcmc_list)
  target_params <- param_names[grepl(param_prefix, param_names)]
  
  summary_df <- as_tibble(summary(mcmc_list[, target_params])$statistics, rownames = "parameter") %>%
    mutate(
      mean = `Mean`,
      lower = `Mean` - 2 * `SD`,
      upper = `Mean` + 2 * `SD`
    ) %>%
    select(parameter, mean, lower, upper) %>%
    arrange(mean)
  
  summary_df
}

best_bank <- best_finder(x_fixed_only3, "beta_bank")
worst_bank <- worst_finder(x_fixed_only3, "beta_bank")

best_state <- best_finder(x_fixed_only3, "beta_state")
worst_state <- worst_finder(x_fixed_only3, "beta_state")

best_sector <- best_finder(x_fixed_only3, "beta_sector")
worst_sector <- worst_finder(x_fixed_only3, "beta_sector")



# gelman_plot_dir <- "04_model_fit/gelman_plots_fixed_only"
# dir.create(gelman_plot_dir, recursive = TRUE, showWarnings = FALSE)
# 
# save_gelman_plot <- function(samples, file_name) {
#   file_path <- file.path(gelman_plot_dir, file_name)
#   
#   png(file_path, width = 1600, height = 1200, res = 150)
#   on.exit(dev.off(), add = TRUE)
#   
#   gelman.plot(
#     samples[, params_to_check(samples)],
#     autoburnin = FALSE,
#     ask = FALSE
#   )
#   
#   file_path
# }

# save_gelman_plot(x_fixed_only, "gelman_fixed_only.png")
# 
# trace_plot_dir <- "04_model_fit/trace_plots_fixed_only"
# dir.create(trace_plot_dir, recursive = TRUE, showWarnings = FALSE)
# 
# save_trace_plot <- function(samples, file_name) {
#   file_path <- file.path(trace_plot_dir, file_name)
#   
#   png(file_path, width = 1600, height = 1200, res = 150)
#   on.exit(dev.off(), add = TRUE)
#   
#   plot(
#     samples[, params_to_check(samples)],
#     trace = TRUE,
#     ask = FALSE
#   )
#   
#   file_path
# }
# 
# save_trace_plot(x_fixed_only, "trace_fixed_only.png")

