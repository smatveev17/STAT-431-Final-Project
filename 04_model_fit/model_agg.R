library(rjags)
load.module("glm")
library(tidyverse)
# setwd("~/Desktop/STAT-431-Final-Project")

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

make_data <- function(df_agg, X = NULL) {
  d <- list(
    y = df_agg$y,
    n = df_agg$n,
    bank = df_agg$Bank_group,
    state = df_agg$State,
    sector = df_agg$NAICS_sector,
    N = nrow(df_agg),
    J = 1,
    B = n_distinct(df_agg$Bank_group),
    S = n_distinct(df_agg$State),
    C = n_distinct(df_agg$NAICS_sector)
  )

  if (!is.null(X)) {
    d$X <- X
    d$J <- ncol(X)
  }

  d
}

d_random <- make_data(df_agg_random)
d_fixed <- make_data(df_agg_fixed, X_fixed)

model_data <- list(random = d_random, fixed = d_fixed)

# Sensible inits centered on logit of mean default rate, overdispersed for Gelman-Rubin
b0 <- qlogis(mean(df_model$PaidInFull))   # ≈ logit of overall default rate

make_inits <- function(J, beta_start, fixed_effect_start, sigma_start, seed) {
  list(
    beta = c(beta_start, rep(fixed_effect_start, J - 1)),
    sigma_bank = sigma_start,
    sigma_state = sigma_start,
    sigma_sector = sigma_start,
    .RNG.name = "base::Wichmann-Hill",
    .RNG.seed = seed
  )
}

make_inits_list <- function(J) {
  list(
    make_inits(J, b0, 0, 0.5, 123),
    make_inits(J, b0 + 1, 0.1, 1, 431),
    make_inits(J, b0 - 1, -0.1, 0.1, 6769)
  )
}

inits_random <- make_inits_list(d_random$J)
inits_fixed <- make_inits_list(d_fixed$J)

monitored_params <- c(
  "beta", "alpha_bank", "alpha_state", "alpha_sector",
  "sigma_bank", "sigma_state", "sigma_sector",
  "tau_sq_bank", "tau_sq_state", "tau_sq_sector",
  "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector",
  "ytilde"
)

system.time({
  model_random <- jags.model(
    "04_model_fit/model_agg.txt",
    data = d_random,
    inits = inits_random,
    n.chains = 3
  )
})

system.time({
  model_fixed <- jags.model(
    "04_model_fit/model_agg_w_fixed",
    data = d_fixed,
    inits = inits_fixed,
    n.chains = 3
  )
})

system.time({
  x_random <- coda.samples(model_random, variable.names = monitored_params, n.iter = 20000)
})

# system.time({
#   x_fixed <- coda.samples(model_fixed, variable.names = monitored_params, n.iter = 20000)
# })


saveRDS(x_random, "04_model_fit/saved_samples/x_random_0510.rds")
# saveRDS(x_fixed, "04_model_fit/saved_samples/x_fixed_0508.rds")


s <- as.tibble(summary(x_random)$statistics, rownames = "parameter")

# Convergence Check
beta_lookup_fixed

params_to_check <- function(x) {
  c(
    varnames(x)[varnames(x) == "beta" | startsWith(varnames(x), "beta[")],
    "tau_sq_bank", "tau_sq_state", "tau_sq_sector"
  )
}

# get trace plots only
plot(x_random[, params_to_check(x_random)], trace = TRUE, ask = TRUE)
# plot(x_fixed[, params_to_check(x_fixed)], trace = TRUE, ask = TRUE)

gelman.diag(x_random[, params_to_check(x_random)], autoburnin = FALSE, multivariate = FALSE)
# gelman.diag(x_fixed[, params_to_check(x_fixed)], autoburnin = FALSE, multivariate = FALSE)

gelman.plot(x_random[, params_to_check(x_random)], autoburnin = FALSE, ask = TRUE)
# gelman.plot(x_fixed[, params_to_check(x_fixed)], autoburnin = FALSE, ask = TRUE)

# Autocorrelation check
autocorr.plot(x_random[, params_to_check(x_random)], ask = TRUE)
# autocorr.plot(x_fixed[, params_to_check(x_fixed)], ask = TRUE)

summary(x_random)
# summary(x_fixed)

gelman_plot_dir <- "04_model_fit/gelman_plots"
dir.create(gelman_plot_dir, recursive = TRUE, showWarnings = FALSE)

save_gelman_plot <- function(samples, file_name) {
  file_path <- file.path(gelman_plot_dir, file_name)

  png(file_path, width = 1600, height = 1200, res = 150)
  on.exit(dev.off(), add = TRUE)

  gelman.plot(
    samples[, params_to_check(samples)],
    autoburnin = FALSE,
    ask = FALSE
  )

  file_path
}

save_gelman_plot(x_random, "gelman_random_effects.png")

if (exists("x_fixed")) {
  save_gelman_plot(x_fixed, "gelman_random_fixed_effects.png")
}

trace_plot_dir <- "04_model_fit/trace_plots"
dir.create(trace_plot_dir, recursive = TRUE, showWarnings = FALSE)

save_trace_plot <- function(samples, file_name) {
  file_path <- file.path(trace_plot_dir, file_name)

  png(file_path, width = 1600, height = 1200, res = 150)
  on.exit(dev.off(), add = TRUE)

  plot(
    samples[, params_to_check(samples)],
    trace = TRUE,
    ask = FALSE
  )

  file_path
}

save_trace_plot(x_random, "trace_random_effects.png")

if (exists("x_fixed")) {
  save_trace_plot(x_fixed, "trace_random_fixed_effects.png")
}
