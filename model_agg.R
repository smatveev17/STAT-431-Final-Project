library(rjags)
load.module("glm")
library(tidyverse)
# setwd("~/Desktop/STAT-431-Final-Project")

data <- read_csv("00_data/sba_clean_50k.csv")

# subset the data by first 10000 rows
data <- data[1:10000,]

# Names of colums
colnames(data)
df1 <- data %>% select(c(MIS_Status,Bank, State, NAICS_sector))

# Convert MIS to binary, 1 if CHGOFF and 0 if PIF
df1 <- df1 %>%
  mutate(MIS_Status = ifelse(MIS_Status == "CHGOFF", 1, 0))

# Convert 50 states and DC to 1:51 factor levels
df1 <- df1 %>%
  mutate(State = as.factor(State)) %>%
  mutate(State = as.numeric(State))

# Convert NAICS_sector to factor levels
df1 <- df1 %>% 
  mutate(NAICS_sector = as.factor(NAICS_sector))

# Create a column n_loans that counts the number of loans for each Bank, then group banks and group banks by 
# case when top20 ~ 1, 20-500 ~ 2, the rest ~3

df1 <- df1 %>%
  group_by(Bank) %>%
  mutate(n_loans = n()) %>%
  ungroup() %>%
  arrange(desc(n_loans)) %>%
  mutate(Bank_group = case_when(
    row_number() <= 20 ~ 1,
    row_number() > 20 & row_number() <= 500 ~ 2,
    TRUE ~ 3
  ))

# Create a new data frame with only the columns we need for the model
df_model <- df1 %>% select(MIS_Status, Bank_group, State, NAICS_sector)
df_model

df_agg <- df_model %>%
  group_by(Bank_group, State, NAICS_sector) %>%
  summarise(y = sum(MIS_Status), n = n(), .groups = "drop") %>%
  mutate(NAICS_sector = as.integer(NAICS_sector))   # factor → integer

d <- list(
  y = df_agg$y, n = df_agg$n,
  bank   = df_agg$Bank_group,
  state  = df_agg$State,
  sector = df_agg$NAICS_sector,
  N = nrow(df_agg),
  J = 1,
  B = length(unique(df_agg$Bank_group)),
  S = length(unique(df_agg$State)),
  C = length(unique(df_agg$NAICS_sector))
)

# Sensible inits centered on logit of mean default rate, overdispersed for Gelman-Rubin
b0 <- qlogis(mean(df_model$MIS_Status))   # ≈ logit of overall default rate

inits <- list(
  list(beta = b0,
       sigma_bank = 0.5, sigma_state = 0.5, sigma_sector = 0.5,
       .RNG.name = "base::Wichmann-Hill", .RNG.seed = 123),
  list(beta = b0 + 1,
       sigma_bank = 1, sigma_state = 1, sigma_sector = 1,
       .RNG.name = "base::Wichmann-Hill", .RNG.seed = 431),
  list(beta = b0 - 1,
       sigma_bank = 0.1, sigma_state = 0.1, sigma_sector = 0.1,
       .RNG.name = "base::Wichmann-Hill", .RNG.seed = 6769)
)

system.time({
  model <- jags.model("04_model_fit/model_agg.txt", data = d, inits = inits, n.chains = 3)
})

system.time({
  x <- coda.samples(model,
                    variable.names = c("beta", "alpha_bank", "alpha_state", "alpha_sector",
                                       "sigma_bank", "sigma_state", "sigma_sector",
                                       "tau_sq_bank", "tau_sq_state", "tau_sq_sector",
                                       "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector"), n.iter = 20000)
})


# saveRDS(x, "04_model_fit/samples.rds")

# Convergence Check

# get trace plots only
plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], trace = TRUE, ask = TRUE)

gelman.diag(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], autoburnin = FALSE, multivariate = FALSE)

gelman.plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], autoburnin = FALSE, ask = TRUE)

# Autocorrelation check
autocorr.plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], ask = TRUE)

summary(x)
