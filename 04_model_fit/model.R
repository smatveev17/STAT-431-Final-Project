library(rjags)
load.module("glm")
library(tidyverse)
setwd("~/Desktop/STAT-431-Final-Project")

data <- read_csv("04_model_fit/sba_clean_full.csv")

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

d <- list(y = df_model$MIS_Status,
          bank = df_model$Bank_group,
          state = df_model$State,
          sector = df_model$NAICS_sector,
          J = 1,  # Only one fixed effect (intercept)
          B = length(unique(df_model$Bank_group)),
          S = length(unique(df_model$State)),
          C = length(unique(df_model$NAICS_sector)))


inits <- list(list(beta = 5, tau_sq_bank = 10, tau_sq_state = 10, tau_sq_sector = 10, 
                   .RNG.name = "base::Wichmann-Hill",
                   .RNG.seed = 123),
              list(beta = 0.1, tau_sq_bank = 0.1, tau_sq_state = 0.1, tau_sq_sector = 0.1,
                   .RNG.name = "base::Wichmann-Hill", 
                   .RNG.seed = 431),
              list(beta = -20, tau_sq_bank = 0.01, tau_sq_state = 0.01, tau_sq_sector = 0.01,
                   .RNG.name = "base::Wichmann-Hill", 
                   .RNG.seed = 6769)
  
)

model <- jags.model("04_model_fit/model.txt", data = d, inits = inits, n.chains = 3)

x <- coda.samples(model, variable.names = c("beta", "alpha_bank", "alpha_state", "alpha_sector", "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector"),
                        n.iter = 20000)

# Convergence Check

# get trace plots only
plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], trace = TRUE, ask = TRUE)

gelman.diag(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], autoburnin = FALSE, multivariate = FALSE)

gelman.plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], autoburnin = FALSE, ask = TRUE)

# Autocorrelation check
autocorr.plot(x[, c("beta", "tau_sq_bank", "tau_sq_state", "tau_sq_sector")], ask = TRUE)

summary(x)
