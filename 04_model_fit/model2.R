library(rjags)
load.module("glm")
library(tidyverse)
setwd("~/Desktop/STAT-431-Final-Project")

data <- read_csv("04_model_fit/sba_clean_full.csv")

# Randomly sample 50000 rows
set.seed(431)
data <- data %>% sample_n(50000)

# add the following columsn 
# beta[3] * DisbursementGross_num[i] +
#   beta[4] * SBA_Portion[i] + 
#   beta[5] * NoEmp[i] +
#   beta[6] * SBA_Appv_num[i] +

# If i want to add UrbanRural_f, should I one hot encode it for a glm?
# Yes, if you want to include UrbanRural_f as a predictor in a glm, you would typically one-hot encode it (also known as creating dummy variables). This means you would create separate binary columns for each category of UrbanRural_f (e.g., Urban, Rural) and use those as predictors in your model. The reference category (e.g., Undefined) would be implicitly represented by the absence of the other categories.

# Names of colums
colnames(data)
df1 <- data %>% select(c(MIS_Status,Bank, State, NAICS_sector, ApprovalFY), DisbursementGross_num, SBA_Portion, NoEmp, SBA_Appv_num, UrbanRural_f)

# Convert MIS to binary, 1 if CHGOFF and 0 if PIF
df1 <- df1 %>%
  mutate(MIS_Status = ifelse(MIS_Status == "CHGOFF", 1, 0))

# Convert 50 states and DC to 1:51 factor levels
df1 <- df1 %>%
  mutate(State = as.factor(State)) %>%
  mutate(State = as.numeric(State))

# One hot encode UrbanRural_f
df1 <- df1 %>%
  mutate(UrbanRural_Urban = ifelse(UrbanRural_f == "Urban", 1, 0),
         UrbanRural_Rural = ifelse(UrbanRural_f == "Rural", 1, 0))

# Convert NAICS_sector to factor levels from 1 to N
df1 <- df1 %>% 
  mutate(NAICS_sector = as.integer(as.factor(NAICS_sector)))

# Create a column n_loans that counts the number of loans for each Bank, then group banks and group banks by 
# case when top 7 banks are groups 1~7, 8-35 are group 8, and the rest are group 9

df1 <- df1 %>%
  group_by(Bank) %>%
  mutate(n_loans = n()) %>%
  ungroup() %>%
  arrange(desc(n_loans)) %>%
  mutate(Bank_group = case_when(
    row_number() <= 7 ~ as.character(row_number()),
    row_number() > 7 & row_number() <= 35 ~ "8",
    TRUE ~ "9"
  ))



# Create a new data frame with only the columns we need for the model
df_model <- df1 %>% select(MIS_Status, Bank_group, State, NAICS_sector, ApprovalFY, DisbursementGross_num, SBA_Portion, NoEmp, SBA_Appv_num, UrbanRural_Urban)
df_model

d <- list(y = df_model$MIS_Status,
          bank = df_model$Bank_group,
          state = df_model$State,
          sector = df_model$NAICS_sector,
          ApprovalFY = as.numeric(scale(df_model$ApprovalFY)),
          DisbursementGross_num = as.numeric(scale(df1$DisbursementGross_num)),
          SBA_Portion = as.numeric(scale(df1$SBA_Portion)),
          NoEmp = as.numeric(scale(df1$NoEmp)),
          SBA_Appv_num = as.numeric(scale(df1$SBA_Appv_num)),
          UrbanRural_Urban = df1$UrbanRural_Urban,
          J = 7,  # Only one fixed effect (intercept)
          B = length(unique(df_model$Bank_group)),
          S = length(unique(df_model$State)),
          C = length(unique(df_model$NAICS_sector)))


inits <- list(list(beta = c(5, rep(-0.1, 6)), tau_sq_bank = 10, tau_sq_state = 10, tau_sq_sector = 10, 
                   .RNG.name = "base::Wichmann-Hill",
                   .RNG.seed = 123),
              list(beta = c(0.5, rep(0.1, 6)), tau_sq_bank = 0.1, tau_sq_state = .1, tau_sq_sector = 0.1,
                   .RNG.name = "base::Wichmann-Hill", 
                   .RNG.seed = 431),
              list(beta = c(-2.5, rep(-0.01, 6)), tau_sq_bank = 0.01, tau_sq_state = .1, tau_sq_sector = 0.01,
                   .RNG.name = "base::Wichmann-Hill", 
                   .RNG.seed = 6769)
  
)

model <- jags.model("04_model_fit/model2.txt", data = d, inits = inits, n.chains = 3)

x2 <- coda.samples(model, variable.names = c("beta", "alpha_bank", "alpha_state", "alpha_sector", "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector"),
                        n.iter = 10000)

# Convergence Check

# get trace plots only
plot(x[, c("beta", "alpha_bank", "alpha_state", "alpha_sector")], ask = TRUE)

# Get column names matching your parameters of interest
params_to_plot <- c("beta", "alpha_bank", "alpha_sector")

# Pull matching columns from the mcmc.list
x_sub <- x2[, grep(paste(params_to_plot, collapse = "|"), varnames(x2))]


plot(x_sub, ask = TRUE)

# Give code to plot 1 trace plot
plot(x2[, "beta"], ask = FALSE)

# Subset the mcmc.list to just the alpha parameters
alpha_cols <- grep("alpha_bank|alpha_state|alpha_sector", varnames(x))
x_alpha <- x[, alpha_cols]

# Gelman-Rubin diagnostic (needs n.chains >= 2, you have 3 — good)
gelman.diag(x_alpha, multivariate = FALSE)  # multivariate = FALSE to get per-parameter R-hat

# Gelman plot
gelman.plot(x_alpha, ask = TRUE)

summary(x)

# Can you check how the counts of Bank of America in each state?
data %>% 
  filter(Bank == "BANK OF AMERICA NATL ASSOC") %>%
  group_by(State) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# give me a bar plot of the counts of Bank of America in each state
data %>%
  filter(Bank == "BANK OF AMERICA NATL ASSOC") %>%
  group_by(State) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(State, -count), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Counts of Bank of America Loans by State",
       x = "State",
       y = "Count of Loans") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Check the bottom 20 states with the least counts of Bank of America loans
data %>%
  filter(Bank == "BANK OF AMERICA NATL ASSOC") %>%
  group_by(State) %>%
  summarise(count = n()) %>%
  arrange(count) %>% 
  head(20)
