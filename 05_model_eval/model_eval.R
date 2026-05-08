library(rjags)
library(tidyverse)
library(coda)

x <- readRDS("04_model_fit/samples.rds")

plot(x[, c("beta", "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector")], ask = TRUE)

summary(x)

## Eval 1 DIC values

dic.samples(model, 10000, type="pD")


## Eval 2: Chi-Square discrepancy

# generate y tilde for a given theta by simulating from the data model (replicate data set)