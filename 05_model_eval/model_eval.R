library(rjags)
library(tidyverse)
library(coda)

x <- readRDS("04_model_fit/samples.rds")

plot(x[, c("beta", "sigma_sq_bank", "sigma_sq_state", "sigma_sq_sector")])

summary(x)

dic.samples(model, 1000, type="pD")
