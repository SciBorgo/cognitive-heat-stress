

# Cognitive analysis
# David N Borg
# June 2021

# Libraries
library(dplyr)
library(janitor)
library(ggplot2)
library(naniar)
library(visdat)
library(tidyverse)
library(bayesplot)
library(brms)
library(modelr)
library(mice)
library(conflicted)

# Conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("filter","select")
conflict_prefer("ar","brms")
conflict_prefer("chisq.test", "janitor")

# Setwd
setwd("~/Downloads/Project - Effect of heat on cognition")

# Load data
imputed_data_pmm = readRDS("~/Downloads/Project - Effect of heat on cognition/imputed-data-pmm.rds")

# Congruent formula
congruent_formula = con_ies ~ 
  con_ies_first + 
  time +
  poly(tcore, degree = 2) +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1+poly(tcore, degree = 2)|id)

# Fit congruent model
fit_con <- brm_multiple(
  formula = congruent_formula,
  family = gaussian(),
  prior = set_prior(horseshoe(df = 1, par_ratio = 1/3), class = "b"),
  cores = 8, 
  chains = 8,
  iter = 10000,
  thin = 5,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  seed = 123,
  data = imputed_data_pmm)

# Check Rhats
round(fit_con$rhats, 3)

# Predictive check and chains
pp_check(fit_con, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_con)

# Model summary
summary(fit_con)

# Save model
save(fit_con, file = "fit_con.RData")



#### END