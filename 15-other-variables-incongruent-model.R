

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


#### Mean tsk
# Congruent formula
incongruent_formula = incon_ies ~ 
  incon_ies_first + 
  time +
  poly(mean_tsk, degree = 2) +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1+poly(mean_tsk, degree = 2)|id)

# Fit congruent model
fit_incon <- brm_multiple(
  formula = incongruent_formula,
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
round(fit_incon$rhats, 3)

# Predictive check and chains
pp_check(fit_incon, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_incon)

# Model summary
summary(fit_incon)

# Save model
save(fit_incon, file = "fit_incon_mtsk.RData")

# Plot: b_polytcoredegreeEQ21
gather_draws(fit_incon, b_polymean_tskdegreeEQ21) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")

# Plot: b_polytcoredegreeEQ22
gather_draws(fit_incon, b_polymean_tskdegreeEQ22) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")






#### RPE
# Congruent formula
incongruent_formula = incon_ies ~ 
  incon_ies_first + 
  time +
  poly(rpe, degree = 2) +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1+poly(rpe, degree = 2)|id)

# Fit congruent model
fit_incon <- brm_multiple(
  formula = incongruent_formula,
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
round(fit_incon$rhats, 3)

# Predictive check and chains
pp_check(fit_incon, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_incon)

# Model summary
summary(fit_incon)

# Save model
save(fit_incon, file = "fit_incon_rpe.RData")

# Plot: b_polytcoredegreeEQ21
gather_draws(fit_incon, b_polyrpedegreeEQ21) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")

# Plot: b_polytcoredegreeEQ22
gather_draws(fit_incon, b_polyrpedegreeEQ22) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")






#### heart_rate
# Congruent formula
incongruent_formula = incon_ies ~ 
  incon_ies_first + 
  time +
  poly(heart_rate, degree = 2) +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1+poly(heart_rate, degree = 2)|id)

# Fit congruent model
fit_incon <- brm_multiple(
  formula = incongruent_formula,
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
round(fit_incon$rhats, 3)

# Predictive check and chains
pp_check(fit_incon, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_incon)

# Model summary
summary(fit_incon)

# Save model
save(fit_incon, file = "fit_incon_hr.RData")

# Plot: b_polytcoredegreeEQ21
gather_draws(fit_incon, b_polyheart_ratedegreeEQ21) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")

# Plot: b_polytcoredegreeEQ22
gather_draws(fit_incon, b_polyheart_ratedegreeEQ22) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")






#### thermal_comfort
# Congruent formula
incongruent_formula = incon_ies ~ 
  incon_ies_first + 
  time +
  poly(thermal_comfort, degree = 2) +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1+poly(thermal_comfort, degree = 2)|id)

# Fit congruent model
fit_incon <- brm_multiple(
  formula = incongruent_formula,
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
round(fit_incon$rhats, 3)

# Predictive check and chains
pp_check(fit_incon, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_incon)

# Model summary
summary(fit_incon)

# Save model
save(fit_incon, file = "fit_incon_thermal_comfort.RData")

# Plot: b_polytcoredegreeEQ21
gather_draws(fit_incon, b_polythermal_comfortdegreeEQ21) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")

# Plot: b_polytcoredegreeEQ22
gather_draws(fit_incon, b_polythermal_comfortdegreeEQ22) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 1, color = "red")



#### END