

# Cognitive analysis
# David N Borg
# June 2021

# Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(brms)
library(modelr)
library(tidybayes)
library(cowplot)

# Load model
load("~/Downloads/Project - Effect of heat on cognition/fit_con.RData")

# Effect: b_con_ies_first
effect = gather_draws(fit_con, b_con_ies_first) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_time
effect = gather_draws(fit_con, b_time) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_polytcoredegreeEQ21
effect = gather_draws(fit_con, b_polytcoredegreeEQ21) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_polytcoredegreeEQ22
effect = gather_draws(fit_con, b_polytcoredegreeEQ22) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_glucose
effect = gather_draws(fit_con, b_glucose) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_blood_osmo
effect = gather_draws(fit_con, b_blood_osmo) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit2
effect = gather_draws(fit_con, b_suit2) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit4
effect = gather_draws(fit_con, b_suit4) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_wbgt30
effect = gather_draws(fit_con, b_wbgt30) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_wbgt37
effect = gather_draws(fit_con, b_wbgt37) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit2:wbgt30
effect = gather_draws(fit_con,`b_suit2:wbgt30`) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit4:wbgt30
effect = gather_draws(fit_con, `b_suit4:wbgt30`) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect)
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit2:wbgt37
effect = gather_draws(fit_con, `b_suit2:wbgt37`) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)

# Effect: b_suit4:wbgt37
effect = gather_draws(fit_con, `b_suit4:wbgt37`) %>%
  mutate(effect = .value)
effect %>% ggplot() + geom_histogram(aes(x = effect))
effect %>% mean_qi(effect>0)
effect %>% mean_qi(effect<0)



#### END