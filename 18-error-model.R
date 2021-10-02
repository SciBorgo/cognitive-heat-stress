

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
library(tidybayes)
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

# Load
data = read_csv("cognitive-errors.csv")

d = data %>%
  mutate(
    time = as.numeric(scale(time, center = T, scale = T)),
    glucose = as.numeric(scale(glucose, center = T, scale = T)),
    blood_osmo = as.numeric(scale(blood_osmo, center = T, scale = T)),
    tcore = as.numeric(scale(tcore, center = T, scale = T)),
    wbgt = as.factor(wbgt),
    suit = as.factor(suit),
    error_prop = error/measureable_responses
  )

# Imputation
imp_datasets <- mice(d, m = 5, method = "pmm", seed = 123)

stripplot(imp_datasets, glucose, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, blood_osmo, pch = 19, xlab = "Imputation number")

# Look at errors
d %>% ggplot() + geom_histogram(aes(x = error_prop), bins = 10)
d %>% ggplot() + geom_histogram(aes(x = error_prop), bins = 100)
table(d$error_prop)

# Error versus Tcore
d %>% ggplot(aes(y = error_prop, x = tcore)) + geom_point() + stat_smooth()
d %>% ggplot(aes(y = error_prop, x = tcore)) + geom_point() + stat_smooth(method = "lm")

d %>% ggplot() + geom_point(aes(y = tcore, x = error_prop))
d %>% filter(error_prop>0) %>% ggplot() + geom_point(aes(y = error_prop, x = tcore))
d %>% filter(error_prop>0) %>% ggplot() + geom_point(aes(y = error_prop, x = blood_osmo))
d %>% filter(error_prop>0) %>% ggplot() + geom_point(aes(y = error_prop, x = time))
d %>% filter(error_prop>0) %>% ggplot() + geom_jitter(aes(y = error_prop, x = wbgt), width = 0.1)
d %>% filter(error_prop>0) %>% ggplot() + geom_jitter(aes(y = error_prop, x = suit), width = 0.1)
d %>% filter(error_prop>0) %>% ggplot() + geom_jitter(aes(y = error_prop, x = order), width = 0.1)
d %>% ggplot() + geom_boxplot(aes(y = tcore, x = as.factor(error_prop)))

d %>% filter(error_prop>0) %>%
  filter(error_prop<0.12) %>%
  ggplot(aes(y = error_prop, x = tcore)) + 
  geom_point() +
  stat_smooth()

# Error formula
error_formula = error_prop ~ 
  time +
  tcore +
  glucose +
  blood_osmo +
  suit*wbgt +
  (1|id)

# Fit betareg model for errors
fit_error <- brm_multiple(
  formula = error_formula,
  family = zero_inflated_beta(link = "logit"),
  prior = set_prior("normal(0,1)", class = "b"),
  cores = 8, 
  chains = 8,
  iter = 10000,
  thin = 5,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  seed = 123,
  data = imp_datasets)

# Check Rhats
round(fit_error$rhats, 3)

# Predictive check and chains
pp_check(fit_error, re_formula = NULL, nsamples = 100)

# Chain convergence
plot(fit_error)

# Model summary
summary(fit_error)

# Save model
save(fit_error, file = "fit_error.RData")

# Conditional effects
conditional_effects(fit_error)


load("/Downloads/Project - Effect of heat on cognition/fit_error.RData")

# Plot
me = conditional_effects(fit_error, prob = 0.95)
df_95 = me[[2]] %>%
  as.data.frame()

me = conditional_effects(fit_error, prob = 0.66)
df_66 = me[[2]] %>%
  as.data.frame()

# Back transform tcore
dat = read_csv("cognitive-errors.csv") %>%
  clean_names()
mu = mean(dat$tcore, na.rm = T)
sigma = sd(dat$tcore, na.rm = T)

# Plot
library(scales)
ggplot() +
  geom_ribbon(data = df_95, aes(x = ((tcore*sigma)+mu), y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = ((tcore*sigma)+mu), y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = ((tcore*sigma)+mu), y = estimate__)) +
  #scale_y_continuous(limits = c(625,850), n.breaks = 8) +
  scale_y_continuous(limits = c(0,0.035), labels = percent_format(accuracy = 1)) +
  labs(x = "Core Body Temperature", y = "Proportion of Errors") +
  facet_grid(~"Marginal Effect of Core Body Temperature on\n the Proportion of Errors") +
  theme_bw(base_size = 10) +
  theme(strip.text.x = element_text(size = 10))
ggsave(file = "tcore-errors.png", units="in", width = 5, height = 3.5, dpi = 600)



#### END