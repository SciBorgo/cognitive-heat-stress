

# Cognitive analysis
# David N Borg
# July 2021

# Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(brms)
library(modelr)
library(tidybayes)
library(cowplot)
library(janitor)

# Data for back transformations
dat = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names() %>%
  filter(time_name %in% c('30','60','90','Final'))

# Congruent plots

# Heart rate: congruent
load("~/Downloads/Project - Effect of heat on cognition/fit_con_hr.RData")

me = conditional_effects(fit_con, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_con, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Back transform hr

mu1 = mean(dat$heart_rate, na.rm = T)
sigma1 = sd(dat$heart_rate, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (heart_rate*sigma1)+mu1, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (heart_rate*sigma1)+mu1, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (heart_rate*sigma1)+mu1, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 7) +
  labs(x = expression(Heart~Rate~(b~min^{-1})), y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Heart Rate on\n Congruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p1

# Heart rate: incongruent
load("~/Downloads/Project - Effect of heat on cognition/fit_incon_hr.RData")

me = conditional_effects(fit_incon, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_incon, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (heart_rate*sigma1)+mu1, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (heart_rate*sigma1)+mu1, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (heart_rate*sigma1)+mu1, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 7) +
  labs(x = expression(Heart~Rate~(b~min^{-1})), y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Heart Rate on\n Incongruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p2





# Mean skin temperate: congruent
load("~/Downloads/Project - Effect of heat on cognition/fit_con_mtsk.RData")

me = conditional_effects(fit_con, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_con, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Back transform mean skin temperature
mu2 = mean(dat$mean_tsk, na.rm = T)
sigma2 = sd(dat$mean_tsk, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 7) +
  labs(x = "Mean Skin Temperature (°C)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Mean Skin Temperature on\n Congruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p3

# Mean skin temperature: incongruent
load("~/Downloads/Project - Effect of heat on cognition/fit_incon_mtsk.RData")

me = conditional_effects(fit_incon, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_incon, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (mean_tsk*sigma2)+mu2, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 7) +
  labs(x = "Mean Skin Temperature (°C)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Mean Skin Temperature on\n Incongruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p4






# RPE: congruent
load("~/Downloads/Project - Effect of heat on cognition/fit_con_rpe.RData")

me = conditional_effects(fit_con, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_con, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Back transform rpe
mu3 = mean(dat$rpe, na.rm = T)
sigma3 = sd(dat$rpe, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (rpe*sigma3)+mu3, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (rpe*sigma3)+mu3, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (rpe*sigma3)+mu3, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "RPE (6-20)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of RPE on\n Congruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p5

# RPE: incongruent
load("~/Downloads/Project - Effect of heat on cognition/fit_incon_rpe.RData")

me = conditional_effects(fit_incon, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_incon, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (rpe*sigma3)+mu3, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (rpe*sigma3)+mu3, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (rpe*sigma3)+mu3, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "RPE (6-20)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of RPE on\n Incongruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p6






# Thermal comfort: congruent
load("~/Downloads/Project - Effect of heat on cognition/fit_con_thermal_comfort.RData")

me = conditional_effects(fit_con, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_con, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Back transform thermal comfort
mu4 = mean(dat$thermal_comfort, na.rm = T)
sigma4 = sd(dat$thermal_comfort, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "Thermal Comfort (1-5)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Thermal Comfort on\n Congruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p7

# Thermal comfort: incongruent
load("~/Downloads/Project - Effect of heat on cognition/fit_incon_thermal_comfort.RData")

me = conditional_effects(fit_incon, prob = 0.95)
df_95 = me[[3]] %>%
  as.data.frame()

me = conditional_effects(fit_incon, prob = 0.66)
df_66 = me[[3]] %>%
  as.data.frame()

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = (thermal_comfort*sigma4)+mu4, y = estimate__)) +
  scale_y_continuous(limits = c(650,850), n.breaks = 6) +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "Thermal Comfort (1-5)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Thermal Comfort on\n Incongruent Performance") +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) -> p8



# Panel plots
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,
          ncol = 2, 
          nrow = 4,
          scale = 0.95, 
          align = 'v',
          axis = "lr",
          labels = c('A','B','C','D','E','F','G','H'),
          label_size = 12)
ggsave(file = "figure-other-variables.png", units="in", width = 6.5, height = 9, dpi = 300)



#### END