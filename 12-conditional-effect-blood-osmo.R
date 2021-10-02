

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

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  filter(Time_name %in% c('30','60','90','Final')) %>%
  select(ID, Time, WBGT_name, Suit, Tcore, Glucose_2, Blood_Osmo_2, Con_IES_first, Incon_IES_first, MD_incon_IES, MD_con_IES,
         RPE, Thermal_comfort, Mean_Tsk, Heart_rate) %>%
  clean_names() %>%
  mutate(
    glucose = glucose_2,
    blood_osmo = blood_osmo_2,
    wbgt = wbgt_name,
    incon_ies = md_incon_ies,
    con_ies = md_con_ies
  ) %>%
  select(-glucose_2,-blood_osmo_2,-wbgt_name,-md_incon_ies,-md_con_ies) %>%
  mutate(
    time = as.numeric(scale(time, center = T, scale = T)),
    glucose = as.numeric(scale(glucose, center = T, scale = T)),
    blood_osmo = as.numeric(scale(blood_osmo, center = T, scale = T)),
    tcore = as.numeric(scale(tcore, center = T, scale = T)),
    con_ies_first = as.numeric(scale(con_ies_first, center = T, scale = T)),
    incon_ies_first = as.numeric(scale(incon_ies_first, center = T, scale = T)),
    mean_tsk = as.numeric(scale(mean_tsk, center = T, scale = T)),
    rpe = as.numeric(scale(rpe, center = T, scale = T)),
    heart_rate = as.numeric(scale(heart_rate, center = T, scale = T)),
    thermal_comfort = as.numeric(scale(thermal_comfort, center = T, scale = T)),
    wbgt = as.factor(wbgt),
    suit = as.factor(suit)
  )


# Load model: Congruent
load("~/Downloads/Project - Effect of heat on cognition/fit_con.RData")

# Plot
me = conditional_effects(fit_con, prob = 0.95)
df_95 = me[[5]] %>%
  as.data.frame()

me = conditional_effects(fit_con, prob = 0.66)
df_66 = me[[5]] %>%
  as.data.frame()

# Back transform tcore
dat = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names()
mu = mean(dat$blood_osmo, na.rm = T)
sigma = sd(dat$blood_osmo, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__)) +
  #geom_point(data = d, aes(x = blood_osmo, y = con_ies)) +
  scale_y_continuous(limits = c(625,850), n.breaks = 8) +
  scale_x_continuous(n.breaks = 5) +
  labs(x = "Serum Osmolality (mmol/kg)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Serum Osmolality on\n Congruent Performance") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_con
plot_con







# Load model: Incongruent
load("~/Downloads/Project - Effect of heat on cognition/fit_incon.RData")

# Plot
me = conditional_effects(fit_incon, prob = 0.95)
df_95 = me[[5]] %>%
  as.data.frame()

me = conditional_effects(fit_incon, prob = 0.66)
df_66 = me[[5]] %>%
  as.data.frame()

# Back transform tcore
dat = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names()
mu = mean(dat$blood_osmo, na.rm = T)
sigma = sd(dat$blood_osmo, na.rm = T)

# Plot
ggplot() +
  geom_ribbon(data = df_95, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_ribbon(data = df_66, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_line(data = df_95, aes(x = ((blood_osmo*sigma)+mu)*1000, y = estimate__)) +
  #geom_point(data = d, aes(x = blood_osmo, y = incon_ies)) +
  scale_y_continuous(limits = c(625,850), n.breaks = 8) +
  scale_x_continuous(n.breaks = 5) +
  labs(x = "Serum Osmolality (mmol/kg)", y = "Inverse Efficiency Score (ms)") +
  facet_grid(~"Effect of Serum Osmolality on\n Incongruent Performance") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_incon
plot_incon



# Panel of conditional effects
plot_grid(plot_con,
          plot_incon,
          ncol = 2, 
          nrow = 1,
          scale = 0.95, 
          align = 'v', 
          axis = "lr",
          labels = c('A','B'),
          label_size = 16)
ggsave(file = "marginal-effect-serum-osmo-plot.png", units="in", width = 8, height = 3.5, dpi = 300)



#### END