

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
library(mice)
library(conflicted)

# Conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("filter","select")

# setwd
setwd("~/Downloads/Project - Effect of heat on cognition")

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  filter(Time_name %in% c('30','60','90','Final')) %>%
  select(ID, Time, WBGT_name, Suit, Tcore, Glucose_2, Blood_Osmo_2, Con_IES_first, Incon_IES_first, MD_incon_IES, MD_con_IES,
         RPE, Thermal_comfort, Mean_Tsk, Heart_rate) %>%
  clean_names() %>%
  mutate(
    id = as.factor(id),
    glucose = glucose_2,
    serum_osmo = blood_osmo_2*1000,
    ensemble = as.factor(suit),
    wbgt = as.factor(wbgt_name),
    con_ies = md_con_ies,
    incon_ies = md_incon_ies
  ) %>%
  select(-glucose_2,-blood_osmo_2,-suit,-md_con_ies,-md_incon_ies,-wbgt_name)

names(d)  

# Exploratory plots: congruent
d %>% ggplot(aes(x = wbgt, y = con_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Wet Bulb Globe Temperature (°C)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Wet Bulb Globe Temperature") +
  theme(strip.text.x = element_text(size = 8)) -> p1

d %>% mutate(Ensemble = recode_factor(ensemble, '1' = 'ICG', '2' = 'ERS', '4' = 'CPCS')) %>% 
  ggplot(aes(x = Ensemble, y = con_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Protective Ensemble", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Ensemble") +
  theme(strip.text.x = element_text(size = 8)) -> p2

d %>% mutate(Ensemble = recode_factor(ensemble, '1' = 'ICG', '2' = 'ERS', '4' = 'CPCS'),
             wbgt = recode_factor(wbgt, '21' = 'WBGT 21', '30' = 'WBGT 30', '37' = 'WBGT 37')) %>% 
  ggplot(aes(x = Ensemble, y = con_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.25, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Protective Ensemble", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~wbgt) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) -> p3

d %>% ggplot(aes(x = serum_osmo, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Serum Osmolality (mmol/kg)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Serum Osmolality") +
  theme(strip.text.x = element_text(size = 8)) -> p4

d %>% ggplot(aes(x = glucose, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Blood Glucose (mmol/L)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Glucose") +
  theme(strip.text.x = element_text(size = 8)) -> p5

d %>% ggplot(aes(x = tcore, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Core Body Temperature (°C)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Core Body Temperature")  +
  theme(strip.text.x = element_text(size = 8)) -> p6

d %>% ggplot(aes(x = time, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Time (min)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Time") +
  theme(strip.text.x = element_text(size = 8)) -> p7

d %>% ggplot(aes(x = heart_rate, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = expression(Heart~Rate~(b~min^{-1})), y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Heart Rate") +
  theme(strip.text.x = element_text(size = 8)) -> p8

d %>% ggplot(aes(x = mean_tsk, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Mean Skin Temperature (°C)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Mean Skin Temperature") +
  theme(strip.text.x = element_text(size = 8)) -> p9

d %>% ggplot(aes(x = rpe, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Rating of Perceived Exertion (6-20)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Rating of Perceived Exertion") +
  scale_x_continuous(n.breaks = 9) +
  theme(strip.text.x = element_text(size = 8)) -> p10

d %>% ggplot(aes(x = thermal_comfort, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Thermal Comfort (1-5)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Thermal Comfort") +
  scale_x_continuous(n.breaks = 9) +
  theme(strip.text.x = element_text(size = 8)) -> p11

d %>% ggplot(aes(x = con_ies_first, y = con_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "First Congruent\nInverse Efficiency Score (ms)", y = "Congruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"First Congruent Performance") +
  theme(strip.text.x = element_text(size = 8)) -> p12

# Panel
plot_grid(p1,p2,p3,p12,p4,p5,p6,p7,p8,p9,p10,p11,
          ncol = 3,
          nrow = 4,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          #labels = c('A','B','C','D','E','F','G','H','I','J','K','L'),
          label_size = 12)
ggsave(file = "exploratory-plots-congruent.png", units="in", width = 8, height = 8, dpi = 300)











# Exploratory plots: incongruent

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  filter(Time_name %in% c('30','60','90','Final')) %>%
  select(ID, Time, WBGT_name, Suit, Tcore, Glucose_2, Blood_Osmo_2, Con_IES_first, Incon_IES_first, MD_incon_IES, MD_con_IES,
         RPE, Thermal_comfort, Mean_Tsk, Heart_rate) %>%
  clean_names() %>%
  mutate(
    id = as.factor(id),
    glucose = glucose_2,
    serum_osmo = blood_osmo_2*1000,
    ensemble = as.factor(suit),
    wbgt = as.factor(wbgt_name),
    con_ies = md_con_ies,
    incon_ies = md_incon_ies
  ) %>%
  select(-glucose_2,-blood_osmo_2,-suit,-md_con_ies,-md_incon_ies,-wbgt_name)

d %>% ggplot(aes(x = wbgt, y = incon_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Wet Bulb Globe Temperature (°C)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Wet Bulb Globe Temperature") +
  theme(strip.text.x = element_text(size = 8)) -> p1

d %>% mutate(Ensemble = recode_factor(ensemble, '1' = 'ICG', '2' = 'ERS', '4' = 'CPCS')) %>% 
  ggplot(aes(x = Ensemble, y = incon_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Protective Ensemble", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Ensemble") +
  theme(strip.text.x = element_text(size = 8)) -> p2

d %>% mutate(Ensemble = recode_factor(ensemble, '1' = 'ICG', '2' = 'ERS', '4' = 'CPCS'),
             wbgt = recode_factor(wbgt, '21' = 'WBGT 21', '30' = 'WBGT 30', '37' = 'WBGT 37')) %>% 
  ggplot(aes(x = Ensemble, y = incon_ies)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.25, alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Protective Ensemble", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~wbgt) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) -> p3

d %>% ggplot(aes(x = serum_osmo, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Serum Osmolality (mmol/kg)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Serum Osmolality") +
  theme(strip.text.x = element_text(size = 8)) -> p4

d %>% ggplot(aes(x = glucose, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Blood Glucose (mmol/L)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Glucose") +
  theme(strip.text.x = element_text(size = 8)) -> p5

d %>% ggplot(aes(x = tcore, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Core Body Temperature (°C)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Core Body Temperature")  +
  theme(strip.text.x = element_text(size = 8)) -> p6

d %>% ggplot(aes(x = time, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Time (min)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Time") +
  theme(strip.text.x = element_text(size = 8)) -> p7

d %>% ggplot(aes(x = heart_rate, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = expression(Heart~Rate~(b~min^{-1})), y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Heart Rate") +
  theme(strip.text.x = element_text(size = 8)) -> p8

d %>% ggplot(aes(x = mean_tsk, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Mean Skin Temperature (°C)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Mean Skin Temperature") +
  theme(strip.text.x = element_text(size = 8)) -> p9

d %>% ggplot(aes(x = rpe, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Rating of Perceived Exertion (6-20)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Rating of Perceived Exertion") +
  scale_x_continuous(n.breaks = 9) +
  theme(strip.text.x = element_text(size = 8)) -> p10

d %>% ggplot(aes(x = thermal_comfort, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "Thermal Comfort (1-5)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"Thermal Comfort") +
  scale_x_continuous(n.breaks = 9) +
  theme(strip.text.x = element_text(size = 8)) -> p11

d %>% ggplot(aes(x = incon_ies_first, y = incon_ies)) +
  geom_point(alpha = 0.35) +
  theme_bw(base_size = 8) +
  labs(x = "First Incongruent\nInverse Efficiency Score (ms)", y = "Incongruent\nInverse Efficiency Score (ms)") +
  facet_wrap(~"First Incongruent Performance") +
  theme(strip.text.x = element_text(size = 8)) -> p12

# Panel
plot_grid(p1,p2,p3,p12,p4,p5,p6,p7,p8,p9,p10,p11,
          ncol = 3,
          nrow = 4,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          #labels = c('A','B','C','D','E','F','G','H','I','J','K','L'),
          label_size = 12)
ggsave(file = "exploratory-plots-incongruent.png", units="in", width = 8, height = 8, dpi = 300)



#### END