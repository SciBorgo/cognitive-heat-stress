

# Cognitive analysis
# David N Borg
# July 2021

# Libraries
library(dplyr)
library(janitor)
library(ggplot2)
library(naniar)
library(visdat)
library(tidyverse)
library(conflicted)
library(cowplot)

# Conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("filter","select")

# setwd
setwd("~/Downloads/Project - Effect of heat on cognition")

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  select(ID, Time, WBGT_name, Suit, Tcore, Glucose_2, Blood_Osmo_2, Con_IES_first, Incon_IES_first, MD_incon_IES, MD_con_IES,
         RPE, Thermal_comfort, Mean_Tsk, Heart_rate) %>%
  clean_names() %>%
  mutate(
    glucose = glucose_2,
    serum_osmo = blood_osmo_2*1000,
    wbgt = as.factor(wbgt_name),
    incon_ies = md_incon_ies,
    con_ies = md_con_ies,
    ensemble = suit
  ) %>%
  select(-glucose_2,-blood_osmo_2,-wbgt_name,-md_incon_ies,-md_con_ies,-con_ies_first,-incon_ies_first,-rpe,-suit) %>%
  select(id,wbgt,ensemble,tcore,glucose,serum_osmo,heart_rate, mean_tsk, incon_ies, con_ies)

names(d)  

# Figure
p_vis_miss = d %>% arrange(desc(tcore)) %>%
  vis_miss() +
  theme(text = element_text(size = 8))

p_fct = gg_miss_fct(d, wbgt) + 
  theme(text = element_text(size = 8)) +
  labs(x = "Wet Bulb Globe Temperature", y = "Variable")

p_scatter_con <- ggplot(d, aes(x = serum_osmo, y = con_ies)) +
  geom_miss_point(size = 2, alpha = 0.65) +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  labs(x = "Serum Osmolality", 
       y = "Congruent Inverse Efficiency Score", 
       colour = "")

p_scatter_incon <- ggplot(d, aes(x = serum_osmo, y = incon_ies)) +
  geom_miss_point(size = 2, alpha = 0.65) +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  labs(x = "Serum Osmolality", 
       y = "Incongruent Inverse Efficiency Score", 
       colour = "")

# Panel
plot_grid(p_vis_miss, p_fct, p_scatter_con, p_scatter_incon,
          ncol = 2,
          nrow = 2,
          align = 'v',
          axis = "lr",
          scale = 0.95,
          labels = c('A','B','C','D'),
          label_size = 12)
ggsave(file = "supplement-figure-missing.png", units="in", width = 10, height = 6, dpi = 600)



#### END