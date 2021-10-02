

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
  filter(Time_name %in% c('Pre')) %>%
  select(ID, Time, WBGT_name, Suit, Tcore, Glucose_2, Blood_Osmo_2, Con_IES_first, Incon_IES_first, MD_incon_IES, MD_con_IES,
         RPE, Thermal_comfort, Mean_Tsk, Heart_rate) %>%
  clean_names() %>%
  mutate(
    glucose = glucose_2,
    blood_osmo = blood_osmo_2,
    wbgt = wbgt_name,
    incon_ies = md_incon_ies,
    con_ies = md_con_ies
  )

# Glucose
d %>% summarise(mu = mean(glucose, na.rm = T),
                sd = sd(glucose, na.rm = T)) %>%
  print(digits = 1)

# Blood osmo
d %>% summarise(mu = mean(blood_osmo*1000, na.rm = T),
                sd = sd(blood_osmo*1000, na.rm = T)) %>%
  print(digits = 0)

# Congruent IES CV
d %>% group_by(id) %>%
  summarise(mu = mean(con_ies, na.rm = T),
            sd = sd(con_ies, na.rm = T)) %>%
  mutate(cv_i = (sd/mu)*100) %>%
  summarise(mu = mean(cv_i, na.rm = T),
            sd = sd(cv_i, na.rm = T))

# Incongruent IES CV
d %>% group_by(id) %>%
  summarise(mu = mean(incon_ies, na.rm = T),
            sd = sd(incon_ies, na.rm = T)) %>%
  mutate(cv_i = (sd/mu)*100) %>%
  summarise(mu = mean(cv_i, na.rm = T),
            sd = sd(cv_i, na.rm = T))



#### END