

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

names(d)  

# Missing data
vis_miss(d)
gg_miss_upset(d)

missing_vars <- sapply(d, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

missing_vars

# Impute
imp_datasets <- mice(d, m = 5, method = "pmm", seed = 123)

# Look at imputations
stripplot(imp_datasets, con_ies_first, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, incon_ies_first, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, rpe, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, thermal_comfort, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, glucose, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, blood_osmo, pch = 19, xlab = "Imputation number")

# Save
saveRDS(imp_datasets, file = "imputed-data-pmm.rds")



#### END