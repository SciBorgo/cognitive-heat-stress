

# Cognitive analysis
# David N Borg
# June 2021

# Libraries
library(dplyr)
library(tidyverse)
library(janitor)

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names() %>%
  drop_na(md_con_ies)

table(d$time_name)

# Break down of trials
d %>%
  filter(time_name == "Pre") %>%
  group_by(wbgt_name,suit_name) %>%
  summarise(count = n())

d %>%
  filter(time_name == "Pre") %>%
  group_by(id,wbgt_name,suit_name,termination_reason_2) %>%
  summarise(count = n()) %>%
  write.csv(file = "supplement1.csv")


# Data under each cognitive condition
d_con = d %>% drop_na(md_con_ies)
nrow(d_con)

d_incon = d %>% drop_na(md_incon_ies)
nrow(d_incon)

# Median (iqr) data per id
d_con %>%
  select(id, md_con_ies) %>%
  group_by(id) %>%
  count() %>%
  summary()

d_incon %>%
  select(id, md_incon_ies) %>%
  group_by(id) %>%
  count() %>%
  summary()


# Number of trials
d = read.csv("cognitive-data-01-10-21.csv") %>%
  filter(Time_name %in% c('Pre')) %>%
  clean_names() %>%
  select(id, suit, wbgt_name, termination_reason_2)

d %>% group_by(id, suit, wbgt_name, termination_reason_2) %>%
  summarise(across(count = n())) %>%
  View()

d_tri_sum = d %>% group_by(id, suit, wbgt_name, termination_reason_2) %>%
  summarise(across(count = n())) 

# Trials per person
d_tri_sum %>%
  group_by(id) %>%
  summarise(count = n()) 

d_tri_sum %>%
  group_by(id) %>%
  summarise(count = n()) %>%
  summary()


