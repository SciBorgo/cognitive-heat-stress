

# Cognitive analysis
# David N Borg
# June 2021

# Libraries
library(dplyr)
library(janitor)
library(conflicted)

# Conflicts
conflict_prefer("filter","dplyr")
conflict_prefer("filter","select")

# Load data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names()

# Summarise blood osmo
d %>% summarise(
  mean = mean(blood_osmo, na.rm = T),
  sd = sd(blood_osmo, na.rm = T)
  )

# Nude mass loss over the trials
d = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names() %>%
  filter(time_name %in% c("Pre","Final")) %>%
  select(id, wbgt_name, suit_name, time_name, nude_mass)

d_pre = d %>% 
  filter(time_name == "Pre") %>% 
  mutate(pre_mass = nude_mass) %>% 
  select(-nude_mass)

d_final = d %>% 
  filter(time_name == "Final") %>% 
  mutate(final_mass = nude_mass) %>% 
  select(-nude_mass)

df = merge(d_pre, d_final, by = c("id", "wbgt_name", "suit_name"))

d_mass = df %>% mutate(mass_diff = final_mass-pre_mass,
         bm_pc_change = ((pre_mass-final_mass)/pre_mass)*100)

hist(d_mass$mass_diff, breaks = 20)
hist(d_mass$bm_pc_change, breaks = 20)

d_mass %>% summarise(median_loss = median(mass_diff),
            quant_25_loss = quantile(mass_diff, prob = c(0.25)),
            quant_75_loss = quantile(mass_diff, prob = c(0.75)),
            median_pc = median(bm_pc_change),
            quant_25_pc = quantile(bm_pc_change, prob = c(0.25)),
            quant_75_pc = quantile(bm_pc_change, prob = c(0.75)))

d_mass %>% mutate(greater_2_pc = cut(bm_pc_change, breaks = c(-10,2,10))) %>%
  group_by(greater_2_pc) %>%
  summarise(count = n())



#### END