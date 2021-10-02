

# Cognitive analysis
# David N Borg
# July 2021

# Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(naniar)

# Merge data
d = read.csv("cognitive-data-01-10-21.csv") %>%
  clean_names() %>%
  drop_na(md_con_ies)

d_err = read.csv("data-cognitive-errors-01-10-21.csv") %>%
  clean_names() %>%
  mutate(prop_err = error/measureable_responses,
         error_made = cut(x = prop_err, breaks = c(-1,0,1)),
         error_made_5pc = cut(x = prop_err, breaks = c(-1,0.05,1))) %>%
  drop_na(prop_err)

# Merge
dat = merge(d, d_err, by = c("id","suit_name","wbgt_name","time_name")) %>%
  filter(time_name %in% c('First','30','60','90','Final'))

# Error range
table(dat$prop_err)

# Tests with errors
dat %>% group_by(error_made) %>%
  summarise(count = n())

# Tests with errors above 5%
dat %>% group_by(error_made_5pc) %>%
  summarise(count = n())


# Merge
names(d_err)
names(d_rt)

dat = merge(d_err, d_rt, by = c("id","suit_name","wbgt_name","time_name")) %>%
  mutate(prop_err = error/measureable_responses)

# Merge reaction time and errors with inverse efficiency scores
df = merge(dat, d_ies, by = c("id","suit_name","wbgt_name","time_name")) 




vis_miss(df) # Missing data
length(unique(df$id))

# Congruent
df %>% ggplot() +
  geom_point(aes(x = tcore, y = congruent_including_incorrect, colour = "red")) +
  geom_point(aes(x = tcore, y = md_con_ies)) +
  geom_smooth(aes(x = tcore, y = congruent_including_incorrect, colour = "red")) +
  geom_smooth(aes(x = tcore, y = md_con_ies))
  theme_bw()

cor.test(df$prop_err, df$congruent_including_incorrect)

# Correlation: incongruent
df %>%
  ggplot(aes(x = incongruent_including_incorrect, y = prop_err)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm")

cor.test(df$prop_err, df$incongruent_including_incorrect)

  
  
#### END