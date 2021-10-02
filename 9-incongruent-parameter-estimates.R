

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
library(cowplot)

# Load model
load("~/Downloads/Project - Effect of heat on cognition/fit_incon.RData")

# Set plot components
cog_theme = theme(panel.grid.major.x=element_blank(),
                  panel.grid.minor.x=element_blank(),
                  panel.grid.major.y=element_blank(),
                  panel.grid.minor.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  strip.text.x = element_text(size = 10))
cog_labs = labs(x = "Estimate", y = "Density")
cog_line = geom_vline(xintercept = 0, linetype = 1, color = "gray10")

# Reduce plot size
plotly_build2 <- function(...) {
  p <- plotly_build(...)
  p$x[c("attrs", "visdat", "cur_data")] <- NULL
  p
}

# Plot: b_con_ies_first
gather_draws(fit_incon, b_incon_ies_first) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Initial IES")-> plot_initial_ies

# Plot: b_time
gather_draws(fit_incon, b_time) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Time") -> plot_time

# Plot: b_polytcoredegreeEQ21
gather_draws(fit_incon, b_polytcoredegreeEQ21) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Core Body Temperature (degree = 1)") -> plot_tcore1

# Plot: b_polytcoredegreeEQ22
gather_draws(fit_incon, b_polytcoredegreeEQ22) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Core Body Temperature (degree = 2)") -> plot_tcore2

# Plot: b_glucose
gather_draws(fit_incon, b_glucose) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Glucose") -> plot_glucose

# Plot: b_blood_osmo
gather_draws(fit_incon, b_blood_osmo) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Serum Osmolality") -> plot_osmo

# Plot: b_suit2
gather_draws(fit_incon, b_suit2) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (ERS)") -> plot_suit1

# Plot: b_suit4
gather_draws(fit_incon, b_suit4) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (CPCS)") -> plot_suit2

# Plot: b_wbgt30
gather_draws(fit_incon, b_wbgt30) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"WBGT (30 °C)") -> plot_wbgt1

# Plot: b_wbgt37
gather_draws(fit_incon, b_wbgt37) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"WBGT (37 °C)") -> plot_wbgt2

# Plot: b_suit2:wbgt30
gather_draws(fit_incon, `b_suit2:wbgt30`) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (ERS) by WBGT (30 °C)") -> plot_suit_wbgt1

# Plot: b_suit4:wbgt30
gather_draws(fit_incon, `b_suit4:wbgt30`) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (CPCS) by WBGT (30 °C)") -> plot_suit_wbgt2

# Plot: b_suit2:wbgt37
gather_draws(fit_incon, `b_suit2:wbgt37`) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (ERS) by WBGT (37 °C)") -> plot_suit_wbgt3

# Plot: b_suit4:wbgt37
gather_draws(fit_incon, `b_suit4:wbgt37`) %>% 
  mutate(effect = .value) %>%
  ggplot() + 
  stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + 
  theme_bw(base_size = 10) + 
  cog_theme + 
  cog_labs + 
  cog_line +
  facet_grid(~"Ensemble (CPCS) by WBGT (37 °C)") -> plot_suit_wbgt4

# Panel of effects (logit)
plot_grid(plot_initial_ies,
          plot_time,
          plot_tcore1,
          plot_tcore2,
          plot_glucose,
          plot_osmo,
          plot_suit1,
          plot_suit2,
          plot_wbgt1,
          plot_wbgt2,
          plot_suit_wbgt1,
          plot_suit_wbgt2,
          plot_suit_wbgt3,
          plot_suit_wbgt4,
          ncol = 3, 
          nrow = 5,
          scale = 0.95, 
          align = 'v', axis = "lr")
ggsave(file = "incongruent-parameter-estimate-plot.png", units="in", width = 9.5, height = 12, dpi = 300)



#### END