library(tidyverse)
library(RPostgres)
library(fixest)
library(e1071)
library(kableExtra)
library(ggthemes)
library(patchwork)
library(did)
library(furrr)
library(latex2exp)
library(bacondecomp)
library(ggforce)
library(fastDummies)
library(progressr)

# set plot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load in compustat data
comp <- read_rds(here::here("Data", "simulation_data.rds"))
estimates <- read_rds(here::here("Data", "estimates.rds"))

## Make plots for alternative estimators
# make function
make_alt_dist_plot <- function(i, name) {
  estimates %>% 
    filter(dt == i) %>% 
    pivot_longer(cols = c(cs, stacked, sa)) %>% 
    mutate(name = case_when(
      name == "cs" ~ "Callaway & Sant'Anna",
      name == "sa" ~ "Sun & Abraham",
      TRUE ~ "Stacked"
    )) %>% 
    ggplot(aes(x = value, group = name, fill = name)) + 
    geom_density(alpha = 1/3) + 
    scale_fill_manual(values = c("#115896", "#719E56", "#BA2F00")) + 
    geom_vline(aes(xintercept = mean(estimates %>% filter(dt == i) %>% pull(te_5_1)),
                   color = "Observation Average"),
               linetype = "dashed", size = 1, alpha = 3/5,) +
    geom_vline(aes(xintercept = mean(estimates %>% filter(dt == i) %>% pull(te_5_2)),
                   color = "Firm Average"),
               linetype = "dashed", size = 1, alpha = 3/5) + 
    scale_color_manual(name = "", values = c("Observation Average" = "#A7473A",
                                             "Firm Average" = "#4B5F6C")) + 
    ggtitle(paste0("Simulation ", i)) + 
    labs(subtitle = TeX(paste0(name, "$\\delta$"))) +
    labs(y = "", x = if_else(i == 5, expression(widehat(delta^'DD')), expression(""))) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
          legend.position = if_else(i == 5, "bottom", "none"),
          legend.title = element_blank())
}

# run all the densities
sim1_alt_estimates <- make_alt_dist_plot(1, "Not Staggered + Constant ")
sim2_alt_estimates <- make_alt_dist_plot(2, "Not Staggered + Dynamic ")
sim3_alt_estimates <- make_alt_dist_plot(3, "Staggered + Constant/Equal ")
sim4_alt_estimates <- make_alt_dist_plot(4, "Staggered + Constant/Unequal ")
sim5_alt_estimates <- make_alt_dist_plot(5, "Staggered + Dynamic/Equal ")
sim6_alt_estimates <- make_alt_dist_plot(6, "Staggered + Dynamic/Unequal ")

# save
Sims_1_6_alt_dist <- (sim1_alt_estimates + sim2_alt_estimates + sim3_alt_estimates) /
  (sim4_alt_estimates + sim5_alt_estimates + sim6_alt_estimates)

ggsave(Sims_1_6_alt_dist, filename = here::here("Figs_Tables", "Sims_1_6_alt_dist.png"), dpi = 500,
       width = 10, height = 8)