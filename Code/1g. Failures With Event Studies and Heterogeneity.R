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

# estimate the fixed effects regression of ROA on firm and year fixed effects
mod <- feols(roa ~ 1 | gvkey + fyear, cluster = "incorp", data = comp)

# get the moments for the residuals from the baseline model
resid_sd <- sd(mod$residuals)
resid_skew <- skewness(mod$residuals)
resid_kurtosis <- kurtosis(mod$residuals)

# get firm and years and state of incorporation
shell <- comp %>% select(gvkey, fyear)

# get the firm and year fes, as well as the standard deviation of ROA
firm_fes <- fixef(mod)$gvkey
n_firm_fes <- length(fixef(mod)$gvkey)
year_fes <- fixef(mod)$fyear
n_year_fes <- length(fixef(mod)$fyear)
sd_roa <- sd(comp$roa)

# function to run simulation, pull firm and year FE, as well as the residuals from their empirical distributions
# then add in treatment effects following our six simulations
run_sim <- function(i, p) {
  
  p()
  
  # pull firm FE from empirical distribution with replacement
  sim_firm_fe <- tibble(
    gvkey = unique(shell$gvkey),
    firm_fe = sample(firm_fes, n_firm_fes, replace = TRUE),
    incorp = sample(state.abb, n_firm_fes, replace = TRUE)
  )
  
  # pull year FE from the empirical distribution with replacement
  sim_year_fe <- tibble(
    fyear = unique(shell$fyear),
    year_fe = sample(year_fes, n_year_fes, replace = TRUE)
  )
  
  # merge in the FE to the firm/year/state observations and add in residuals from the 
  # empirical distribution. ROA is the linear combination of the FEs and the residual
  data <- shell %>% 
    left_join(sim_firm_fe, by = "gvkey") %>% 
    left_join(sim_year_fe, by = "fyear") %>% 
    mutate(resid = sample(mod$residuals, length(mod$residuals), replace = TRUE),
           roa = firm_fe + year_fe + resid)
  
  # randomly assign the state of incorporation into treatment groups
  # put random states into a vector
  random_states <- sample(state.abb, length(state.abb), replace = FALSE) 
  
  # now add in the treatment effect -  Multiple Treatment Periods and Dynamic Equal Treatment Effects
  dt <- data %>%
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ),
      # add in treatment effects - 3% of standard deviation of ROA added per year
      delta_base = case_when(
        fyear >= group & group == 1989 ~ .1*sd_roa,
        fyear >= group & group == 1998 ~ .05*sd_roa,
        fyear >= group & group == 2007 ~ .01*sd_roa, 
        TRUE ~ 0
      ),
      # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
      delta = delta_base * (fyear - group + 1),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(fyear >= group, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - group,
      # get a first treat variable for CS
      first_treat = group) %>% 
    # make dummy cols
    mutate(Pre = ifelse(rel_year < -5, 1, 0),
           `rel_year_-5` = if_else(rel_year == -5, 1, 0),
           `rel_year_-4` = if_else(rel_year == -4, 1, 0),
           `rel_year_-3` = if_else(rel_year == -3, 1, 0),
           `rel_year_-2` = if_else(rel_year == -2, 1, 0),
           rel_year_0 = if_else(rel_year == 0, 1, 0),
           rel_year_1 = if_else(rel_year == 1, 1, 0),
           rel_year_2 = if_else(rel_year == 2, 1, 0),
           rel_year_3 = if_else(rel_year == 3, 1, 0),
           rel_year_4 = if_else(rel_year == 4, 1, 0),
           rel_year_5 = if_else(rel_year == 5, 1, 0),
           Post = ifelse(rel_year > 5, 1, 0))
  
  # put time indicators into vector
  indicators <- c("Pre", paste0("`", "rel_year_", c(-5:-2, 0:5), "`"), "Post")
  
  # estimate model
  mod <- feols(treat_roa ~ .[indicators] | gvkey + fyear, data = dt, cluster = "incorp")
  
  # export results we need
  broom::tidy(mod) %>% 
    # drop the binned indicators
    filter(!(term %in% c("Pre", "Post"))) %>% 
    # add in a time variable
    mutate(t = c(-5:-2, 0:5)) %>% 
    select(t, estimate) %>% 
    # add in omitted category
    bind_rows(tibble(t = -1, estimate = 0)) %>% 
    arrange(t) %>% 
    mutate(sim = i) %>% 
    # get true effect
    mutate(true_te = map_dbl(c(-5:5), function(x) {dt %>% filter(rel_year == x) %>% pull(delta) %>% mean}))
}

# parallelize and do 500 simulations
x <- 1:500
options(future.globals.maxSize= 891289600)
set.seed(28101695)
plan(multisession, workers = 6)
with_progress({
  p <- progressor(steps = length(x))
  out <- future_map_dfr(
    .x = x, 
    .f = run_sim,
    p = p,
    .options = furrr_options(globals = c("mod", "shell", "firm_fes", "n_firm_fes",
                                         "year_fes", "n_year_fes", "sd_roa"),
                             packages = c("tidyverse", "fixest", "fastDummies", "broom"),
                             seed = TRUE)
  )})

# plot
p1 <- out %>% 
  group_by(t) %>% 
  summarize(est = mean(estimate),
            true_effect = mean(true_te),
            lower_ci = quantile(estimate, probs = 0.025),
            upper_ci = quantile(estimate, probs = 0.975)) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = est)) + 
  geom_line(aes(x = t, y = true_effect, color = "True Effect"), linetype = "dashed", size = 2) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              color = "lightgrey", alpha = 1/4) + 
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") +
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("No Real Pre-Trends") + 
  ylim(c(-0.04, 0.075)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# Do the opposite - real pre trends but looks like none
# function to run simulation, pull firm and year FE, as well as the residuals from their empirical distributions
# then add in treatment effects following our six simulations
run_sim_2 <- function(i, p) {
  
  p()
  
  # pull firm FE from empirical distribution with replacement
  sim_firm_fe <- tibble(
    gvkey = unique(shell$gvkey),
    firm_fe = sample(firm_fes, n_firm_fes, replace = TRUE),
    incorp = sample(state.abb, n_firm_fes, replace = TRUE)
  )
  
  # pull year FE from the empirical distribution with replacement
  sim_year_fe <- tibble(
    fyear = unique(shell$fyear),
    year_fe = sample(year_fes, n_year_fes, replace = TRUE)
  )
  
  # merge in the FE to the firm/year/state observations and add in residuals from the 
  # empirical distribution. ROA is the linear combination of the FEs and the residual
  data <- shell %>% 
    left_join(sim_firm_fe, by = "gvkey") %>% 
    left_join(sim_year_fe, by = "fyear") %>% 
    mutate(resid = sample(mod$residuals, length(mod$residuals), replace = TRUE),
           roa = firm_fe + year_fe + resid)
  
  # randomly assign the state of incorporation into treatment groups
  # put random states into a vector
  random_states <- sample(state.abb, length(state.abb), replace = FALSE) 
  
  # now add in the treatment effect -  Multiple Treatment Periods and Dynamic Equal Treatment Effects
  dt <- data %>%
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ),
      # add in treatment effects - 3% of standard deviation of ROA added per year
      delta_base = case_when(
        fyear < group & group == 1989 ~ -0.01*sd_roa,
        fyear < group & group == 1998 ~ -0.035*sd_roa,
        fyear < group & group == 2007 ~ -0.035*sd_roa, 
        TRUE ~ 0
      ),
      # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
      delta = delta_base * (group - fyear),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(fyear >= group, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - group,
      # get a first treat variable for CS
      first_treat = group) %>% 
    # make dummy cols
    mutate(Pre = ifelse(rel_year < -5, 1, 0),
           `rel_year_-5` = if_else(rel_year == -5, 1, 0),
           `rel_year_-4` = if_else(rel_year == -4, 1, 0),
           `rel_year_-3` = if_else(rel_year == -3, 1, 0),
           `rel_year_-2` = if_else(rel_year == -2, 1, 0),
           rel_year_0 = if_else(rel_year == 0, 1, 0),
           rel_year_1 = if_else(rel_year == 1, 1, 0),
           rel_year_2 = if_else(rel_year == 2, 1, 0),
           rel_year_3 = if_else(rel_year == 3, 1, 0),
           rel_year_4 = if_else(rel_year == 4, 1, 0),
           rel_year_5 = if_else(rel_year == 5, 1, 0),
           Post = ifelse(rel_year > 5, 1, 0))
  
  # put time indicators into vector
  indicators <- c("Pre", paste0("`", "rel_year_", c(-5:-2, 0:5), "`"), "Post")
  
  # estimate model
  mod <- feols(treat_roa ~ .[indicators] | gvkey + fyear, data = dt, cluster = "incorp")
  
  broom::tidy(mod) %>% 
    filter(!(term %in% c("Pre", "Post"))) %>% 
    mutate(t = c(-5:-2, 0:5)) %>% 
    select(t, estimate) %>% 
    bind_rows(tibble(t = -1, estimate = 0)) %>% 
    arrange(t) %>% 
    mutate(sim = i) %>% 
    mutate(true_te = map_dbl(c(-5:5), function(x) {dt %>% filter(rel_year == x) %>% pull(delta) %>% mean}))
}

# parallelize and do 500 simulations
x <- 1:500
options(future.globals.maxSize= 891289600)
set.seed(28101695)
plan(multisession, workers = 6)
with_progress({
  p <- progressor(steps = length(x))
  out <- future_map_dfr(
    .x = x, 
    .f = run_sim_2,
    p = p,
    .options = furrr_options(globals = c("mod", "shell", "firm_fes", "n_firm_fes",
                                         "year_fes", "n_year_fes", "sd_roa"),
                             packages = c("tidyverse", "fixest", "fastDummies", "broom"), 
                             seed = TRUE)
  )})

# plot
p2 <- out %>% 
  group_by(t) %>% 
  summarize(est = mean(estimate),
            true_effect = mean(true_te),
            lower_ci = quantile(estimate, probs = 0.025),
            upper_ci = quantile(estimate, probs = 0.975)) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = est)) + 
  geom_line(aes(x = t, y = true_effect, color = "True Effect"), linetype = "dashed", size = 2) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              color = "lightgrey", alpha = 1/4) + 
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") +
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Actual Pre-Trends") + 
  ylim(c(-0.035, 0.02)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# combine the two plots and save
plot <- p1 + p2

ggsave(plot, filename = here::here("Figs_Tables", "bad_event_study_plots.png"), dpi = 500,
        width = 8, height = 4)