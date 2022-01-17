library(tidyverse)
library(fixest)
library(ggthemes)
library(patchwork)
library(furrr)
library(fastDummies)
library(progressr)

# set plot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load in compustat data
comp <- read_rds(here::here("Data", "simulation_data.rds"))

# estimate the fixed effects regression of ROA on firm and year fixed effects
mod <- feols(roa ~ 1 | gvkey + fyear, cluster = "incorp", data = comp)

# get firm and years and state of incorporation
shell <- comp %>% select(gvkey, fyear)

# get the firm and year fes, as well as the standard deviation of ROA
firm_fes <- fixef(mod)$gvkey
n_firm_fes <- length(fixef(mod)$gvkey)
year_fes <- fixef(mod)$fyear
n_year_fes <- length(fixef(mod)$fyear)
sd_roa <- sd(comp$roa)

## Show that 0 treatment effects will end up significant with heterogeneity
run_sim <- function(i, p) {
  # need this for progress bar to work
  p()
  
  # pull firm FE from empirical distribution with replacement and 
  # randomly assign state of incorporation
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
  random_states <- sample(state.abb, length(state.abb), replace = FALSE) 
  
  # function for different trend decay rates
  get_t <- function(val) {
    
  # simulate treatment effects centered at 0 
  taus <- rnorm(3, 0, val*sd_roa) 
  
    # put data together
    dt <- data %>%
      mutate(
        # figure out treatment group based on random ordering of states of incorporation
        group = case_when(
          incorp %in% random_states[1:17] ~ 1989,
          incorp %in% random_states[18:35] ~ 1998,
          incorp %in% random_states[35:50] ~ 2007
        ),
        # add in treatment effects - varying percent of standard deviation of ROA added per year
        delta_base = case_when(
          fyear >= group & group == 1989 ~ taus[1],
          fyear >= group & group == 1998 ~ taus[2],
          fyear >= group & group == 2007 ~ taus[3], 
          TRUE ~ 0
        ),
        # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
        # vary the power variable which represents the decay rate
        delta = if_else(delta_base == 0, 0, delta_base * (fyear - group + 1)),
        # new ROA is the sum of the old ROA and the treatment effect
        treat_roa = roa + delta,
        # make indicator variable for obs when treatment is turned on for the TWFE regs
        treat = ifelse(fyear >= group, 1, 0),
        # make a relative-to-treatment year variable
        rel_year = fyear - group,
        # get a first treat variable for CS
        first_treat = group)
    
    feols(treat_roa ~ treat | gvkey + fyear, cluster = "incorp", data = dt) %>% 
      broom::tidy(conf.int = TRUE) %>% 
      select(statistic) %>% 
      # add in variables for the size of the break, and the amount of decay
      mutate(val = val)
}

  # estimate for a sequence of values
  map_dfr(seq(0, 0.1, by = 0.0025), get_t) %>% 
    mutate(sim = i)
}

# parallelize and do 500 simulations
x <- 1:500
options(future.globals.maxSize= 891289600)
set.seed(20210915)
plan(multisession, workers = 6)
with_progress({
  p <- progressor(steps = length(x))
  out <- future_map_dfr(
    .x = x, 
    .f = run_sim,
    p = p,
    .options = furrr_options(globals = c("mod", "shell", "firm_fes", "n_firm_fes",
                                         "year_fes", "n_year_fes", "sd_roa"),
                             packages = c("tidyverse", "fixest", "e1071", "did", "fastDummies"), 
                             seed = TRUE))
})

# make a plot that we can highlight under the curve with tstats bw -1.96 and 1.96
# first get the data for the full curve using ggplot_build
p1data <- ggplot_build(
  out %>% 
    mutate(insig = if_else(abs(statistic) < 1.96, 1, 0)) %>% 
    filter(val == 0.03) %>% 
    ggplot(aes(x = statistic)) + geom_density()
)$data[[1]]

# get percent insignificant
percent_insig <- out %>% 
  filter(val == 0.03) %>%
  mutate(insig = if_else(statistic %>% between(-1.96, 1.96), 1, 0)) %>% 
  summarize(m = mean(insig)) %>% 
  pull(m)

# make label for the plot
plotlabel <- glue::glue(scales::label_percent(accuracy = .1)(percent_insig),
                        " insignificant \n t-Stats")

# now make the plot - gray for the whole curve, red for between
# -1.96 and 1.96
p1 <- p1data %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_area(fill = "#4B5F6C", color = "darkgray", alpha = 1/3) + 
  geom_area(
    data = p1data %>% filter(x %>% between(-1.96, 1.96)),
    aes(x = x, y = y), 
    fill = "#A7473A",
    alpha = 3/4
  ) + 
  annotate("label", x = 14, y = 0.045, label = plotlabel,
           size = 2) + 
  geom_curve(aes(x = 14, y = .05, xend = 0, yend = 0.055),
             color = "#A7473A", 
             arrow = arrow(length = unit(0.03, "npc"))) + 
  scale_x_continuous(breaks = c(-20, -10, -1.96, 0, 1.96, 10, 20),
                     labels = c(-20, -10, -1.96, 0, 1.96, 10, 20)) + 
  scale_y_continuous(limits = c(0, 0.06)) + 
  labs(x = "t-Statistic", y = "Density") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 8),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 5))

# plot 2
p2data <- out %>% 
  mutate(insig = if_else(abs(statistic) < 1.96, 1, 0)) %>% 
  group_by(val) %>% 
  summarize(insig = mean(insig))

p2 <- p2data %>% 
  ggplot(aes(x = val, y = insig)) + 
  geom_line() + 
  geom_hline(yintercept = 0.95, color = "#A7473A", linetype = "dashed") + 
  labs(x = "Percent of ROA Standard Deviation", y = "Percent \n Insignificant") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360,
                                    size = 8),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 5)) + 
  scale_x_continuous(breaks = seq(0, .1, length.out = 5),
                     labels = scales::percent) + 
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent)

# save
ggsave(p1, filename = here::here("Figs_Tables", "rejection_plots_1.png"),
       dpi = 800, width = 4, height = 3)
ggsave(p2, filename = here::here("Figs_Tables", "rejection_plots_2.png"),
       dpi = 800, width = 4, height = 3)