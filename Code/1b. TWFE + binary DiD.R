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
# then add in treatment effects following DGP in our six simulations
run_sim <- function(i, p) {
  
  p()
  
  # pull firm FEfrom empirical distribution with replacement,
  # also uniformly assign state of incorporation
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
  
  # merge in the FE to the firm/year observations and add in residuals from the 
  # empirical distribution. ROA is the linear combination of the FEs and the residual
  data <- shell %>% 
    left_join(sim_firm_fe, by = "gvkey") %>% 
    left_join(sim_year_fe, by = "fyear") %>% 
    mutate(resid = sample(mod$residuals, length(mod$residuals), replace = TRUE),
           roa = firm_fe + year_fe + resid)
  
  # save the moments of the residuals from this dataset
  sim_mod <- feols(roa ~ 1 | gvkey + fyear, cluster = "incorp", data = data)
  mom <- c(sd(sim_mod$residuals), skewness(sim_mod$residuals), kurtosis(sim_mod$residuals))
  
  # randomly assign the state of incorporation into treatment groups
  # put random states into a vector
  random_states <- sample(state.abb, length(state.abb), replace = FALSE) 
  
  # now add in the treatment effect - One Treatment Period, Constant Treatment Effects
  data1 <- data %>% 
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:25] ~ "T",
        incorp %in% random_states[26:50] ~ "C"),
      # add in treatment effects - constant half of a standard deviation of ROA
      delta = case_when(fyear >= 1998 & group == "T" ~ 0.5*sd_roa,
                        TRUE ~ 0),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(group == "T" & fyear >= 1998, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - 1998,
      # get a first treat variable for CS
      first_treat = if_else(group == "T", 1998, 0))
  
  # One Treatment Period, Dynamic Treatment Effects
  data2 <- data %>% 
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:25] ~ "T",
        incorp %in% random_states[26:50] ~ "C"),
      # add in treatment effects - percentage of standard deviation of ROA added per year
      delta_base = case_when(fyear >= 1998 & group == "T" ~ 0.05*sd_roa,
                             TRUE ~ 0),
      # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
      delta = delta_base * (fyear - 1998 + 1),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(group == "T" & fyear >= 1998, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - 1998,
      # get a first treat variable for CS
      first_treat = if_else(group == "T", 1998, 0))
  
  #  Multiple Treatment Periods and Constant Equal Treatment Effects
  data3 <- data %>% 
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ), 
      # add in treatment effects - half a percentage of standard deviation of ROA
      delta = case_when(
        fyear >= group & group == 1989 ~ .5*sd_roa,
        fyear >= group & group == 1998 ~ .5*sd_roa,
        fyear >= group & group == 2007 ~ .5*sd_roa, 
        TRUE ~ 0
      ),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(fyear >= group, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - group,
      # get a first treat variable for CS
      first_treat = group)
  
  # Multiple Treatment Periods and Constant Different Treatment Effects 
  data4 <- data %>% 
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ), 
      # add in treatment effects - varying percentage of standard deviation of ROA
      delta = case_when(
        fyear >= group & group == 1989 ~ .5*sd_roa,
        fyear >= group & group == 1998 ~ .3*sd_roa,
        fyear >= group & group == 2007 ~ .1*sd_roa, 
        TRUE ~ 0
      ),
      # new ROA is the sum of the old ROA and the treatment effect
      treat_roa = roa + delta,
      # make indicator variable for obs when treatment is turned on for the TWFE regs
      treat = ifelse(fyear >= group, 1, 0),
      # make a relative-to-treatment year variable
      rel_year = fyear - group,
      # get a first treat variable for CS
      first_treat = group)
  
  # Multiple Treatment Periods and Dynamic Equal Treatment Effects
  data5 <- data %>%
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ),
      # add in treatment effects - 3% of standard deviation of ROA added per year
      delta_base = case_when(
        fyear >= group & group == 1989 ~ .03*sd_roa,
        fyear >= group & group == 1998 ~ .03*sd_roa,
        fyear >= group & group == 2007 ~ .03*sd_roa, 
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
      first_treat = group)
  
  #  Multiple Treatment Periods and Dynamic Unequal Treatment Effects
  data6 <- data %>%
    mutate(
      # figure out treatment group based on random ordering of states of incorporation
      group = case_when(
        incorp %in% random_states[1:17] ~ 1989,
        incorp %in% random_states[18:35] ~ 1998,
        incorp %in% random_states[36:50] ~ 2007
      ),
      # add in treatment effects - varying percent of standard deviation of ROA added per year
      delta_base = case_when(
        fyear >= group & group == 1989 ~ .05*sd_roa,
        fyear >= group & group == 1998 ~ .03*sd_roa,
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
      first_treat = group)
  
  # make function to get estimates and treatment effects from data for k in 1:6
  get_est <- function(k) {
    
    # load in k-specific data
    dt <- get(paste0("data", k))
    
    # get values
    # full treatment effect
    # observation level
    full_te_1 <- dt %>% filter(treat == 1) %>% pull(delta) %>% mean()
    # firm level average
    full_te_2 <- dt %>% filter(treat == 1) %>% group_by(gvkey) %>% 
      summarize(m = mean(delta)) %>% pull(m) %>% mean()
    
    # full treatment effects with no 2007 treatment group
    full_te_no_2007_1 <- dt %>% filter(treat == 1 & first_treat < 2007) %>% pull(delta) %>% mean()
    full_te_no_2007_2 <- dt %>% filter(treat == 1 & first_treat < 2007) %>% 
      group_by(gvkey) %>% summarize(m = mean(delta)) %>% pull(m) %>% mean()
    
    # treatment effect for just years 1 - 5 after treatment
    te_5_1 <- dt %>% filter(treat == 1 & rel_year %in% 0:5 & first_treat < 2007) %>% 
      pull(delta) %>% mean()
    te_5_2 <- dt %>% filter(treat == 1 & rel_year %in% 0:5 & first_treat < 2007) %>% 
      group_by(gvkey) %>% summarize(m = mean(delta)) %>% pull(m) %>% mean()
    
    # get twfe estimates on full data
    twfe <- feols(treat_roa ~ treat | gvkey + fyear, cluster = "incorp", data = dt)$coefficients[1]
    
    # get CS estimates
    # first full the full set of attgts
    CS_out <- att_gt(yname = "treat_roa", 
                     data = dt,
                     gname = "first_treat",
                     idname = "gvkey", 
                     tname = "fyear", 
                     bstrap = F, 
                     cband = F,
                     est_method = "reg",
                     control_group = "notyettreated",
                     print_details = F,
                     panel = TRUE,
                     allow_unbalanced_panel = TRUE)
    
    # aggregate by group and then take average over number in groups
    cs_aggte_group <-  aggte(CS_out, type = "group", min_e = 0, max_e = 5, bstrap = FALSE, cband = FALSE)
    
    cs <- weighted.mean(
      cs_aggte_group$att.egt,
      dt %>% 
        filter(treat == 1 & rel_year %in% 0:5 & first_treat < 2007) %>% 
        group_by(first_treat) %>% 
        summarize(n = length(unique(gvkey))) %>% 
        arrange(first_treat) %>% 
        pull(n)
    )
    
    # Stacked regressions
    # first make the stacked datasets
    # get the treatment cohorts
    cohorts <- dt %>% 
      # drop never treateds, and also 2007 when everyone is treated
      filter(!(first_treat %in% c(0, 2007))) %>% 
      pull(first_treat) %>% 
      unique()
    
    # make formula to create the dataset
    getdata <- function(j) {
      
      #keep what we need
      dt %>% 
        # keep treated units and all units not treated within -5 to 5
        filter(first_treat == j | first_treat == 0 | first_treat > j + 5) %>% 
        # keep just year -5 to 5
        filter(fyear >= j - 5 & fyear <= j + 5) %>%
        # create an indicator for the dataset
        mutate(df = j)
    }
    
    # get data stacked
    stacked_data <- map_df(cohorts, getdata)
    
    # get stacked value
    stacked <- feols(treat_roa ~ treat | gvkey^df + fyear^df, data = stacked_data)$coefficients[1]
    
    # finally get the sun and abraham value
    # need to make a dataset without observations more than 5 years after treatment 
    sa_data <- dt %>% 
      filter(treat == 0 | rel_year <= 5) %>% 
      filter(fyear < 2007)
    
    # run SA model through the fixest package
    sun_ab <- feols(treat_roa ~ 1 + sunab(first_treat, fyear) | gvkey + fyear, sa_data)
    # get the overall att
    sa <- summary(sun_ab, agg = "att")$coeftable[1]
    
    tibble(sim = i, dt = k, full_te_1 = full_te_1, full_te_2 = full_te_2, full_te_no_2007_1 = full_te_no_2007_1,
           full_te_no_2007_2 = full_te_no_2007_2, te_5_1 = te_5_1, te_5_2 = te_5_2,
           twfe = twfe, cs = cs, stacked = stacked, sa = sa)
  }
  
  # run it for our six simulations and store results in a dataset
  estimates <- map_dfr(1:6, get_est)
  
  # get moments into a tibble as well
  moments <- tibble(
    moment = 1:3,
    value = mom
  ) %>% 
    mutate(sim = i)
  
  # output a list of both objects that we want
  list(moments = moments,
       estimates = estimates)
  
}

# parallelize and do 500 simulations
x <- 1:500
options(future.globals.maxSize= 891289600)
set.seed(28101695)
plan(multisession, workers = 6)
with_progress({
  p <- progressor(steps = length(x))
  out <- future_map(
    .x = x, 
    .f = run_sim,
    p = p,
    .options = furrr_options(globals = c("mod", "shell", "firm_fes", "n_firm_fes",
                                         "year_fes", "n_year_fes", "sd_roa"),
                             packages = c("tidyverse", "fixest", "e1071", "did", "fastDummies"),
                             seed = TRUE)
  )})

# unpack the two datasets
moments <- do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = out)$moments
estimates <- do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = out)$estimates

# save a version
saveRDS(moments, here::here("Data", "moments.rds"))
saveRDS(estimates, here::here("Data", "estimates.rds"))

## Next - do one pull of the simulation to plot the group means
# pull firm FE from empirical distribution with replacement
set.seed(28101695)
sim_firm_fe <- tibble(
  gvkey = unique(shell$gvkey),
  firm_fe = sample(firm_fes, n_firm_fes, replace = TRUE),
  incorp = sample(state.abb, n_firm_fes, replace = TRUE)
)

# pull year FE from the empirical distribution with replacement
sim_year_fe <- tibble(
  fyear = unique(comp$fyear),
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

#  One Treatment Period, Constant Treatment Effects
data1 <- data %>% 
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:25] ~ "T",
      incorp %in% random_states[26:50] ~ "C"),
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta = case_when(fyear >= 1998 & group == "T" ~ 0.5*sd_roa,
                      TRUE ~ 0),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(group == "T" & fyear >= 1998, 1, 0),
    dt = "Simulation 1")

#  One Treatment Period, Dynamic Treatment Effects
data2 <- data %>% 
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:25] ~ "T",
      incorp %in% random_states[26:50] ~ "C"),
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta_base = case_when(fyear >= 1998 & group == "T" ~ 0.05*sd_roa,
                           TRUE ~ 0),
    # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
    delta = delta_base * (fyear - 1998 + 1),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(group == "T" & fyear >= 1998, 1, 0),
    dt = "Simulation 2")

#  Multiple Treatment Periods and Constant Equal Treatment Effects
data3 <- data %>% 
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:17] ~ 1989,
      incorp %in% random_states[18:35] ~ 1998,
      incorp %in% random_states[36:50] ~ 2007
    ), 
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta = case_when(
      fyear >= group & group == 1989 ~ .5*sd_roa,
      fyear >= group & group == 1998 ~ .5*sd_roa,
      fyear >= group & group == 2007 ~ .5*sd_roa, 
      TRUE ~ 0
    ),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(fyear >= group, 1, 0),
    group = as.character(group),
    dt = "Simulation 3")

#  Multiple Treatment Periods and Constant Different Treatment Effects 
data4 <- data %>% 
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:17] ~ 1989,
      incorp %in% random_states[18:35] ~ 1998,
      incorp %in% random_states[36:50] ~ 2007
    ), 
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta = case_when(
      fyear >= group & group == 1989 ~ .5*sd_roa,
      fyear >= group & group == 1998 ~ .3*sd_roa,
      fyear >= group & group == 2007 ~ .1*sd_roa, 
      TRUE ~ 0
    ),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(fyear >= group, 1, 0))

#  Multiple Treatment Periods and Dynamic Equal Treatment Effects
data5 <- data %>%
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:17] ~ 1989,
      incorp %in% random_states[18:35] ~ 1998,
      incorp %in% random_states[36:50] ~ 2007
    ),
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta_base = case_when(
      fyear >= group & group == 1989 ~ .03*sd_roa,
      fyear >= group & group == 1998 ~ .03*sd_roa,
      fyear >= group & group == 2007 ~ .03*sd_roa, 
      TRUE ~ 0
    ),
    # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
    delta = delta_base * (fyear - group + 1),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(fyear >= group, 1, 0))

# Multiple Treatment Periods and Unequal Dynamic Treatment Effects
data6 <- data %>%
  mutate(
    # figure out treatment group based on random ordering of states of incorporation
    group = case_when(
      incorp %in% random_states[1:17] ~ 1989,
      incorp %in% random_states[18:35] ~ 1998,
      incorp %in% random_states[36:50] ~ 2007
    ),
    # add in treatment effects - percentage of standard deviation of ROA added per year
    delta_base = case_when(
      fyear >= group & group == 1989 ~ .05*sd_roa,
      fyear >= group & group == 1998 ~ .03*sd_roa,
      fyear >= group & group == 2007 ~ .01*sd_roa, 
      TRUE ~ 0
    ),
    # true treatment effect is the cumulative sum of this - dynamic trend break treatment effect
    delta = delta_base * (fyear - group + 1),
    # new ROA is the sum of the old ROA and the treatment effect
    treat_roa = roa + delta,
    # make indicator variable for obs when treatment is turned on for the TWFE regs
    treat = ifelse(fyear >= group, 1, 0))

# plot for simulation 1
sim1_means <- data1 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "ROA", color = "Group") + 
  geom_vline(xintercept = 1997.5, color = '#4B5F6C',
             linetype = "dashed", size = 1) + 
  scale_y_continuous(limits = c(-0.5*sd_roa, 1.5*sd_roa)) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Simulation 1") + 
  labs(subtitle = expression(paste("Not Staggered + Constant ", delta))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot for simulation 2
sim2_means <- data2 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  geom_vline(xintercept = 1997.5, color = '#4B5F6C',
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  scale_y_continuous(limits = c(-0.5*sd_roa, 1.5*sd_roa)) + 
  ggtitle("Simulation 2") + 
  labs(subtitle = expression(paste("Not Staggered + Dynamic ", delta))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot for simulation 3
sim3_means <- data3 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  scale_y_continuous(limits = c(-0.5*sd_roa, 1.5*sd_roa)) + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C", 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a", 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 3") + 
  labs(subtitle = expression(paste("Staggered + Constant/Equal ", delta))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot for simulation 4
sim4_means <- data4 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "ROA", color = "Group") + 
  scale_y_continuous(limits = c(-.5*sd_roa, 1.5*sd_roa)) + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C", 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a", 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 4") + 
  labs(subtitle = expression(paste("Staggered + Constant/Unequal ", delta))) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot for simulation 5
sim5_means <- data5 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  scale_y_continuous(limits = c(-.5*sd_roa, 1.5*sd_roa)) + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C", 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a", 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 5") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Equal ", delta))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot for simulation 6
sim6_means <- data6 %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/30, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  scale_y_continuous(limits = c(-.5*sd_roa, 1.5*sd_roa)) + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C", 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a", 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 6") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Unequal ", delta))) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# plot treatment paths - sims 1 - 3
Sims_1_3_trends <- sim1_means + sim2_means + sim3_means

#save
ggsave(Sims_1_3_trends, filename = here::here("Figs_Tables", "Sims_1_3_trends.png"), dpi = 500,
       width = 10, height = 4)

# plot treatment paths - sims 4 - 6
Sims_4_6_trends <- sim4_means + sim5_means + sim6_means

#save
ggsave(Sims_4_6_trends, filename = here::here("Figs_Tables", "Sims_4_6_trends.png"), dpi = 500,
       width = 10, height = 4)

# now plot the distributions for the bottom panels
# make formula to make the plots
make_dist_plot <- function(i, name) {
  estimates %>% 
    filter(dt == i) %>% 
    ggplot(aes(x = twfe)) + 
    geom_density(aes(fill = "TWFE Estimates"), alpha = 3/5) + 
    geom_vline(aes(xintercept = mean(estimates %>% filter(dt == i) %>% pull(full_te_1)),
                   color = "Observation Average"),
               linetype = "dashed", size = 1, alpha = 3/5,) +
    geom_vline(aes(xintercept = mean(estimates %>% filter(dt == i) %>% pull(full_te_2)),
                   color = "Firm Average"),
               linetype = "dashed", size = 1, alpha = 3/5) + 
    ggtitle(paste0("Simulation ", i)) + 
    labs(subtitle = TeX(paste0(name, "$\\delta$"))) +
    labs(y = "", x = if_else(i %in% c(2, 5), expression(widehat(delta^'DD')), expression(""))) + 
    scale_color_manual(name = "", values = c("Observation Average" = "#A7473A",
                                             "Firm Average" = "#4B5F6C")) + 
    scale_fill_manual(name = "", values = c("TWFE Estimates" = "#CACFD0")) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
          legend.position = if_else(i %in% c(2, 5), "bottom", "none"))
}

# run all the densities
sim1_estimates <- make_dist_plot(1, "Not Staggered + Constant ")
sim2_estimates <- make_dist_plot(2, "Not Staggered + Dynamic ")
sim3_estimates <- make_dist_plot(3, "Staggered + Constant/Equal ")
sim4_estimates <- make_dist_plot(4, "Staggered + Constant/Unequal ")
sim5_estimates <- make_dist_plot(5, "Staggered + Dynamic/Equal ")
sim6_estimates <- make_dist_plot(6, "Staggered + Dynamic/Unequal ")

# plot estimates of TWFE DD
Sims_1_3_dist <- sim1_estimates + sim2_estimates + sim3_estimates
Sims_4_6_dist <- sim4_estimates + sim5_estimates + sim6_estimates

# save
ggsave(Sims_1_3_dist, filename = here::here("Figs_Tables", "Sims_1_3_dist.png"), dpi = 500,
       width = 10, height = 4)
ggsave(Sims_4_6_dist, filename = here::here("Figs_Tables", "Sims_4_6_dist.png"), dpi = 500,
       width = 10, height = 4)
