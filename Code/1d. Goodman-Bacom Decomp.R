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

## Now do the BG decomposition
# First make a balanced panel dataset of simulation 6
# merge in the FE to the firm/year/state observations and add in residuals from the 
# empirical distribution. ROA is the linear combination of the FEs and the residual
set.seed(28101695)
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

# combine all of the data
data <- crossing(gvkey = sample(sim_firm_fe$gvkey, floor(n_firm_fes/10), replace = FALSE), 
                 fyear = sim_year_fe$fyear) %>%
  left_join(sim_firm_fe, by = "gvkey") %>% 
  left_join(sim_year_fe, by = "fyear") %>% 
  mutate(resid = sample(mod$residuals, floor(n_firm_fes/10) * n_year_fes, replace = TRUE),
         roa = firm_fe + year_fe + resid) %>% 
  left_join(comp %>% select(gvkey, incorp) %>% distinct())

# randomly assign the state of incorporation into treatment groups
# put random states into a vector
random_states <- sample(state.abb, length(state.abb), replace = FALSE) 

# Multiple Treatment Periods and Constant Different Treatment Effects 
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

# Multiple Treatment Periods and Dynamic Equal Treatment Effects
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

# Multiple Treatment Periods and Dynamic Treatment Effects
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

# Figure - GB Decomposition -------------------------------------------------------------
# calculate the bacon decomposition without covariates for Sims 4-6

bacon_4 <- bacon(treat_roa ~ treat,
                 data = data4,
                 id_var = "gvkey",
                 time_var = "fyear")

bacon_5 <- bacon(treat_roa ~ treat,
                 data = data5,
                 id_var = "gvkey",
                 time_var = "fyear")


bacon_6 <- bacon(treat_roa ~ treat,
                 data = data6,
                 id_var = "gvkey",
                 time_var = "fyear")

# make data frames with all of the info for the plots
bacon_4_plotdata <- bacon_4 %>% 
  # fix up the names
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>%
  # get the true weights and estimates from the simulation
  mutate(
    weight2 = case_when(
      treated == "89" ~ mean(data4$group == 1989)/2,
      treated == "98" ~ mean(data4$group == 1998)/2,
      treated == "07" ~ mean(data4$group == 2007)/2),
    estimate2 = case_when(
      treated == "89" & untreated == "98" ~ .5*sd_roa,
      treated == "89" & untreated == "07" ~ .5*sd_roa,
      treated == "98" & untreated == "07" ~ .3*sd_roa,
      treated == "98" & untreated == "89" ~ .3*sd_roa,
      treated == "07" & untreated == "98" ~ .1*sd_roa,
      treated == "07" & untreated == "89" ~ .1*sd_roa)) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))


# function to get true group level treatment effects
get_true_te <- function(dt, start, end, grp) {
  dt %>% 
    filter(treat == 1 & group == grp & fyear %>% between(start, end)) %>% 
    group_by(gvkey) %>% 
    summarize(mdelta = mean(delta)) %>% 
    pull(mdelta) %>% 
    mean()
}

bacon_5_plotdata <- bacon_5 %>% 
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>% 
  # get the true weights and estimates from the simulation
  mutate(
    weight2 = case_when(
      treated == "89" ~ mean(data5$group == 1989)/2,
      treated == "98" ~ mean(data5$group == 1998)/2,
      treated == "07" ~ mean(data5$group == 2007)/2),
    estimate2 = case_when(
      treated == "89" & untreated == "98" ~ get_true_te(data5, 1989, 1997, 1989),
      treated == "89" & untreated == "07" ~ get_true_te(data5, 1989, 2006, 1989),
      treated == "98" & untreated == "07" ~ get_true_te(data5, 1998, 2006, 1998),
      treated == "98" & untreated == "89" ~ get_true_te(data5, 1998, 2015, 1998),
      treated == "07" & untreated == "98" ~ get_true_te(data5, 2007, 2017, 2007),
      treated == "07" & untreated == "89" ~ get_true_te(data5, 2007, 2017, 2007))) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))

bacon_6_plotdata <- bacon_6 %>% 
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>% 
  # get the true weights and estimates from the simulation
  mutate(
    weight2 = case_when(
      treated == "89" ~ mean(data6$group == 1989)/2,
      treated == "98" ~ mean(data6$group == 1998)/2,
      treated == "07" ~ mean(data6$group == 2007)/2),
    estimate2 = case_when(
      treated == "89" & untreated == "98" ~ get_true_te(data6, 1989, 1997, 1989),
      treated == "89" & untreated == "07" ~ get_true_te(data6, 1989, 2006, 1989),
      treated == "98" & untreated == "07" ~ get_true_te(data6, 1998, 2006, 1998),
      treated == "98" & untreated == "89" ~ get_true_te(data6, 1998, 2015, 1998),
      treated == "07" & untreated == "98" ~ get_true_te(data6, 2007, 2017, 2007),
      treated == "07" & untreated == "89" ~ get_true_te(data6, 2007, 2017, 2007))) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))

### merge in the true values 
# set colors, fills, and shapes for the decomp plot
colors <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
            "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
            "Earlier vs. Later Treated - True Value" = "#A7473A", 
            "Later vs. Earlier Treated - True Value" = "#4B5F6C")

fills <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
           "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
           "Earlier vs. Later Treated - True Value" = "white", 
           "Later vs. Earlier Treated - True Value" = "white",
           "07" = "#51806a")

fills <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
           "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
           "Earlier vs. Later Treated - True Value" = "white", 
           "Later vs. Earlier Treated - True Value" = "white")

shapes <- c("Earlier vs. Later Treated - DiD Estimate" = 21, 
            "Later vs. Earlier Treated - DiD Estimate" = 24,
            "Earlier vs. Later Treated - True Value" = 21,
            "Later vs. Earlier Treated - True Value" = 24)

# sim4 plot
sim4 <- bacon_4_plotdata %>% 
  # jigger things so that they look better
  mutate(estimate_vl = if_else(treated == "89" & untreated == "07" | treated == "98" & untreated == "07" | 
                                 treated == "07" & untreated == "98", 
                               estimate_vl + 0.01, estimate_vl)) %>% 
  arrange(desc(weight)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", label = "T = '89 \n C = '98", x = .12, y = .125, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .21, y = .15, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .16, y = .10, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .21, y = .11, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .20, y = .05, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .12, y = .06, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  labs(x = "", y = expression(widehat(delta^'DD'))) + 
  ggtitle("Simulation 4") + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# sim 5 plot
sim5 <- bacon_5_plotdata %>% 
  arrange(desc(weight)) %>% 
  # jigger things so that they look better
  mutate(estimate_vl = if_else(treated == "89" & untreated == "07" | treated == "98" & untreated == "07" | 
                                 treated == "07" & untreated == "98" & estimate == "estimate2", 
                               estimate_vl + 0.01, estimate_vl)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last"),
            show.legend = FALSE) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", label = "T = '89 \n C = '98", x = .11, y = .07, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .22, y = .09, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .225, y = .05, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .22, y = -.06, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .16, y = -.07, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .115, y = -.05, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  labs(x = "Weight", y = "") + 
  ggtitle("Simulation 5") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

# sim 6  plot
sim6 <- bacon_6_plotdata %>% 
  # jigger things so that they look better
  mutate(estimate_vl = if_else(treated == "98" & untreated == "89" & estimate == "estimate2" | 
                                 treated == "07" & untreated == "98" & estimate == "estimate2", 
                               estimate_vl + 0.02, estimate_vl)) %>% 
  arrange(desc(weight)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last"),
            show.legend = FALSE) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  annotate("text", label = "T = '89 \n C = '98", x = .11, y = 0.15, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .21, y = 0.15, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .225, y = 0.05, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .225, y = -.18, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .145, y = -.20, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .11, y = -0.13, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  ylim(c(-0.22, .19)) +
  labs(x = "", y = "") + 
  geom_mark_circle(aes(description = "Bad \n 2x2 Below", filter = treated == "07" & untreated == "89" & weight == "weight"),
                   con.type = "straight", label.buffer = unit(5, 'mm'), expand = unit(10, "mm"), fill = "#51806a",
                   label.fontsize = 8, con.arrow = arrow(length = unit(0.1, "inches")), label.fill = "#51806a30", show.legend = FALSE) + 
  ggtitle("Simulation 6") + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# make subplot showing 2007 treated v. 1989 control 
colors2 <- c("Treated" = "#A7473A", "Control" = "#4B5F6C")

# make subplot
subplot <- data6 %>% 
  filter((group == 2007 | group == 1989) & fyear >= 1989) %>% 
  mutate(group = if_else(group == 2007, "Treated", "Control")) %>% 
  ggplot(aes(x = fyear, y = treat_roa, group = gvkey)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>%
      group_by(group, fyear) %>% 
      summarize(treat_roa = mean(treat_roa)),
    aes(x = fyear, y = treat_roa, group = group,
        color = group), size = 1) + 
  scale_color_manual(values = colors2) + 
  ylim(c(-.5*sd_roa, 1.5*sd_roa)) + 
  labs(x = "", y = "ROA") + 
  geom_vline(xintercept = 2006.5, color = "#A7473A" , 
             linetype = "dashed", size = 1) + 
  ggtitle("Biased 2x2 Estimate From Simulation 6") + 
  labs(subtitle = expression(paste("Treated = ",'G'['2007'], "; Control = ", 'G'['1989']))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# combine and save  
GB_decomp_sims <- (sim4 + sim5 + sim6) / (subplot) 
ggsave(GB_decomp_sims, filename = here::here("Figs_Tables", "GB_decomp_sims.png"), dpi = 500,
       width = 10, height = 8)