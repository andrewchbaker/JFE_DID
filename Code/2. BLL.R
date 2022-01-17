# load packages
library(tidyverse)
library(patchwork)
library(fastDummies)
library(ggthemes)
library(did)
library(bacondecomp)
library(kableExtra)
library(fixest)

# set defaults
select <- dplyr::select
# set plot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))
options(knitr.kable.NA = '')

# set seed for CS bootstrap estimator to be replicable
set.seed(20210215)

# load data. This is downloaded from https://dataverse.nl/dataset.xhtml?persistentId=hdl:10411/15996.
data <- haven::read_dta(here::here("Reps/BLL/bbb/macro_workfile.dta"))

# make relative year, treatment indicator, and and log gini variables
data <- data %>% 
  mutate(rel_year = wrkyr - branch_reform,
         log_gini = log(gini),
         treat = `_intra`)

# function to get significance stars
make_stars <- function(t, dof) {
  if (2 * pt(-t, df=dof) < 0.01) {
    ptstar <- "***"
  } else if (2 * pt(-t, df=dof) < 0.05) {
    ptstar <- "**"
  } else if (2 * pt(-t, df=dof) < 0.1) {
    ptstar <- "*"
  } else {
    ptstar <- ""
  }
  return(ptstar)
}

# estimate the models
# no controls
mod1 <- feols(log_gini ~ treat | statefip + wrkyr, cluster = "statefip", data = data)

# controls
mod2 <- feols(log_gini ~ treat + gsp_pc_growth + prop_blacks + prop_dropouts + prop_female_headed + 
               unemploymentrate | statefip + wrkyr, cluster = "statefip", data = data)

# Table 4 - Bacon Decomposition -----------------------------------------------------
# calculate the bacon decomposition without covariates
bacon_out <- bacon(log_gini ~ treat,
                   data = data,
                   id_var = "state",
                   time_var = "wrkyr") %>% 
  # rename Later vs. Always Treated to Later v. Earlier Treated
  # because the code changed and it is accurate
  mutate(type = if_else(type == "Later vs Always Treated", "Later vs Earlier Treated", type))

# first get the total weight for each group. 
total_weights <- bacon_out %>% 
  group_by(type) %>% 
  summarize(weight = sum(weight))

# get the weighted average within group
group_avg <- bacon_out %>% 
  group_by(type) %>% 
  summarize(avg = weighted.mean(estimate, weight),
            weights = sum(weight))

# make the table
BLL_decomp <- group_avg %>% 
  kable("latex", digits = 3, align = 'lcc',
        booktabs = T,
        col.names = c("Type", "Weighted \n Average", "Total \n Weight"),
        label = "BLL_decomp") %>% 
  kable_styling(position = "center", font_size = 8,
                latex_options = c("HOLD_position", "scale_down"))

# save
write_lines(BLL_decomp, file = here::here("Figs_Tables", "BLL_decomp.tex"))

# Figure 5 - Bacon Decomp -------------------------------------------------
# first early v late plot
EvL <- bacon_out %>% 
  filter(type == "Earlier vs Later Treated") %>% 
  ggplot(aes(x = weight, y = estimate)) + 
  geom_point(size = 3, alpha = 1/2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = group_avg$avg[1], color = "darkred", size = 2) + 
  labs(x = "", y = expression(widehat(delta^'DD'))) + 
  ggtitle(paste0("Early vs Later Treated \n Total Weight =", scales::percent(total_weights$weight[1]))) + 
  scale_y_continuous(limits = c(-.12, 0.12)) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# late v early plot
LvE <- bacon_out %>% 
  filter(type == "Later vs Earlier Treated") %>% 
  ggplot(aes(x = weight, y = estimate)) + 
  geom_point(size = 3, alpha = 1/2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = group_avg$avg[2], color = "darkred", size = 2) + 
  labs(x = "Weight", y = expression(widehat(delta^'DD'))) + 
  scale_y_continuous(limits = c(-.12, 0.12)) + 
  ggtitle(paste0("Later vs Earlier Treated \n Total Weight = ", scales::percent(total_weights$weight[2]))) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# combine the figures
BLL_decomp_plot <- EvL / LvE

# save
ggsave(BLL_decomp_plot, filename = here::here("Figs_Tables", "BLL_decomp_plot.png"), 
       dpi = 500, width = 5, height = 20/3)

# Figure 7 - timing of adoption -------------------------------------------
BLL_timing <- data %>% 
  select(state, wrkyr, branch_reform) %>% 
  mutate(state = fct_reorder(state, rank(desc(state)))) %>% 
  mutate(post = if_else(wrkyr < branch_reform, "Pre", "Post")) %>% 
  mutate(post = factor(post, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(x = wrkyr, y = state)) + 
  geom_tile(aes(fill = as.factor(post)), alpha = 3/4) + 
  scale_fill_manual(values = c("#4B5F6C", "#A7473A")) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white")) 

# save
ggsave(BLL_timing, filename = here::here("Figs_Tables", "BLL_timing.png"), 
       dpi = 500, width = 5, height = 20/3)

# Figure 8 - Fixed Event Studies ------------------------------------------
# make dummy variables 
data_dummies <- data %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
  # bin end points
  mutate(`rel_year_-10` = if_else(rel_year <= -10, 1, 0),
         rel_year_15 = if_else(rel_year >= 15, 1, 0))

# make the formula to estimate
indicators <- c(paste0("`", "rel_year_", c(-10:-1, 1:15), "`"))

# estimate model as published
es_published <- feols(log_gini ~ .[indicators] | wrkyr + statefip,
                      data = data_dummies, cluster = "statefip")

# plot
ES_1 <- broom::tidy(es_published, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = c(-10:-1, 1:15)) %>% 
  # substract out the the mean for beta -10 to -1
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(A)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.04, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# panel B - don't detrend
ES_2 <- broom::tidy(es_published, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = c(-10:-1, 1:15)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(B)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.04, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# panel C - don't detrend and also include the full set of relative time indicators
# make dummy variables 
data_dummies <- data %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))

# get the relative year indicators - drop most negative and 0.
yrs <- sort(unique(data_dummies$rel_year))
yrs <- yrs[which(yrs != min(yrs) & yrs != 0)]

# make formula
indicators <- c(paste0("`", "rel_year_", yrs, "`"))

# estimate the model and plot
# estimate the model
ES_fix1 <- feols(log_gini ~ .[indicators] | wrkyr + statefip,
                 data = data_dummies, cluster = "statefip")

# plot
ES_3 <- broom::tidy(ES_fix1, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = yrs) %>% 
  filter(t %>% between(-10, 15)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(C)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# fig 8d - drop firms treated before the panel and all years once everyone is treated.
# make dummy variables 
data_dummies <- data %>% 
  # drop states treated before the sample
  filter(branch_reform >= 1977) %>% 
  # drop observations after which everyone is treated
  filter(wrkyr <= 1998) %>% 
  # remove dummy variables for firms treated in the last year
  mutate(rel_year = if_else(branch_reform == max(branch_reform), NA_real_, rel_year)) %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))

# get the relative year indicators 
yrs <- sort(unique(data_dummies$rel_year))
yrs <- yrs[which(yrs != min(yrs) & yrs != 0)]

# make formula
indicators <- c(paste0("`", "rel_year_", yrs, "`"))

# estimate the model and plot
# estimate the model
ES_fix2 <- feols(log_gini ~ .[indicators] | wrkyr + statefip,
                 data = data_dummies, cluster = "statefip")

# plot
ES_4 <- broom::tidy(ES_fix2, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = yrs) %>% 
  filter(t %>% between(-10, 15)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(D)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.08, 0.08, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# combine plots
BLL_ES <- (ES_1 + ES_2)/(ES_3 + ES_4)

#save
ggsave(BLL_ES, filename = here::here("Figs_Tables", "BLL_ES.png"), 
       dpi = 500, width = 10, height = 20/3)

# Remedies ----------------------------------------------------------------
# get treated years that we can estimate
treats <- data %>% 
  filter(branch_reform >= 1977 & branch_reform < max(branch_reform)) %>% 
  pull(branch_reform) %>% 
  unique() %>% 
  sort()

# function to get treat-year specific cohorts
make_dt <- function(tyr) {
  data %>% 
    # drop all observations on or after 1999 when everyone is treated
    filter(wrkyr < 1999) %>% 
    # keep firms in the adopt year or those firms in years t + 10
    filter(branch_reform == tyr | branch_reform > tyr + 10) %>% 
    # keep just years t - 5 to t + 10
    filter(wrkyr %>% between(tyr - 5, tyr + 10)) %>% 
    # replace adopt year to NA if not in treated year to make dummies
    mutate(branch_reform = if_else(branch_reform == tyr, branch_reform, NA_real_),
           rel_year = wrkyr - branch_reform) %>% 
    select(statefip, wrkyr, branch_reform, rel_year, log_gini) %>% 
    mutate(dt = as.character(tyr))
}

# stack the datasets
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
  # interact cluster with statefip
  mutate(cluster = paste0(statefip, "_", dt))

# make formula
indicators <- c(paste0("`", "rel_year_", c(-5:-1, 1:10), "`"))

# estimate the model and plot
# estimate the model
stack1 <- feols(log_gini ~ .[indicators] | wrkyr^dt + statefip^dt,
                 cluster = "cluster", data = stacked_data)

# plot
BLL_stack1 <- broom::tidy(stack1, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = c(-5:-1, 1:10)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(A)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# remake but allowing for more observations to enter
# get treated years that we can estimate
treats <- data %>% 
  filter(branch_reform >= 1977 & branch_reform < max(branch_reform)) %>% 
  pull(branch_reform) %>% 
  unique() %>% 
  sort()

# function to get treat-year specific cohorts
make_dt <- function(tyr) {
  data %>% 
    # keep firms in the adopt year or those obs without treatment before t + 10
    filter(branch_reform == tyr | (branch_reform > tyr & wrkyr < branch_reform)) %>% 
    # keep just years t - 5 to t + 10
    filter(wrkyr %>% between(tyr - 5, tyr + 10)) %>% 
    # replace adopt year to NA to make dummies
    mutate(branch_reform = if_else(branch_reform == tyr, branch_reform, NA_real_),
           rel_year = wrkyr - branch_reform,
           treat = if_else(is.na(branch_reform) | wrkyr < tyr, 0, 1)) %>% 
    select(statefip, wrkyr, branch_reform, rel_year, log_gini, treat) %>% 
    mutate(dt = as.character(tyr))
}

# stack the datasets
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(statefip, "_", dt))

# make formula
indicators <- c(paste0("`", "rel_year_", c(-5:-1, 1:10), "`"))

# estimate the model and plot
# estimate the model
stack2 <- feols(log_gini ~ .[indicators] | wrkyr^dt + statefip^dt,
                cluster = "cluster", data = stacked_data)

# estimate the model and plot
# estimate the model
BLL_stack2 <- broom::tidy(stack2, conf.int = TRUE) %>%
  # add in the relative time variable
  mutate(t = c(-5:-1, 1:10)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(B)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# combine and save
BLL_stack <- BLL_stack1 + BLL_stack2 

#save
ggsave(BLL_stack, filename = here::here("Figs_Tables", "BLL_stack.png"), 
       dpi = 500, width = 10, height = 20/6)

# CS Method ---------------------------------------------------------------
# never treateds only as control states
# make the dataaset - drop states treated before 1977
data_cs <- data %>% 
  # drop states treated before data
  filter(branch_reform >= 1977) %>% 
  # keep only observations through 1998
  filter(wrkyr <= 1998) %>% 
  # set branch reform = 0 for last treated state
  mutate(branch_reform = if_else(branch_reform == 1999, 0, branch_reform)) %>% 
  select(log_gini, wrkyr, statefip, branch_reform)

# run
out1 <- att_gt(yname = "log_gini",
               data = data_cs,
               tname = "wrkyr",
               idname = "statefip",
               gname = "branch_reform",
               xformla = NULL,
               control_group = "nevertreated",
               est_method = "reg",
               print_details = FALSE,
               bstrap = T,
               cband = T,
               clustervars = "statefip")

# make the dynamic event study
es1 <- aggte(out1, type="dynamic", min_e = -5, max_e = 10)

# plot
BLL_CS1 <- tidy(es1) %>% 
  as_tibble() %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(A)") + 
  geom_errorbar(aes(ymin = point.conf.low, ymax = point.conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

## estimate with `notyettreated'
# make the dataset - drop states treated before 1977
data_cs <- data %>% 
  # drop states treated before data
  filter(branch_reform >= 1977) %>% 
  # keep only observations through 1998
  filter(wrkyr <= 1998) %>% 
  select(statefip, wrkyr, branch_reform, log_gini)

# run
out2 <- att_gt(yname = "log_gini",
               data = data_cs,
               tname = "wrkyr",
               idname = "statefip",
               gname = "branch_reform",
               xformla = NULL,
               control_group = "notyettreated",
               est_method = "reg",
               print_details = FALSE,
               bstrap = T,
               cband = T,
               clustervars = "statefip")

# make the dynamic event study
es2 <- aggte(out2, type="dynamic", min_e = -5, max_e = 10)

# plot
BLL_CS2 <- tidy(es2) %>% 
  as_tibble() %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(B)") + 
  geom_errorbar(aes(ymin = point.conf.low, ymax = point.conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Percent \n Change", x = "Years Relative to Deregulation") + 
  scale_x_continuous(breaks = seq(-10, 15, by = 5)) + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02),
                     label = scales::percent_format(accuracy = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# combine and save
BLL_CS <- BLL_CS1 + BLL_CS2

#save
ggsave(BLL_CS, filename = here::here("Figs_Tables", "BLL_CS.png"), 
       dpi = 500, width = 10, height = 20/6)

## Make the regression table
# run a static regression on the stacked dataset
# estimate the model
static_stack <- feols(log_gini ~ treat | wrkyr^dt + statefip^dt,
                      cluster = "cluster", data = stacked_data)

BLL_table <- bind_rows(
  bind_cols(
    # get the standard error and estimates into a table for mod1
    broom::tidy(mod1, se = "cluster") %>%
      mutate(variable = "Bank deregulation") %>% 
      select(variable, estimate, std.error) %>%
      mutate(t = estimate/std.error,
             estimate = paste0(as.character(format(round(estimate, 3), nsmall = 3)),
                               make_stars(abs(t), 1500)),
             std.error = paste0("(", as.character(format(round(std.error, 3), nsmall = 3)), ")")) %>% 
      select(-t) %>% 
      pivot_longer(cols = c(estimate, std.error),
                   names_to = "name",
                   values_to = "Log Gini"), 
    
    # get the standard error and estimates into a table for mod2
    broom::tidy(mod2, se = "cluster") %>%
      filter(term == "treat") %>%   
      mutate(variable = "Bank deregulation") %>% 
      select(variable, estimate, std.error) %>%
      mutate(t = estimate/std.error,
             estimate = paste0(as.character(format(round(estimate, 3), nsmall = 3)),
                               make_stars(abs(t), 1500)),
             std.error = paste0("(", as.character(format(round(std.error, 3), nsmall = 3)), ")")) %>% 
      select(-t) %>% 
      pivot_longer(cols = c(estimate, std.error),
                   names_to = "name",
                   values_to = "Log Gini") %>% 
      select(`Log Gini`)
  ) %>% set_names(c("variable", "name", "lg1", "lg2")),
  # add in adjusted r2 and the number of observations
  tibble(
    variable = c("Observations", 'Adj. R2'),
    name = rep("estimate", 2),
    "lg1" = c(as.character(format(round(broom::glance(mod1)$nobs, 0), nsmall = 0)), 
              as.character(format(round(broom::glance(mod1)$adj.r.squared, 2), nsmall = 2))),
    "lg2" = c(as.character(format(round(broom::glance(mod2)$nobs, 0), nsmall = 0)), 
              as.character(format(round(broom::glance(mod2)$adj.r.squared, 2), nsmall = 2)))
  )) %>% 
  # drop name and make table
  mutate(variable = if_else(name == "estimate", variable, NA_character_)) %>% 
  select(-name) %>% 
  # add in alternative estimators
  bind_rows(
    tibble(
      variable = c("Callaway & Sant'Anna", NA_character_, "Stacked Regression", NA_character_),
      lg1 = c(paste0(as.character(format(round(es2$overall.att, 3), nsmall = 3)),
                     make_stars(abs(es2$overall.att/es2$overall.se), 1500)),
              paste0("(", as.character(format(round(es2$overall.se, 3), nsmall = 3)), ")"),
              paste0(as.character(format(round(static_stack$coeftable[1], 3), nsmall = 3)),
                     make_stars(abs(static_stack$coeftable[1]/static_stack$coeftable[2]), 1500)),
              paste0("(", as.character(format(round(static_stack$coeftable[2], 3), nsmall = 3)), ")")),
      lg2 = rep(NA_character_, 4))
    ) %>% 
  kable("latex", escape = F, align = 'lcc',
        booktabs = T,
        col.names = c(" ", "Log Gini", "Log Gini"),
        label = "BLL_table", 
        caption = "The Impact of Deregulation on Income Inequality") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>% 
  add_header_above(c(" " = 1, "No \n Controls" = 1, "With \n Controls" = 1)) %>% 
  pack_rows("Alternative Estimators", 5, 8)

# save
write_lines(BLL_table, file = here::here("Figs_Tables", "BLL_table.tex"))