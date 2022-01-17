# load packages
library(tidyverse)
library(kableExtra)
library(bacondecomp)
library(ggthemes)
library(did)
library(patchwork)
library(fastDummies)
library(fixest)

# set themes and output location
select <- dplyr::select
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))
# save out into dropbox folder
options(knitr.kable.NA = '')

# set seed for CS bootstrap estimator to be replicable
set.seed(20210215)

# load the data
data <- haven::read_dta(here::here("Reps/FHLT", 'reformdata.dta'))

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

# function to get info from models
get_info <- function(est, modelname, type, variable) {
  broom::tidy(est) %>% 
    filter(term == variable) %>% 
    select(estimate, statistic, std.error) %>% 
    mutate(mod = modelname, type = type) 
}

# estimate the models
mod1 <- feols(qw ~ post + itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
               debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data)

mod2 <- feols(qw ~ post1 + itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
               debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code, 
             cluster = "ccode", data = data)

# estimate the two models without controls
mod3 <- feols(qw ~ post | year + code, cluster = "ccode", data = data)

mod4 <- feols(qw ~ post1 | year + code, cluster = "ccode", data = data)

# Event Study + Timing Graphs ---------------------------------------------
enacts <- bind_rows(
  # major reforms by country year
  data %>% 
    group_by(ccode, year) %>% 
    summarize(reform_type = "Major Reforms",
              post = mean(post),
              count = n()),
  # first reforms by country year
  data %>% 
    group_by(ccode, year) %>% 
    summarize(reform_type = "First Reforms",
              post = mean(post1),
              count = n())
) %>% 
  mutate(reform_type = factor(reform_type, 
                              levels = c("Major Reforms", "First Reforms")))

# make the timing plot
FHLT_TIMING <- enacts %>% 
  mutate(post = if_else(post == 1, "Post", "Pre"),
         post = factor(post, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(x = year, y = ccode)) + 
  geom_tile(aes(fill = as.factor(post), alpha = count)) + 
  scale_alpha(range = c(0.5, 1)) + 
  scale_fill_manual(values = c("#4B5F6C", "#A7473A")) + 
  labs(x = "Year", y = "Country") + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white")) + 
  guides(alpha = "none") + 
  facet_wrap(~reform_type)

# # make the event study estimates
# # function to estimate the event study DID by reform type and with and without covariates
# run_es <- function(reformtype, title, lastyear) {
#   
#   # make relative time dummies with data
#   dt <- data %>%
#     # drop after last treated year
#     filter(year < lastyear) %>% 
#     mutate(rel_year = year - {{reformtype}},
#            rel_year = if_else({{reformtype}} == lastyear, NA_real_, rel_year)) %>% 
#     # make dummies
#     dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
#                ignore_na = TRUE) %>% 
#     mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))
#   
#   # get the relative year indicators 
#   yrs <- sort(unique(dt$rel_year))
#   # drop most negative and time t = -1
#   yrs <- yrs[which(yrs != min(yrs) & yrs != -1)]
#   
#   # get the indicator names
#   indicators <- c(paste0("`", "rel_year_", yrs, "`"))
#   
#   # estimate the model
#   mod <- feols(qw ~ .[indicators] | year + code, cluster = "ccode", data = dt)
#   
#   # estimate the model and plot
#   broom::tidy(mod, conf.int = TRUE) %>%
#     # add in the relative time variable
#     mutate(t = yrs) %>% 
#     filter(t %>% between(-5, 5)) %>% 
#     select(t, estimate, conf.low, conf.high) %>% 
#     bind_rows(
#       tibble(
#         t = -1, estimate = 0, conf.low = 0, conf.high = 0
#       )
#     ) %>% 
#     # plot
#     ggplot(aes(x = t, y = estimate)) + 
#     geom_point(fill = "white", shape = 21) + geom_line() + 
#     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
#                   linetype = "longdash", show.legend = FALSE) + 
#     geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
#     geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
#     labs(y = "Effect", x = "Years Relative to Reform") + 
#     scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
#     ggtitle(title) + 
#     theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
#           plot.title = element_text(hjust = 0.5))
# 
# }
# 
# # estimate the two event studies
# FHLT_ES1 <- run_es(reform, "(A)", 2007)
# FHLT_ES2 <- run_es(firstreform, "(B)", 2006)
# 
# # combine the plots
# FHLT_ES <- FHLT_ES1 + FHLT_ES2
# 
# # combine the timing plot and the event study plots and save
# FHLT_TIMING_ES <- FHLT_TIMING + FHLT_ES + plot_layout(nrow = 2, heights = c(1.5, 1))

# save
ggsave(FHLT_TIMING, filename = here::here("Figs_Tables", "FHLT_TIMING.png"), 
       dpi = 500, width = 8, height = 4)

# Remedies ----------------------------------------------------------------
# Callaway Sant'Anna
# make id variable
ids <- tibble(
  code = unique(data$code)
) %>% 
  mutate(firm = 1:n())

# bring in id
data_cs <- data %>% 
  left_join(ids, by = "code") %>% 
  group_by(ccode) %>% 
  mutate(ccode = cur_group_id()) %>% 
  ungroup() %>% 
  select(qw, year, firm, reform, firstreform, ccode)

# run estimate
cs1 <- att_gt(yname = "qw",
              data = data_cs,
              tname = "year",
              idname = "firm",
              gname = "reform",
              clustervars = "ccode",
              bstrap = T,
              cband = T,
              est_method = "reg",
              xformla = NULL,
              control_group = "notyetreated",
              print_details = FALSE, 
              panel = TRUE,
              allow_unbalanced_panel = TRUE)

# make the dynamic event study
es1 <- aggte(cs1, type="dynamic", na.rm = TRUE, min_e = -5, max_e = 5)

# plot
FHLT_CS1 <- tidy(es1) %>% 
  as_tibble() %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(A)") + 
  geom_errorbar(aes(ymin = point.conf.low, ymax = point.conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Effect", x = "Years Relative to Reform") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# first reforms
# run CS estimator
cs2 <- att_gt(yname = "qw",
              data = data_cs,
              tname = "year",
              idname = "firm",
              gname = "firstreform",
              clustervars = "ccode",
              bstrap = T,
              cband = T,
              est_method = "reg",
              xformla = NULL,
              control_group = "notyettreated",
              print_details = FALSE, 
              panel = TRUE,
              allow_unbalanced_panel = TRUE)

# make the dynamic event study
es2 <- aggte(cs2, type="dynamic",  min_e = -5, max_e =5, na.rm = TRUE)

# plot
FHLT_CS2 <- broom::tidy(es2) %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(B)") + 
  geom_errorbar(aes(ymin = point.conf.low, ymax = point.conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Effect", x = "Years Relative to Reform") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# stacked regression with full exclusion
stacked <- function(reformvar, lastyear, title) {
  
  # get treated years that we can estimate
  treats <- data %>% 
    filter({{reformvar}} < lastyear) %>% 
    pull({{reformvar}}) %>% 
    unique() %>% 
    sort()
  
  # function to get treat-year specific datasets
  make_dt <- function(tyr) {
    data %>% 
      filter(year < lastyear) %>% 
      # keep firms in the adopt year pre-treatment observations
      filter({{reformvar}} == tyr | ({{reformvar}} > tyr & year < {{reformvar}})) %>% 
      # keep just years t -5 to t + 5
      filter(year %>% between(tyr - 5, min(tyr + 5, lastyear - 1))) %>% 
      # replace adopt year to NA to make dummies
      mutate(newyear = if_else({{reformvar}} == tyr, {{reformvar}}, NA_real_),
             rel_year = year - newyear,
             treat = if_else(is.na(newyear) | year < newyear, 0, 1)) %>% 
      select(code, year, ccode, newyear, rel_year, qw, treat) %>% 
      mutate(dt = as.character(tyr))
  }
  
  # run over out treated years
  stacked_data <- map_dfr(treats, make_dt) %>% 
    dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
               ignore_na = TRUE) %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
    mutate(cluster = paste0(ccode, "_", dt))
  
  # make formula
  yrs <- sort(unique(stacked_data$rel_year))
  
  # drop time t = -1
  yrs <- yrs[which(yrs != -1)]
  
  # make covariates and formula
  indicators <- c(paste0("`", "rel_year_", yrs, "`"))
  
  # estimate the model
  mod <- feols(qw ~ .[indicators] | year^dt + code^dt, cluster = "cluster", data = stacked_data)
  
  # plot
  plot <- broom::tidy(mod, conf.int = TRUE) %>%
    # add in the relative time variable
    mutate(t = yrs) %>% 
    filter(t %>% between(-5, 5)) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(
      tibble(
        t = -1, estimate = 0, conf.low = 0, conf.high = 0
      )
    ) %>% 
    # plot
    ggplot(aes(x = t, y = estimate)) + 
    geom_point(fill = "white", shape = 21) + geom_line() + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  linetype = "longdash", show.legend = FALSE) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
    labs(y = "Effect", x = "Years Relative to Reform") + 
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
    ggtitle(title) + 
    theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
          plot.title = element_text(hjust = 0.5))
  
  # estimate the static reg
  static_reg <- feols(qw ~ treat | year^dt + code^dt, cluster = "cluster", data = stacked_data)
  
  # output both
  list(plot = plot,
       static_reg = static_reg)
}

# estimate the two event studies
fhlt_stack_reform <- stacked(reform, 2007, "(A)")
fhlt_stack_firstreform <- stacked(firstreform, 2006, "(B)")

# estimate the two event studies
FHLT_stack1 <- fhlt_stack_reform$plot

# do same for plot 2
FHLT_stack2 <- fhlt_stack_firstreform$plot

# combine and save
FHLT_CS <- FHLT_CS1 + FHLT_CS2 
FHLT_STACK <-  FHLT_stack1 + FHLT_stack2
ggsave(FHLT_CS, filename = here::here("Figs_Tables", "FHLT_CS.png"), 
       dpi = 800, width = 10, height = 20/6)
ggsave(FHLT_STACK, filename = here::here("Figs_Tables", "FHLT_STACK.png"), 
       dpi = 800, width = 10, height = 20/6)

### Make the regression table
# first get the static stacked regresion estimates
static_stack1 <- fhlt_stack_reform$static_reg
static_stack2 <- fhlt_stack_firstreform$static_reg

# show the with and without controls side by side
FHLT_table <- bind_rows(
  get_info(mod1, "Major Reform", "Controls", "post"),
  get_info(mod2, "First Reform", "Controls", "post1"),
  get_info(mod3, "Major Reform", "No Controls", "post"),
  get_info(mod4, "First Reform", "No Controls", "post1"),
) %>%
  rowwise() %>% 
  # make estimate and statistic into three digits with stars
  mutate(estimate = paste0(as.character(format(round(estimate, 3), nsmall = 3)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  ungroup() %>% 
  # push and pull
  pivot_longer(cols = c(estimate, std.error),
               names_to = "variable",
               values_to = "value") %>% 
  pivot_wider(id_cols = variable, 
              names_from = c(mod, type), 
              values_from = c(value)) %>% 
  select(-variable) %>% 
  # add in rows for controls and such
  bind_rows(
    tibble(
      `Major Reform_Controls` = c(rep("Yes", 3), "196,016", 
                                  as.character(format(round(broom::glance(mod1)$adj.r.squared, 3), nsmall = 3))),
      `First Reform_Controls` = c(rep("Yes", 3), "196,016", 
                                  as.character(format(round(broom::glance(mod2)$adj.r.squared, 3), nsmall = 3))),
      `Major Reform_No Controls` = c("No", rep("Yes", 2), "196,016",
                                     as.character(format(round(broom::glance(mod3)$adj.r.squared, 3), nsmall = 3))),
      `First Reform_No Controls` = c("No", rep("Yes", 2), "196,016", 
                                     as.character(format(round(broom::glance(mod4)$adj.r.squared, 3), nsmall = 3)))
    )
  ) %>% 
  mutate(Variable = c("Post", NA_character_, "Control variables", "Firm fixed effects", 
                      "Year fixed effects", "Observations", "Adj. R2")) %>% 
  select(Variable, everything()) %>% 
  # add in new estimators
  bind_rows(
    tibble(
      Variable = c("Callaway & Sant'Anna", NA_character_, "Stacked Regression", NA_character_),
      `Major Reform_Controls` = rep(NA_character_, 4),
      `First Reform_Controls` = rep(NA_character_, 4),
      `Major Reform_No Controls` = c(paste0(as.character(format(round(es1$overall.att, 3), nsmall = 3)),
                                            make_stars(abs(es1$overall.att/es1$overall.se), 1500)),
                                     paste0("(", as.character(format(round(es1$overall.se, 3), nsmall = 3)), ")"),
                                     paste0(as.character(format(round(static_stack1$coeftable[1], 3), nsmall = 3)),
                                            make_stars(abs(static_stack1$coeftable[1]/static_stack1$coeftable[2]), 1500)),
                                     paste0("(", as.character(format(round(static_stack1$coeftable[2], 3), nsmall = 3)), ")")),
      `First Reform_No Controls` = c(paste0(as.character(format(round(es2$overall.att, 3), nsmall = 3)),
                                            make_stars(abs(es2$overall.att/es2$overall.se), 1500)),
                                     paste0("(", as.character(format(round(es2$overall.se, 3), nsmall = 3)), ")"),
                                     paste0(as.character(format(round(static_stack2$coeftable[1], 3), nsmall = 3)),
                                            make_stars(abs(static_stack2$coeftable[1]/static_stack2$coeftable[2]), 1500)),
                                     paste0("(", as.character(format(round(static_stack2$coeftable[2], 3), nsmall = 3)), ")"))
    )
  ) %>% 
  # make and report table
  kable("latex", align = 'lcccc', booktabs = T,
        col.names = c("Variable", rep(c("Major Reform", "First Reform"), 2)),
        label = "FHLT_table", 
        caption = "The Impact of Board Reforms on Firm Value") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>% 
  add_header_above(c(" " = 1, "With Covariates" = 2, "Without Covariates" = 2)) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 4)) %>% 
  pack_rows("Alternative Estimators", 8, 11)

# save
write_lines(FHLT_table, file = here::here("Figs_Tables", "FHLT_table.tex"))

## Finally, plots showing event study design changes
# make a restricted dataset that FHLT use for event studies
data_restricted <- data %>% 
  filter(nreform <= 5) %>% 
  # make relative time indicators - they use the year before the indicator
  mutate(`rel_year_-4` = if_else(year - reform == -5, 1, 0),
         `rel_year_-3` = if_else(year - reform == -4, 1, 0),
         `rel_year_-2` = if_else(year - reform == -3, 1, 0),
         `rel_year_-1` = if_else(year - reform == -2, 1, 0),
         rel_year_0 = if_else(year - reform == -1, 1, 0),
         rel_year_1 = if_else(year - reform == 0, 1, 0),
         rel_year_2 = if_else(year - reform == 1, 1, 0),
         rel_year_3 = if_else(year - reform == 2, 1, 0),
         rel_year_4 = if_else(year - reform == 3, 1, 0),
         rel_year_5 = if_else(year - reform == 4, 1, 0),
         rel_year_6 = if_else(year - reform == 5, 1, 0),
         rel_year_2_plus = if_else(year - reform >= 1, 1, 0))

# model one - as published
mod1 <- feols(qw ~ `rel_year_-1` + rel_year_0 + rel_year_1 + rel_year_2_plus + 
                itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
                debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  ## keep just the variables we need and add in time indicators
  filter(str_detect(term, "rel_year")) %>% 
  mutate(t = c(-1, 0, 1, 2),
         model = "Model 1")

# model 2 - drop the binning
mod2 <- feols(qw ~ `rel_year_-3` + `rel_year_-2` + `rel_year_-1` + rel_year_0 + rel_year_1 +
                rel_year_2_plus + itenforce + postto + divtax + capgaintax + loggdppc + fdi +
                rulelaw + lntaw + logage + debttaw + cashtoaw + ppesalesw + forsale2w + 
                rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  ## keep just the variables we need and add in time indicators
  filter(str_detect(term, "rel_year")) %>% 
  mutate(t = c(-3:2),
         model = "Model 2")

# model 3 - fully saturate the model
mod3 <- feols(qw ~ `rel_year_-3` + `rel_year_-2` + `rel_year_-1` + rel_year_1 + rel_year_2 + rel_year_3 + 
                rel_year_4 + rel_year_5 + rel_year_6 + 
                itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
                debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  ## keep just the variables we need and add in time indicators
  filter(str_detect(term, "rel_year")) %>%
  mutate(t = c(-3:-1, 1:6)) %>% 
  # add in the relative indicator for 0
  bind_rows(tibble(
    term = "rel_year_0", estimate = 0, std.error = 0, statistic = 0.,
    p.value = 0, conf.low = 0, conf.high = 0, t = 0
  )) %>%
  mutate(model = "Model 3")

## do our model event study
# make relative time dummies with data
dt <- data %>%
  # drop after last treated year
  filter(year < 2007) %>% 
  mutate(rel_year = year - reform + 1,
         rel_year = if_else(reform == 2007, NA_real_, rel_year)) %>% 
  # make dummies
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))

# get the relative year indicators 
yrs <- sort(unique(dt$rel_year))
# drop most negative and time t = -1
yrs <- yrs[which(yrs != min(yrs) & yrs != 0)]

# get the indicator names
indicators <- c(paste0("`", "rel_year_", yrs, "`"))

# estimate the model
mod4 <- feols(qw ~ .[indicators] | year + code, cluster = "ccode", data = dt) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(str_detect(term, "rel_year")) %>%
  mutate(t = yrs) %>% 
  filter(t %>% between(-4, 6)) %>%  
  bind_rows(tibble(
    term = "rel_year_0", estimate = 0, std.error = 0, statistic = 0.,
    p.value = 0, conf.low = 0, conf.high = 0, t = 0
  )) %>%
  mutate(model = "Model 4")

# combine plots for major reform
p1 <- bind_rows(mod1, mod2, mod3, mod4) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  scale_x_continuous(breaks = seq(-4, 6, by = 1)) + 
  scale_color_manual(values = c("#44781E", "#2C3B75", "#B8321A")) + 
  labs(x = "Relative Year", y = "Estimate") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) + 
  facet_wrap(~model, scales = "free_x", nrow = 1)

# Now do the same thing for first reforms
# make a restricted dataset
data_restricted <- data %>% 
  filter(nreform1 <= 5) %>% 
  mutate(`rel_year_-4` = if_else(year - firstreform == -5, 1, 0),
         `rel_year_-3` = if_else(year - firstreform == -4, 1, 0),
         `rel_year_-2` = if_else(year - firstreform == -3, 1, 0),
         `rel_year_-1` = if_else(year - firstreform == -2, 1, 0),
         rel_year_0 = if_else(year - firstreform == -1, 1, 0),
         rel_year_1 = if_else(year - firstreform == 0, 1, 0),
         rel_year_2 = if_else(year - firstreform == 1, 1, 0),
         rel_year_3 = if_else(year - firstreform == 2, 1, 0),
         rel_year_4 = if_else(year - firstreform == 3, 1, 0),
         rel_year_5 = if_else(year - firstreform == 4, 1, 0),
         rel_year_6 = if_else(year - firstreform == 5, 1, 0),
         rel_year_2_plus = if_else(year - firstreform >= 1, 1, 0))

# model one - as published
mod1 <- feols(qw ~ `rel_year_-1` + rel_year_0 + rel_year_1 + rel_year_2_plus + 
                itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
                debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(str_detect(term, "rel_year")) %>% 
  mutate(t = c(-1, 0, 1, 2),
         model = "Model 1")

# model 2 - drop the binning
mod2 <- feols(qw ~ `rel_year_-3` + `rel_year_-2` + `rel_year_-1` + rel_year_0 + rel_year_1 +
                rel_year_2_plus + itenforce + postto + divtax + capgaintax + loggdppc + fdi +
                rulelaw + lntaw + logage + debttaw + cashtoaw + ppesalesw + forsale2w + 
                rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(str_detect(term, "rel_year")) %>% 
  mutate(t = c(-3:2),
         model = "Model 2")

# model 3 - fully saturate the model
mod3 <- feols(qw ~ `rel_year_-3` + `rel_year_-2` + `rel_year_-1` + rel_year_1 + rel_year_2 + rel_year_3 + 
                rel_year_4 + rel_year_5 + rel_year_6 + 
                itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
                debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data_restricted) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(str_detect(term, "rel_year")) %>%
  mutate(t = c(-3:-1, 1:6)) %>% 
  bind_rows(tibble(
    term = "rel_year_0", estimate = 0, std.error = 0, statistic = 0.,
    p.value = 0, conf.low = 0, conf.high = 0, t = 0
  )) %>%
  mutate(model = "Model 3")

## do our model event study
# make relative time dummies with data
dt <- data %>%
  # drop after last treated year
  filter(year < 2006) %>% 
  mutate(rel_year = year - firstreform + 1,
         rel_year = if_else(firstreform == 2006, NA_real_, rel_year)) %>% 
  # make dummies
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))

# get the relative year indicators 
yrs <- sort(unique(dt$rel_year))
# drop most negative and time t = -1
yrs <- yrs[which(yrs != min(yrs) & yrs != 0)]

# get the indicator names
indicators <- c(paste0("`", "rel_year_", yrs, "`"))

# estimate the model
mod4 <- feols(qw ~ .[indicators] | year + code, cluster = "ccode", data = dt) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(str_detect(term, "rel_year")) %>%
  mutate(t = yrs) %>% 
  filter(t %>% between(-4, 6)) %>%  
  bind_rows(tibble(
    term = "rel_year_0", estimate = 0, std.error = 0, statistic = 0.,
    p.value = 0, conf.low = 0, conf.high = 0, t = 0
  )) %>%
  mutate(model = "Model 4")

# combine plots for major reform
p2 <- bind_rows(mod1, mod2, mod3, mod4) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash", show.legend = FALSE) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  scale_color_manual(values = c("#44781E", "#2C3B75", "#B8321A")) + 
  scale_x_continuous(breaks = seq(-4, 6, by = 1)) + 
  labs(x = "Relative Year", y = "Estimate") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360)) + 
  facet_wrap(~model, scales = "free_x", nrow = 1)

# save
ggsave(p1, file = here::here("Figs_Tables", "modifed_event_studies_1.png"),
       dpi = 800, width = 10, height = 3)
ggsave(p2, file = here::here("Figs_Tables", "modifed_event_studies_2.png"),
       dpi = 800, width = 10, height = 3)