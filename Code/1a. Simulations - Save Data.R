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

# source my passwords
source('/Users/andrew/Box Sync/Projects/Passwords/Password.R')

# Connect to WRDS Server --------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = user,
                  password = password,
                  dbname = 'wrds',
                  sslmode = 'require')

# download compustat data
comp <- tbl(wrds, sql("SELECT gvkey, fyear,  oibdp, at, indfmt, datafmt, popsrc, consol, fic, sich FROM comp.funda")) %>%
  # filter the data - between 1979 and 2015, non-missing assets, and in the US
  filter(indfmt == 'INDL' & datafmt == 'STD' & popsrc == 'D' & consol == 'C' & !is.na(fyear) & 
           fyear %>% between(1979, 2015) & !is.na(at) & at > 0 & fic == "USA") %>% 
  # make ROA variable
  group_by(gvkey) %>% 
  mutate(roa = oibdp / lag(at),
         gvkey = as.numeric(gvkey)) %>% 
  # drop missing ROA
  filter(!is.na(roa)) %>% 
  # drop 1979 observations - just needed lagged assets
  filter(fyear >= 1980) %>% 
  ungroup() %>% 
  collect()

# download comp header which has more info on location etc
comp_header <- tbl(wrds, sql("SELECT * FROM crsp.comphead")) %>% 
  mutate(gvkey = as.numeric(gvkey)) %>% 
  collect()

# merge in state of incorporation and industry information
comp <- comp %>% 
  left_join(comp_header %>% select(gvkey, incorp, sic)) %>% 
  # drop if state of incorporation is missing or not 50 states
  filter(!is.na(incorp) & incorp %in% state.abb) %>% 
  # clean up SIC code - use historical sic code if available, if not use header sic
  mutate(sich = coalesce(sich, sic)) %>% 
  # drop financial firms
  filter(!(sich %in% c(6000:6999)))

# make sure that each firm has at least 5 observations
comp <- comp %>% 
  group_by(gvkey) %>% 
  add_tally() %>% 
  filter(n >= 5) %>% 
  ungroup()

# winsorize ROA at 99, and censor at -1
wins <- function(x) {
  # winsorize and return
  case_when(
    is.na(x) ~ NA_real_,
    x < -1 ~ -1,
    x > quantile(x, 0.99, na.rm = TRUE) ~ quantile(x, 0.99, na.rm = TRUE),
    TRUE ~ x
  )
}

# winsorize ROA by year
comp <- comp %>% 
  group_by(fyear) %>% 
  mutate(roa = wins(roa)) %>% 
  arrange(gvkey, fyear) %>% 
  ungroup()

# save
saveRDS(comp, here::here("Data", "simulation_data.rds"))