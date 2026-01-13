# Source: eurostat, HICP - inflation rate
# URL: https://ec.europa.eu/eurostat/databrowser/view/tec00118/default/table?lang=en&category=t_prc.t_prc_hicp

library(tidyverse)
library(skimr)
library(plm)

rm(list = ls())

infl_raw <- read_csv("../data/raw/HICP-inflation-rate_eurostat.csv")

# Description of the Dataset ----------------------------------------------

skim(infl_raw)
unique(infl_raw$`Geopolitical entity (reporting)`)
unique(infl_raw$`TIME_PERIOD`)

## Check panel structure
is.pbalanced(infl_raw %>% 
               rename(country = `Geopolitical entity (reporting)`,
                      period = `TIME_PERIOD`,
                      inflation = `OBS_VALUE`) %>%
               select(country, period, inflation) %>%
               pdata.frame(index = c("country", "period")))
infl_raw %>%
  rename(country = `Geopolitical entity (reporting)`,
         period = `TIME_PERIOD`) %>%
  select(country, period) %>%
  table()

## The dataset contains information on the inflation rates in various European countries and Russia from 2013 to 2024.
## It is an unbalanced panel dataset as there are several observations missing for the United Kingdom, Montenegro, Kosovo, and Albania.
## The data is sourced from Eurostat and includes details such as the country, year, and the inflation rates.
## The observations are recorded on an annual basis.


# Cleaning & Data Exploration ---------------------------------------------

## multiple columns in the dataset are redundant as they either contain the same information for all rows (in this case, the information is already mentioned in the description of the dataset) or are fully empty and not needed for analysis.
## therefore, the cleaned dataset will only use selected columns that are relevant for the analysis.
infl <- infl_raw %>%
  select(c("country" = "Geopolitical entity (reporting)",
           "period" = "TIME_PERIOD",
           "inflation_rate" = "OBS_VALUE"))

## The foot note indicator "*" in "Kosovo*" is from the original dataset. It indicates that the designation is without prejudice to positions on status, and is in line with UNSCR 1244/1999 and the ICJ Opinion on the Kosovo declaration of independence.
## To avoid confusion, the asterisk is removed in the cleaned dataset.
infl <- infl %>%
  mutate(country = str_replace(country, "Kosovo\\*", "Kosovo"))

skim(infl)

stats <- infl %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(inflation_rate)),
    min_infl = min(inflation_rate, na.rm = TRUE),
    max_infl = max(inflation_rate, na.rm = TRUE),
    mean_infl = mean(inflation_rate, na.rm = TRUE),
    sd_infl = sd(inflation_rate, na.rm = TRUE)
  ) %>%
  arrange(country)


# Save cleaned data -------------------------------------------------------
write_csv(infl, "../data/processed/inflation_rates_clean.csv")
