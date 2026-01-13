# Source: eurostat, Total unemployment rate
# URL: http://ec.europa.eu/eurostat/databrowser/view/tps00203/default/table?lang=en&category=t_labour.t_employ.t_lfsi.t_une

library(tidyverse)
library(skimr)
library(plm)

rm(list = ls())

unempl_raw <- read_csv("../data/raw/total-unemployment-rate_eurostat.csv")

# Description of the Dataset ----------------------------------------------

skim(unempl_raw)
unique(unempl_raw$`Geopolitical entity (reporting)`)
unique(unempl_raw$`TIME_PERIOD`)

## Check panel structure
is.pbalanced(unempl_raw %>% 
               rename(country = `Geopolitical entity (reporting)`,
                      period = `TIME_PERIOD`,
                      unempl_rate = `OBS_VALUE`) %>%
               select(country, period, unempl_rate) %>%
               pdata.frame(index = c("country", "period")))
unempl_raw %>%
  rename(country = `Geopolitical entity (reporting)`,
         period = `TIME_PERIOD`) %>%
  select(country, period) %>%
  table()

## The dataset contains information on the total unemployment rates in various European countries from 2013 to 2024.
## It is an unbalanced panel dataset as the observations for Bosnia and Herzegovina are only available from 2021 onwards and the observations for Montenegro are only available until 2020.
## The data is sourced from Eurostat and includes details such as the country, year, and the unemployment rate as percentage of population in the labour force (from 15 to 74).
## The observations are recorded on an annual basis.



# Cleaning & Data Exploration ---------------------------------------------

## multiple columns in the dataset are redundant as they either contain the same information for all rows (in this case, the information is already mentioned in the description of the dataset) or are fully empty and not needed for analysis.
## therefore, the cleaned dataset will only use selected columns that are relevant for the analysis.
unempl <- unempl_raw %>%
  select(c("country" = "Geopolitical entity (reporting)",
           "period" = "TIME_PERIOD",
           "unemployment_rate" = "OBS_VALUE"))

skim(unempl)

stats <- unempl %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(unemployment_rate)),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE),
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    sd_rate = sd(unemployment_rate, na.rm = TRUE)
  ) %>%
  arrange(country)


# Save cleaned data -------------------------------------------------------
write_csv(unempl, "../data/processed/unemployment_rates_clean.csv")
