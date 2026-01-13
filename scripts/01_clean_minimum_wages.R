# Source: eurostat, Minimum wages
# URL: https://ec.europa.eu/eurostat/databrowser/view/tps00155/default/table?lang=en&category=t_labour.t_earn

library(tidyverse)
library(skimr)
library(plm)

rm(list = ls())

min_wage_raw <- read_csv("../data/raw/minimum-wages_eurostat.csv")


# Description of the Dataset ----------------------------------------------

skim(min_wage_raw)
unique(min_wage_raw$`Geopolitical entity (reporting)`)
unique(min_wage_raw$`TIME_PERIOD`)

## Check panel structure
is.pbalanced(min_wage_raw %>% 
               rename(country = `Geopolitical entity (reporting)`,
                      period = `TIME_PERIOD`,
                      minimum_wage_euro = `OBS_VALUE`) %>%
               # filter(country != "United Kingdom") %>% 
               select(country, period, minimum_wage_euro) %>%
               pdata.frame(index = c("country", "period")))
min_wage_raw %>%
  rename(country = `Geopolitical entity (reporting)`,
         period = `TIME_PERIOD`) %>%
  select(country, period) %>%
  table()
filter(min_wage_raw, `Geopolitical entity (reporting)` == "United Kingdom")

## The dataset contains information on minimum wages in various European countries as well as the United States of America from 2020 until 2025.
## It is an unbalanced panel dataset as the observations for the United Kingdom are only available for 2020.
## The data is sourced from Eurostat and includes details such as the country, year, and the minimum wage amount in euros.
## The dataset provides semi-annual observations (S1 and S2) for each year, representing the minimum wage values for the first and second halves of the year, respectively.


# Cleaning & Data Exploration ---------------------------------------------

## multiple columns in the dataset are redundant as they either contain the same information for all rows (in this case, the information is already mentioned in the description of the dataset) or are fully empty and not needed for analysis.
min_wage <- min_wage_raw %>% 
  select(-c("STRUCTURE", "STRUCTURE_ID", "STRUCTURE_NAME", 
            "freq", "Time frequency",                               # The observations are recorded on a semi-annual basis in all cases, as mentioned in the description of the dataset.
            "currency", "Currency",                                 # The currency is Euro in all cases, as mentioned in the description of the dataset.
            "geo",                                                  # The information about the country is already contained in the column "Geopolitical entity (reporting)".
            "Time",                                                 # The column Time is fully empty. The information about the time period is contained in the column "TIME_PERIOD".
            "Observation value",                                    # The column "OBS_VALUE" already contains the minimum wage values.
            "OBS_FLAG", "Observation status (Flag) V2 structure",   # The missing values are already indicated with "NA" in the column "OBS_VALUE".
            "CONF_STATUS", "Confidentiality status (flag)"))        # Both columns are fully empty.

## for better readability, the columns will be renamed
min_wage <- min_wage %>% 
  rename(
    country = `Geopolitical entity (reporting)`,
    period = `TIME_PERIOD`,
    minimum_wage_euro = `OBS_VALUE`
  )

skim(min_wage)

## There are some countries that have no observations. These will be removed from the dataset as they do not contribute any information for the analysis.
NA_countries <- min_wage %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(minimum_wage_euro))
  ) %>%
  filter(n_obs == n_missing)
min_wage <- min_wage %>%
  filter(!country %in% NA_countries$country)

skim(min_wage)

stats <- min_wage %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(minimum_wage_euro)),
    min_min_wage = min(minimum_wage_euro, na.rm = TRUE),
    max_min_wage = max(minimum_wage_euro, na.rm = TRUE),
    mean_wage = mean(minimum_wage_euro, na.rm = TRUE),
    sd_wage = sd(minimum_wage_euro, na.rm = TRUE)
  ) %>%
  arrange(country)


# Save cleaned data -------------------------------------------------------
write_csv(min_wage, "../data/processed/min_wage_clean.csv")
