# Source: eurostat, Population on 1 January
# URL: https://ec.europa.eu/eurostat/databrowser/view/tps00001/default/table?lang=en&category=t_demo.t_demo_pop

library(tidyverse)
library(skimr)
library(plm)

rm(list = ls())

pop_raw <- read_csv("../data/raw/population-on-1-January_eurostat.csv")

# Description of the Dataset ----------------------------------------------

skim(pop_raw)
unique(pop_raw$`Geopolitical entity (reporting)`)
unique(pop_raw$`TIME_PERIOD`)

## Check panel structure
is.pbalanced(pop_raw %>% 
               rename(country = `Geopolitical entity (reporting)`,
                      period = `TIME_PERIOD`,
                      inflation = `OBS_VALUE`) %>%
               select(country, period, inflation) %>%
               pdata.frame(index = c("country", "period")))
pop_raw %>%
  rename(country = `Geopolitical entity (reporting)`,
         period = `TIME_PERIOD`) %>%
  select(country, period) %>%
  table()

## The dataset contains information on the populations in various European countries from 2014 to 2025.
## It is an unbalanced panel dataset as there are several observations missing for the Andorra, Armenia, Belarus, Bosnia and Herzegovina, Kosovo, Monaco, Russia, San Marino, Ukraine, and the United Kingdom.
## The data is sourced from Eurostat and includes details such as the country, year, and the inflation rates.
## The observations are recorded on an annual basis.


# Cleaning & Data Exploration ---------------------------------------------

## multiple columns in the dataset are redundant as they either contain the same information for all rows (in this case, the information is already mentioned in the description of the dataset) or are fully empty and not needed for analysis.
## therefore, the cleaned dataset will only use selected columns that are relevant for the analysis.
pop <- pop_raw %>%
  select(c("country" = "Geopolitical entity (reporting)",
           "period" = "TIME_PERIOD",
           "population" = "OBS_VALUE"))

## The foot note indicator "*" in "Kosovo*" is from the original dataset. It indicates that the designation is without prejudice to positions on status, and is in line with UNSCR 1244/1999 and the ICJ Opinion on the Kosovo declaration of independence.
## To avoid confusion, the asterisk is removed in the cleaned dataset.
pop <- pop %>%
  mutate(country = str_replace(country, "Kosovo\\*", "Kosovo"))

skim(pop)

stats <- pop %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(population)),
    min_pop = min(population, na.rm = TRUE),
    max_pop = max(population, na.rm = TRUE),
    mean_pop = mean(population, na.rm = TRUE),
    sd_pop = sd(population, na.rm = TRUE)
  ) %>%
  arrange(country)


# Save cleaned data -------------------------------------------------------
write_csv(pop, "../data/processed/population_clean.csv")
