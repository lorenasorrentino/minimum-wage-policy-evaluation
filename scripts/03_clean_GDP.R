# Source: eurostat, GDP per capita in PPS
# URL: https://ec.europa.eu/eurostat/databrowser/view/tec00114/default/table?lang=en&category=t_prc.t_prc_ppp

library(tidyverse)
library(skimr)
library(plm)

rm(list = ls())

GDP_raw <- read_csv("../data/raw/GDP-per-capita-in-PPS_eurostat.csv")

# Description of the Dataset ----------------------------------------------

skim(GDP_raw)
unique(GDP_raw$`Geopolitical entity (reporting)`)
unique(GDP_raw$`TIME_PERIOD`)

## Check panel structure
is.pbalanced(GDP_raw %>% 
               rename(country = `Geopolitical entity (reporting)`,
                      period = `TIME_PERIOD`,
                      GDP = `OBS_VALUE`) %>%
               select(country, period, GDP) %>%
               pdata.frame(index = c("country", "period")))
GDP_raw %>%
  rename(country = `Geopolitical entity (reporting)`,
         period = `TIME_PERIOD`) %>%
  select(country, period) %>%
  table()

## The dataset contains information on the GDP (in purchasing power parities (PPPs)) in various European countries from 2013 to 2024.
#### (The use of PPPs ensures that the GDP of all countries is valued at a uniform price level and thus reflects only differences in the actual volume of the economy.)
## It is an unbalanced panel dataset as the observations for the United Kingdom are only available until 2020.
## The data is sourced from Eurostat and includes details such as the country, year, and the GDP.
## The observations are recorded on an annual basis.



# Cleaning & Data Exploration ---------------------------------------------

## multiple columns in the dataset are redundant as they either contain the same information for all rows (in this case, the information is already mentioned in the description of the dataset) or are fully empty and not needed for analysis.
## therefore, the cleaned dataset will only use selected columns that are relevant for the analysis.
GDP <- GDP_raw %>%
  select(c("country" = "Geopolitical entity (reporting)",
           "period" = "TIME_PERIOD",
           "GDP_in_PPS" = "OBS_VALUE"))

skim(GDP)

stats <- GDP %>%
  group_by(country) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(GDP_in_PPS)),
    min_GDP = min(GDP_in_PPS, na.rm = TRUE),
    max_GDP = max(GDP_in_PPS, na.rm = TRUE),
    mean_GDP = mean(GDP_in_PPS, na.rm = TRUE),
    sd_GDP = sd(GDP_in_PPS, na.rm = TRUE)
  ) %>%
  arrange(country)


# Save cleaned data -------------------------------------------------------
write_csv(GDP, "../data/processed/GDP_clean.csv")
