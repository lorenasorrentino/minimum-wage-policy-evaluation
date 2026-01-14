library(tidyverse)
library(skimr)

rm(list = ls())

min_wage <- read_csv("../data/processed/min_wage_clean.csv")
unempl <- read_csv("../data/processed/unemployment_rates_clean.csv")
GDP <- read_csv("../data/processed/GDP_clean.csv")
infl <- read_csv("../data/processed/inflation_rates_clean.csv")
pop <- read_csv("../data/processed/population_clean.csv")


# Ensure uniform format ---------------------------------------------------

# Before building the panel, the periods in the dataset min_wage will be aggregated into a yearly format.
# For this, the average minimum wage for each year is calculated.
min_wage <- min_wage %>%
  mutate(year = str_sub(period, 1, 4)) %>%
  group_by(country, year) %>%
  summarise(minimum_wage_euro = mean(minimum_wage_euro, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(period = year)

# The variable period in the dataset min_wage will also be converted to numeric.
min_wage <- min_wage %>%
  mutate(period = as.numeric(period))

# Now, all datasets are in a yearly format and can be merged to build the panel dataset.


# Build panel data --------------------------------------------------------

panel_data <- min_wage %>%
  full_join(unempl, by = c("country", "period")) %>% 
  full_join(GDP, by = c("country", "period")) %>%
  full_join(infl, by = c("country", "period")) %>%
  full_join(pop, by = c("country", "period"))

panel_data <- panel_data %>% rename("year" = "period")

skim(panel_data)


# Save panel data ---------------------------------------------------------

write_csv(panel_data, "../data/processed/panel_final.csv")
