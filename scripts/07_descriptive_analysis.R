
# In the following I will compute descriptive analysis in order to properly understand variation across countries and time before conducting the regression analysis.
# It also serves as a check to make sure that the data has been cleaned properly and there are no obvious errors.

library(tidyverse)
library(readr)

panel <- read_csv("../data/processed/panel_final.csv")


# Summary Statistics ------------------------------------------------------

glimpse(panel)
summary(panel)

# The summary statistics show that there are considerable variations across countries and over time, particularly for minimum wage levels and unemployment rates.

# Notably, the number of missing observations differs substantially across variables. This is mainly due to differences in data availability across countries and years in the original sources.
# In particular, statutory minimum wage data are only reported for countries with a legally defined minimum wage, leading to a large number of missing values for this variable.


# Unemployment Rate Trends ------------------------------------------------

panel %>% 
  group_by(year) %>% 
  summarise(unempl_rate_trend = mean(unemployment_rate, na.rm = TRUE))

ggplot(panel, aes(x = year, y = unemployment_rate)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Unemployment Rate Over Time",
       x = "Year",
       y = "Average Unemployment Rate (%)")
ggsave("../figures/avg_unempl_rate_trend.png")

# The figure shows the average unemployment rate across countries over time.
# Unemployment declines steadily from 12% in 2013 to below 7% in 2019.
# In 2020, unemployment temporarily increases, consistent with the economic disruption caused by the COVID-19 pandemic.
# From 2021 onwards, unemployment resumes its downward trend, reaching historically low levels by 2023-2024.

ggplot(panel %>% filter(country %in% c("Germany", "France", "Spain", "Belgium", "Türkiye", "Poland")), 
       aes(x = year, y = unemployment_rate, color = country)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Unemployment Rate Over Time by Country",
       x = "Year",
       y = "Unemployment Rate (%)")
ggsave("../figures/unempl_rate_trends_by_country.png")

# The figure highlights substential heterogeneity in unemployment dynamics across countries.
# While some countries, such as Germany, display persistently low unemployment rates, others, such as Spain and Türkiye, exhibit higher levels and greater volatility.
# These differences motivate the use of panel data techniques that control for time-invariant country characteristics.


# Minimum Wage Trends -----------------------------------------------------

panel %>% 
  group_by(year) %>% 
  summarise(min_wage_trend = mean(minimum_wage_euro, na.rm = TRUE))

ggplot(panel, aes(x = year, y = minimum_wage_euro)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Minimum Wage Over Time",
       x = "Year",
       y = "Average Minimum Wage (Euro)")
ggsave("../figures/avg_min_wage_trend.png")

ggplot(panel %>% filter(country %in% c("Germany", "France", "Spain", "Belgium", "Türkiye", "Poland")), 
       aes(x = year, y = minimum_wage_euro, color = country)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Minimum Wage Over Time by Country",
       x = "Year",
       y = "Minimum Wage (Euro)")
ggsave("../figures/min_wage_trends_by_country.png")

# Both figure shows that from 2021 onwards, average minimum wage levels generally increased, coinciding with a period of elevated inflation and active minimum wage adjustments in many countries.

# Consequently, while unemployment rates decline steadily over the sample period, minimum wage levels rise markedly.
# This combination of falling unemployment and increasing minimum wages motivates the subsequent regression analysis examining the relationship between minimum wage policies and labor market outcomes.