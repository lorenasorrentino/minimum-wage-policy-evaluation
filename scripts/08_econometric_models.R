library(tidyverse)
library(lmtest)
library(skimr)
library(plm)
library(sandwich)
library(car)

rm(list = ls())

panel <- read_csv("../data/processed/panel_final.csv")


# Sample Restrictions -----------------------------------------------------

# Notably, in R, any regression automatically drops rows with missing values, which means that each model may use a different sample size.
# This would affect the comparability across models and their interpretation.
# Therefore, I will explicitly restrict the sample to observations with non-missing values for all variables.

reg_data <- panel %>% 
  select(country, year, unemployment_rate, minimum_wage_euro, GDP_in_PPS, inflation_rate, population) %>%
  drop_na()

skim(reg_data)
summary(reg_data)
glimpse(reg_data)

unique(reg_data$country)

is.pbalanced(reg_data %>%
               pdata.frame(index = c("country", "year")))
reg_data %>%
  select(country, year) %>%
  table()

# The regression sample includes 26 countries observed over the period from 2020 to 2024. It is an unbalanced panel dataset as the observations for some countries are not available for all years.
# All regressions will be estimated using complete-case observations.


# Baseline Pooled OLS -----------------------------------------------------

# This pooled OLS specification provides a first descriptive assessment of the relationship between minimum wages and unemployment.
# The model provides a descriptive benchmark. It ignores country differences and common shocks such as COVID.

pooled_ols_bm <- lm(unemployment_rate ~ minimum_wage_euro, data = reg_data)
summary(pooled_ols_bm)

# In this pooled OLS specification without additional controls, higher minimum wage levels are associated with lower unemployment rates.
# The model says that a 100 € increase in the minimum wage is associated with a 0.167 percentage point decrease in unemployment. While this is statistically significant, the economic magnitude is small.

# It is crucial to mention that this baseline pooled OLS estimates the relationship between minimum wages and unemployment without accounting for country-specific characteristics, which makes it subject to omitted variable bias.
# Countries with higher minimum wages are often more economically developed and have lower structural unemployment, so the observed negative association may reflect cross-country differences rather than causal effects.


# Pooled OLS Including Controls -------------------------------------------

# To mitigate the issue of omitted variable bias, the analysis will first include macroeconomic controls that are likely correlated with both minimum wage levels and unemployment rates.

pooled_ols <- lm(unemployment_rate ~ minimum_wage_euro + GDP_in_PPS + inflation_rate + population, data = reg_data)
summary(pooled_ols)

# After controlling for macroeconomic conditions, the minimum wage effect disappears and the GDP becomes the important predictor of unemployment.
# Notably, countries with higher minimum wages also tend to have higher GDP.


# Country Fixed Effects ---------------------------------------------------

# While pooled OLS specifications provide a useful descriptive benchmark, they do not account for unobserved country-specific characteristics that may be correlated with both minimum wage levels and unemployment.
# To address this concern, the analysis proceeds with fixed effects models that control for time-invariant heterogeneity across countries.
# This approach exploits within-country variation over time and substantially reduces omitted variable bias arising from persistent cross-country differences.

# Given the short panel structure, fixed effects are preferred to first-difference estimators, as they exploit within-country variation while retaining more observations.

fe_model <- plm(unemployment_rate ~ 0 + minimum_wage_euro + GDP_in_PPS + inflation_rate + population, 
                data = reg_data,
                index = c("country", "year"),
                model = "within")
summary(fe_model)

# These fixed effects estimates indicate a statistically significant association between minimum wage increases and lower unemployment within countries over time.
# However, these results should be interpreted with caution, as the analysis does not exploit a clearly exogenous source of variation in minimum wage policies.

# Notably, a difference-in-differences approach is not employed, as minimum wage changes occur gradually and across multiple countries rather than through a single, discrete policy intervention with a clear treatment and control group.


# Main Specification: Two-Way Fixed Effects -------------------------------

# The two-way fixed effects model (country and year fixed effects) is chosen as the main specification.
# It controls for time-invariant country characteristics as well as common macroeconomic shocks affecting all countries in a given year, thereby minimizing omitted variable bias.

fe_tw_model <- plm(unemployment_rate ~ 0 + minimum_wage_euro + GDP_in_PPS + inflation_rate + population,
                data = reg_data,
                index = c("country", "year"),
                model = "within",
                effect = "twoways")
summary(fe_tw_model)

# Once year fixed effects are included, the minimum wage coefficient becomes statistically insignificant.
# This suggests that the association observed in simpler specifications largely reflects common macroeconomic shocks and synchronized policy adjustments rather than independent within-country variations in minimum wage policies.



# Robust Standard Errors with Clustering ----------------------------------

fe_tw_clustered <- coeftest(
  fe_tw_model,
  vcov = vcovHC(fe_tw_model, type = "HC1", cluster = "group")
)
fe_tw_clustered

# Standard errors are clustered at the country level to account for serial correlation and heteroskedasticity within countries over time.


# Multicollinearity -------------------------------------------------------

vif(pooled_ols)

# Variance Inflation Factors (VIFs) are below conventional thresholds, suggesting that multicollinearity is not a major concern in the pooled specification.
# While multicollinearity may increase in fixed effects models by construction, this primarily affects standard errors rather than coefficient consistency.


# Robustness Check: Lagged minimum wages ----------------------------------

rob.reg_data <- reg_data %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  mutate(min_wage_lag = lag(minimum_wage_euro)) %>% 
  ungroup()

fe_tw_lag_model <- plm(
  unemployment_rate ~ 0 + min_wage_lag + GDP_in_PPS + inflation_rate + population,
  data = rob.reg_data,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

summary(fe_tw_lag_model)

coeftest(
  fe_tw_lag_model,
  vcov = vcovHC(fe_tw_lag_model, type = "HC1", cluster = "group"))

# As a robustness check, the analysis considers a lagged minimum wage specification.
# This allows for delayed labor market adjustments and reduces concerns about contemporaneous reverse causality.

# The results remain qualitatively similar, with no statistically significant effect of minimum wages once country and year fixed effects are included.
