# ============================================================================
# POL 213 — Week 1: A Review of OLS
# ============================================================================

# ---- Setup ----

library(tidyverse)
library(juanr)
library(broom)
library(modelsummary)

# county-level electoral data from the 2020 presidential election
small_elections = elections %>% 
  mutate(
    per_dem_2020 = per_dem_2020 * 100,
    income_10k = hh_income / 10000
  ) %>% 
  select(name, state, per_dem_2020, income_10k,
         white_pct = white, black_pct = black)

# take a look
small_elections

# We want to identify counties with similar racial make ups 

# ---- Fit the model ----

# OLS: Democratic vote share on income, controlling for racial composition
dem_mod = lm(per_dem_2020 ~ income_10k + white_pct + black_pct,
             data = small_elections)

# regression table
modelsummary(dem_mod,
  title = "Outcome: Democratic vote share (%)",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|F|RMSE",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "income_10k"  = "Income ($10k)",
    "white_pct"   = "% White",
    "black_pct"   = "% Black"
  )
)
# For every additional 10k of income, democratic vote share increases by 4.8
# effect = coefficient 

# ---- Residuals ----

# augment() gives you fitted values, residuals, and diagnostics
preds = augment(dem_mod)

# .fitted = Y-hat (model's prediction for each observation)
# .resid  = Y - Y-hat (how far off the prediction is)
# positive residual = model under-predicts
# negative residual = model over-predicts


# ---- Predictions: a single scenario ----

# predict Democratic vote share for a specific county profile
scenario = crossing(
  income_10k = 4,
  white_pct = 80,
  black_pct = 10
)

augment(dem_mod, newdata = scenario, se_fit = TRUE, interval = "confidence")

# .fitted is Y-hat; .lower and .upper are the 95% confidence interval


# ---- Predictions: vary treatment, hold controls constant ----

# vary income from min to max; hold racial composition at sample means
scenario = crossing(
  income_10k = seq(
    min(small_elections$income_10k, na.rm = TRUE),
    max(small_elections$income_10k, na.rm = TRUE)
  ),
  white_pct = mean(small_elections$white_pct, na.rm = TRUE),
  black_pct = mean(small_elections$black_pct, na.rm = TRUE)
)

preds2 = augment(dem_mod, newdata = scenario, se_fit = TRUE,
                interval = "confidence")
preds2


# ---- Visualize the marginal effect of income ----

ggplot(preds2, aes(x = income_10k, y = .fitted,
                  ymin = .lower, ymax = .upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_rug(data = small_elections, aes(x = income_10k),
           inherit.aes = FALSE, alpha = 0.3, sides = "b") +
  labs(
    x = "Median household income (in $10,000s)",
    y = "Predicted Democratic vote share (%)"
  )


# ---- Constant across controls ----

# Varying income and maximum amount of controls 
scenario2 = crossing(
  income_10k = seq(
    min(small_elections$income_10k, na.rm = TRUE),
    max(small_elections$income_10k, na.rm = TRUE)
  ),
  white_pct = c(99 , 98 , 97 , 96 , na.rm = TRUE),
  black_pct = min(small_elections$black_pct, na.rm = TRUE)
)

preds3 = augment(dem_mod, newdata = scenario2, se_fit = TRUE,
                 interval = "confidence")
preds3

ggplot(preds3, aes(x = income_10k, y = .fitted,
                   ymin = .lower, ymax = .upper, 
                   color = factor(white_pct))) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_rug(data = small_elections, aes(x = income_10k),
           inherit.aes = FALSE, alpha = 0.3, sides = "b") +
  labs(
    x = "Median household income (in $10,000s)",
    y = "Predicted Democratic vote share (%)"
  )

# ---- The OLS formula vs. lm() ----

# compute beta_1 by hand using the formula we derived:
# beta_1 = sum((x - x_bar)(y - y_bar)) / sum((x - x_bar)^2)
x = small_elections$income_10k
y = small_elections$per_dem_2020

# Covariance over Variance 
sum((x - mean(x, na.rm = TRUE)) * (y - mean(y, na.rm = TRUE)), na.rm = TRUE) /
  sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)

# same thing with lm() (simple bivariate model)
lm(per_dem_2020 ~ income_10k, data = small_elections)
