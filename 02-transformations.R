# ============================================================================
# POL 213 — Week 2: Common Transformations
# ============================================================================

# ---- Setup ----

library(tidyverse)
library(juanr)
library(broom)
library(modelsummary)
library(marginaleffects)
library(gapminder)
library(vdemdata)

# V-Dem data: country-year panel, post-1945
protest = vdem |>
  filter(year >= 1946) |>
  transmute(
    country = country_name,
    year,
    democracy = v2x_polyarchy,
    mobilization = v2cademmob,
    ln_gdppc = log(e_gdppc),
    ln_pop = log(e_pop)
  ) |>
  drop_na()


# ---- Rescaling X ----

# income in raw dollars vs. $10,000s — same model, different ruler
mod_raw = lm(per_dem_2020 ~ hh_income, data = elections)
mod_10k = lm(per_dem_2020 ~ income_10k, data = elections |> mutate(income_10k = hh_income / 10000))

# coefficient changes by factor of 10,000; t-stat and p-value identical
tidy(mod_raw)
tidy(mod_10k)


# ---- Rescaling Y ----

# vote share as percent (0-100) instead of proportion (0-1)
elections_pct = elections |> mutate(dem_pct = per_dem_2020 * 100)
mod_pct = lm(dem_pct ~ hh_income, data = elections_pct)

# coefficient grows by factor of 100; t-stat and p-value identical
tidy(mod_raw)
tidy(mod_pct)

# ---- Standardize the Outcome ----

# Standardizing = subtract the mean (scale is centered at zero), 
# divide by the standard deviation (SD measures how far the variable moves from average)


# ============================================================================
# Quadratic: democracy and mobilization
# ============================================================================

# ---- Fit the quadratic model ----

# adding democracy^2 lets the effect of democracy curve; allows the effect of democracy to change
quad_mod = lm(mobilization ~ democracy + democracy_2 + ln_gdppc + ln_pop,
              data = mutate(protest, democracy_2 = democracy^2))

modelsummary(
  list("Quadratic" = quad_mod),
  title = "Outcome: Pro-democracy mobilization",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|F|RMSE",
  coef_rename = c(
    "(Intercept)"  = "Intercept",
    "democracy"    = "Democracy",
    "democracy_2"  = "Democracy²",
    "ln_gdppc"     = "ln(GDP per capita)",
    "ln_pop"       = "ln(Population)"
  )
)

# The effect of democracy on mobilization depends on the current level of democracy

# ---- The marginal effect depends on where you are ----

# ME = β₁ + 2β₂ · democracy
b1 = coef(quad_mod)["democracy"]
b2 = coef(quad_mod)["democracy_2"]

# plug in three representative values
me_points = c(0.15, 0.48, 0.80)
b1 + 2 * b2 * me_points
# positive at low democracy, near zero at middle, negative at high


# ---- Predicted values: crossing() → augment() → plot ----
# "What level of Y do we expect at this level of X?"

# step 1: build a scenario grid — vary democracy, hold controls at means
scenario = crossing(
  democracy = seq(min(protest$democracy), max(protest$democracy), length.out = 100),
  ln_gdppc = mean(protest$ln_gdppc),
  ln_pop = mean(protest$ln_pop)
) |>
  mutate(democracy_2 = democracy^2)

# step 2: get predictions with confidence intervals
preds = augment(quad_mod, newdata = scenario, se_fit = TRUE,
                interval = "confidence")

# step 3: plot
ggplot(preds, aes(x = democracy, y = .fitted, ymin = .lower, ymax = .upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_rug(data = protest, aes(x = democracy),
           inherit.aes = FALSE, alpha = 0.1, sides = "b") +
  labs(x = "Electoral democracy index",
       y = "Predicted pro-democracy mobilization")


# ---- Marginal effects plot ----
# "How much does Y change if we increase X by one unit?"


# plot how the effect of democracy changes across the democracy scale
me_data = crossing(
  democracy = seq(min(protest$democracy), max(protest$democracy), length.out = 200)
) |>
  mutate(me = b1 + 2 * b2 * democracy)

ggplot(me_data, aes(x = democracy,
                    y = me)) + # The change in protest as democracy increases 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  labs(x = "Democracy",
       y = "Marginal effect on mobilization")

# The effect is no longer constant 

# ============================================================================
# Interactions: income × South
# ============================================================================

# ---- Fit the interaction ----

small_elections = elections |>
  mutate(
    income_10k = hh_income / 10000,
    south = as.numeric(census_region == "South")
  )

# interact income with south
mod_interact = lm(per_dem_2020 ~ income_10k * south, data = small_elections)

modelsummary(
  list("No interaction" = lm(per_dem_2020 ~ income_10k + south, data = small_elections),
       "With interaction" = mod_interact),
  title = "Outcome: Democratic vote share (2020)",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|F|RMSE",
  coef_rename = c(
    "(Intercept)"      = "Intercept",
    "income_10k"       = "Income ($10k)",
    "south"            = "South",
    "income_10k:south" = "Income × South"
  )
)


# ---- Calculus with a binary moderator ----

# ME of income = β₁ + β₃ · South
coefs = coef(mod_interact)

# Non-South (South = 0):
coefs["income_10k"] + coefs["income_10k:south"] * 0
# Nonsouth: Each additional 10k is 0.054 more points more Democratic

# South (South = 1):
coefs["income_10k"] + coefs["income_10k:south"] * 1
# South: Each additional 10k is 0.015 more points more Democratic 


# ---- Visualize the interaction ----

grid = crossing(
  income_10k = seq(2, 15, length.out = 100),
  south = c(0, 1)
) |>
  mutate(region = if_else(south == 1, "South", "Non-South"))

preds = augment(mod_interact, newdata = grid, interval = "confidence")

ggplot(preds, aes(x = income_10k, y = .fitted,
                  ymin = .lower, ymax = .upper,
                  color = region, fill = region)) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(x = "Median household income ($10k)",
       y = "Predicted Democratic vote share",
       color = NULL, fill = NULL)


# ============================================================================
# Log(X): GDP and life expectancy
# ============================================================================

# ---- Prepare data ----

# Gapminder 2007 cross-section
gap07 = gapminder |>
  filter(year == 2007) |>
  mutate(gdp_10k = gdpPercap / 10000)

gap07 |> select(country, continent, gdp_10k, lifeExp) |> head()


# ---- Fit the log(X) model ----

# log(GDP) encodes diminishing returns
mod_logx = lm(lifeExp ~ log(gdp_10k), data = gap07)

modelsummary(
  list("Level-log" = mod_logx),
  title = "Outcome: Life expectancy (years)",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|F|RMSE",
  coef_rename = c(
    "(Intercept)"   = "Intercept",
    "log(gdp_10k)"  = "ln(GDP per capita, $10k)"
  )
)


# ---- Marginal effect: ME = β₁ / X ----

# the effect shrinks as GDP grows
b_logx = coef(mod_logx)["log(gdp_10k)"]
gdp_values = c(0.1, 0.5, 1.0, 4.0)  # in $10k
b_logx / gdp_values / 10  # years per extra $1,000


# ---- Predicted life expectancy ----

scenario_gap = crossing(gdp_10k = seq(0.025, 5, length.out = 200))
preds_gap = augment(mod_logx, newdata = scenario_gap, interval = "confidence")

ggplot(preds_gap, aes(x = gdp_10k, y = .fitted, ymin = .lower, ymax = .upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = gap07, aes(x = gdp_10k, y = lifeExp),
             inherit.aes = FALSE, alpha = 0.3, size = 2) +
  labs(x = "GDP per capita ($10,000s)",
       y = "Predicted life expectancy (years)")


# ---- Marginal effects with slopes() ----

# slopes() computes ∂Y/∂X with standard errors via the delta method
mfx_gap = slopes(mod_logx, variables = "gdp_10k",
                  newdata = datagrid(gdp_10k = seq(0.05, 5, length.out = 200)))

ggplot(mfx_gap, aes(x = gdp_10k, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "GDP per capita ($10,000s)",
       y = "Marginal effect on life expectancy (years per $10k)")


# ============================================================================
# Log(Y): GDP and exports
# ============================================================================

# ---- Prepare data ----

trade_2010 = trade |>
  filter(year == 2010) |>
  mutate(gdp_trillion = gdp / 1e12) |>
  drop_na(exports, gdp)


# ---- Fit the log(Y) model ----

# log(exports) on GDP: proportional scaling
mod_logy = lm(log(exports) ~ gdp_trillion, data = trade_2010)

# ME = β₁ · Y — the effect grows with the level of Y


# ---- Predicted exports (exponentiate back to original scale) ----

scenario_trade = tibble(gdp_trillion = seq(0.01, 14, length.out = 200))
preds_logy = augment(mod_logy, newdata = scenario_trade, interval = "confidence") |>
  mutate(across(c(.fitted, .lower, .upper), exp))  # back to millions

ggplot(preds_logy, aes(x = gdp_trillion, y = .fitted, ymin = .lower, ymax = .upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = trade_2010, aes(x = gdp_trillion, y = exports),
             inherit.aes = FALSE, alpha = 0.3, size = 2) +
  labs(x = "GDP (trillions $)",
       y = "Predicted exports (millions $)")


# ============================================================================
# Log-log: elasticity of exports with respect to GDP
# ============================================================================

# ---- Fit the log-log model ----

mod_loglog = lm(log(exports) ~ log(gdp_trillion), data = trade_2010)

# β₁ is the elasticity: % change in exports per 1% change in GDP
coef(mod_loglog)["log(gdp_trillion)"]


# ---- Predicted exports (log-log) ----

preds_loglog = augment(mod_loglog, newdata = scenario_trade, interval = "confidence") |>
  mutate(across(c(.fitted, .lower, .upper), exp))

ggplot(preds_loglog, aes(x = gdp_trillion, y = .fitted, ymin = .lower, ymax = .upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = trade_2010, aes(x = gdp_trillion, y = exports),
             inherit.aes = FALSE, alpha = 0.3, size = 2) +
  labs(x = "GDP (trillions $)",
       y = "Predicted exports (millions $)")


# ============================================================================
# Can the data tell us what to do?
# ============================================================================

# ---- Comparing linear vs. log(X) visually ----

# fit a linear model for comparison
mod_linear_gap = lm(lifeExp ~ gdp_10k, data = gap07)

# predict from both models on the same grid
grid_gap = tibble(gdp_10k = seq(0.025, 5, length.out = 300)) |>
  mutate(
    linear = predict(mod_linear_gap, newdata = pick(everything())),
    logx = predict(mod_logx, newdata = pick(everything()))
  )

# overlay both fits — log(X) clearly captures the curvature
ggplot(gap07, aes(x = gdp_10k, y = lifeExp)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_line(data = grid_gap, aes(y = linear, color = "Linear"), linewidth = 1.2) +
  geom_line(data = grid_gap, aes(y = logx, color = "Log(X)"), linewidth = 1.2) +
  scale_color_manual(values = c("Linear" = "orange", "Log(X)" = "steelblue")) +
  labs(x = "GDP per capita ($10,000s)", y = "Life expectancy (years)", color = NULL)


# ---- Higher-degree polynomials chase noise ----

# draw two independent subsamples and fit quadratic vs. degree-6 to each
set.seed(213)
samples = protest |>
  slice_sample(n = 600) |>
  mutate(sample = rep(c("Sample A", "Sample B"), each = 300))

# fit both models within each subsample
models = samples |>
  nest_by(sample) |>
  mutate(
    quad = list(lm(mobilization ~ poly(democracy, 2, raw = TRUE) +
                     ln_gdppc + ln_pop, data = data)),
    flex = list(lm(mobilization ~ poly(democracy, 6, raw = TRUE) +
                     ln_gdppc + ln_pop, data = data))
  )

# prediction grid
grid_overfit = crossing(
  democracy = seq(0.02, 0.92, length.out = 200),
  ln_gdppc = mean(protest$ln_gdppc),
  ln_pop = mean(protest$ln_pop)
)

# get predictions from each model × sample
overfit_preds = models |>
  reframe(bind_rows(
    augment(quad, newdata = grid_overfit) |> mutate(model = "Quadratic"),
    augment(flex, newdata = grid_overfit) |> mutate(model = "Degree 6")
  ))

# the quadratic replicates; the degree-6 tells different stories
ggplot(overfit_preds, aes(x = democracy, y = .fitted, color = model)) +
  geom_line(linewidth = 1) +
  facet_wrap(~sample) +
  labs(x = "Electoral democracy index", y = "Predicted mobilization", color = NULL)


# ---- The winner's curse: low power inflates significant results ----

# simulate z-scores for an underpowered interaction
set.seed(213)
z_scores = rnorm(1e6, mean = 0.7, sd = 1)

# among those that pass p < 0.05, how big is the average z-score?
significant = z_scores[z_scores > 1.96]
mean(significant)  # ~2.5, far above the true mean of 0.7


# ---- R² measures prediction, not identification ----

# fit a linear model for comparison
linear_mod = lm(mobilization ~ democracy + ln_gdppc + ln_pop, data = protest)

# compare R² across specifications
tibble(
  Model = c("Linear", "Quadratic"),
  R2 = c(summary(linear_mod)$r.squared, summary(quad_mod)$r.squared)
)
# R² goes up, but the key question is whether β is right, not whether Ŷ is right
