library(survival)
library(tidyverse)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# Create status and cap Inf at 49 (your simulation length)
df <- df |>
  mutate(
    status = if_else(is.finite(rt), 1, 0),  # 0 = censored (Inf), 1 = recovered
    rt_capped = if_else(is.finite(rt), rt, 49)  # replace Inf with censoring time
  )

# Check how many are censored
df |> count(status)

# Fit Tobit / censored regression
tobit_mod <- survreg(
  Surv(rt_capped, status) ~ mgm + rcp + windcase,
  data = df,
  dist = "gaussian"  # or "lognormal" if rt is right-skewed
)

summary(tobit_mod)

# Extract continuous fitted values for all rows (including censored ones)
df$rt_imputed <- predict(tobit_mod, type = "response")

# Sanity check: censored cases should now have rt_imputed > 49
df |>
  group_by(status) |>
  summarise(mean_rt = mean(rt_imputed),
            min_rt  = min(rt_imputed),
            max_rt  = max(rt_imputed))
