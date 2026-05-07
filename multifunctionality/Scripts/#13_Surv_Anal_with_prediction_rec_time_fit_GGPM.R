# install.packages("flexsurv") 
# A parametric survival model — it assumes the recovery times follow a specific distribution, which allows extrapolation beyond t=50: Given that several of your combinations are clearly not recovering within 50 years, a parametric model would be much more informative ecologically — it would give you probable recovery times with CI even for the never-recovered cases.
# The workflow would be:
  
# Fit all 4 distributions
# Pick best by AIC
# Extract T50/T80/T90/T99 including extrapolated values for censored combinations
# Use those in your bar charts and quantile tables

# it will tell us which distribution fits your recovery data best before we extract any quantiles or build plots.
# A few things to expect:
#  •	Weibull is most common for ecological recovery processes
#  •	Lognormal fits well when recovery is slow-starting then accelerates
#  •	Generalized Gamma is the most flexible — if it wins by a large margin it means none of the simpler distributions fit well

library(flexsurv)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 1. Fit parametric models ───────────────────────────────────────────────────

aft_weibull     <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                               data = df_surv, dist = "weibull")
aft_lognormal   <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                               data = df_surv, dist = "lognormal")
aft_loglogistic <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                               data = df_surv, dist = "llogis")
aft_gamma       <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                               data = df_surv, dist = "gamma")
aft_gengamma    <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                               data = df_surv, dist = "gengamma")  # most flexible

# ── 2. Compare by AIC ─────────────────────────────────────────────────────────

aic_table <- tibble(
  Distribution = c("Weibull", "Lognormal", "Log-logistic", "Gamma", "Gen. Gamma"),
  AIC          = c(AIC(aft_weibull), AIC(aft_lognormal), AIC(aft_loglogistic), 
                   AIC(aft_gamma),   AIC(aft_gengamma)),
  logLik       = c(as.numeric(logLik(aft_weibull)), as.numeric(logLik(aft_lognormal)),
                   as.numeric(logLik(aft_loglogistic)), as.numeric(logLik(aft_gamma)),
                   as.numeric(logLik(aft_gengamma)))
) |>
  mutate(delta_AIC = round(AIC - min(AIC), 2),
         AIC       = round(AIC, 2),
         logLik    = round(logLik, 2)) |>
  arrange(AIC)

as.data.frame(aic_table) |> print() # The best model is the one with lowest AIC (or delta_AIC = 0).



# Generalized Gamma wins clearly — delta_AIC of 15.8 over the next best (Log-logistic) is a very strong signal. A delta_AIC > 10 is generally considered decisive.
# This makes ecological sense because Generalized Gamma is the most flexible — it nests Weibull, Gamma, and Lognormal as special cases, meaning your recovery process doesn't conform to a simple symmetric or monotonic hazard shape.
# Now let's extract the quantiles including extrapolation beyond 50 years:

# This will now give actual projected recovery times instead of NAs for the never-recovered combinations. Values > 50 are extrapolated 

# ── Extract quantiles from best parametric model (Gen. Gamma) ─────────────────

# All Management x Climate combinations
newdata_grid <- expand.grid(
  Management = levels(df_surv$Management),
  Climate    = levels(df_surv$Climate)
)

# Extract T50, T80, T90, T99 with CI
quantile_parametric <- map_dfr(1:nrow(newdata_grid), function(i) {
  
  nd <- newdata_grid[i, , drop = FALSE]
  
  q <- summary(aft_gengamma,
               newdata = nd,
               type    = "quantile",
               quantiles = c(0.50, 0.80, 0.90, 0.99),
               ci      = TRUE,
               conf.int = 0.95)[[1]]
  
  tibble(
    Management = nd$Management,
    Climate    = nd$Climate,
    T50        = q[1, "est"],
    T50_lower  = q[1, "lcl"],
    T50_upper  = q[1, "ucl"],
    T80        = q[2, "est"],
    T80_lower  = q[2, "lcl"],
    T80_upper  = q[2, "ucl"],
    T90        = q[3, "est"],
    T90_lower  = q[3, "lcl"],
    T90_upper  = q[3, "ucl"],
    T99        = q[4, "est"],
    T99_lower  = q[4, "lcl"],
    T99_upper  = q[4, "ucl"]
  )
}) |>
  mutate(
    Climate    = factor(Climate,    levels = c("refclim", "rcp45", "rcp85")),
    Management = factor(Management, levels = c("ADAPTATION", "BIOECONOMY",
                                               "BAU", "CONSERVATION", "UNMANAGED"))
  ) |>
  arrange(Climate, Management)

# This will now give actual projected recovery times instead of NAs for the never-recovered combinations. Values > 50 are extrapolated 
# ── Print results ─────────────────────────────────────────────────────────────
cat("Recovery time quantiles — Generalized Gamma parametric model\n")
cat("(values beyond 50y = extrapolated beyond simulation horizon)\n\n")
as.data.frame(quantile_parametric) |> 
  mutate(across(where(is.numeric), ~ round(., 1))) |>
  print()




################################################################################
# Now for the visualization, I'd suggest a connected dot plot (Cleveland-style) faceted by climate, showing all 4 quantiles per Management as connected points with CI ribbons — much more informative than bars for time data:

library(tidyverse)

# ── Reshape to long format ────────────────────────────────────────────────────

quantile_long <- quantile_parametric |>
  pivot_longer(
    cols      = c(T50, T80, T90, T99),
    names_to  = "Quantile",
    values_to = "Time"
  ) |>
  mutate(
    Lower = case_when(
      Quantile == "T50" ~ T50_lower,
      Quantile == "T80" ~ T80_lower,
      Quantile == "T90" ~ T90_lower,
      Quantile == "T99" ~ T99_lower
    ),
    Upper = case_when(
      Quantile == "T50" ~ T50_upper,
      Quantile == "T80" ~ T80_upper,
      Quantile == "T90" ~ T90_upper,
      Quantile == "T99" ~ T99_upper
    ),
    Quantile = factor(Quantile, levels = c("T50", "T80", "T90", "T99")),
    Extrapolated = Time > 50  # flag extrapolated values
  ) |>
  select(Management, Climate, Quantile, Time, Lower, Upper, Extrapolated)

# ── Color palette ─────────────────────────────────────────────────────────────

mgm_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BIOECONOMY"   = "#009E73",
  "BAU"          = "#0072B2",
  "CONSERVATION" = "#D55E00",
  "UNMANAGED"    = "#999999"
)

# ── Plot ──────────────────────────────────────────────────────────────────────

ggplot(quantile_long, 
       aes(x = Time, y = Management, color = Management, shape = Extrapolated)) +
  
  # CI ribbons
  geom_errorbarh(
    aes(xmin = Lower, xmax = Upper),
    height    = 0.25,
    linewidth = 0.5,
    alpha     = 0.7
  ) +
  
  # Points
  geom_point(size = 3) +
  
  # Connect quantiles per Management with a line
  geom_line(
    aes(group = Management),
    linewidth = 0.4,
    alpha     = 0.5,
    linetype  = "dashed"
  ) +
  
  # Simulation horizon reference line
  geom_vline(xintercept = 50, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = 51, y = 0.6, label = "Sim. horizon",
           color = "red", size = 3, hjust = 0) +
  
  scale_color_manual(values = mgm_colors) +
  scale_shape_manual(
    values = c("FALSE" = 16, "TRUE" = 1),  # filled = observed, open = extrapolated
    labels = c("FALSE" = "Within horizon", "TRUE" = "Extrapolated")
  ) +
  
  scale_x_continuous(
    breaks = c(10, 20, 30, 40, 50, 75, 100, 125),
    limits = c(10, 135)
  ) +
  
  facet_grid(Climate ~ Quantile,
             labeller = labeller(
               Climate  = c(refclim = "Reference climate",
                            rcp45   = "RCP4.5",
                            rcp85   = "RCP8.5"),
               Quantile = c(T50 = "50% recovered",
                            T80 = "80% recovered",
                            T90 = "90% recovered",
                            T99 = "99% recovered")
             )) +
  
  labs(
    x     = "Recovery time [years]",
    y     = NULL,
    title = "Projected Recovery Times — Generalized Gamma Parametric Model",
    subtitle = "Open circles = extrapolated beyond 50-year simulation horizon | Error bars = 95% CI",
    color = "Management",
    shape = NULL
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 9)
  ) +
  guides(
    color = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1)
  )


################################################################################
#        parametric model likelihood
################################################################################

# ── Variance Decomposition — Generalized Gamma Parametric Model ───────────────

# ── 1. Fit nested models ──────────────────────────────────────────────────────

aft_null <- flexsurvreg(Surv(time, recovered) ~ 1,
                        data = df_surv, dist = "gengamma")

aft_mgm  <- flexsurvreg(Surv(time, recovered) ~ Management,
                        data = df_surv, dist = "gengamma")

aft_clim <- flexsurvreg(Surv(time, recovered) ~ Climate,
                        data = df_surv, dist = "gengamma")

aft_both <- flexsurvreg(Surv(time, recovered) ~ Management + Climate,
                        data = df_surv, dist = "gengamma")

aft_full <- flexsurvreg(Surv(time, recovered) ~ Management * Climate,
                        data = df_surv, dist = "gengamma")

# ── 2. Type II LRT decomposition ──────────────────────────────────────────────

null_loglik <- aft_null$loglik

lrt_mgm_unique  <- 2 * (aft_both$loglik - aft_clim$loglik)  # mgm | clim
lrt_clim_unique <- 2 * (aft_both$loglik - aft_mgm$loglik)   # clim | mgm
lrt_interaction <- 2 * (aft_full$loglik - aft_both$loglik)  # interaction
lrt_total       <- lrt_mgm_unique + lrt_clim_unique + lrt_interaction

# ── 3. Variance decomposition table ───────────────────────────────────────────

variance_decomp_aft <- tibble(
  Term              = c("Management", "Climate", "Management × Climate"),
  LRT_chi2          = round(c(lrt_mgm_unique, lrt_clim_unique, lrt_interaction), 2),
  df                = c(
    length(levels(df_surv$Management)) - 1,
    length(levels(df_surv$Climate))    - 1,
    (length(levels(df_surv$Management)) - 1) * (length(levels(df_surv$Climate)) - 1)
  ),
  p_value           = round(c(
    pchisq(lrt_mgm_unique, df = length(levels(df_surv$Management)) - 1, 
           lower.tail = FALSE),
    pchisq(lrt_clim_unique, df = length(levels(df_surv$Climate)) - 1,    
           lower.tail = FALSE),
    pchisq(lrt_interaction, df = (length(levels(df_surv$Management)) - 1) *
             (length(levels(df_surv$Climate)) - 1),  
           lower.tail = FALSE)
  ), 4),
  pct_explained     = round(c(
    lrt_mgm_unique, lrt_clim_unique, lrt_interaction
  ) / lrt_total * 100, 1)
)

# ── 4. Nagelkerke R² ──────────────────────────────────────────────────────────
# (plain CI used in survfit for stability at tails)

n <- nrow(df_surv)

nagelkerke <- function(full_loglik, null_loglik, n) {
  lrt    <- 2 * (full_loglik - null_loglik)
  r2_cs  <- 1 - exp(-lrt / n)
  r2_max <- 1 - exp(-2 * abs(null_loglik) / n)
  round(r2_cs / r2_max, 3)
}

r2_table_aft <- tibble(
  Model         = c("Management only", "Climate only",
                    "Management + Climate", "Management * Climate"),
  Nagelkerke_R2 = c(
    nagelkerke(aft_mgm$loglik,  null_loglik, n),
    nagelkerke(aft_clim$loglik, null_loglik, n),
    nagelkerke(aft_both$loglik, null_loglik, n),
    nagelkerke(aft_full$loglik, null_loglik, n)
  )
)

# ── 5. Print results ──────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════════\n")
cat("Variance Decomposition — Generalized Gamma AFT (Type II LRT)\n")
cat("═══════════════════════════════════════════════════════════════\n")
as.data.frame(variance_decomp_aft) |> print()

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Overall Explanatory Power — Nagelkerke R²\n")
cat("═══════════════════════════════════════════════════════════════\n")
as.data.frame(r2_table_aft) |> print()

# ── 6. Compare Cox vs Parametric decomposition ────────────────────────────────

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Comparison: Cox vs Generalized Gamma decomposition\n")
cat("═══════════════════════════════════════════════════════════════\n")
tibble(
  Term                  = c("Management", "Climate", "Management × Climate"),
  Cox_pct               = c(27.8, 59.9, 12.4),   # from your earlier results
  GenGamma_pct          = variance_decomp_aft$pct_explained,
  Cox_R2_full           = 0.438,
  GenGamma_R2_full      = r2_table_aft$Nagelkerke_R2[4]
) |> as.data.frame() |> print()



# What's new vs the Cox version:
# The math is identical — Type II LRT and Nagelkerke R² — but now the log-likelihoods come from the Generalized Gamma parametric model which:

# Accounts for the distributional shape of recovery times
# Uses information from extrapolated recovery beyond 50 years
# Should give higher R² overall since the parametric model fits better (lower AIC)

# The comparison table at the end will directly show you whether the % attribution to Management vs Climate changes when you account for extrapolation — that's the scientifically interesting part. 