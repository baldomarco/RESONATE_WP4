# Marco Baldo 25-04-2026 contact: baldo@fld.czu.cz
# Survival Analysis essential
# Multifunctionality Article: 

################################################################################
#   TEST P11 - CORRECTION OF THE MAIN SURVIVAL ANALYSIS TEST (COX ADJUSTED)
################################################################################

HERE IS THE FINAL VERSION IN WHICH I FIXED THE PROBLEM OF MANAGEMENT * CLIMATE 
TO ANALYSIS INTERACTIONS BETWENN THEM AND CLUSTERING PER RCM AND WIND CASES
  
library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50

management_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BAU"          = "#185FA5",
  "BIOECONOMY"   = "#1D9E75",
  "CONSERVATION" = "#D85A30",
  "UNMANAGED"    = "#787670"
)


# ── 0. Add group key to original df ──────────────────────────────────────────
df <- df |>
  mutate(
    rcp     = recode(rcp, `-` = "refclim"),
    group_key = paste(mgm, rcp, model, windcase, sep = "_")
  )

# ── 1. Prepare survival data — keep track for columns cluster  ────────────────
df_surv <- df |>
  mutate(
    Management = factor(mgm),
    Climate    = factor(rcp),
    cluster_id = paste(model, windcase, sep = "_"),
    recovered  = ifelse(is.infinite(rt), 0L, 1L),
    time       = ifelse(is.infinite(rt), sim_horizon, rt)
  ) |>
  filter(!is.na(Management), !is.na(Climate))

# df check for SA censored and numbers of stratified cases
df_surv |> count(Management, recovered)


# ── 2. Log-rank p-values (one per Climate row) ─────────────────────────
p_value_annotations <- df_surv |>
  group_by(Climate) |>
  group_modify(~ {
    sdiff <- survdiff(Surv(time, recovered) ~ Management, data = .x)
    p_val <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
    tibble(p_value_label = paste0("p = ", format.pval(p_val, digits = 3)))
  })


# ── 3. Cox model + predicted survival curves (Management * Climate) ───────────
cox_model <- coxph(
  Surv(time, recovered) ~ Management * Climate + cluster(cluster_id),
  data = df_surv
)


all_plot_data <- data.frame()

for (clim in levels(df_surv$Climate)) {
  for (mgm_level in levels(df_surv$Management)) {
    
    newdata <- data.frame(
      Management = mgm_level,
      Climate    = clim,
      cluster_id = NA
    )
    
    fit <- survfit(cox_model, newdata = newdata)
    
    n_sims <- df_surv |>
      filter(Climate == clim, Management == mgm_level) |>
      nrow()
    
    all_plot_data <- bind_rows(
      all_plot_data,
      surv_summary(fit, data = df_surv) |>
        mutate(
          Management = mgm_level,
          Climate    = clim,
          n_sims     = n_sims
        )
    )
  }
}


# Test the cox model
cox.zph(cox_model)


# ── 4. Enforce monotonic recovery curves ─────────────────────────────────────
all_plot_data <- all_plot_data |>
  group_by(Climate, Management) |>
  arrange(time, .by_group = TRUE) |>
  mutate(
    surv  = 1 - cummax(1 - surv),
    lower = 1 - cummax(1 - lower),
    upper = 1 - cummax(1 - upper)
  ) |>
  ungroup()

# Summary of group sizes for transparency 
all_plot_data |>
  distinct(Management, Climate, n_sims) |>
  arrange(Climate, Management) |>
  as.data.frame() |>
  print()



# ── 5. Plot ───────────────────────────────────────────────────────────────────
final_plot <- ggplot(all_plot_data,
                     aes(x = time, y = 1 - surv,
                         color = Management, fill = Management)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower),
              alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values  = management_colors) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 0.5) +
  geom_text(data = p_value_annotations,
            aes(x = 10, y = 1.15, label = p_value_label),
            inherit.aes = FALSE, hjust = 0, size = 2.8, fontface = "italic") +
  facet_grid(Climate ~ Management) +
  ylim(0, 1.25) +
  labs(
    title = "Growing Stocks Recovery by Management — Faceted by Climate",
    x     = "Time [years]",
    y     = "Cumulative recovery probability",
    color = "Management", fill = "Management"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = "bottom",
    legend.title         = element_text(face = "bold", size = 12),
    plot.title           = element_text(hjust = 0.5, face = "bold", size = 14,
                                        margin = margin(b = 10)),
    strip.text           = element_text(face = "bold", size = 12),
    strip.placement      = "outside",
    strip.background     = element_rect(fill = "grey90", color = "black"),
    axis.text.x          = element_text(angle = 45, hjust = 1),
    panel.border         = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(final_plot)

#ggsave("Fig_resilience_survival.pdf", final_plot, width = 36, height = 14, units = "cm", dpi = 300)


################################################################################
#   VARIANCE PARTITIONING ANALYSIS
################################################################################

MAKING THE VARIANCE PARTITIONING IN THE SURVIVAL ANALYSIS COX model response

# ── Survival-based Variance Decomposition ─────────────────────────────────────

# ── 1. Fit nested Cox models ───────────────────────────────────────────────────

# Null model (intercept only)
cox_null <- coxph(Surv(time, recovered) ~ 1, data = df_surv)

# Main effect Management only
cox_mgm  <- coxph(Surv(time, recovered) ~ Management + cluster(cluster_id), 
                  data = df_surv)

# Main effect Climate only
cox_clim <- coxph(Surv(time, recovered) ~ Climate + cluster(cluster_id), 
                  data = df_surv)

# Both main effects
cox_both <- coxph(Surv(time, recovered) ~ Management + Climate + cluster(cluster_id), 
                  data = df_surv)

# Full model with interaction
cox_full <- coxph(Surv(time, recovered) ~ Management * Climate + cluster(cluster_id), 
                  data = df_surv)


# Initial clarificative test
summary(cox_full)


# ── 2. Likelihood Ratio Tests (LRT) ───────────────────────────────────────────

# LRT for each term = 2 * (logLik(larger) - logLik(smaller))
lrt_mgm  <- 2 * (cox_mgm$loglik[2]  - cox_null$loglik[2])
lrt_clim <- 2 * (cox_clim$loglik[2] - cox_null$loglik[2])
lrt_both <- 2 * (cox_both$loglik[2] - cox_null$loglik[2])
lrt_full <- 2 * (cox_full$loglik[2] - cox_null$loglik[2])

# Unique contribution of each term (Type II decomposition)
lrt_mgm_unique  <- 2 * (cox_both$loglik[2] - cox_clim$loglik[2])  # mgm | clim
lrt_clim_unique <- 2 * (cox_both$loglik[2] - cox_mgm$loglik[2])   # clim | mgm
lrt_interaction <- 2 * (cox_full$loglik[2] - cox_both$loglik[2])  # interaction
lrt_total       <- lrt_mgm_unique + lrt_clim_unique + lrt_interaction

# ── 3. Convert to % of explained variation (analogous to η²) ──────────────────

variance_decomp <- tibble(
  Term             = c("Management", "Climate", "Management × Climate"),
  LRT_chi2         = round(c(lrt_mgm_unique, lrt_clim_unique, lrt_interaction), 2),
  df               = c(
    length(levels(df_surv$Management)) - 1,
    length(levels(df_surv$Climate))    - 1,
    (length(levels(df_surv$Management)) - 1) * (length(levels(df_surv$Climate)) - 1)
  ),
  p_value          = round(c(
    pchisq(lrt_mgm_unique,  df = length(levels(df_surv$Management)) - 1, lower.tail = FALSE),
    pchisq(lrt_clim_unique, df = length(levels(df_surv$Climate))    - 1, lower.tail = FALSE),
    pchisq(lrt_interaction, df = (length(levels(df_surv$Management)) - 1) * 
             (length(levels(df_surv$Climate))   - 1), lower.tail = FALSE)
  ), 4),
  pct_explained    = round(c(
    lrt_mgm_unique, lrt_clim_unique, lrt_interaction
  ) / lrt_total * 100, 1)
)

# ── 4. Nagelkerke R² per model (overall explanatory power) ────────────────────
# R²_Nagelkerke = (1 - exp(-LRT/n)) / (1 - exp(2*logLik(null)/n))

# ── Fixed Nagelkerke R² ───────────────────────────────────────────────────────

n <- nrow(df_surv)

# For Cox, null loglik is loglik[1] (before any covariates)
null_loglik <- cox_mgm$loglik[1]  # same for all models

nagelkerke <- function(full_loglik, null_loglik, n) {
  lrt    <- 2 * (full_loglik - null_loglik)
  r2_cs  <- 1 - exp(-lrt / n)
  r2_max <- 1 - exp(-2 * abs(null_loglik) / n)  # abs() fixes sign issue
  round(r2_cs / r2_max, 3)
}

r2_table <- tibble(
  Model         = c("Management only", "Climate only",
                    "Management + Climate", "Management * Climate"),
  Nagelkerke_R2 = c(
    nagelkerke(cox_mgm$loglik[2],  null_loglik, n),
    nagelkerke(cox_clim$loglik[2], null_loglik, n),
    nagelkerke(cox_both$loglik[2], null_loglik, n),
    nagelkerke(cox_full$loglik[2], null_loglik, n)
  )
)

as.data.frame(r2_table) |> print()

# ── 5. Print results ───────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════\n")
cat("Variance Decomposition — Cox LRT (Type II, analogous to η²)\n")
cat("═══════════════════════════════════════════════════════\n")
as.data.frame(variance_decomp) |> print()

cat("\n═══════════════════════════════════════════════════════\n")
cat("Overall Explanatory Power — Nagelkerke R²\n")
cat("═══════════════════════════════════════════════════════\n")
as.data.frame(r2_table) |> print()



# Save RT survival analysis cox model 
# Variance Decomposition — Cox LRT (Type II, analogous to η²)\n
write.xlsx(
  variance_decomp,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "RT_variance_partitioning.xlsx"
  ),
  sheetName = "RT_variance_partitioning",
  overwrite = TRUE
)


# Save RT survival analysis cox model Overall Explanatory Power — Nagelkerke R²\n
write.xlsx(
  r2_table,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "RT_variance_partitioning_Overall_Explanatory_Power.xlsx"
  ),
  sheetName = "Overall Explanatory Power",
  overwrite = TRUE
)