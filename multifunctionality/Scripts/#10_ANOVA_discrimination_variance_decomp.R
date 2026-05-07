# Marco Baldo 25-11-2024 contact: baldo@fld.czu.cz
# ANOVA Discrimination - variable decomposition analysis
# Multifunctionality Article: 

library(tidyverse)
library(readxl)
library(patchwork)
library(ggplot2)

# Load data
df <- read_excel("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260407_MF_ES_score_individual_values.xlsx",
                 sheet = "Analysis")

# Rename for clarity
var_labels <- c(
  harvest           = "Annual harvest",
  increment         = "Annual increment",
  `Total carbon`    = "Carbon stock",
  `Deadwood carbon` = "Carbon in deadwood",
  LAI               = "LAI",
  NEP               = "NEP",
  Shannon           = "Shannon index",
  `Standing volume` = "Standing volume"
)

svc_labels <- c(
  Timber      = "Wood production",
  Climate     = "Climate regulation",
  Bidiversity = "Biodiversity",
  Water       = "Water regulation"
)

# It is for Reusability and Readability. 
# as.factor: needed because the aov function needs categorical predictors to be factors
# recode : recode(Variable, harvest = "Annual harvest") finds every cell in Variable that says "harvest" and replaces it with "Annual harvest".
# !!! : "splicing operator" just says: "take this vector and spread its contents as individual named arguments into the function call."

df <- df |>
  mutate(Variable           = recode(Variable,          !!!var_labels),
         `Ecosystem service` = recode(`Ecosystem service`, !!!svc_labels),
         Management         = as.factor(Management),
         Climate            = as.factor(Climate))


# Discriminatory power: SD of cell means #

# Each unique combination of Climate × Management is a cell. If you have 3 climates × 4 management types = 12 cells, and each cell has multiple replicate observations, this averages them down to one value per cell. This removes within-cell noise.
# each indicator (variable within the ES), you take the SD of those 12 cell means. This measures how much the indicator varies across all Climate × Management combinations — i.e. its discriminatory power. A high SD means the indicator responds strongly to different conditions and is therefore good at distinguishing between them. In short: mean first to collapse replicates, SD second to measure spread across experimental conditions. If you skipped step 1 and computed SD directly on raw values, you'd be mixing within-cell noise with between-cell variation, which would inflate the discriminatory power estimate.
disc_power <- df |>
  group_by(`Ecosystem service`, Variable, Climate, Management) |>
  summarise(cell_mean = mean(Value), .groups = 'drop') |>
  group_by(`Ecosystem service`, Variable) |>
  summarise(disc = sd(cell_mean), .groups = 'drop')


# Two-way ANOVA with replication  <- MAIN ANALYSIS (Two-way ANOVA theory — Zar (2010), Biostatistical Analysis, 5th ed., Prentice Hall)

# For each ecosystem service × indicator combination, it fits a two-way ANOVA with replication and extracts how much of the total variance is explained by each source.
# aov(Value ~ Management * Climate) — fits the two-way ANOVA. The * means it tests three things simultaneously: Main effect of Management, Main effect of Climate, Their interaction (Management:Climate) — i.e. does the effect of management depend on climate?

anova_res <- df |>
  group_by(`Ecosystem service`, Variable) |>
  group_modify(~ {
    fit <- aov(Value ~ Management * Climate, data = .x)                         # aov(), summary.aov() — base R stats package: Chambers & Hastie (1992), Statistical Models in S. 
    s   <- summary(fit)[[1]]                                                    # extracts the ANOVA table as a dataframe with rows for each source and columns for Sum Sq, F value, p-value etc.
    rownames(s) <- trimws(rownames(s))                                          #  removes trailing whitespace from row names (the bug we fixed earlier).
    ss_total <- sum(s$`Sum Sq`)                                                 # total variance in the data, used to convert raw sums of squares into percentages. pct_mgmt/climate/inter/resid — each term's percentage of total variance. These four should sum to 100%. F_ and p_ values — the F-statistic and p-value for each term, used for the significance stars in panel a.
    tibble(
      pct_mgmt    = s['Management',         'Sum Sq'] / ss_total * 100,
      pct_climate = s['Climate',            'Sum Sq'] / ss_total * 100,
      pct_inter   = s['Management:Climate', 'Sum Sq'] / ss_total * 100,
      pct_resid   = s['Residuals',          'Sum Sq'] / ss_total * 100,
      F_mgmt      = s['Management',         'F value'],
      F_climate   = s['Climate',            'F value'],
      F_inter     = s['Management:Climate', 'F value'],
      p_mgmt      = s['Management',         'Pr(>F)'],
      p_climate   = s['Climate',            'Pr(>F)'],
      p_inter     = s['Management:Climate', 'Pr(>F)'],
    )
  }) |> ungroup()


#####
stop here there is a statistical test to see the residuals distribution. If not needed do not run it (or push dev.off() once ended)

# Do not run if you don't want the test.
{
# Test of residuals for H
test <- df |> filter(`Ecosystem service` == "Biodiversity", Variable == "Shannon index")
fit <- aov(Value ~ Management * Climate, data = test)
s <- summary(fit)[[1]]
rownames(s)

plot(fit)  # diagnostic plots
shapiro.test(residuals(fit))  # normality test
bartlett.test(Value ~ interaction(Management, Climate), data = test)  # equal variance test
}


#######################
results <- disc_power |>
  left_join(anova_res, by = c('Ecosystem service', 'Variable')) |>
  arrange(desc(disc)) |>
  mutate(Variable = fct_reorder(Variable, disc))

max_sd <- max(results$disc)

results <- results |>
  mutate(sig = case_when(
    p_mgmt < 0.001 ~ "***",
    p_mgmt < 0.01  ~ "**",
    p_mgmt < 0.05  ~ "*",
    TRUE           ~ "ns"
  ))


#-------------------------------------------------------------------------------
# Let's plot them

# Colours
svc_colors <- c(
  'Wood production'    = '#185FA5',
  'Biodiversity'       = '#1D9E75',
  'Climate regulation' = '#D85A30',
  'Water regulation'   = '#BA7517'
)
src_colors <- c(
  Management              = '#185FA5',
  Climate                 = '#97C459',
  Interaction              = '#D0CECA',
  `Residual (within-cell)` = '#787670'
)


# Panel A: discriminatory power
pA <- ggplot(results,
             aes(x = disc, y = Variable,
                 fill = `Ecosystem service`)) +
  geom_text(aes(x = disc, label = sig), hjust = -0.3, size = 3.5) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = svc_colors,
                    name   = 'Ecosystem service') +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, max_sd * 1.12)) +
  labs(x = 'Discriminatory power (SD of cell means)',
       y = NULL, title = 'a') +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = 'bottom')+
  # pA legend
  theme(legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(4,4,4,4))+
  guides(fill = guide_legend(nrow = 2)) # to edit the legend panel order

# Panel B: variance decomposition, bars scaled to 100%,
#          bar height proportional to discriminatory power
plot_decomp <- results |>
  select(Variable, disc, pct_mgmt, pct_climate, pct_inter, pct_resid) |>
  pivot_longer(cols = starts_with('pct_'),
               names_to  = 'source',
               values_to = 'pct') |>
  mutate(
    source = recode(source,
                    pct_mgmt    = 'Management',
                    pct_climate = 'Climate',
                    pct_inter   = 'Interaction',
                    pct_resid   = 'Residual (within-cell)'),
    source = factor(source, levels = rev(c("Management", "Climate", "Interaction", "Residual (within-cell)")))
  )

pB <- ggplot(plot_decomp,
             aes(x = pct, y = Variable,
                 fill = source)) +
  geom_col(position = 'stack') +
  scale_fill_manual(values = src_colors,
                    name   = 'Variance source') +
  scale_x_continuous(expand = c(0,0), limits = c(0,101),
                     labels = function(x) paste0(x,'%')) +
  labs(x = 'Variance decomposition (%)',
       y = NULL, title = 'b') +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_blank(),
        legend.position    = 'bottom')+
  # pB  
  theme(legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(4,4,4,4))+
  guides(fill = guide_legend(nrow = 2)) # to edit the legend panel order

pA + pB  # no plot_layout guides collect

#ggsave("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/Fig_Multifun_VarDec.pdf",
#      width = 32, height = 15, units = "cm")

Multifun_VarDec <- pA + pB

Multifun_VarDec


################################################################################
#        THE END OF THE 1ST ANALYSIS
################################################################################



################################################################################
#        2ND ANALYSIS ON RECOVERY   
################################################################################

# THIS IS MAKING THE TOW-WAY ANOVA ON RECOVERY METRICS NORMILIZING THE SCALE THROUGH THE Z-SCORE, FIXING THE NA VALUES
# NOTE HERE IMPORTANT WE USED BY LAURA 2026-04-21 SEE THE CSV FILE NAME : 20260421_impact_recoverytime_auc.csv ON TEAMS FOLDER

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 1. Pivot to long format ───────────────────────────────────────────────────
# Keep windcase in — do NOT pre-average over it
df_long <- df |>
  select(model, rcp, mgm, windcase, one.minus.norm.auc, impact, rt) |>
  mutate(rcp = recode(rcp, `-` = "refclim")) |>
  mutate(rt = ifelse(is.infinite(rt), max(rt[is.finite(rt)]) + 1, rt)) |> # HERE WE ADD MAX+1 AT THE NA VALUES IN THE RECOVERY TIME WHEN THEY DO NOT RECOVER IN THAT INTERVAL OF TIME
  pivot_longer(
    cols      = c(one.minus.norm.auc, impact, rt),
    names_to  = "Variable",
    values_to = "Value"
  ) |>
  mutate(
    Variable   = recode(Variable,
                        one.minus.norm.auc      = "1 - Normalized AUC",
                        impact                  = "Wind Disturbance Impact [m3/ha]",
                        rt                      = "Recovery Time [years]"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp)
  ) |>
  group_by(Variable) |>
  mutate(Value = scale(Value)[,1]) |>   # z-score per variable: The z-score transforms each value as: z = (x − mean) / SD - So for each variable separately, it subtracts the mean and divides by the standard deviation. The result is that: mean becomes 0, SD becomes 1. All variables end up on the same unitless scale ("how many standard deviations from the mean").
  ungroup() 


#df_long <- df_long |> filter(Climate != "refclim") # Remove the reference climate because the inconsistancy with the number of cells (mng x clim) in Anova compared with the RCMs (x4).

# ── 2. Discriminatory power: SD of cell means ─────────────────────────────────
disc_power <- df_long |>
  group_by(Variable, Climate, Management) |>
  summarise(cell_mean = mean(Value, na.rm = TRUE), .groups = "drop") |>
  group_by(Variable) |>
  summarise(disc = sd(cell_mean), .groups = "drop")


# ── 3. Two-way ANOVA with replication ─────────────────────────────────────────
anova_res <- df_long |>
  group_by(Variable) |>
  group_modify(~ {
    fit <- aov(Value ~ Management * Climate, data = .x)
    s   <- summary(fit)[[1]]
    rownames(s) <- trimws(rownames(s))
    ss_total <- sum(s$`Sum Sq`)
    tibble(
      pct_mgmt    = s["Management",         "Sum Sq"] / ss_total * 100,
      pct_climate = s["Climate",            "Sum Sq"] / ss_total * 100,
      pct_inter   = s["Management:Climate", "Sum Sq"] / ss_total * 100,
      pct_resid   = s["Residuals",          "Sum Sq"] / ss_total * 100,
      F_mgmt      = s["Management",         "F value"],
      F_climate   = s["Climate",            "F value"],
      F_inter     = s["Management:Climate", "F value"],
      p_mgmt      = s["Management",         "Pr(>F)"],
      p_climate   = s["Climate",            "Pr(>F)"],
      p_inter     = s["Management:Climate", "Pr(>F)"]
    )
  }) |> ungroup()

#####
stop

# Do not run if you don't want the test.
{
  # Test of residuals — run separately for each variable
  test <- df_long |> filter(Variable == "1 - Normalized AUC")
  # test <- df_long |> filter(Variable == "Total Disturbance Impact [m3/ha]")
  # test <- df_long |> filter(Variable == "Recovery Time [years]")
  
  fit <- aov(Value ~ Management * Climate, data = test)
  s <- summary(fit)[[1]]
  rownames(s)
  
  plot(fit)                                                               # diagnostic plots
  shapiro.test(residuals(fit))                                            # normality test
  bartlett.test(Value ~ interaction(Management, Climate), data = test)    # equal variance test
  
}


# ── 4. Combine & annotate significance ────────────────────────────────────────
results <- disc_power |>
  left_join(anova_res, by = "Variable") |>
  arrange(desc(disc)) |>
  mutate(
    Variable = fct_reorder(Variable, disc),
    sig = case_when(
      p_mgmt < 0.001 ~ "***",
      p_mgmt < 0.01  ~ "**",
      p_mgmt < 0.05  ~ "*",
      TRUE           ~ "ns"
    )
  )

max_sd <- max(results$disc)


# ── 5. Colours ────────────────────────────────────────────────────────────────
var_colors <- c(
  "1 - Normalized AUC"                   = "#185FA5",
  "Wind Disturbance Impact [m3/ha]"      = "#D85A30",
  "Recovery Time [years]"                = "#1D9E75"
)

src_colors <- c(
  "Management"              = "#185FA5",
  "Climate"                 = "#97C459",
  "Interaction"             = "#D0CECA",
  "Residual (within-cell)"  = "#787670"
)


# ── 6. Panel A: discriminatory power ─────────────────────────────────────────
pA <- ggplot(results,
             aes(x = disc, y = Variable, fill = Variable)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sig), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = var_colors) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, max_sd * 1.15)) +
  labs(x = "Discriminatory power (SD of cell means, z-scored)",
       y = NULL, title = "a") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "none")       # colour already on y-axis labels


# ── 7. Panel B: variance decomposition ───────────────────────────────────────
plot_decomp <- results |>
  select(Variable, disc, pct_mgmt, pct_climate, pct_inter, pct_resid) |>
  pivot_longer(cols = starts_with("pct_"),
               names_to  = "source",
               values_to = "pct") |>
  mutate(
    source = recode(source,
                    pct_mgmt    = "Management",
                    pct_climate = "Climate",
                    pct_inter   = "Interaction",
                    pct_resid   = "Residual (within-cell)"),
    source = factor(source,
                    levels = rev(c("Management", "Climate",
                                   "Interaction", "Residual (within-cell)")))
  )

pB <- ggplot(plot_decomp,
             aes(x = pct, y = Variable, fill = source)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = src_colors, name = "Variance source") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Variance decomposition (%)",
       y = NULL, title = "b") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_blank(),
        legend.position    = "bottom",
        legend.title       = element_text(face = "bold"),
        legend.box.background = element_rect(colour = "black"),
        legend.margin      = margin(4, 4, 4, 4)) +
  guides(fill = guide_legend(nrow = 2))


# ── 8. Combine & save ─────────────────────────────────────────────────────────
final_panel <- pA + pB
final_panel

#ggsave("Fig_resilience_indicators.pdf",
#       width = 32, height = 10, units = "cm")


################################################################################
#        THE END OF THE 2ND ANALYSIS
################################################################################



################################################################################
#       3RD SURVIVAL ANALYSIS ON RECOVERY TIME
################################################################################


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

# ── 1. Prepare survival data — keep traceability columns ─────────────────────
df_surv <- df |>
  mutate(
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),
    recovered  = ifelse(is.infinite(rt), 0L, 1L),
    time       = ifelse(is.infinite(rt), sim_horizon, rt)
  ) |>
  filter(!is.na(Management), !is.na(Climate))

# df check for SA censored and numbers of stratified cases
df_surv |> count(Management, recovered)


# ── 2. Log-rank p-values (one per Management column) ─────────────────────────
p_value_annotations <- df_surv |>
  group_by(Management) |>
  group_modify(~ {
    sdiff <- tryCatch(
      survdiff(Surv(time, recovered) ~ Climate, data = .x),
      error = function(e) NULL
    )
    if (is.null(sdiff) || is.na(sdiff$chisq)) return(tibble(p_value_label = NA))
    p_val <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
    tibble(p_value_label = paste0("Log-rank p = ", format.pval(p_val, digits = 3)))
  }) |>
  ungroup() |>
  filter(!is.na(p_value_label)) |>
  crossing(Climate = levels(df_surv$Climate))  # repeat across all rows


# ── 3. Cox model + predicted survival curves (per Climate) ────────────────────
all_plot_data <- data.frame()
for (clim in levels(df_surv$Climate)) {
  
  sub <- df_surv |> filter(Climate == clim)
  if (nrow(sub) < 5 || all(sub$recovered == 0)) next
  
  cox_model <- tryCatch(
    coxph(Surv(time, recovered) ~ Management, data = sub),  
    error = function(e) NULL
  )
  if (is.null(cox_model)) next
  
  for (mgm_level in levels(df_surv$Management)) {
    
    if (!(mgm_level %in% sub$Management)) next
    
    fit <- survfit(cox_model,
                   newdata = data.frame(Management = mgm_level),
                   conf.type = "plain")
    
    sim_ids <- df_surv |>
      filter(Climate == clim, Management == mgm_level) |>
      pull(group_key) |>
      paste(collapse = "; ")
    
    all_plot_data <- bind_rows(
      all_plot_data,
      surv_summary(fit, data = sub) |>
        mutate(
          Management = mgm_level,
          Climate    = clim,
          n_sims     = nrow(sub |> filter(Management == mgm_level)),
          sim_ids    = sim_ids
        )
    )
  }
}

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
    title = "Stand Recovery by Management — Faceted by Climate",
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

# Save Survival Analysis based on Cox models for the Kepler-Meir curves results
write.csv(
  all_plot_data,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "Survival_Analysis_K-M_Cox_results_with_simID.csv"
  ))


################################################################################
#        PROPOTION OF RECOVERY - TABLE CREATION
################################################################################


# ── 6. SWITCHERS RECOVERY ANALYSIS BASED ON REAL OBSERVATIONS ─────────────────

# MUY BIEN - NOW LET'S APPLY SOMETHING MORE MEANINGFUL FOR INTERPRETATION OF THE RESILIENCE - RECOVERY RESULTS

# HERE IS THE RECOVERY PROPORTION BASED ON REAL DATA NOT IN SURVIVAL ANALYSIS PROBABILITY
# Prepare data

plot_df <- df %>%
  mutate(
    rcp = if_else(rcp == "-", "refclim", rcp),
    rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    mgm = factor(mgm, levels = c("ADAPTATION", "BIOECONOMY", "BAU", "CONSERVATION", "UNMANAGED")),
    recovered = rt <= 28
  ) %>%
  group_by(mgm, rcp) %>%
  mutate(
    total = n(),
    rep_id = row_number(),
    prop = 1 / total  # each repetition contributes 1/total to the bar
  ) %>%
  ungroup()


################################################################################

# To extract the fastes recovery time among the scenarios 
#      (in our case is adaptation under rcp85)

################################################################################
# ── Step 7. Extract T99 for all combinations ──────────────────────────────────

t99_results <- data.frame()

for (clim in levels(df_surv$Climate)) {
  
  sub <- df_surv |> filter(Climate == clim)
  if (nrow(sub) < 5 || all(sub$recovered == 0)) next
  
  cox_model <- tryCatch(
    coxph(Surv(time, recovered) ~ Management, data = sub),
    error = function(e) NULL
  )
  if (is.null(cox_model)) next
  
  for (mgm_level in levels(df_surv$Management)) {
    
    if (!(mgm_level %in% sub$Management)) next
    
    fit <- survfit(cox_model,
                   newdata = data.frame(Management = mgm_level),
                   conf.type = "plain")
    
    q <- quantile(fit, probs = 0.9999)  # 99% recovered
    
    t99_results <- bind_rows(
      t99_results,
      tibble(
        Management = mgm_level,
        Climate    = clim,
        T99        = q$quantile[1],
        T99_lower  = q$lower[1],
        T99_upper  = q$upper[1]
      )
    )
  }
}

# ── Step 8. Find the earliest T99 across all combinations ─────────────────────

t99_threshold <- t99_results |>
  filter(!is.na(T99)) |>
  slice_min(T99, n = 1)

cat("Earliest T99 combination:\n")
as.data.frame(t99_threshold)

# Extract the threshold year
threshold_year <- round(t99_threshold$T99)
cat("\nUsing threshold year:", threshold_year, "\n")


# ── 9. SWITCHER RECOVERY ANALISYS BASED ON SURVIVAL ANALYSIS RESULTS ──────────
# HERE IS THE RECOVERY PROPORTION BASED ON SURVIVAL ANALYSIS PROBABILITY

# Step 1: extract recovery at t=30 (last event time <= 30) per combo
recovery_28 <- all_plot_data %>%
  mutate(
    #Climate = if_else(Climate == "-", "refclim", Climate),
    Climate = factor(Climate, levels = c("refclim", "rcp45", "rcp85")),
    Management = factor(Management, levels = c("ADAPTATION", "BIOECONOMY", "BAU", "CONSERVATION", "UNMANAGED"))
  ) %>%
  filter(time <= 28) %>% # EDIT HERE TO EDIT THE SELECTED YEAR
  group_by(Management, Climate) %>%
  slice_max(time, n = 1) %>%          # last KM step before or at t=30
  ungroup() %>%
  mutate(
    recovery       = 1 - surv,
    recovery_upper = 1 - lower,       # CI flips!
    recovery_lower = 1 - upper
  )



# ── step 10. SAVE THE TABLE      ──────────────────────────────────────────────
library(openxlsx)

# Save repetition proportion based on survival analysis cox model 
# first total recovery year per cell = ADAPTATION RCP85 Y28 

write.xlsx(
  recovery_28,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260501_R_proportion_Y28.xlsx"
  ),
  sheetName = "20260501_R_proportion_Y28",
  overwrite = TRUE
)


# ── Step 11.      PLOTTING THE RESULTS               ──────────────────────────

# LET'S HAVE THE SAME COLOR AND VERTICAL MANAGEMENT TEXT
# Disturbance colors (wind + bark beetle style from your reference figure)
disturbance_colors <- c(
  "ADAPTATION"   = "#4E9B6F",  # teal/green like wind
  "BIOECONOMY"   = "#4E9B6F",
  "BAU"          = "#4E9B6F",
  "CONSERVATION" = "#4E9B6F",
  "UNMANAGED"    = "#4E9B6F"
)

ggplot(recovery_28, aes(x = Management, y = recovery, fill = Management)) +
  geom_col(width = 0.7, fill = "#4E9B6F", alpha = 0.85) +
  geom_errorbar(
    aes(ymin = recovery_lower, ymax = recovery_upper),
    width = 0.25,
    linewidth = 0.6,
    color = "grey30"
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.6) +
  scale_y_continuous(
    limits = c(0, 1.15),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~ Climate, ncol = 3, labeller = labeller(Climate = c(
    refclim = "Reference climate",
    rcp45   = "RCP4.5",
    rcp85   = "RCP8.5"
  ))) +
  labs(
    x     = NULL,
    y     = "Cumulative recovery by year 28 [%]",
    title = "Stand Recovery by Year 28 — Faceted by Climate"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    legend.position  = "none",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



################################################################################
# ── 12. LET'S MAKE A TABLE OF RECOVERY TIME  ───────────────────────────────────

# ── Extract recovery time quantiles from Cox predicted curves ─────────────────

quantile_results <- data.frame()

for (clim in levels(df_surv$Climate)) {
  
  sub <- df_surv |> filter(Climate == clim)
  if (nrow(sub) < 5 || all(sub$recovered == 0)) next
  
  cox_model <- tryCatch(
    coxph(Surv(time, recovered) ~ Management, data = sub),
    error = function(e) NULL
  )
  if (is.null(cox_model)) next
  
  for (mgm_level in levels(df_surv$Management)) {
    
    if (!(mgm_level %in% sub$Management)) next
    
    fit <- survfit(cox_model,
                   newdata = data.frame(Management = mgm_level),
                   conf.type = "plain")  # stable CI at tails
    
    # Extract quantiles directly from survfit object
    # quantile() on survfit returns time at which surv crosses 0.5, 0.2, 0.1
    # (i.e. 50%, 80%, 90% recovered)
    q <- quantile(fit, probs = c(0.5, 0.8, 0.9))
    
    quantile_results <- bind_rows(
      quantile_results,
      tibble(
        Management   = mgm_level,
        Climate      = clim,
        T50          = q$quantile[1],   # 50% recovered
        T50_lower    = q$lower[1],
        T50_upper    = q$upper[1],
        T80          = q$quantile[2],   # 80% recovered
        T80_lower    = q$lower[2],
        T80_upper    = q$upper[2],
        T90          = q$quantile[3],   # 90% recovered
        T90_lower    = q$lower[3],
        T90_upper    = q$upper[3]
      )
    )
  }
}

# ── Clean up and format ───────────────────────────────────────────────────────
quantile_results <- quantile_results |>
  mutate(
    Climate    = factor(Climate, levels = c("refclim", "rcp45", "rcp85")),
    Management = factor(Management, levels = c("ADAPTATION", "BIOECONOMY", 
                                               "BAU", "CONSERVATION", "UNMANAGED"))
  ) |>
  arrange(Climate, Management)

# ── SAVE THE TABLE       ──────────────────────────────────────────────────────

library(openxlsx)

# Save RT based on quintiles proportion of the survival analysis cox model
write.xlsx(
  quantile_results,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260423_RT_based_on_quantile.xlsx"
  ),
  sheetName = "20260423_RT_based_on_quantile",
  overwrite = TRUE
)


# ── Summary table: fraction of combinations reaching each quantile ────────────
# EDIT PER MANAGEMENT TYPE
quantile_summary <- quantile_results |>
  summarise(
    T50_reached  = sum(!is.na(T50)),
    T80_reached  = sum(!is.na(T80)),
    T90_reached  = sum(!is.na(T90)),
    total        = n()
  ) |>
  mutate(
    T50_pct = round(T50_reached / total * 100, 1),
    T80_pct = round(T80_reached / total * 100, 1),
    T90_pct = round(T90_reached / total * 100, 1)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) |>
  filter(str_detect(metric, "reached|pct")) |>
  mutate(
    Threshold = str_extract(metric, "T\\d+"),
    Type      = if_else(str_detect(metric, "reached"), "n", "pct")
  ) |>
  select(Threshold, Type, value) |>
  pivot_wider(names_from = Type, values_from = value) |>
  mutate(
    total      = nrow(quantile_results),
    Summary    = paste0(n, "/", total, " (", pct, "%)")
  ) |>
  select(Threshold, Summary)

as.data.frame(quantile_summary)


# This table is essentially a presence/absence of reaching each threshold, where the value is the time expressed as % of the 50-year horizon, and NA = never reached.
# So basically inferior is the value faster is the recovery.

sim_horizon <- 50  # adjust if different

quantile_table <- quantile_results |>
  mutate(
    T50_pct = round(T50 / sim_horizon * 100, 1),
    T80_pct = round(T80 / sim_horizon * 100, 1),
    T90_pct = round(T90 / sim_horizon * 100, 1),
    # CI bounds as % of horizon too
    T50_lower_pct = round(T50_lower / sim_horizon * 100, 1),
    T50_upper_pct = round(T50_upper / sim_horizon * 100, 1),
    T80_lower_pct = round(T80_lower / sim_horizon * 100, 1),
    T80_upper_pct = round(T80_upper / sim_horizon * 100, 1),
    T90_lower_pct = round(T90_lower / sim_horizon * 100, 1),
    T90_upper_pct = round(T90_upper / sim_horizon * 100, 1)
  ) |>
  select(Management, Climate,
         T50_pct, T50_lower_pct, T50_upper_pct,
         T80_pct, T80_lower_pct, T80_upper_pct,
         T90_pct, T90_lower_pct, T90_upper_pct)

as.data.frame(quantile_table)

# Save RT based on quintiles proportion of the survival analysis cox model
write.xlsx(
  quantile_table,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260423_Recovery_based_on_time_proportion.xlsx"
  ),
  sheetName = "20260423_R_based_on_time_proportion",
  overwrite = TRUE
)


################################################################################
#   VARIANCE PARTITIONING ANALYSIS
################################################################################

MAKING THE VARIANCE PARTITIONING IN THE SURVIVAL ANALYSIS COX model.response

# ── Survival-based Variance Decomposition ─────────────────────────────────────

# ── 1. Fit nested Cox models ───────────────────────────────────────────────────

# Null model (intercept only)
cox_null <- coxph(Surv(time, recovered) ~ 1, 
                  data = df_surv)

# Main effect Management only
cox_mgm  <- coxph(Surv(time, recovered) ~ Management, 
                  data = df_surv)

# Main effect Climate only
cox_clim <- coxph(Surv(time, recovered) ~ Climate, 
                  data = df_surv)

# Both main effects
cox_both <- coxph(Surv(time, recovered) ~ Management + Climate, 
                  data = df_surv)

# Full model with interaction
cox_full <- coxph(Surv(time, recovered) ~ Management * Climate, 
                  data = df_surv)

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


# Save Survival Analysis based on Cox models for the Kepler-Meir curves results
write.csv(
  all_plot_data,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "Survival_Analysis_K-M_Cox_results.csv"
  ))