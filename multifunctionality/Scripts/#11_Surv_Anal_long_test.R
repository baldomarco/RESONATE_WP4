# Marco Baldo 25-11-2024 contact: baldo@fld.czu.cz
# ANOVA Discrimination - variable decomposition analysis test
# Survival Analysis tests
# Multifunctionality Article: 

#1##############################################################################
#    SECOND ANALYSIS ON RECOVERY   
################################################################################

THIS IS MAKING THE TOW-WAY ANOVA ON RECOVERY METRICS BUT WITHOUT FIXING THE NA VALUES NEITHER THE Z-SCORE

# Load data
df <- read_excel("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260420_Two_way_ANOVA_resilience.xlsx",
                 sheet = "Two_way_ANOVA_resilience")

# ── 1. Pivot to long format ───────────────────────────────────────────────────
# Keep windcase in — do NOT pre-average over it
df_long <- df |>
  select(model, rcp, mgm, windcase, norm.auc, total, recovery.time) |>
  mutate(
    rcp = recode(rcp, `-` = "refclim")   # rename "-" to a readable level
  ) |>
  pivot_longer(
    cols      = c(norm.auc, total, recovery.time),
    names_to  = "Variable",
    values_to = "Value"
  ) |>
  mutate(
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),          # now 3 levels: refclim, rcp45, rcp85
    Variable   = recode(Variable,
                        norm.auc      = "Normalized AUC",
                        total         = "Total Disturbance Impact [m3/ha]",
                        recovery.time = "Recovery Time [years]")
  )


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
  "Normalized AUC"                   = "#185FA5",
  "Total Disturbance Impact [m3/ha]" = "#D85A30",
  "Recovery Time [years]"            = "#1D9E75"
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
  labs(x = "Discriminatory power (SD of cell means)",
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


#2###############################################################################
#           TEST P2
################################################################################

THIS IS MAKING THE TOW-WAY ANOVA ON RECOVERY METRICS NORMILIZING THE SCALE THROUGH THE Z-SCORE, BUT WITHOUT FIXING THE NA VALUES

# !!! To generate the df table run the script in "C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/2_Analyses_figures/2d_Resilience_and_MF_AUC.R"
# This is the table created mainly on Laura's script data rendering but with the edit of the disturbance impact based only on wind volume disturbed/impact.

# Load data
df <- read_excel("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260420_Two_way_ANOVA_resilience.xlsx",
                 sheet = "Two_way_ANOVA_resilience")

# ── 1. Pivot to long format ───────────────────────────────────────────────────
# Keep windcase in — do NOT pre-average over it
df_long <- df |>
  select(model, rcp, mgm, windcase, norm.auc, total, recovery.time) |>
  mutate(rcp = recode(rcp, `-` = "refclim")) |>
  pivot_longer(
    cols      = c(norm.auc, total, recovery.time),
    names_to  = "Variable",
    values_to = "Value"
  ) |>
  mutate(
    Variable   = recode(Variable,
                        norm.auc      = "Normalized AUC",
                        total         = "Total Disturbance Impact [m3/ha]",
                        recovery.time = "Recovery Time [years]"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp)
  ) |>
  group_by(Variable) |>
  mutate(Value = scale(Value)[,1]) |>   # z-score per variable: The z-score transforms each value as: z = (x − mean) / SD - So for each variable separately, it subtracts the mean and divides by the standard deviation. The result is that: mean becomes 0, SD becomes 1. All variables end up on the same unitless scale ("how many standard deviations from the mean").
  ungroup()

#df_long <- df_long |> filter(Climate != "refclim")

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
  test <- df_long |> filter(Variable == "Normalized AUC")
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
  "Normalized AUC"                   = "#185FA5",
  "Total Disturbance Impact [m3/ha]" = "#D85A30",
  "Recovery Time [years]"            = "#1D9E75"
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


#3###############################################################################
#           TEST P3
################################################################################


THIS IS MAKING THE TOW-WAY ANOVA ON RECOVERY METRICS NORMILIZING THE SCALE THROUGH THE Z-SCORE, FIXING THE NA VALUES
NOTE HERE IMPORTANT WE USED BASICALLY THE SAME TABLE OF BEFORE BUT MADE BY LAURA 2026-04-21 SEE THE CSV FILE NAME

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


#4###############################################################################
#           TEST P4
################################################################################

Version without recovery time

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 1. Pivot to long format ───────────────────────────────────────────────────
df_long <- df |>
  select(model, rcp, mgm, windcase, one.minus.norm.auc, impact) |>
  mutate(rcp = recode(rcp, `-` = "refclim")) |>
  pivot_longer(
    cols      = c(one.minus.norm.auc, impact),
    names_to  = "Variable",
    values_to = "Value"
  ) |>
  mutate(
    Variable   = recode(Variable,
                        one.minus.norm.auc = "1 - Normalized AUC",
                        impact             = "Wind Disturbance Impact [m3/ha]"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp)
  ) |>
  group_by(Variable) |>
  mutate(Value = scale(Value)[,1]) |>
  ungroup()

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
  test <- df_long |> filter(Variable == "1 - Normalized AUC")
  # test <- df_long |> filter(Variable == "Wind Disturbance Impact [m3/ha]")
  
  fit <- aov(Value ~ Management * Climate, data = test)
  s <- summary(fit)[[1]]
  rownames(s)
  
  plot(fit)
  shapiro.test(residuals(fit))
  bartlett.test(Value ~ interaction(Management, Climate), data = test)
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
  "1 - Normalized AUC"              = "#185FA5",
  "Wind Disturbance Impact [m3/ha]" = "#D85A30"
)

src_colors <- c(
  "Management"             = "#185FA5",
  "Climate"                = "#97C459",
  "Interaction"            = "#D0CECA",
  "Residual (within-cell)" = "#787670"
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
        legend.position    = "none")

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





#5###############################################################################
#           TEST P5
################################################################################


THIS IS MAKING THE SURVIVAL ANALYSIS K-M ONE THE RECOVERY TIME CAUSE MISSING 
VALUES AND WE WANT TO VISUALIZE THE RECOVERY FUNCTION

THIS IS THE FIRST VERSION NOT THE SAME APPLIED IN THE BDV ARTICLE

HERE THERE IS ALSO THE COX MODEL TEST AND THE 
SCHOENDELF TEST
################################################################################

library(survival)
library(survminer)
library(tidyverse)
library(ggplot2)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 0. Simulation horizon ─────────────────────────────────────────────────────
sim_horizon <- 50   

# ── 1. Prepare data ───────────────────────────────────────────────────────────
df_surv <- df |>
  mutate(
    rcp        = recode(rcp, `-` = "refclim"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),
    status     = ifelse(is.infinite(rt), 0L, 1L),  # 1 = recovered, 0 = censored
    time       = ifelse(is.infinite(rt), sim_horizon, rt)
  )

df_surv |> count(status)
# 1 → recovered (exact time known)
# 0 → never recovered within simulation (right-censored)


# ── 2. Kaplan-Meier by Management ─────────────────────────────────────────────
km_fit <- survfit(Surv(time, status) ~ Management, data = df_surv)

p_km <- ggsurvplot(
  km_fit,
  data             = df_surv,
  fun              = "event",          # cumulative recovery (1 - S)
  conf.int         = TRUE,
  palette          = "Dark2",
  xlab             = "Time [years]",
  ylab             = "Cumulative recovery probability",
  title            = "Recovery by Management",
  legend.title     = "Management",
  ggtheme          = theme_minimal(base_size = 11),
  risk.table       = TRUE,
  risk.table.height = 0.25
)
p_km

# ── 3. KM faceted by Climate ──────────────────────────────────────────────────
p_km_facet <- ggsurvplot_facet(
  km_fit,
  data     = df_surv,
  facet.by = "Climate",
  fun      = "event",
  conf.int = TRUE,
  palette  = "Dark2",
  xlab     = "Time [years]",
  ylab     = "Cumulative recovery probability",
  ggtheme  = theme_minimal(base_size = 11)
)
p_km_facet


# ── 4. Log-rank test ──────────────────────────────────────────────────────────
survdiff(Surv(time, status) ~ Management, data = df_surv)

# Per climate
df_surv |>
  group_by(Climate) |>
  group_modify(~ {
    test <- survdiff(Surv(time, status) ~ Management, data = .x)
    tibble(
      chi_sq  = test$chisq,
      df      = length(test$n) - 1,
      p_value = 1 - pchisq(test$chisq, df = length(test$n) - 1)
    )
  })


# ── 5. Cox proportional hazards ───────────────────────────────────────────────
cox_fit <- coxph(Surv(time, status) ~ Management * Climate, data = df_surv)
summary(cox_fit)

# Tidy hazard ratios
cox_tidy <- broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  rename(HR = estimate, CI_low = conf.low, CI_high = conf.high)

print(cox_tidy, n = Inf)


# ── 6. Check proportional hazards assumption ──────────────────────────────────
ph_test <- cox.zph(cox_fit)
print(ph_test)       # p < 0.05 → assumption violated for that term
ggcoxzph(ph_test)    # should look like flat horizontal lines



#6###############################################################################
#           TEST P6
################################################################################

THIS IS MAKING THE SURVIVAL ANALYSIS K-M ONE THE RECOVERY TIME CAUSE MISSING 
VALUES AND WE WANT TO VISUALIZE THE RECOVERY FUNCTION
THIS IS THE SAME ANALYSIS APPLIED IN BDV WITH COX MODEL INTEGRATED AND 
PVALUE BASED ON MNG INFLUENCE ON RECOVERY COMPERING MGM WITHIN THE SAME CLIMATE

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

# ── 1. Prepare survival data ──────────────────────────────────────────────────
df_surv <- df |>
  mutate(
    rcp        = recode(rcp, `-` = "refclim"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),
    recovered  = ifelse(is.infinite(rt), 0L, 1L),   # 0 = censored, 1 = recovered
    time       = ifelse(is.infinite(rt), sim_horizon, rt)
  ) |>
  filter(!is.na(Management), !is.na(Climate))

# Sanity check
df_surv |> count(Management, recovered)


# ── 2. Log-rank p-values (one per Climate row) ────────────────────────────────

p_value_annotations <- df_surv |>
  group_by(Climate) |>
  group_modify(~ {
    sdiff <- tryCatch(
      survdiff(Surv(time, recovered) ~ Management, data = .x),
      error = function(e) NULL
    )
    if (is.null(sdiff) || is.na(sdiff$chisq)) return(tibble(p_value_label = NA))
    p_val <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
    tibble(p_value_label = paste0("Log-rank p = ", format.pval(p_val, digits = 3)))
  }) |>
  ungroup() |>
  filter(!is.na(p_value_label)) |>
  crossing(Management = levels(df_surv$Management))  # repeat across all columns



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
    
    fit <- survfit(cox_model, newdata = data.frame(Management = mgm_level))
    
    all_plot_data <- bind_rows(
      all_plot_data,
      surv_summary(fit) |>
        mutate(Management = mgm_level, Climate = clim)
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


#7###############################################################################
#           TEST P7
################################################################################

THIS IS MAKING THE SURVIVAL ANALYSIS K-M ON THE RECOVERY TIME CAUSE MISSING 
VALUES AND WE WANT TO VISUALIZE THE RECOVERY FUNCTION. THIS IS THE SAME 
ANALYSIS APPLIED IN BDV WITH COX MODEL ONLY ON MANAGEMENT
PVALUE BASED ON CLIM INFLUENCE ON RECOVERY COMPERING RCP WITHIN THE SAME MGM


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

# ── 1. Prepare survival data ──────────────────────────────────────────────────
df_surv <- df |>
  mutate(
    rcp        = recode(rcp, `-` = "refclim"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),
    recovered  = ifelse(is.infinite(rt), 0L, 1L),   # 0 = censored, 1 = recovered
    time       = ifelse(is.infinite(rt), sim_horizon, rt)
  ) |>
  filter(!is.na(Management), !is.na(Climate))

# Sanity check
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
    
    fit <- survfit(cox_model, newdata = data.frame(Management = mgm_level), conf.type = "arcsin")
    
    all_plot_data <- bind_rows(
      all_plot_data,
      surv_summary(fit) |>
        mutate(Management = mgm_level, Climate = clim)
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
    title = "Growing Stock Recovery by Management — Faceted by Climate",
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




#8###############################################################################
#        TEST P8 - PROPOTION OF RECOVERY - TABLE CREATION
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

# Color palette matching your reference chart style
mgm_colors <- c(
  "ADAPTATION"  = "#E69F00",
  "BIOECONOMY"  = "#009E73",
  "BAU"         = "#0072B2",
  "CONSERVATION"= "#D55E00",
  "UNMANAGED"   = "#999999"
)

ggplot(plot_df, aes(x = mgm, y = prop, fill = mgm, alpha = recovered)) +
  geom_col(
    position = "stack",
    color = "white",
    linewidth = 0.2
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.6) +
  scale_fill_manual(values = mgm_colors) +
  scale_alpha_manual(
    values = c("TRUE" = 0.9, "FALSE" = 0.25),
    labels = c("TRUE" = "Recovered (≤28y)", "FALSE" = "Not recovered (>28y)")
  ) +
  scale_y_continuous(
    limits = c(0, 1.15),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~ rcp, ncol = 3, labeller = labeller(rcp = c(
    refclim = "Reference climate",
    rcp45   = "RCP4.5",
    rcp85   = "RCP8.5"
  ))) +
  labs(
    x        = NULL,
    y        = "Repetitions recovered [%]",
    title    = "Stand Recovery by Year 28 — Faceted by Climate",
    fill     = "Management",
    alpha    = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill  = guide_legend(nrow = 1, override.aes = list(alpha = 0.9)),
    alpha = guide_legend(nrow = 1)
  )


################################################################################

# To extract the fastes recovery time among the scenarios 
#      (in our case is adaptation under rcp85)

################################################################################
# ── Step 1: Extract T99 for all combinations ──────────────────────────────────

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

# ── Step 2: Find the earliest T99 across all combinations ─────────────────────

t99_threshold <- t99_results |>
  filter(!is.na(T99)) |>
  slice_min(T99, n = 1)

cat("Earliest T99 combination:\n")
as.data.frame(t99_threshold)

# Extract the threshold year
threshold_year <- round(t99_threshold$T99)
cat("\nUsing threshold year:", threshold_year, "\n")



# ── 7. SWITCHER RECOVERY ANALISYS BASED ON SURVIVAL ANALYSIS RESULTS ──────────
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

# Step 2: management color palette
mgm_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BIOECONOMY"   = "#009E73",
  "BAU"          = "#0072B2",
  "CONSERVATION" = "#D55E00",
  "UNMANAGED"    = "#999999"
)

# Step 3: plot
ggplot(recovery_28, aes(x = Management, y = recovery, fill = Management)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = recovery_lower, ymax = recovery_upper),
    width = 0.25,
    linewidth = 0.6,
    color = "grey30"
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.6) +
  scale_fill_manual(values = mgm_colors) +
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
    title = "Stand Recovery by Year 28 — Faceted by Climate",
    fill  = "Management"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

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
    title = "Growing Stock Recovery by Year 28 — Faceted by Climate"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    legend.position  = "none",
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# ── 8. LET'S MAKE A TABLE OF RECOVERY TIME  ───────────────────────────────────

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
    "20260423_RT_based_on_proportion.xlsx"
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
  sheetName = "20260423_R_based_on_T_prop",
  overwrite = TRUE
)


#9###############################################################################
#   TEST P9 - VARIANCE PARTITIONING ANALYSIS
################################################################################

MAKING THE VARIANCE PARTITIONING IN THE SURVIVAL ANALYSIS COX model.response

# ── Survival-based Variance Decomposition ─────────────────────────────────────

library(survival)
library(tidyverse)

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

# Analysis of Deviance Table
anova(cox_full)

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



# ── 6. Save the Results in two diff tables  ───────────────────────────────────

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




#10#############################################################################
#   TEST P10 - CORRECTION OF THE MAIN SURVIVAL ANALYSIS TEST (COX ADJUSTED)
################################################################################


HERE IS THE VERSION IN WHICH I FIXED THE PROBLEM OF MANAGEMENT * CLIMATE AS INTERACTION - NO ADDITIVE
TO ANALYSIS INTERACTIONS BETWENN THEM AND CLUSTERING PER RCM AND WIND CASES
THIS IS THE SAME ANALYSIS APPLIED IN BDV WITH COX MODEL INTEGRATED AND 
PVALUE BASED ON CLIM INFLUENCE ON RECOVERY COMPERING RCP WITHIN THE SAME MGM


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



#11#############################################################################
#   TEST P11 - CORRECTION OF THE MAIN SURVIVAL ANALYSIS TEST K-M CURVE LAURA
################################################################################

THIS IS THE SECTION WHERE LAURA AND ME FIXED THE SURVIVAL ANALYSIS BASED ON WHAT
WAS IMPORTANT FOR OUR REFERENCE CASES AND OUR MODEL

HERE THERE IS THE FIRST PART KEPLEN-MEIER SCRIPT WITHOUT COX MODELS

library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data
#df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")
# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50
penalty_primary    <- 50   # primary imputation:  rt = sim_horizon (boundary)
penalty_sensitivity <- 75  # sensitivity check:   rt = 75 (beyond window)

management_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BAU"          = "#185FA5",
  "BIOECONOMY"   = "#1D9E75",
  "CONSERVATION" = "#D85A30",
  "UNMANAGED"    = "#787670"
)

# ── 1. Recode & flag non-recoverers ──────────────────────────────────────────
df <- df |>
  mutate(
    rcp       = recode(rcp, `-` = "refclim"),
    group_key = paste(mgm, rcp, model, windcase, sep = "_"),
    # TRUE for the 27 cases that never recovered
    is_censored = is.infinite(rt) | is.na(rt)
  )

# Sanity check: confirm censored cases are clustered in rcp45
df |>
  filter(is_censored) |>
  count(rcp, mgm) |>
  print()

# ── 2. Impute rt — primary & sensitivity ─────────────────────────────────────
# Primary: rt_imp = 50 for non-recoverers
# Sensitivity: rt_imp_sens = 75 for non-recoverers
df <- df |>
  mutate(
    rt_imp      = if_else(is_censored, as.numeric(penalty_primary),    rt),
    rt_imp_sens = if_else(is_censored, as.numeric(penalty_sensitivity), rt)
  )

# Quick summary: compare distributions with and without imputation
cat("\n── rt distribution summary (recovered cases only) ──\n")
df |> filter(!is_censored) |> summarise(
  n    = n(),
  mean = mean(rt),
  sd   = sd(rt),
  med  = median(rt),
  min  = min(rt),
  max  = max(rt)
) |> print()

cat("\n── rt_imp distribution summary (all cases, primary imputation) ──\n")
df |> summarise(
  n           = n(),
  n_imputed   = sum(is_censored),
  mean        = mean(rt_imp),
  sd          = sd(rt_imp),
  med         = median(rt_imp),
  min         = min(rt_imp),
  max         = max(rt_imp)
) |> print()


a<-df %>% select(mgm, model, windcase,rcp, rt_imp, is_censored)
head(a)

df2 <- a %>%  mutate(  event = ifelse(is_censored, 0, 1)  )

surv_obj <- Surv(  time  = df2$rt_imp,  event = df2$event)

#  MANAGEMENT EFFECT:
km_mgm <- survfit(  surv_obj ~ mgm,  data = df2)

ggsurvplot(  km_mgm,  data = df2,  risk.table = TRUE,  pval = TRUE,
             conf.int = TRUE,  xlab = "Years after disturbance",  
             ylab = "Probability of NOT yet recovering",
             legend.title = "Management",  ggtheme = theme_bw())

#  RCP EFFECT:
km_rcp <- survfit(  surv_obj ~ rcp   ,  data = df2)

ggsurvplot( km_rcp, data = df2, risk.table = TRUE,pval = TRUE,  conf.int = TRUE,
            xlab = "Years after disturbance",
            ylab = "Probability of NOT yet recovering",
            legend.title = "Management",
            ggtheme = theme_bw() )


summary(km_rcp, times = 23)

# Which recovers faster?
# Look at t specific year
summary(km_mgm, times = 23)
km23 <- summary(km_mgm, times = 23)


# CHANGE TO PROBABILITY OF RECOVERY: (fun="event")
ggsurvplot(  km_mgm,  data = df2,  risk.table = TRUE,  pval = TRUE,fun = "event",
             conf.int = TRUE,  xlab = "Years after disturbance",  
             ylab = "Probability of recovering",
             legend.title = "Management",  ggtheme = theme_bw())



ggsurvplot( km_rcp, data = df2, risk.table = TRUE,pval = TRUE,  conf.int = TRUE,fun = "event",
            xlab = "Years after disturbance",
            ylab = "Probability of recovering",
            legend.title = "Management",
            ggtheme = theme_bw() )



#12#############################################################################
#   TEST P12 - CORRECTION OF THE MAIN SURVIVAL ANALYSIS TEST K-M CURVE MARCO
################################################################################

SAME BUT ADAPTED JUST FOR THE VISUALIZATION

library(tidyverse)
library(survival)
library(survminer)
library(gridExtra)
library(grid)

# ── Parameters ─────────────────────────────────────────────────────────────────
sim_horizon <- 50

management_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BAU"          = "#185FA5",
  "BIOECONOMY"   = "#1D9E75",
  "CONSERVATION" = "#D85A30",
  "UNMANAGED"    = "#787670"
)

rcp_colors <- c(
  "refclim" = "#4DAF4A",
  "rcp45"   = "#FF7F00",
  "rcp85"   = "#E41A1C"
)

# ── Data prep ──────────────────────────────────────────────────────────────────
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv") |>
  mutate(
    rcp   = recode(rcp, `-` = "refclim"),
    time  = if_else(is.infinite(rt) | is.na(rt), as.numeric(sim_horizon), rt),
    event = if_else(is.infinite(rt) | is.na(rt), 0L, 1L),
    mgm   = factor(mgm, levels = names(management_colors)),
    rcp   = factor(rcp, levels = names(rcp_colors))
  )

# ── Helper: extract legend labels from survfit strata names ───────────────────
# survfit prepends "var=value"; strip the prefix so palette matching works
strip_strata <- function(fit, var) {
  gsub(paste0(var, "="), "", names(fit$strata))
}

# ── Helper: build ggsurvplot grobs with correct colors ────────────────────────
make_km <- function(fit, data, palette, strata_var, legend_title, title = NULL) {
  
  labs    <- strip_strata(fit, strata_var)   # e.g. c("refclim","rcp45","rcp85")
  palette <- palette[labs]                   # reorder/subset to match strata order
  
  p <- ggsurvplot(
    fit, data = data,
    fun               = "event",
    conf.int          = TRUE,
    risk.table        = TRUE,
    risk.table.y.text = FALSE,
    pval              = TRUE,
    pval.size         = 3.5,
    palette           = palette,             # now correctly ordered
    legend.labs       = labs,
    title             = title,
    xlab              = "Years after disturbance",
    ylab              = "Cumulative recovery",
    legend.title      = legend_title,
    font.main         = c(11, "bold"),
    font.x            = 9, font.y = 9,
    font.tickslab     = 8, font.legend = 8,
    tables.theme      = theme_cleantable(),
    ggtheme           = theme_bw(base_size = 9)
  )
  
  sc <- scale_x_continuous(limits = c(0, sim_horizon), breaks = seq(0, sim_horizon, 10))
  p$plot  <- p$plot  + sc + theme(plot.margin = margin(4, 6, 2, 6))
  p$table <- p$table + sc + theme(plot.margin = margin(2, 6, 4, 6))
  
  list(
    plot  = ggplotGrob(p$plot),
    table = ggplotGrob(p$table)
  )
}

# ── Helper: equalise grob widths across a list of grobs ───────────────────────
align_widths <- function(grob_list) {
  max_w <- do.call(grid::unit.pmax, lapply(grob_list, function(g) g$widths))
  lapply(grob_list, function(g) { g$widths <- max_w; g })
}

# ── Plot A: KM by Management ───────────────────────────────────────────────────
km_mgm <- survfit(Surv(time, event) ~ mgm, data = df)
g_mgm  <- make_km(km_mgm, df, management_colors, "mgm", "Management")

# ── Plot B: KM by RCP ─────────────────────────────────────────────────────────
km_rcp <- survfit(Surv(time, event) ~ rcp, data = df)
g_rcp  <- make_km(km_rcp, df, rcp_colors, "rcp", "Climate")

# ── Variant: per-Management, curves by RCP ────────────────────────────────────
g_variant <- lapply(levels(df$mgm), function(m) {
  sub <- filter(df, mgm == m)
  fit <- survfit(Surv(time, event) ~ rcp, data = sub)
  make_km(fit, sub, rcp_colors, "rcp", "Climate", title = m)
})

# ── Assemble overview (Management | RCP side by side) ─────────────────────────
ov_plots  <- align_widths(list(g_mgm$plot,  g_rcp$plot))
ov_tables <- align_widths(list(g_mgm$table, g_rcp$table))

overview <- arrangeGrob(
  ov_plots[[1]],  ov_plots[[2]],
  ov_tables[[1]], ov_tables[[2]],
  ncol    = 2,
  heights = c(3, 1),
  top     = textGrob("Forest Recovery — Overview",
                     gp = gpar(fontsize = 13, fontface = "bold"))
)

# ── Assemble variant (5 management panels) ────────────────────────────────────
var_plots  <- align_widths(lapply(g_variant, `[[`, "plot"))
var_tables <- align_widths(lapply(g_variant, `[[`, "table"))

variant <- arrangeGrob(
  grobs   = c(var_plots, var_tables),
  ncol    = 5,
  heights = c(3, 1),
  top     = textGrob("Forest Recovery by Management — Climate Breakdown",
                     gp = gpar(fontsize = 13, fontface = "bold"))
)

# ── Print ──────────────────────────────────────────────────────────────────────
grid.newpage(); grid.draw(overview)
grid.newpage(); grid.draw(variant)




#13#############################################################################
#     TEST P13  TWO-WAY ANOVA ON NEW K-M CURVE INITIAL DF WITH PENALTIES
################################################################################

THIS IS MAKING THE TOW-WAY ANOVA ON RECOVERY METRICS NORMILIZING THE SCALE THROUGH THE Z-SCORE, FIXING THE NA VALUES
NOTE HERE IMPORTANT WE USED BASICALLY THE SAME TABLE OF BEFORE BUT MADE BY LAURA 2026-04-21 SEE THE CSV FILE NAME

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# ----------------
# 1. PREPARE DATA
# ----------------
df_long <- a |>
  mutate(
    rcp = recode(rcp, `-` = "refclim"),
    Management = as.factor(mgm),
    Climate    = as.factor(rcp),
    Variable   = "Recovery Time [years]",
    Value      = rt_imp
  ) |>
  select(model, windcase, Management, Climate, Variable, Value)

# ----------------
# 2. Z-SCORE (same logic as before)
# ----------------
df_long <- df_long |>
  group_by(Variable) |>
  mutate(Value = scale(Value)[,1]) |>
  ungroup()

# ----------------
# 3. DISCRIMINATORY POWER
# ----------------
disc_power <- df_long |>
  group_by(Variable, Climate, Management) |>
  summarise(cell_mean = mean(Value, na.rm = TRUE), .groups = "drop") |>
  group_by(Variable) |>
  summarise(disc = sd(cell_mean), .groups = "drop")

# ----------------
# 4. TWO-WAY ANOVA
# ----------------
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
  }) |>
  ungroup()

# ----------------
# 5. COMBINE RESULTS
# ----------------
results <- disc_power |>
  left_join(anova_res, by = "Variable") |>
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

# ----------------
# 6. COLORS
# ----------------
var_colors <- c(
  "Recovery Time [years]" = "#1D9E75"
)

src_colors <- c(
  "Management"              = "#185FA5",
  "Climate"                 = "#97C459",
  "Interaction"             = "#D0CECA",
  "Residual (within-cell)"  = "#787670"
)

# ----------------
# 7. PANEL A
# ----------------
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
  theme(legend.position = "none")

# ----------------
# 8. PANEL B
# ----------------
plot_decomp <- results |>
  select(Variable, pct_mgmt, pct_climate, pct_inter, pct_resid) |>
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
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

# ----------------
# 9. FINAL
# ----------------
final_panel <- pA + pB
final_panel



#14#############################################################################
#  TEST P14  MEAN DIFFERENCE FROM REFERENCE AND ORIGINAL DF RT WITH PENALTIES
################################################################################

HERE WE APPLIED THE SAME CODE AND ANALYSIS LAURA MADE AT THE BEGINNING OF
THE PROBLEM TO CONFIGURATE THE SURVIVAL PREDICTION ON RECOVERY TIME OF NOT RECOVERED CASES.
SIMPLE MEAN DIFFERENCE FROM REFERENCE CLIMATE AND BAU MANAGEMENT WITH PENALTIES AND SENSITIVITY TEST

library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50
penalty_primary    <- 50   # primary imputation:  rt = sim_horizon (boundary)
penalty_sensitivity <- 75  # sensitivity check:   rt = 75 (beyond window)

management_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BAU"          = "#185FA5",
  "BIOECONOMY"   = "#1D9E75",
  "CONSERVATION" = "#D85A30",
  "UNMANAGED"    = "#787670"
)

# ── 1. Recode & flag non-recoverers ──────────────────────────────────────────
df <- df |>
  mutate(
    rcp       = recode(rcp, `-` = "refclim"),
    group_key = paste(mgm, rcp, model, windcase, sep = "_"),
    # TRUE for the 27 cases that never recovered
    is_censored = is.infinite(rt) | is.na(rt)
  )

# Sanity check: confirm censored cases are clustered in rcp45
df |>
  filter(is_censored) |>
  count(rcp, mgm) |>
  print()

# ── 2. Impute rt — primary & sensitivity ─────────────────────────────────────
# Primary: rt_imp = 50 for non-recoverers
# Sensitivity: rt_imp_sens = 75 for non-recoverers
df <- df |>
  mutate(
    rt_imp      = if_else(is_censored, as.numeric(penalty_primary),    rt),
    rt_imp_sens = if_else(is_censored, as.numeric(penalty_sensitivity), rt)
  )

# Quick summary: compare distributions with and without imputation
cat("\n── rt distribution summary (recovered cases only) ──\n")
df |> filter(!is_censored) |> summarise(
  n    = n(),
  mean = mean(rt),
  sd   = sd(rt),
  med  = median(rt),
  min  = min(rt),
  max  = max(rt)
) |> print()

cat("\n── rt_imp distribution summary (all cases, primary imputation) ──\n")
df |> summarise(
  n           = n(),
  n_imputed   = sum(is_censored),
  mean        = mean(rt_imp),
  sd          = sd(rt_imp),
  med         = median(rt_imp),
  min         = min(rt_imp),
  max         = max(rt_imp)
) |> print()

# ── 3. Long format for ANOVA/discrimination plot ──────────────────────────────
# Mirrors the structure of your existing impact / one.minus.norm.auc analysis
# Three resilience variables: impact, one.minus.norm.auc, rt_imp
df_long <- df |>
  select(mgm, rcp, model, windcase, group_key, is_censored,
         impact, one.minus.norm.auc, rt_imp, rt_imp_sens) |>
  pivot_longer(
    cols      = c(impact, one.minus.norm.auc, rt_imp),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(
    Management = factor(mgm),
    Climate    = factor(rcp)
  )

# ── 4. Compute mean differences vs. reference climate (refclim) ──────────────
# Same logic as your existing discrimination plot
ref_means <- df_long |>
  filter(rcp == "refclim") |>
  group_by(mgm, variable) |>
  summarise(ref_mean = mean(value, na.rm = TRUE), .groups = "drop")

mean_diffs <- df_long |>
  filter(rcp != "refclim") |>
  group_by(mgm, rcp, variable) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se_val   = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  left_join(ref_means, by = c("mgm", "variable")) |>
  mutate(
    mean_diff = mean_val - ref_mean,
    ci_low    = mean_diff - 1.96 * se_val,
    ci_high   = mean_diff + 1.96 * se_val
  )

# ── 5. Sensitivity check: repeat with rt_imp_sens ────────────────────────────
df_long_sens <- df |>
  select(mgm, rcp, model, windcase, is_censored,
         impact, one.minus.norm.auc, rt_imp_sens) |>
  rename(rt_imp = rt_imp_sens) |>          # reuse same column name for pivot
  pivot_longer(
    cols      = c(impact, one.minus.norm.auc, rt_imp),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(Management = factor(mgm), Climate = factor(rcp))

ref_means_sens <- df_long_sens |>
  filter(rcp == "refclim") |>
  group_by(mgm, variable) |>
  summarise(ref_mean = mean(value, na.rm = TRUE), .groups = "drop")

mean_diffs_sens <- df_long_sens |>
  filter(rcp != "refclim") |>
  group_by(mgm, rcp, variable) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se_val   = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  left_join(ref_means_sens, by = c("mgm", "variable")) |>
  mutate(
    mean_diff = mean_val - ref_mean,
    ci_low    = mean_diff - 1.96 * se_val,
    ci_high   = mean_diff + 1.96 * se_val,
    imputation = "sensitivity (rt=75)"
  )

mean_diffs <- mean_diffs |> mutate(imputation = "primary (rt=50)")

# Combined for sensitivity comparison
mean_diffs_combined <- bind_rows(mean_diffs, mean_diffs_sens)

# ── 6. Main discrimination plot (primary imputation) ─────────────────────────
# Replicates your existing plot style, now with rt_imp as third variable
variable_labels <- c(
  "impact"           = "Impact",
  "one.minus.norm.auc" = "one.minus.norm.auc",
  "rt_imp"           = "Recovery time (rt)"
)

plot_primary <- ggplot(
  mean_diffs |> mutate(variable = recode(variable, !!!variable_labels)),
  aes(x = variable, y = mean_diff, fill = rcp)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.7),
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  # Flag that rt non-recoverers are imputed
  annotate("text", x = 3, y = Inf, vjust = 1.5, hjust = 0.5,
           label = "rt non-recoverers\nimputed at 50 yr",
           size = 2.5, color = "grey40", fontface = "italic") +
  scale_fill_manual(
    values = c("rcp45" = "#56B4E9", "rcp85" = "#E69F00"),
    labels = c("rcp45" = "RCP 4.5", "rcp85" = "RCP 8.5")
  ) +
  facet_grid(variable ~ mgm, scales = "free_y", switch = "y") +
  labs(
    title = "Mean difference from reference climate — Impact, AUC, Recovery Time",
    x     = "Variable",
    y     = "Mean difference",
    fill  = "RCP",
    caption = paste0(
      "Note: 27 non-recovering cases under RCP4.5 imputed at rt = ",
      penalty_primary, " yr (simulation horizon).\n",
      "Sensitivity analysis with rt = ", penalty_sensitivity,
      " yr available in plot_sensitivity object."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = "black"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
    plot.caption     = element_text(size = 8, color = "grey40", hjust = 0)
  )

print(plot_primary)

# ── 7. Sensitivity comparison plot ───────────────────────────────────────────
# Shows only rt_imp to highlight sensitivity of imputation choice
plot_sensitivity <- mean_diffs_combined |>
  filter(variable == "rt_imp") |>
  mutate(variable = recode(variable, !!!variable_labels)) |>
  ggplot(aes(x = rcp, y = mean_diff, fill = imputation)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.7),
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c(
    "primary (rt=50)"      = "#009E73",
    "sensitivity (rt=75)"  = "#CC79A7"
  )) +
  facet_wrap(~ mgm, nrow = 1) +
  labs(
    title   = "Sensitivity analysis — effect of imputation value on Recovery Time (rt)",
    x       = "Climate scenario",
    y       = "Mean difference from reference",
    fill    = "Imputation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(plot_sensitivity)

# ── 8. Export imputed dataset for downstream use (survival analysis etc.) ─────
# This is the clean version of df ready for your Cox/AFT pipeline
df_final <- df |>
  select(mgm, rcp, model, windcase, group_key,
         impact, one.minus.norm.auc,
         rt, rt_imp, rt_imp_sens, is_censored)

# Uncomment to save:
# write_csv(df_final, "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/df_with_rt_imputed.csv")



#15#############################################################################
#       TEST P15  COX MODELS WITH REFERENCES CLIM AND BAU
################################################################################

HERE THERE IS THE CORRECTION FOR THE COX MODEL WE APPLIED TILL NOW MADE BY LAURA
THE POINT OF CORRECTION COMES EXPECIALLY WHEN IT IS USED AS REFERENCES REFCLIM AND BAU
ALSO THERE IS A COMPARISON BETWEEN THE TWO COX MODELS WITH STAT TEST OF PERFORMANCES


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

# ── 1. Recode & flag non-recoverers ──────────────────────────────────────────
df <- df |>
  mutate(
    rcp       = recode(rcp, `-` = "refclim"),
    group_key = paste(mgm, rcp, model, windcase, sep = "_"),
    # TRUE for the 27 cases that never recovered
    is_censored = is.infinite(rt) | is.na(rt)
  )

# Sanity check: confirm censored cases are clustered in rcp45
df |>
  filter(is_censored) |>
  count(rcp, mgm) |>
  print()

# ── 2. Impute rt — primary & sensitivity ─────────────────────────────────────
# Primary: rt_imp = 50 for non-recoverers
# Sensitivity: rt_imp_sens = 75 for non-recoverers
df <- df |>
  mutate(
    rt_imp      = if_else(is_censored, as.numeric(sim_horizon),    rt)
  )

df <- df %>%  mutate(  event = ifelse(is_censored, 0, 1)  )

#-------------------------------------------------------------- SET REFERENCE CASES:
# For the Cox model need to set baseline which they compare things: 
# Compared to BAU, how does each management change recovery rate?
# Compared to REFLCIM, how does each rcp change recovery rate?

df$mgm <- relevel(as.factor(df$mgm), ref = "BAU")
df$rcp <- relevel(as.factor(df$rcp), ref = "refclim")

# the first one here is the reference:
levels(as.factor(df$mgm))
levels(as.factor(df$rcp))
#-------------------------------------------------------------------------------------------
surv_obj <- Surv( time = df$rt_imp,  event = df$event)

# --- FIT AN ADDITIVE MODEL:

#This estimates:
## management effects adjusted for climate
## climate effects adjusted for management

cox1 <- coxph(  surv_obj ~ mgm + rcp ,  data = df)

# --- FIT INTERACTION MODEL
# Does management effectiveness change under climate change?
cox2 <- coxph(surv_obj ~ mgm * rcp ,  data = df)

summary(cox1)
summary(cox2)


#---- COMAPRE THE MODELS:
#If interaction improves model substantially → climate modifies management effectiveness.
#If not → management and climate effects are mostly additive.

AIC(cox1, cox2)
anova(cox1, cox2, test = "LRT")

# PRoportional hazards assumption Check proportional hazards
cox.zph(cox2)


#Non-significant p-values --> PH assumption OK.

#Significant p-values -->Effects change over time.
#Ecologically this may mean:  management helps early but not late recovery.


#---------------------------- GRAPHICAL OPTIONS:

library(broom)

#--- COX1

hr_df1 <- broom::tidy(cox1, exponentiate = TRUE, conf.int = TRUE)
ggplot(hr_df1,
       aes(x = estimate,
           y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,      xmax = conf.high),     height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed",        color = "red") +
  scale_x_log10() +
  labs(   x = "Hazard ratio (log scale)",   y = "",   title = "Effects on recovery rate") +
  theme_bw(base_size = 14)

#--- COX2
hr_df2 <- broom::tidy(cox2, exponentiate = TRUE, conf.int = TRUE)
ggplot(hr_df2,
       aes(x = estimate,
           y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,    xmax = conf.high),         height = 0.2) +
  geom_vline(xintercept = 1,       linetype = "dashed",            color = "red") +
  scale_x_log10() +
  labs(  x = "Hazard ratio (log scale)",  y = "",   title = "Effects on recovery rate"  ) +
  theme_bw(base_size = 14)


#-------------------------------------------- Interaction plot
newdat <- expand.grid(
  mgm = levels(df$mgm),
  rcp = levels(df$rcp)
)

sf <- survfit(cox2, newdata = newdat)
med <- surv_median(sf)
med$mgm <- newdat$mgm
med$rcp <- newdat$rcp

ggplot(med,   aes(x = rcp,           y = median,           color = mgm,           group = mgm)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  
  scale_color_manual(values = management_colors) +
  
  labs(    y = "Predicted recovery time",
           x = "Climate scenario"
  ) +
  
  theme_bw(base_size = 14)

#------------------------------ maybe these are below not meaningful... need to check it

sf <- survfit(cox1, newdata = newdat)
med <- surv_median(sf)
med$mgm <- newdat$mgm
med$rcp <- newdat$rcp

ggplot(med,   aes(x = rcp,           y = median,           color = mgm,           group = mgm)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  
  scale_color_manual(values = management_colors) +
  
  labs(    y = "Predicted recovery time",
           x = "Climate scenario"
  ) +
  
  theme_bw(base_size = 14)

#---------- ???For diagnostics, visualize Schoenfeld residuals:
ph_test <- cox.zph(cox2)

ggcoxzph(ph_test)
