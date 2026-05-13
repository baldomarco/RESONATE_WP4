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
