################################################################################
###
### Code illustrating the Appendix table analyses in:
###
###     Marco Baldo - Laura Dobor 
###             15-06-2026
### 
###
###
### Contact: Marco Baldo (baldo@fld.czu.cz)
###
### 
################################################################################

library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(openxlsx)

# ES tab absolute values -------------------------------------------------------
# Working directory
# path <- ""C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/"

ES_tab_csv <- read.csv(
  "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/New Tables For Laura/20260630_MF_ES_score_individual_values.csv"
)


# Change variable names
variable_mapping <- list(
  annual.harvest = c("harvest", "Timber"),
  annual.increment = c("increment", "Timber"),
  LAI = c("LAI", "Water"),
  standing.volume = c("Standing volume", "Water"),
  standing.volume.decidious = c("Standing volume", "Water"),
  NEP = c("NEP", "Climate"),
  carbonstock = c("Total carbon", "Climate"),
  deadwood.c.ag = c("Deadwood carbon", "Biodiversity"),
  shannon_BA_landscape = c("Shannon", "Biodiversity"),
  shannon_VOL_landscape = c("Shannon", "Biodiversity")
)


# Filter to have only interested variables and create the scenario column

ES_tab_csv_filtered <- ES_tab_csv %>%
  filter(name %in% names(variable_mapping)) %>%
  mutate(
    Climate = rcp,
    Management = mgm,
    Scenario = paste(model, windcase, sep = "_"),   # create the scenario column
    
    Variable = sapply(name, \(x) variable_mapping[[x]][1]),
    `Ecosystem service` = sapply(name, \(x) variable_mapping[[x]][2]),
    Value = value,
    
    # unit correction (kg -> tons)
    Value = case_when(
      name %in% c("carbonstock", "deadwood.c.ag", "NEP") ~ Value / 1000,
      #name %in% c("NEP") ~ Value / 100,   # careful here isn't in tons!!
      TRUE ~ Value,
    )
  ) %>%
  dplyr::select(Climate, Management, Scenario,
         `Ecosystem service`, Variable, Value)


# Aggregate at scenarion level and save

ES_scenario <- ES_tab_csv_filtered %>%
  group_by(Scenario, Climate, Management,
           `Ecosystem service`, Variable) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Saving
write.xlsx(
  ES_scenario,
  file = "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260630_ES_individual_values_absolute.xlsx",
  sheetName = "20260630_ES_abs_value",
  overwrite = TRUE
)

# Final summary table

ES_summary <- ES_scenario %>%
  group_by(Climate, Management,
           `Ecosystem service`, Variable) %>%
  summarise(
    Value = sprintf(
      "%.2f (%.2f-%.2f)",
      mean(Value, na.rm = TRUE),
      min(Value, na.rm = TRUE),
      max(Value, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Saving
write.xlsx(
  ES_summary,
  file = "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260630_ES_Mean_abs.xlsx",
  sheetName = "20260630_ES_Mean_abs",
  overwrite = TRUE
)


# ES tab normalized ------------------------------------------------------------
# Working directory
# path <- ""C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/"

# Import the xlsx
ES_tab <- read_xlsx("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260407_MF_ES_score_individual_values.xlsx",
                    sheet = "Analysis")
ES_tab

# Variables gathering and range
str(ES_tab)

ES_summary <- ES_tab %>%
  group_by(Climate, `Ecosystem service`, Management, Variable) %>%
  summarise(
    Value = sprintf(
      "%.2f (%.2f-%.2f)",
      mean(Value, na.rm = TRUE),
      min(Value, na.rm = TRUE),
      max(Value, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Make summary only mean
library(openxlsx)

write.xlsx(
  ES_summary,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260627_ES_Mean_Score.xlsx"
  ),
  sheetName = "20260627_ES_Mean_Score",
  overwrite = TRUE
)

# MLF tab ----------------------------------------------------------------------
# Working directory
# Import the CSV
# multifunc_tab <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260415_MF_ES_score.csv")

# Import the CSV for the new table with annual increment GWL solved

multifunc_tab <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/New Tables For Laura/20260630_MF_ES_score.csv")
multifunc_tab

multifunc_tab <- multifunc_tab %>%
  mutate(rcp = ifelse(rcp == "-", "refclim", rcp))

# Variables gathering and range
str(multifunc_tab)

# Let's create separate tables per Ecosystem Function
climate_tab <- multifunc_tab %>%
  dplyr::select(rcp, model, mgm, windcase, Climate)

production_tab <- multifunc_tab %>%
  dplyr::select(rcp, model, mgm, windcase, Production)

water_tab <- multifunc_tab %>%
  dplyr::select(rcp, model, mgm, windcase, Water)

biodiversity_tab <- multifunc_tab %>%
  dplyr::select(rcp, model, mgm, windcase, Biodiversity)

multifunctionality_tab <- multifunc_tab %>%
  dplyr::select(rcp, model, mgm, windcase, score)


# Create the Summary Tables

summary_tab <- multifunc_tab %>%
  group_by(mgm, rcp, model) %>%
  summarise(
    Climate_mean = mean(Climate, na.rm = TRUE),
    Climate_min  = min(Climate, na.rm = TRUE),
    Climate_max  = max(Climate, na.rm = TRUE),
    
    Production_mean = mean(Production, na.rm = TRUE),
    Production_min  = min(Production, na.rm = TRUE),
    Production_max  = max(Production, na.rm = TRUE),
    
    Water_mean = mean(Water, na.rm = TRUE),
    Water_min  = min(Water, na.rm = TRUE),
    Water_max  = max(Water, na.rm = TRUE),
    
    Biodiversity_mean = mean(Biodiversity, na.rm = TRUE),
    Biodiversity_min  = min(Biodiversity, na.rm = TRUE),
    Biodiversity_max  = max(Biodiversity, na.rm = TRUE),
    
    Multifunctionality_mean = mean(score, na.rm = TRUE),
    Multifunctionality_min  = min(score, na.rm = TRUE),
    Multifunctionality_max  = max(score, na.rm = TRUE),
    
    .groups = "drop"
  )

# --- --     Create a publication table      --  --- #

summary_tab <- multifunc_tab %>%
  group_by(rcp, mgm) %>%
  summarise(
    Climate = sprintf("%.2f (%.2f-%.2f)", mean(Climate), min(Climate), max(Climate)),
    Production = sprintf("%.2f (%.2f-%.2f)", mean(Production), min(Production), max(Production)),
    Water = sprintf("%.2f (%.2f-%.2f)", mean(Water), min(Water), max(Water)),
    Biodiversity = sprintf("%.2f (%.2f-%.2f)", mean(Biodiversity), min(Biodiversity), max(Biodiversity)),
    score = sprintf("%.2f (%.2f-%.2f)", mean(score), min(score), max(score)),
    .groups = "drop"
  )

summary_tab_mean <- summary_tab %>%
  mutate(across(
    Climate:score,
    ~ as.numeric(sub(" .*", "", .))
  ))


# MLF diff tab -----------------------------------------------------------------
# --- --     Create a differential publication table      --  --- #

mf_full <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/New Tables For Laura/20260630_MF_ES_score.csv")

str(mf_full)

# Make the relative differential analysis from reference climate

rel_individual <- mf_full %>%
  mutate(rcp = ifelse(rcp == "-", "refclim", rcp)) %>%
  group_by(mgm, windcase) %>%
  mutate(
    Climate_ref = Climate[rcp == "refclim"],
    Production_ref = Production[rcp == "refclim"],
    Water_ref = Water[rcp == "refclim"],
    Biodiversity_ref = Biodiversity[rcp == "refclim"],
    score_ref = score[rcp == "refclim"]
  ) %>%
  ungroup() %>%
  filter(rcp != "refclim") %>%
  mutate(
    Climate_diff = 100 * (Climate - Climate_ref) / Climate_ref,
    Production_diff = 100 * (Production - Production_ref) / Production_ref,
    Water_diff = 100 * (Water - Water_ref) / Water_ref,
    Biodiversity_diff = 100 * (Biodiversity - Biodiversity_ref) / Biodiversity_ref,
    score_diff = 100 * (score - score_ref) / score_ref
  )


# Isolate the mean - min - max values per mgm, rcp and ES + MLF score
rel_summary <- rel_individual %>%
  dplyr::select(rcp, mgm,
         Climate_diff, Production_diff, Water_diff, Biodiversity_diff,
         score_diff) %>%
  pivot_longer(
    cols = -c(rcp, mgm),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(rcp, mgm, indicator) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rcp, mgm, indicator)

# check
rel_summary

# Wide more readable format
rel_summary_wide <- rel_summary %>%
  tidyr::pivot_wider(
    names_from = indicator,
    values_from = c(mean, min, max)
  )


# Save the summary table
library(openxlsx)

# Make summary with min-max and mean
write.xlsx(
  summary_tab,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260620_MF_ES_Summary_Score.xlsx"
  ),
  sheetName = "20260630_MF_ES_Summary_Score",
  overwrite = TRUE
)


# Make summary only mean
write.xlsx(
  summary_tab_mean,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260630_MF_ES_Mean_Score.xlsx"
  ),
  sheetName = "20260630_MF_ES_Mean_Score",
  overwrite = TRUE
)


# Make summary table of differential analysis on MLF
write.xlsx(
  rel_summary_wide,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260630_MF_ES_Diff_Score.xlsx"
  ),
  sheetName = "20260630_MF_ES_Diff_Score",
  overwrite = TRUE
)


#-------------------------------------------------------------------------------
# reshape the data to have a wide format

library(tidyr)

summary_long <- summary_tab %>%
  pivot_longer(cols = Climate:score, names_to = "Function", values_to = "Value")

final_table <- summary_long %>%
  unite(col = "Climate_mgm", rcp, mgm, sep = "_") %>%
  pivot_wider(names_from = Climate_mgm, values_from = Value)

# Rename and ordering

final_table <- final_table %>%
  rename(Function = 1) %>%   # avoid empty column name ("" breaks flextable)
  mutate(Function = factor(Function, 
                           levels = c("Climate", "Production", "Water", "Biodiversity", "score"))) %>%
  arrange(Function)

# reorder columns → refclim first, then rcp45, then rcp85
final_table <- final_table %>%
  dplyr::select(
    Function,
    starts_with("refclim"),
    starts_with("rcp45"),
    starts_with("rcp85")
  )

# ensure clean data.frame + valid names
final_table <- as.data.frame(final_table)
colnames(final_table) <- make.names(colnames(final_table), unique = TRUE)









#-------------------------------------------------------------------------------
# Export
library(flextable)
library(officer)


ft <- flextable(final_table) # tibble structure sometimes breaking

ft <- add_header_row(
  ft,
  values = c(
    "", 
    "Reference climate", 
    "RCP4.5", 
    "RCP8.5"
  ),
  colwidths = c(
    1,
    sum(grepl("refclim", colnames(final_table))),
    sum(grepl("rcp45", colnames(final_table))),
    sum(grepl("rcp85", colnames(final_table)))
  )
)


# Rotate management labels vertically
ft <- rotate(
  ft,
  part = "header",
  j = 2:ncol(final_table),   # exclude first column (Function)
  rotation = "btlr"          # top-to-bottom, right-to-left
)

# Create the Word doc table

# First way good when portrait table
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/multifunctionality_table.docx")

# Second way good when landscape table

doc <- read_docx() %>%
  body_end_section_landscape() %>%   # switch page to horizontal
  body_add_flextable(ft)

print(doc, target = "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/multifunctionality_table.docx")











#-------------------------------------------------------------------------------
# RECOVERY METRICS ANALYSIS ================================================

# Recovery time from K-M analysis (single values)
recovery_km <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/1b_Median_recovery_timings.csv", row.names = 1)

recovery_km <- recovery_km %>%
  rownames_to_column("group") %>%
  mutate(
    group = gsub("^group=", "", group),
    rcp = sub("\\..*", "", group),
    mgm = sub(".*\\.", "", group)
  ) %>%
  select(rcp, mgm, median)

# Impact, AUC data - keep all individual values for min-max
impact_auc <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260421_impact_recoverytime_auc.csv", row.names = 1)

impact_auc <- impact_auc %>%
  mutate(rcp = ifelse(rcp == "-", "refclim", rcp)) %>%
  select(mgm, rcp, impact, one.minus.norm.auc)

# ===== SUMMARY TABLE (mean(min-max)) =====

# Get mean(min-max) for Impact and AUC
impact_auc_summary <- impact_auc %>%
  group_by(rcp, mgm) %>%
  summarise(
    wind_impact = sprintf("%.2f (%.2f–%.2f)",
                          mean(impact, na.rm = TRUE),
                          min(impact, na.rm = TRUE),
                          max(impact, na.rm = TRUE)),
    auc = sprintf("%.3f (%.3f–%.3f)",
                  mean(one.minus.norm.auc, na.rm = TRUE),
                  min(one.minus.norm.auc, na.rm = TRUE),
                  max(one.minus.norm.auc, na.rm = TRUE)),
    .groups = "drop"
  )

# Merge with recovery time
summary_table <- impact_auc_summary %>%
  left_join(recovery_km, by = c("rcp", "mgm")) %>%
  mutate(
    rt = sprintf("%.2f", median)
  ) %>%
  select(rcp, mgm, wind_impact, auc, rt) %>%
  rename(
    `Wind Impact [%]` = wind_impact,
    `1 - (norm AUC)` = auc,
    `Recovery Time [year]` = rt
  )

# ===== DIFFERENTIAL TABLE (% change from refclim) =====

# Impact, AUC data - keep all individual values for min-max
impact_auc <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260421_impact_recoverytime_auc.csv", row.names = 1)

impact_auc <- impact_auc %>%
  mutate(rcp = ifelse(rcp == "-", "refclim", rcp))

# Calculate individual differentials first
diff_individual <- impact_auc %>%
  group_by(mgm,windcase) %>%
  mutate(
    wind_ref = impact[rcp == "refclim"],
    auc_ref = one.minus.norm.auc[rcp == "refclim"]
  ) %>%
  ungroup() %>%
  filter(rcp != "refclim") %>%
  mutate(
    wind_diff = 100 * (impact - wind_ref) / wind_ref,
    auc_diff = 100 * (one.minus.norm.auc - auc_ref) / auc_ref
  ) %>%
  select(rcp, mgm, wind_diff, auc_diff)

# Get recovery time differential
recovery_diff <- recovery_km %>%
  group_by(mgm) %>%
  mutate(
    rt_ref = median[rcp == "refclim"]
  ) %>%
  ungroup() %>%
  filter(rcp != "refclim") %>%
  mutate(
    rt_diff = 100 * (median - rt_ref) / rt_ref
  ) %>%
  select(rcp, mgm, rt_diff)

# Aggregate to mean(min-max)
diff_data <- diff_individual %>%
  group_by(rcp, mgm) %>%
  summarise(
    wind_mean = mean(wind_diff, na.rm = TRUE),
    wind_min = min(wind_diff, na.rm = TRUE),
    wind_max = max(wind_diff, na.rm = TRUE),
    auc_mean = mean(auc_diff, na.rm = TRUE),
    auc_min = min(auc_diff, na.rm = TRUE),
    auc_max = max(auc_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(recovery_diff, by = c("rcp", "mgm")) %>%
  mutate(
    wind_impact = sprintf("%.2f (%.2f–%.2f)",
                          wind_mean, wind_min, wind_max),
    auc = sprintf("%.2f (%.2f–%.2f)",
                  auc_mean, auc_min, auc_max),
    rt = sprintf("%.2f", rt_diff)
  ) %>%
  select(rcp, mgm, wind_impact, auc, rt) %>%
  rename(
    `Wind Impact [%]` = wind_impact,
    `1 - (norm AUC)` = auc,
    `Recovery Time [year]` = rt
  )

# SAVE TO EXCEL
# mean(min-max) - rt comes as unique value from the K-M function results - time to recover median cases
write.xlsx(
  list(
    Summary = summary_table,
    Differential = diff_data
  ),
  file = "20260627_Recovery_Complete_Table.xlsx",
  overwrite = TRUE
)

# Summary of differences RT - Imp - AUC
write.xlsx(
  list(
    Summary = diff_data,
    Differential = diff_data
  ),
  file = "20260627_Recovery_Diff_Table.xlsx",
  overwrite = TRUE
)


#-------------------------------------------------------------------------------
# ============ MULTIFUNCTIONAL RECOVERY DISTANCE ANALYSIS  =====================

# Import the xlsx - previous when annual increment was GWL
# MLF_RES_DIST <- read.csv("C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/Output_summary_tables/2d_distances_in_the_normalized_space_individual_values_20260615.csv")

MLF_RES_DIST <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/New Tables For Laura/2d_distances_in_the_normalized_space_individual_values_20260630b.csv")
MLF_RES_DIST

# Variables gathering and range
str(MLF_RES_DIST)

MLF_RES_DIST_summary <- MLF_RES_DIST %>%
  group_by(rcp, mgm) %>%
  summarise(
    dist = sprintf(
      "%.3f (%.3f-%.3f)",
      mean(dist, na.rm = TRUE),
      min(dist, na.rm = TRUE),
      max(dist, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Make summary only mean
library(openxlsx)

write.xlsx(
  MLF_RES_DIST_summary,
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260630_MLF_RES_DIST_summary.xlsx"
  ),
  sheetName = "20260630_MLF_RES_DIST_summary",
  overwrite = TRUE
)

# DIFFERENTIAL TABLE (% change from refclim) =============================
MLF_RES_DIST


# Compute the percentage distance from reference condition in the proper stratification
MLF_RES_DIST_diff <- MLF_RES_DIST %>%
  filter(!rcp %in% c("-", "refclim")) %>%
  left_join(
    MLF_RES_DIST %>% 
      filter(rcp %in% c("-", "refclim")) %>% 
      dplyr::select(mgm, windcase, dist_ref = dist),
    by = c("mgm", "windcase")
  ) %>%
  mutate(
    dist_diff = 100 * (dist - dist_ref) / dist_ref
  ) %>%
  
  # Group by your target metrics and summarize
  group_by(rcp, mgm, windcase) %>%
  summarise(
    mean_diff = mean(dist_diff, na.rm = TRUE),
    min_diff  = min(dist_diff, na.rm = TRUE),
    max_diff  = max(dist_diff, na.rm = TRUE),
    .groups = "drop"
  )


# Make the final mean min max summary to see CC effect on resilience-multifunctionality trade off
MLF_RES_DIST_diff_summary <- MLF_RES_DIST_diff %>%
  group_by(rcp, mgm) %>%
  summarise(
    dist_summary = sprintf(
      "%.2f (%.2f-%.2f)",
      mean(mean_diff),
      min(min_diff),
      max(max_diff)
    ),
    .groups = "drop"
  )



# Save both tables to same Excel file
write.xlsx(
  list(
    Summary = MLF_RES_DIST_summary,
    Differential = MLF_RES_DIST_diff
  ),
  file = file.path(
    "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/",
    "20260630_MLF_RES_DIST_Diff_summary.xlsx"
  ),
  overwrite = TRUE
)

cat("\n===== DIFFERENTIAL TABLE (% change from refclim) =====\n")
print(MLF_RES_DIST_diff)
