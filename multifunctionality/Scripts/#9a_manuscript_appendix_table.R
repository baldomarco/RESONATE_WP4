# Multifunctionality Score Table

library(readr)
library(dplyr)

# Working directory
# path <- ""C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/"

# Import the CSV
multifunc_tab <- read.csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/Tables/20260415_MF_ES_score.csv")
multifunc_tab

multifunc_tab <- multifunc_tab %>%
  mutate(rcp = ifelse(rcp == "-", "refclim", rcp))

# Variables gathering and range
str(multifunc_tab)

# Let's create separate tables per Ecosystem Function
climate_tab <- multifunc_tab %>%
  select(rcp, model, mgm, windcase, Climate)

production_tab <- multifunc_tab %>%
  select(rcp, model, mgm, windcase, Production)

water_tab <- multifunc_tab %>%
  select(rcp, model, mgm, windcase, Water)

biodiversity_tab <- multifunc_tab %>%
  select(rcp, model, mgm, windcase, Biodiversity)

multifunctionality_tab <- multifunc_tab %>%
  select(rcp, model, mgm, windcase, score)


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
    Climate = sprintf("%.2f (%.2f–%.2f)", mean(Climate), min(Climate), max(Climate)),
    Production = sprintf("%.2f (%.2f–%.2f)", mean(Production), min(Production), max(Production)),
    Water = sprintf("%.2f (%.2f–%.2f)", mean(Water), min(Water), max(Water)),
    Biodiversity = sprintf("%.2f (%.2f–%.2f)", mean(Biodiversity), min(Biodiversity), max(Biodiversity)),
    score = sprintf("%.2f (%.2f–%.2f)", mean(score), min(score), max(score)),
    .groups = "drop"
  )

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
  select(
    Function,
    starts_with("refclim"),
    starts_with("rcp45"),
    starts_with("rcp85")
  )

# ensure clean data.frame + valid names
final_table <- as.data.frame(final_table)
colnames(final_table) <- make.names(colnames(final_table), unique = TRUE)

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

