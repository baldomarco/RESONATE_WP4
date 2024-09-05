# Necessary libraries and packages (make sure they are installed)
library(mgcv)
library(tidyr)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(writexl)
library(ggradar)
library(openxlsx)

# Path to search the data folder
dataroot <- "I:/iLand/2024/output/20240814/Final_Analysis/select/"

# CREATE NEW EMPTY DATAFRAME FOR COEFFICIENTS
coefficients_df <- data.frame(
  run = character(),
  sd_residuals_impact = numeric(),
  mad_residuals_impact = numeric(),
  iqr_residuals_impact = numeric(),
  EDF_impact = numeric(),
  REML_impact = numeric(),
  R_squared_adj_impact = numeric(),
  Deviance_explained_impact = numeric(),
  Scale_est_impact = numeric(),
  N_impact = numeric(),
  sd_residuals_cum_impact = numeric(),
  mad_residuals_cum_impact = numeric(),
  iqr_residuals_cum_impact = numeric(),
  EDF_cum_impact = numeric(),
  REML_cum_impact = numeric(),
  R_squared_adj_cum_impact = numeric(),
  Deviance_explained_cum_impact = numeric(),
  Scale_est_cum_impact = numeric(),
  N_cum_impact = numeric(),
  management = character(),
  climate = character(),
  stringsAsFactors = FALSE
)

# Empty dataframe to store Rel_Imp_mean and Cumulative_Impact
impact_summary_df <- data.frame(
  run = character(),
  Rel_Imp_mean = numeric(),
  Cumulative_Impact = numeric(),
  stringsAsFactors = FALSE
)

#-------------------------------------------------------------------------------
# Open a PDF device to save all plots
pdf(paste0(dataroot, "20240822_GAM_Spline_Residual_Plots.pdf"))

# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
all_v <- list.files(dataroot, ".sqlite")

# Loop through each scenario and calculate stability coefficients
for (i in seq_along(all_v)) {
  
  # Name of the database
  file <- paste0(dataroot, all_v[i])
  
  # Control
  print(file)
  
  # Assign a case for every single dataframe
  case <- gsub(".sqlite", "", all_v[i])
  
  # Extract management and climate information
  management <- gsub("_.*", "", case)
  climate <- case_when(
    grepl("Hist Clim", case) ~ "Hist Clim",
    grepl("RCP45", case) ~ "RCP45",
    grepl("RCP85", case) ~ "RCP85",
    TRUE ~ "Unknown"
  )
  
  
  # Connect to the database
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)
  
  landscape <- dbReadTable(db1, "landscape")
  barkbeetle <- dbReadTable(db1,"barkbeetle")
  wind <- dbReadTable(db1,"wind")
  dbDisconnect(db1)  # close the database
  
  #----------------------------------------------------------------------------
  # FILTER FOR THE FIRST 81 YEARS 2020-2100
  
  landscape <- landscape %>%
    filter(year <= 80) %>%
    mutate(year = year + 2020, run = case)
  
  barkbeetle <- barkbeetle %>%
    filter(year <= 80) %>%
    mutate(year = year + 2020, run = case)
  
  wind <- wind %>%
    filter(year <= 80) %>%
    mutate(year = year + 2020, run = case)
  
  #-----------------------------------------------------------------------------
  # CREATE THE CALCULATION FOR DAMAGES
  
  landscape.area <- landscape$area[1]
  
  lnd_volume <- landscape %>% group_by(year) %>%
    summarise(tot_vol = sum(volume_m3), .groups = 'drop')
  
  # WIND AND BARKBEETLE MERGING
  
  damage <- data.frame(
    year = barkbeetle$year,
    barkbeetle = barkbeetle$killedVolume
  )
  
  damage <- left_join(damage, wind[,c(1,8)], by = "year")
  damage <- left_join(damage, lnd_volume, by = "year")
  
  # Replace NA values with 0 in the entire dataframe
  damage <- damage %>%
    mutate_all(~replace_na(.x, 0))
  
  impact <- data.frame(
    year = damage[,1],
    impact = (damage[,2] + damage[,3]) / landscape.area
  )
  
  damage <- left_join(damage, impact, by = "year")
  
  rel_imp <- data.frame(
    year = damage$year,
    rel_imp = ((damage$impact / damage$tot_vol) * 100)
  )
  
  damage <- left_join(damage, rel_imp, by = "year")
  
  #------------------------------------------------------------
  # CUMULATIVE DAMAGE
  
  damage <- damage %>%
    arrange(year) %>%
    mutate(cumulative_impact = cumsum(impact)) %>%
    ungroup()
  
  # RELATIVE IMPACT OF DISTURBANCE MEAN
  Rel_Imp_mean <- mean(damage$rel_imp)
  
  # Extract last year's cumulative impact
  Cumulative_Impact <- tail(damage$cumulative_impact, 1)
  
  # Store Rel_Imp_mean and Cumulative_Impact
  impact_summary_df <- rbind(impact_summary_df, data.frame(
    run = case,
    Rel_Imp_mean = Rel_Imp_mean,
    Cumulative_Impact = Cumulative_Impact
  ))
  
  #-----------------------------------------------------------------------------
  # DAMAGE IMPACT ABSOLUTE GAM MODEL DETRENDING 
  
  # Fit a GAM model to the landscape volume data for impact
  gam_fit_impact <- gam(impact ~ s(year), data = damage, method = "REML")
  gam_summary_impact <- summary(gam_fit_impact)
  
  # Extract coefficients for impact
  coefficients_df <- rbind(coefficients_df, data.frame(
    run = case,
    sd_residuals_impact = sd(residuals(gam_fit_impact)),
    mad_residuals_impact = mean(abs(residuals(gam_fit_impact))),
    iqr_residuals_impact = IQR(residuals(gam_fit_impact)),
    EDF_impact = gam_summary_impact$s.table[1, "edf"],
    REML_impact = gam_summary_impact$sp.criterion,
    R_squared_adj_impact = gam_summary_impact$r.sq,
    Deviance_explained_impact = gam_summary_impact$dev.expl,
    Scale_est_impact = gam_summary_impact$scale,
    N_impact = gam_summary_impact$n,
    sd_residuals_cum_impact = NA,
    mad_residuals_cum_impact = NA,
    iqr_residuals_cum_impact = NA,
    EDF_cum_impact = NA,
    REML_cum_impact = NA,
    R_squared_adj_cum_impact = NA,
    Deviance_explained_cum_impact = NA,
    Scale_est_cum_impact = NA,
    N_cum_impact = NA,
    management = management,
    climate = climate
  ))
  
  # Plot the original data and the fitted spline for impact
  p1 <- ggplot(damage, aes(x = year, y = impact)) +
    geom_point() +
    geom_line(aes(y = predict(gam_fit_impact)), col = "blue", lwd = 1) +
    ggtitle(paste("GAM Spline Fit for Disturbance Impact Over Years (Case:", case, ")")) +
    labs(x = "Year", y = "Wood Volume Damaged") +
    theme_bw()
  
  p2 <- ggplot(damage, aes(x = year)) +
    geom_segment(aes(y = 0, yend = residuals(gam_fit_impact), xend = year), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste("Residuals of GAM Spline Fit (Case:", case, ")")) +
    labs(x = "Year", y = "Residuals") +
    theme_bw()
  
  # Save the plots in the PDF
  grid.arrange(p1, p2, nrow = 2)
  
  #-----------------------------------------------------------------------------
  # DAMAGE IMPACT CUMULATIVE GAM MODEL DETRENDING 
  
  # Fit a GAM model to the cumulative impact data
  gam_fit_cum_impact <- gam(cumulative_impact ~ s(year), data = damage, method = "REML")
  gam_summary_cum_impact <- summary(gam_fit_cum_impact)
  
  # Extract coefficients for cumulative impact
  coefficients_df[NROW(coefficients_df), c(
    "sd_residuals_cum_impact", "mad_residuals_cum_impact", "iqr_residuals_cum_impact", 
    "EDF_cum_impact", "REML_cum_impact", "R_squared_adj_cum_impact", 
    "Deviance_explained_cum_impact", "Scale_est_cum_impact", "N_cum_impact"
  )] <- list(
    sd(residuals(gam_fit_cum_impact)),
    mean(abs(residuals(gam_fit_cum_impact))),
    IQR(residuals(gam_fit_cum_impact)),
    gam_summary_cum_impact$s.table[1, "edf"],
    gam_summary_cum_impact$sp.criterion,
    gam_summary_cum_impact$r.sq,
    gam_summary_cum_impact$dev.expl,
    gam_summary_cum_impact$scale,
    gam_summary_cum_impact$n
  )
  
  # Plot the original data and the fitted spline for cumulative impact
  p3 <- ggplot(damage, aes(x = year, y = cumulative_impact)) +
    geom_point() +
    geom_line(aes(y = predict(gam_fit_cum_impact)), col = "blue", lwd = 1) +
    ggtitle(paste("GAM Spline Fit for Cumulative Disturbance Impact Over Years (Case:", case, ")")) +
    labs(x = "Year", y = "Cumulative Impact") +
    theme_bw()
  
  p4 <- ggplot(damage, aes(x = year)) +
    geom_segment(aes(y = 0, yend = residuals(gam_fit_cum_impact), xend = year), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste("Residuals of GAM Spline Fit (Case:", case, ")")) +
    labs(x = "Year", y = "Residuals") +
    theme_bw()
  
  # Save the plots in the PDF
  grid.arrange(p3, p4, nrow = 2)
}

# Close the PDF device
dev.off()

#-------------------------------------------------------------------------------
# SAVE RESULTS TO EXCEL FILES

# Coefficients data
write.xlsx(coefficients_df, paste0(dataroot, "20240822_GAM_Coefficients_Analysis.xlsx"), rowNames = FALSE)

# Impact summary data
write.xlsx(impact_summary_df, paste0(dataroot, "20240822_Impact_Summary.xlsx"), rowNames = FALSE)


#-------------------------------------------------------------------------------
# CREATE PLOTS TO VISUALIZE THE STABILITY COEFFICIENTS FOR EACH CASE
#-------------------------------------------------------------------------------
pdf(paste0(dataroot, "20240821_Bar_chart_Residual_coeff.pdf"),height = 8, width = 12)

ggplot(coefficients_df, aes(x = run)) +
  geom_bar(aes(y = sd_residuals), stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = mad_residuals), stat = "identity", fill = "lightgreen", alpha = 0.7) +
  geom_bar(aes(y = iqr_residuals), stat = "identity", fill = "orange", alpha = 0.5) +
  labs(x = "Run", y = "Stability Coefficients", title = "Stability Coefficients Across Runs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(coefficients_df, aes(x = run)) +
  geom_bar(aes(y = sd_residuals), stat = "identity", fill = "lightblue") +
  labs(x = "Run", y = "Stability Coefficients", title = "Stability Coefficients Across Runs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(coefficients_df, aes(x = run)) +
  geom_bar(aes(y = mad_residuals), stat = "identity", fill = "lightgreen", alpha = 0.7)+
  labs(x = "Run", y = "Stability Coefficients", title = "Stability Coefficients Across Runs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(coefficients_df, aes(x = run)) +
  geom_bar(aes(y = iqr_residuals), stat = "identity", fill = "orange", alpha = 0.5) +
  labs(x = "Run", y = "Stability Coefficients", title = "Stability Coefficients Across Runs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()
#-------------------------------------------------------------------------------
# CREATE RADAR PLOTS TO VISUALIZE MANAGEMENT STRATEGIES ACROSS CLIMATE SCENARIOS
#-------------------------------------------------------------------------------

library(ggradar)

# Prepare data for radar plots (Relative Values)
prepare_radar_data_relative <- function(df) {
  if (nrow(df) == 0) {
    stop("Data frame is empty; no data to plot.")
  }
  
  # Normalize values to a range [0, 1]
  df <- df %>%
    mutate(
      sd_residuals = as.numeric(sd_residuals),
      mad_residuals = as.numeric(mad_residuals),
      iqr_residuals = as.numeric(iqr_residuals)
    ) %>%
    select(management, sd_residuals, mad_residuals, iqr_residuals) %>%
    gather(key = "Metric", value = "Value", -management) %>%
    group_by(Metric) %>%
    mutate(Value = (Value - min(Value)) / (max(Value) - min(Value))) %>%
    ungroup()
  
  return(df)
}

# Prepare data for radar plots (Absolute Values)
prepare_radar_data_absolute <- function(df) {
  if (nrow(df) == 0) {
    stop("Data frame is empty; no data to plot.")
  }
  
  df <- df %>%
    mutate(
      sd_residuals = as.numeric(sd_residuals),
      mad_residuals = as.numeric(mad_residuals),
      iqr_residuals = as.numeric(iqr_residuals)
    ) %>%
    select(management, sd_residuals, mad_residuals, iqr_residuals) %>%
    gather(key = "Metric", value = "Value", -management)
  
  return(df)
}

# Define a radar plot function
create_radar_plot <- function(data, title, legend = TRUE) {
  if (nrow(data) == 0) {
    stop("Data frame is empty; no data to plot.")
  }
  
  radar_plot <- data %>%
    spread(key = Metric, value = Value) %>%
    ggradar(grid.min = 0) +
    labs(title = title) +
    theme(
      legend.position = if (legend) "bottom" else "none",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, hjust = 0.5)
    )
  
  return(radar_plot)
}

# Separate the data for different climate scenarios
df_hist <- coefficients_df %>% filter(climate == "Hist Clim")
df_rcp45 <- coefficients_df %>% filter(climate == "RCP45")
df_rcp85 <- coefficients_df %>% filter(climate == "RCP85")

# Prepare data for radar plots
df_hist_long_rel <- prepare_radar_data_relative(df_hist)
df_rcp45_long_rel <- prepare_radar_data_relative(df_rcp45)
df_rcp85_long_rel <- prepare_radar_data_relative(df_rcp85)

df_hist_long_abs <- prepare_radar_data_absolute(df_hist)
df_rcp45_long_abs <- prepare_radar_data_absolute(df_rcp45)
df_rcp85_long_abs <- prepare_radar_data_absolute(df_rcp85)

# Create radar plots for each climate scenario (Relative Values)
radar_hist_rel <- create_radar_plot(df_hist_long_rel, "Historical Climate")
radar_rcp45_rel <- create_radar_plot(df_rcp45_long_rel, "RCP45 Climate Scenario", legend = TRUE)
radar_rcp85_rel <- create_radar_plot(df_rcp85_long_rel, "RCP85 Climate Scenario", legend = TRUE)

# Create radar plots for each climate scenario (Absolute Values)
radar_hist_abs <- create_radar_plot(df_hist_long_abs, "Historical Climate")
radar_rcp45_abs <- create_radar_plot(df_rcp45_long_abs, "RCP45 Climate Scenario", legend = TRUE)
radar_rcp85_abs <- create_radar_plot(df_rcp85_long_abs, "RCP85 Climate Scenario", legend = TRUE)

# Save radar plots in a single PDF
pdf(paste0(dataroot, "20240821_management_radar_plots.pdf"), height = 8, width = 14)

# Create radar plots for each climate scenario (Relative Values)
radar_hist_rel 
radar_rcp45_rel 
radar_rcp85_rel

# Create radar plots for each climate scenario (Absolute Values)
radar_hist_abs 
radar_rcp45_abs
radar_rcp85_abs


# Close the PDF device
dev.off()
