# Install and load the necessary packages
# Uncomment the following lines to install packages if not already installed
# install.packages("mgcv")
# install.packages("writexl")
# install.packages("ggradar")
# install.packages("openxlsx")

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
  sd_residuals = numeric(),
  mad_residuals = numeric(),
  iqr_residuals = numeric(),
  management = character(),
  climate = character(),
  EDF = numeric(),
  REML = numeric(),
  R_squared_adj = numeric(),
  Deviance_explained = numeric(),
  Scale_est = numeric(),
  N = numeric(),
  stringsAsFactors = FALSE
)

# Open a PDF device to save all plots
pdf(paste0(dataroot, "20240821_GAM_Spline_Residual_Plots.pdf"))

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
  
  # Read in the landscape data
  landscape <- dbReadTable(db1, "landscape")
  dbDisconnect(db1)  # close the database
  
  # Collect landscape data:
  landscape <- landscape %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 80 -> 2020 to 2100
           run = case)
  
  # Calculate landscape volume
  lnd_volume <- landscape %>% group_by(year) %>% 
    summarise(tot_vol = sum(volume_m3), .groups = 'drop')
  
  # Fit a GAM model to the landscape volume data
  gam_fit <- gam(tot_vol ~ s(year), data = lnd_volume, method = "REML")
  
  # Generate the summary of the GAM model
  gam_summary <- summary(gam_fit)
  
  # Extract estimated degrees of freedom (EDF) and REML score
  edf <- gam_summary$s.table[1, "edf"]  # Extract the EDF for the smooth term
  reml <- gam_summary$sp.criterion      # Extract the REML score
  
  # Extract additional model information
  r_squared_adj <- gam_summary$r.sq
  deviance_explained <- gam_summary$dev.expl
  scale_est <- gam_summary$scale
  n <- gam_summary$n
  
  # Extract residuals from the model
  residuals <- residuals(gam_fit)
  
  # Calculate stability coefficients (SD, MAD, IQR)
  sd_residuals <- sd(residuals)
  mad_residuals <- mean(abs(residuals))
  iqr_residuals <- IQR(residuals)
  
  # Store coefficients and metadata in the dataframe
  coefficients_df <- rbind(coefficients_df, data.frame(
    run = case,
    sd_residuals = sd_residuals,
    mad_residuals = mad_residuals,
    iqr_residuals = iqr_residuals,
    management = management,
    climate = climate,
    EDF = edf,
    REML = reml,
    R_squared_adj = r_squared_adj,
    Deviance_explained = deviance_explained,
    Scale_est = scale_est,
    N = n
  ))
  
  # Plot the original data and the fitted spline
  p1 <- ggplot(lnd_volume, aes(x = year, y = tot_vol)) +
    geom_point() +
    geom_line(aes(y = predict(gam_fit)), col = "blue", lwd = 1) +
    ggtitle(paste("GAM Spline Fit for Volume Over Years (Case:", case, ")")) +
    labs(x = "Year", y = "Volume")+
    theme_bw()
  
  # Plot the residuals
  p2 <- ggplot(lnd_volume, aes(x = year)) +
    geom_segment(aes(y = 0, yend = residuals, xend = year), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste("Residuals of GAM Spline Fit (Case:", case, ")")) +
    labs(x = "Year", y = "Residuals")+
    theme_bw()
  
  #_______________________________________________________________________________
  # This tells the colors:
  
  species.we.have<-unique(landscape$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE
  
  
  # LIST OF ALL POSSIBLE SPECIES
  
  cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
              "bepe"="#fadfad", 
              "casa"="#7eeadf", "coav"="#20c6b6",  
              "tipl"="#645394", "ulgl"="#311432" ,
              "saca"="#D8BFD8",  "soar"="#DDA0DD", "soau"="#BA55D3",
              "pice"="#D27D2D", "pini"="#a81c07",
              "algl"="#2ECBE9","tico"="#128FC8",  "potr"="#00468B","poni"="#5BAEB7",
              "frex"="#fe9cb5","cabe"="#fe6181","acps"="#fe223e",
              "lade"="#FFFE71","abal"="#FFD800", "pisy"="#A4DE02",
              "fasy"="#76BA1B", "piab"="#006600",
              "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900" 
  )
  
  
  # COLORATION ORDER FOR ALL THE POSSIBLE SPECIES
  
  new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                     "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")
  
  
  # This will show at the end only the species we really have on the landscape. 
  
  cols<-cols.all[names(cols.all) %in% species.we.have]
  new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]
  #_______________________________________________________________________________
  
  
  # STARTING PLOTS
  
  # Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
  #-------------------------------------------------------------------------------
  
p3 <- ggplot(landscape, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
      geom_area() +
      scale_fill_manual(values=cols[new_order_gg], guide= "none")+
      ggtitle("volume")+
      labs(x = "Year",y="Volume m3/ha",fill = "Species")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_bw()
  
  # Save the plots in the PDF
  grid.arrange(p1, p2, p3, ncol = 1)
}

# Close the PDF device
dev.off()

# Save the coefficients_df to an Excel file
write.xlsx(coefficients_df, file = paste0(dataroot, "GAM_management_coefficients_20240821.xlsx"))


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
  