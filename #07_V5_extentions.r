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
pdf(paste0(dataroot, "20240822_2_Combined_GAM_Spline_Residual_Plots.pdf"))

# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
all_v <- list.files(dataroot, ".sqlite")

# Initialize lists to store plots by climate scenario
plots_by_climate <- list(
  "Hist Clim" = list(),
  "RCP45" = list(),
  "RCP85" = list()
)

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
  
  # Collect landscape data
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
    labs(x = "Year", y = "Volume") +
    theme_bw()
  
  # Plot the residuals
  p2 <- ggplot(lnd_volume, aes(x = year)) +
    geom_segment(aes(y = 0, yend = residuals, xend = year), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste("Residuals of GAM Spline Fit (Case:", case, ")")) +
    labs(x = "Year", y = "Residuals") +
    theme_bw()
  
  # Extract species information for plotting
  species.we.have <- unique(landscape$species)
  cols <- cols.all[names(cols.all) %in% species.we.have]
  new_order_gg <- new_order_gg.all[new_order_gg.all %in% species.we.have]
  
  # Plot volume by species
  p3 <- ggplot(landscape, aes(year, volume_m3, fill = factor(species, levels = new_order_gg))) +
    geom_area() +
    scale_fill_manual(values = cols[new_order_gg], guide = "none") +
    ggtitle("Volume by Species") +
    labs(x = "Year", y = "Volume m3/ha", fill = "Species") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  # Add plots to the list for the respective climate scenario
  if (climate %in% names(plots_by_climate)) {
    plots_by_climate[[climate]] <- c(plots_by_climate[[climate]], list(p1, p2, p3))
  }
}

# Plot the stored plots by climate scenario
for (climate in names(plots_by_climate)) {
  plots <- plots_by_climate[[climate]]
  if (length(plots) > 0) {
    # Arrange the plots for the current climate scenario on one page
    grid.arrange(grobs = plots, ncol = 3, top = climate)
  }
}

# Close the PDF device
dev.off()


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
pdf(paste0(dataroot, "20240822_3_Combined_GAM_Spline_Residual_Plots.pdf"))

# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
all_v <- list.files(dataroot, ".sqlite")

# Initialize lists to store plots by management type
plots_by_management <- list(
  "Clearcut" = list(),
  "Selective" = list(),
  "Shelterwood" = list(),
  "Agroforestry" = list(),
  "Other" = list()
)

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
  
  # Collect landscape data
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
    labs(x = "Year", y = "Volume") +
    theme_bw()
  
  # Plot the residuals
  p2 <- ggplot(lnd_volume, aes(x = year)) +
    geom_segment(aes(y = 0, yend = residuals, xend = year), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste("Residuals of GAM Spline Fit (Case:", case, ")")) +
    labs(x = "Year", y = "Residuals") +
    theme_bw()
  
  # Extract species information for plotting
  species.we.have <- unique(landscape$species)
  cols <- cols.all[names(cols.all) %in% species.we.have]
  new_order_gg <- new_order_gg.all[new_order_gg.all %in% species.we.have]
  
  # Plot volume by species
  p3 <- ggplot(landscape, aes(year, volume_m3, fill = factor(species, levels = new_order_gg))) +
    geom_area() +
    scale_fill_manual(values = cols[new_order_gg], guide = "none") +
    ggtitle("Volume by Species") +
    labs(x = "Year", y = "Volume m3/ha", fill = "Species") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  # Add plots to the list for the respective management type
  if (management %in% names(plots_by_management)) {
    plots_by_management[[management]] <- c(plots_by_management[[management]], list(p1, p2, p3))
  }
}

# Plot the stored plots by management type
for (management in names(plots_by_management)) {
  plots <- plots_by_management[[management]]
  if (length(plots) > 0) {
    # Arrange the plots for the current management type on one page
    grid.arrange(grobs = plots, ncol = 3, top = management)
  }
}

# Close the PDF device
dev.off()
