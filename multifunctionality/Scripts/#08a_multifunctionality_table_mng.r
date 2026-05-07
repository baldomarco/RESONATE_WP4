
# Load required libraries
library(tidyr)
library(RSQLite)
library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)
library(gridExtra)
library(fields)
#_______________________________________________________________________________
# Path to search the data
dataroot <- "I:/iLand/2024/output/20241105/ADP_hist/"
# WOOD REMOVED BY MANAGEMENT
removals <- c()
# AGGREGATED AT LANDSCAPE THE MAIN FOREST INFORMATION BY SPECIES
lnd <- c()
# DETAILED INFORMATION ON FOREST MANAGEMENT (ABE) ACTIVITIES PER RESOURCE UNIT
aUnit <- c()
# BARK BEETLE
bb <-c()
# WIND
w <- c()
# DISTURBANCE DAMAGES
damage.all<-c()
# TABLE FOR RESONATE COMMITMENT
multifunctionality_table <- c()
# SHANNON INDEX PER RU (1ha)
H <- c()
# SHANNON INDEX AT LANDSCAPE SCALE
H_avg <- c()
# GIVE THE NAMES AT THE RUNS
scenarios <- c("ADAPTATION HISTORICAL",
               "ADAPTATION RCP 4.5",
               "ADAPTATION RCP 8.5",
               "BAU HISTORICAL",
               "BAU RCP 4.5",
               "BAU RCP 8.5",
               "BIOECONOMY HISTORICAL",
               "BIOECONOMY RCP 4.5",
               "BIOECONOMY RCP 8.5",
               "CONSERVATION HISTORICAL",
               "CONSERVATION RCP 4.5",
               "CONSERVATION RCP 8.5")
# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
all_v <- list.files(dataroot, ".sqlite")
# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
for (i in (1:length(all_v)))  {
  # Name of the database
  file <-paste0(dataroot, all_v[i])
  # Control
  print(file)
  # Assign a case for every single dataframe
  case <- scenarios[i]
  # connect to the database
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)
  tables.in.the.file <- dbListTables(db1)
  print(tables.in.the.file)
  #-----------------------------------------------
  abeStand <- dbReadTable(db1,"abeStand")
  abeStandDetail <- dbReadTable(db1,"abeStandDetail")
  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
  abeUnit <- dbReadTable(db1, "abeUnit")
  carbon <- dbReadTable(db1,"carbon")
  carbonflow <- dbReadTable(db1,"carbonflow")
  landscape <- dbReadTable(db1,"landscape")
  landscape_removed <- dbReadTable(db1,"landscape_removed")
  #management <- dbReadTable(db1, "management")
  barkbeetle <- dbReadTable(db1,"barkbeetle")
  wind <- dbReadTable(db1,"wind")
  #dynamicstand <- dbReadTable(db1, "dynamicstand") # dys <- c() with it in case line 48. Add at the end of the loop the required dataset formation
  stand <- dbReadTable(db1,"stand")
  # close the database connection
  dbDisconnect(db1)
  #-----------------------------------------------------------------------------
  # CREATE THE CALCULATION FOR DAMAGES
  landscape.area<-landscape$area[1]                                             # CREATE THE VARIABLE FOR LANDSCAPE AREA
  lnd_volume = landscape %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
    summarise(tot_vol = sum(volume_m3),
              .groups = 'drop')
  head(lnd_volume)
  # WIND AND BARKBEETLE MERGING
  head(barkbeetle)
  head(wind)
  damage <- data.frame(year=barkbeetle$year,                                    # CREATE THE DATA FRAME FOR FOR DAMAGE OF BARKBEETLE
                       barkbeetle=barkbeetle$killedVolume)
  # ADD WIND IMPACT IN THE DAMAGE DATA FRAME                                    # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
  damage<-left_join(damage,wind[,c(1,8)],by=("year"))                           # NB ...wind[,c(1,8)]... Means all the row, column 1 (year),8 (killedVolume).
  damage<-left_join(damage,lnd_volume,by=("year"))                              # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  # Replace NA values with 0 in the entire dataframe
  damage <- damage %>%
    mutate_all(~replace_na(.x, 0))
  # CREATE VARIABLES AND ADD NEW COLUMNS: IMPACT (BB+WIND), IMPACT % ON TOTAL VOLUME.
  # damage$killedVolume[which(is.na(damage$killedVolume)==TRUE)] <- 0             # FOR MAKE THE na = 0 !!!! "as down but for the right example in the case of impact
  # damage$wind[which(is.na(damage$wind)==TRUE)] <-0                              # FOR MAKE THE na = 0 !!!! "
  # TOTAL IMPACT IN M3/ha
  impact <- data.frame(year=damage[,1],
                       impact= (damage[,2] + damage[,3])/landscape.area)        # "impact <- data.frame(year=damage$year,impact=damage$barkbeetle+damage$killedVolume)" killed volume is the
  # IT IS THE SAME CODE FOR CREATE THE DATAFRAME IMPACT variable
  # Add the variable
  damage <- left_join(damage,impact,by=("year"))
  # IMPACT RELATIVE (%) AT THE TOTAL VOLUME
  rel_imp <- data.frame(year=damage$year,
                        rel_imp=((damage$impact/damage$tot_vol)*100))
  damage<-left_join(damage,rel_imp,by=("year"))
  # GIVE THE NAME AT EVERY VARIABLE
  # colnames(damage)<-c("year","barkbeetle","case","wind","volume","impact_m3","relative_imp")
  # damage <- damage[,-3]
  head(damage)
  #-----------------------------------------------------------------------------
  # Make the 3 categories of removals:
  activity.names<-unique(abeStandRemoval$activity)    # here I list all different type of activites
  swcuts<- grepl("sw",activity.names)                 # I look for only which has "sw" this grepl gives TRUE/FALSE
  activity.names.sw<-activity.names[swcuts]           # collect the activity names with sw
  activity.names.notsw<-activity.names[!swcuts]       # collect the activity names withOUT sw
  finalcut <- grepl("final",activity.names)           # I look for only which has "sw" this grepl gives TRUE/FALSE
  activity.names.final<-activity.names[finalcut]           # collect the activity names with sw
  salvager <- grepl("salvage",activity.names)
  activity.names.salvager<-activity.names[salvager]
  # Here I filter only the listed activity names and calculate thinning/finalcut values for every year
  # (each line is per ha for a stand, so I scale with the area, sum up all the harvest on the landscape and then divide it with the whole area to get again per ha)
  ab.salvaged <- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.salvager)    %>%
                              group_by(year)   %>%   summarise(volume=sum(volumeSalvaged*area)/landscape.area, type="salvaged", run=case))
  ab.regcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>%
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="regcut", run=case))
  ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.final)    %>%
                              group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
  ab.thinnig<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>%
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="thinning", run=case))
  # COMPILE THE LIST OF DATASET
  removals<-rbind(removals,ab.regcuts,ab.thinnig,ab.salvaged,ab.finalcuts)
  # Compile the multifunctionality table of variables
  # Growing stock
  multifunctionality <- landscape %>%
    group_by(year) %>%  # Group by year and run separately
    summarise(
      crown_coverage = sum(LAI),
      standing_volume = sum(volume_m3),# Summing the variable of interest
      .groups = 'drop'  # To avoid grouped output if not needed
    )
  # Net annual increment
  # Calculate cumulative year increment
  GWL <- landscape %>%
    group_by(year) %>%
    summarise(Cumulated_increment = sum(gwl_m3)) %>%
    ungroup()
  # Calculate annual increment
  GWL <- GWL %>%
    mutate(Annual_increment = Cumulated_increment - lag(Cumulated_increment, default = NA))
  # Calculate the mean annual increment to create the first year input
  mean_increment <- mean(GWL$Annual_increment, na.rm = TRUE)
  # Substitute the na with the mean value
  GWL <- GWL %>%
    mutate(Annual_increment = ifelse(is.na(Annual_increment), mean_increment, Annual_increment))
  
  # TOTAL IMPACT IN M3/ha
  Disturbance_mortality <- data.frame(year=damage[,1],
                                      impact= (damage[,2] + damage[,3])/landscape.area)
  
  # Create a new row with the average impact of the following years for year 0
  new_row <- data.frame(year = 0, impact = mean(Disturbance_mortality$impact, na.rm = TRUE))
  
  # Bind the new row to the existing data frame
  Disturbance_mortality <- rbind(new_row, Disturbance_mortality)
  
  # Annual harvest
  AH = data.frame(year= abeUnit[,1],
                  Annual_harvest= abeUnit[,12])
  
  # Create a new row with the average Annual_harvest of the following years for year 0
  new_row <- data.frame(year = 0, Annual_harvest = mean(AH$Annual_harvest, na.rm = TRUE))
  
  # Bind the new row to the existing data frame
  AH <- rbind(new_row, AH)
  
  # Natural mortality
  # Filter rows with "N" in the "reason" column and mutate to create "Natural_mortality" column
  # Then group by year and calculate the sum of Natural_mortality
  NM <- landscape_removed %>%
    filter(reason == "N") %>%
    mutate(Natural_mortality_1 = volume_m3 / landscape.area) %>%
    group_by(year) %>%
    summarize(Natural_mortality = sum(Natural_mortality_1, na.rm = TRUE))
  
  # Create a new row with the average Natural_mortality of the following years for year 0
  new_row <- data.frame(year = 0, Natural_mortality = mean(NM$Natural_mortality, na.rm = TRUE))
  
  # Bind the new row to the existing data frame
  NM <- rbind(new_row, NM)
  
  #-----------------------------------------------------------------------------
  # Biodiversity
  #-----------------------------------------------------------------------------
  # To define the species to be removed
  species_to_remove <- c("piab", "pisy", "abal",
                         "lade", "psme", "pini",
                         "pice")
  
  # Use subset to filter the dataframe
  filtered_broadl <- subset(landscape, !species %in% species_to_remove)
  
  # Sum BA and volume for every broadleaf species in every year
  broadleaf_summary <- filtered_broadl %>%
    group_by(year) %>%
    summarize(broadleaf_ba = sum(basal_area_m2), broadleaf_volume = sum(volume_m3))
  total_summary <- landscape %>%
    group_by(year) %>%
    summarize(total_ba = sum(basal_area_m2), total_volume = sum(volume_m3))
  
  # calculate the percentage on the total and select the final variables
  broadleaf_proportion <- left_join(broadleaf_summary, total_summary, by = "year") %>%
    mutate(broadleaf_ba_percent = (broadleaf_ba / total_ba) * 100,
           broadleaf_volume_percent = (broadleaf_volume / total_volume) * 100) %>%
    select(year, broadleaf_ba_percent, broadleaf_volume_percent)
  
  #-----------------------------------------------------------------------------
  # Climate regulation FES
  
  # Total Aboveground DW Carbon
  total_AG_DW_C_sim <- carbon %>%
    group_by(year) %>%
    summarise(tot_deadwood = sum(snags_c, snagsOther_c_ag, downedWood_c_ag), .groups = "drop")
  
  # Add year 0 as the average from year 1 of the whole variable time series to make the multifunctionality dataframe possible to be merged
  total_AG_DW_C_sim <- rbind(
    data.frame(year = 0, tot_deadwood = mean(total_AG_DW_C_sim$tot_deadwood[total_AG_DW_C_sim$year == 1], na.rm = TRUE)),
    total_AG_DW_C_sim
  )
  
  # Total Carbon
  Carbon_sink <- carbon %>%
    group_by(year) %>%
    summarise(totalC_kgha_iland = sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c,
                                      regeneration_c, snags_c, snagsOther_c, downedWood_c,
                                      litter_c, soil_c), .groups = "drop")
  
  # Add year 0 as the average from year 1
  Carbon_sink <- rbind(
    data.frame(year = 0, totalC_kgha_iland = mean(Carbon_sink$totalC_kgha_iland[Carbon_sink$year == 1], na.rm = TRUE)),
    Carbon_sink
  )
  
  # NEP
  NEP_df <- carbonflow %>%
    group_by(year) %>%
    summarise(NEP = mean(NEP, na.rm = TRUE), .groups = "drop")
  
  # Add year 0 as the average from year 1
  NEP_df <- rbind(
    data.frame(year = 0, NEP = mean(NEP_df$NEP[NEP_df$year == 1], na.rm = TRUE)),
    NEP_df
  )
  
  
  # Create the final data frame
  variables <- data.frame(year=multifunctionality$year,
                          Annual_increment=GWL$Annual_increment,
                          Annual_harvest=AH$Annual_harvest,
                          Deadwood=total_AG_DW_C_sim$tot_deadwood,
                          Deciduous_tree_vol=broadleaf_proportion$broadleaf_volume_percent,
                          Deciduous_tree_ba=broadleaf_proportion$broadleaf_ba_percent,
                          LAI=multifunctionality$crown_coverage,
                          Standing_volume=multifunctionality$standing_volume,
                          NEP=NEP_df$NEP,
                          Carbon_sink=Carbon_sink$totalC_kgha_iland,
                          Disturbance_impact=Disturbance_mortality$impact,
                          Natural_mortality=NM$Natural_mortality)
  landscape.area<-landscape$area[1]                                             # CREATE THE VARIABLE FOR LANDSCAPE AREA
  #-----------------------------------------------------------------------------
  # CREATE SHANNON VARIABLE FROM DYNAMIC STAND
  # Within your loop
  stand <- stand %>%
    select(year,  ru, species, volume_m3, basal_area_m2)  # Replace col1, col2, col3 with the names of the columns you want
  # Calculate the Shannon diversity index for both BA and VOL
  shannon_index <- stand %>%
    group_by(year, ru) %>%
    filter(basal_area_m2 > 0 | volume_m3 > 0) %>%
    summarize(shannon_BA = diversity(basal_area_m2, base = exp(1)),
              shannon_VOL = diversity(volume_m3, base = exp(1)))
  # Shannon avg
  shannon_index_avg <- shannon_index %>%
    group_by(year) %>%
    summarize(shannon_BA_avg = mean(shannon_BA),
              shannon_VOL_avg = mean(shannon_VOL))
  #-----------------------------------------------------------------------------
  # CREATE SHANNON VARIABLE FROM STAND
  # Within your loop
  #dynamicstand <- dynamicstand %>%
  # select(year,  rid, species, X_if_dbh_20_volume_0_sum, if_dbh_20_and_dbh_40_volume_0_sum, if_dbh_40_and_dbh_60_volume_0_sum, if_dbh_60_1_0_sum, basalarea_sum)  # Replace col1, col2, col3 with the names of the columns you want
  # create a variable of total volume
  #dynamicstand <- dynamicstand %>%
  # mutate(total_vol_sum = rowSums(select(., X_if_dbh_20_volume_0_sum:if_dbh_60_1_0_sum)))
  # Calculate the Shannon diversity index for both BA and VOL
  #shannon_index <- dynamicstand %>%
  # group_by(year, rid) %>%
  #filter(basalarea_sum > 0 | total_vol_sum > 0) %>%
  #summarize(shannon_BA = diversity(basalarea_sum, base = exp(1)),
  #         shannon_VOL = diversity(total_vol_sum, base = exp(1)))
  # Shannon avg
  #shannon_index_avg <- shannon_index %>%
  # group_by(year) %>%
  #summarize(shannon_BA_avg = mean(shannon_BA),
  #         shannon_VOL_avg = mean(shannon_VOL))
  #-----------------------------------------------------------------------------
  # Collect landscape data:
  landscape <- landscape %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  lnd <- rbind(lnd, landscape)
  # Collect abeUnit data
  abeUnit <- abeUnit %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  aUnit <- rbind(aUnit, abeUnit)
  # Collect barkbeetle data FOR CREATE THE VARIABLE BB FOR ALL THE RUNS
  barkbeetle <- barkbeetle %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  bb <- rbind(bb, barkbeetle)
  # Collect wind data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  wind <- wind %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  w <- rbind(w, wind)
  # CREATE THE VARIABLE DAMAGE FOR ALL THE RUNS
  damage <- damage %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  damage.all <- rbind(damage.all, damage)
  # CREATE THE VARIABLES FOR RESONATE WP4
  variables <- variables %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  multifunctionality_table <- rbind(multifunctionality_table, variables)
  # CREATE Shannon index FOR RESONATE WP4
  shannon_index <- shannon_index %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  H <- rbind(H, shannon_index)
  # Create Shannon index averages at landscape scale
  shannon_index_avg <- shannon_index_avg %>%
    filter(year <= 80) %>%  # Keep only the first 80 years
    mutate(year = year + 2020,  # Replace years: from 0 to 79 -> 2021 to 2100
           run = case)
  H_avg <- rbind(H_avg, shannon_index_avg)
}  # end of loop



write.csv(multifunctionality_table, paste0(dataroot,"Multifunctionality_Table_ADP_hist.csv"), row.names = TRUE)
write.csv(H, paste0(dataroot,"Multifunctionality_Table_H_ADP_hist.csv"), row.names = TRUE)
write.csv(H_avg, paste0(dataroot,"Multifunctionality_Table_H_avg_ADP_hist.csv"), row.names = TRUE)



#-------------------------------------------------------------------------------
# Wood and productivity
Mean_timber_stock <- mean(multifunctionality_table$Standing_volume)

Mean_annual_harvest <- mean(multifunctionality_table$Annual_harvest)

Cumulative_timber_harvest_2021_60 <- multifunctionality_table %>%
  filter(year >= 2021 & year <= 2060) %>% 
  summarise(cumulative_harvest = sum(Annual_harvest, na.rm = TRUE))

Mean_annual_increment <- mean(multifunctionality_table$Annual_increment)

# Climate Regulation
Mean_carbon_stock <- mean(multifunctionality_table$Carbon_sink)

Cumulative_NEP_2021_60 <- multifunctionality_table %>%
  filter(year >= 2021 & year <= 2060) %>% 
  summarise(cumulative_NEP = sum(NEP, na.rm = TRUE))

# Biodiversity
Mean_Shannon_index <- mean(H_avg$shannon_BA_avg)

Mean_Shannon_index <- mean(H_avg$shannon_VOL_avg)

Mean_deadwood_pool <- mean(multifunctionality_table$Deadwood)      
 
Mean_deciduous_tree_vol <- mean(multifunctionality_table$Deciduous_tree_vol)

Mean_deciduous_tree_ba <- mean(multifunctionality_table$Deciduous_tree_ba)

# Water protection
Mean_LAI <- mean(multifunctionality_table$LAI) 
 
Mean_standing_volume <- mean(multifunctionality_table$Standing_volume)

# Forest risks
Mean_disturbance_impact <- mean(multifunctionality_table$Disturbance_impact)

#-------------------------------------------------------------------------------
# CREATE THE TABLE FINAL TABLE

# Create a summary table
summary_table <- data.frame(
  Mean_timber_stock = mean(multifunctionality_table$Standing_volume, na.rm = TRUE),
  Mean_annual_harvest = mean(multifunctionality_table$Annual_harvest, na.rm = TRUE),
  Cumulative_timber_harvest_2021_60 = multifunctionality_table %>%
    filter(year >= 2021 & year <= 2060) %>% 
    summarise(cumulative_harvest = sum(Annual_harvest, na.rm = TRUE)) %>% 
    pull(cumulative_harvest),
  Mean_annual_increment = mean(multifunctionality_table$Annual_increment, na.rm = TRUE),
  Mean_carbon_stock = mean(multifunctionality_table$Carbon_sink, na.rm = TRUE),
  Cumulative_NEP_2021_60 = multifunctionality_table %>%
    filter(year >= 2021 & year <= 2060) %>% 
    summarise(cumulative_NEP = sum(NEP, na.rm = TRUE)) %>% 
    pull(cumulative_NEP),
  Mean_Shannon_index_BA = mean(H_avg$shannon_BA_avg, na.rm = TRUE),
  Mean_Shannon_index_VOL = mean(H_avg$shannon_VOL_avg, na.rm = TRUE),
  Mean_deadwood_pool = mean(multifunctionality_table$Deadwood, na.rm = TRUE),
  Mean_deciduous_tree_vol = mean(multifunctionality_table$Deciduous_tree_vol, na.rm = TRUE),
  Mean_deciduous_tree_ba = mean(multifunctionality_table$Deciduous_tree_ba, na.rm = TRUE),
  Mean_LAI = mean(multifunctionality_table$LAI, na.rm = TRUE),
  Mean_standing_volume = mean(multifunctionality_table$Standing_volume, na.rm = TRUE),
  Mean_disturbance_impact = mean(multifunctionality_table$Disturbance_impact, na.rm = TRUE),
  Scenario = multifunctionality_table %>%
    filter(year == 2021) %>% 
    select(run)
)

write.csv(summary_table, paste0(dataroot, "Multifunctionality_Summary_Table_ADP_hist.csv"), row.names = TRUE)

# Transpose the summary table
transposed_summary_table <- as.data.frame(t(summary_table))

# Write the transposed summary table to a CSV file
write.csv(transposed_summary_table, paste0(dataroot, "Multifunctionality_Summary_Table_ADP_hist_transposed.csv"), row.names = TRUE)
