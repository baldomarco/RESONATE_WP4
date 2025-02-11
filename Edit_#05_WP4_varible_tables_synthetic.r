# Install required packages if not already installed
# install.packages(c("RSQLite", "dplyr", "ggplot2"))

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
# Path to search the data folder
# dataroot <- "C:/iLand/2024/output/20240808/"
dataroot <- "D:/iLand/2025/WP4_RESONATE_V2.0/iLandDist - new version 2.0 - 2025/output/ADP/"

# CREATE NEW EMPTY DATAFRAME
removals <- c()
lnd <- c()
aUnit <- c()
bb <-c()
w <- c()
damage.all<-c()
recovery.all<-c()
recovery_year.all<-c()
H <- c()
H_avg <- c()

# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
all_v <- list.files(dataroot, ".sqlite") 

# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED
for (i in (1:length(all_v)))  {   
  
  # Name of the database
  file <-paste0(dataroot, all_v[i]) 
  
  # Control
  print(file)
  
  # Assign a case for every single dataframe 
  case<-all_v[i]
  
  # connect to the database
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)
  tables.in.the.file <- dbListTables(db1)
  print(tables.in.the.file)
  
  #-----------------------------------------------
  landscape <- dbReadTable(db1,"landscape")
  abeUnit <- dbReadTable(db1, "abeUnit")
  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
  barkbeetle <- dbReadTable(db1,"barkbeetle")
  wind <- dbReadTable(db1,"wind")
  
  dbDisconnect(db1)    # close the file
  
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
  
  
  # CREATE VARIABLES AND ADD NEW COLUMNS: IMPACT (BB+WIND), IMPACT % ON TOTAL VOLUME.
  
  # damage$killedVolume[which(is.na(damage$killedVolume)==TRUE)] <- 0             # FOR MAKE THE na = 0 !!!! "as down but for the right example in the case of impact
  
  # damage$wind[which(is.na(damage$wind)==TRUE)] <-0                              # FOR MAKE THE na = 0 !!!! "
  
  # TOTAL IMPACT IN M3/ha
  impact <- data.frame(year=damage[,1],
                       impact= (damage[,2] + damage[,4])/landscape.area)        # "impact <- data.frame(year=damage$year,impact=damage$barkbeetle+damage$killedVolume)" killed volume is the 
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
  
  
  #-----------------------------------------------------------------------------
  # RECOVERY ANALYSIS - CREATE THE DATASET FOR THE RECOVERY ANALYSIS
  #-----------------------------------------------------------------------------
  
  # Calculate total carbon for the runs
  
  sum_carbon <- landscape |> dplyr::group_by(year) |>
    dplyr::summarise(
      TC = sum(total_carbon_kg)
    )
  print(sum_carbon)
  
  # Step 1: Filter for year 49 and for years 49 to 80
  tc_year_49_value <- filter(sum_carbon, year == 49)$TC
  tc_years_49_to_100 <- filter(sum_carbon, year >= 49 & year <= 100)
  
  # Ensure you have TC values summarized or otherwise prepared for each run in year 49
  
  # Step 2 and 3: Join and calculate the percentage change
  results <- tc_years_49_to_100 %>%
    mutate(
      TC_49 = tc_year_49_value,
      Percentage_Change = (((TC_49 - TC) / TC_49) * 100) * -1,
      year_of_impact = year - 49
    ) %>%
    select(year, Percentage_Change, year_of_impact) %>%
    rename(Year = year)
  
  # Step 4: View or further process the results
  print(results)
  
  #-----------------------------------------------------------------------------
  # CREATE A DATA LIST FOR THE RECOVERY YEAR IN THE SIMULATIONS
  
  # Find the impact of disturbance
  impact_of_disturbance <- sum_carbon %>%
    filter(year %in% c(49, 50)) %>%
    pivot_wider(names_from = year, values_from = TC) %>%
    mutate(
      Impact_Percentage = ((`49` - `50`) / `49`) * 100
    )
  
  print(impact_of_disturbance)
  
  impact_of_disturbance <- sum_carbon %>%
    filter(year %in% c(49, 50)) %>%
    pivot_wider(names_from = year, values_from = TC) %>%
    mutate(
      Impact_Percentage = ((`49` - `50`) / `49`) * 100
    )
  
  print(impact_of_disturbance)
  
  # Find the first year after the disturbance when TC values reach or exceed TC in year 49
  recovery_year <- min(sum_carbon$year[which(sum_carbon$year > 50 & sum_carbon$TC >= tc_year_49_value)])
  
  # Create a new data frame for the recovery year
  recovery_year_df <- data.frame(y = recovery_year)
  
  # Add a common key to `impact_of_disturbance` and `recovery_year_df` if needed
  impact_of_disturbance$key <- seq_len(nrow(impact_of_disturbance))
  recovery_year_df$key <- seq_len(nrow(recovery_year_df))
  
  # Merge the `impact_of_disturbance` data frame with the `recovery_year_df`
  recovery_year_tab <- merge(impact_of_disturbance, recovery_year_df, by = "key")
  
  # Select only the `Impact_Percentage` column from `impact_of_disturbance` and rename `y` to `recovery_year`
  recovery_year <- recovery_year_tab %>% select(Impact_Percentage, recovery_year = y)                        # changed name at y column
  
  # Print the resulting merged data frame
  print(recovery_year)
  
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
  # COMPILE THE LIST OF DATASET
  removals<-rbind(removals,ab.regcuts,ab.thinnig,ab.salvaged,ab.finalcuts)
  
  
  # Collect landscape data:
  landscape<- (landscape %>% mutate(run=case))
  lnd<-rbind(lnd, landscape)
  
  # Collect abeUnit data
  abeUnit<-(abeUnit %>% mutate(run=case))
  aUnit<-rbind(aUnit, abeUnit)
  
  # Collect barkbeetle data FOR CREATE THE VARIABLE BB FOR ALL THE RUNS
  barkbeetle <-(barkbeetle %>% mutate(run=case))
  bb <-rbind(bb, barkbeetle)
  
  # Collect wind data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  wind <-(wind %>% mutate(run=case))
  w <-rbind(w, wind)
  
  # CREATE THE VARIABLE DAMAGE FOR ALL THE RUNS
  damage <-(damage %>% mutate(run=case))
  damage.all<-rbind(damage.all, damage)  
  
  # CREATE THERECOVERY TABLE
  recovery <- (results%>% mutate(run=case))
  recovery.all <- rbind(recovery.all,recovery)
  
  # CREATE THERECOVERY TABLE
  recovery_year <- (recovery_year%>% mutate(run=case))
  recovery_year.all <- rbind(recovery_year.all,recovery_year)
  
  # Create Shannon index averages at landscape scale
  shannon_index_avg <- (shannon_index_avg %>% mutate(run=case))
  H_avg <- rbind(H_avg, shannon_index_avg)
  
}

#-------------------------------------------------------------------------------
# GENERATE THE TABLES NAMES FOR GRAPHS

generate_new_name <- function(run_name) {
  # Extract the management type (ADAPTATION, BAU, BIOECONOMY, CONSERVATION)
  management <- ifelse(grepl("^ADAPTATION", run_name), "ADAPTATION", 
                       ifelse(grepl("^BAU", run_name), "BAU", 
                              ifelse(grepl("^BIOECONOMY", run_name), "BIOECONOMY", 
                                     ifelse(grepl("^CONSERVATION", run_name), "CONSERVATION", NA))))
  
  # Extract the RCP value (4.5 or 8.5)
  rcp <- ifelse(grepl("Hist", run_name), "REF", 
                ifelse(grepl("rcp45", run_name), "RCP45", 
                       ifelse(grepl("rcp85", run_name), "RCP85", NA)))
  
  # Extract the RCM value (EC-EARTH_RACMO22E, HadGEM2, MPI_CCLM, NCC_HIRHAM5)
  rcm <- ifelse(grepl("Hist_Clim", run_name), "CLIM", 
                ifelse(grepl("EC-EARTH", run_name), "RCM1", 
                       ifelse(grepl("HadGEM2", run_name), "RCM2",
                              ifelse(grepl("MPI_CCLM", run_name), "RCM3",
                                     ifelse(grepl("NCC_HIRHAM5", run_name), "RCM4", NA)))))
  
  # Combine the parts into the new name
  new_name <- paste(management, rcp, rcm, sep = "_")
  
  return(new_name)
}

# Apply the function to the 'run' column in each data frame
removals <- removals %>%
  mutate(new_run_name = sapply(run, generate_new_name))

lnd <- lnd %>%
  mutate(new_run_name = sapply(run, generate_new_name))

aUnit <- aUnit %>%
  mutate(new_run_name = sapply(run, generate_new_name))

bb <- bb %>%
  mutate(new_run_name = sapply(run, generate_new_name))

w <- w %>%
  mutate(new_run_name = sapply(run, generate_new_name))

damage.all <- damage.all %>%
  mutate(new_run_name = sapply(run, generate_new_name))

recovery.all <- recovery.all %>%
  mutate(new_run_name = sapply(run, generate_new_name))

recovery_year.all <- recovery_year.all %>%
  mutate(new_run_name = sapply(run, generate_new_name))

H_avg <- H_avg %>%
  mutate(new_run_name = sapply(run, generate_new_name))

#_______________________________________________________________________________
#### TO SUMMARIZE THE CUTTING ACTIVITIES ######
values <- data.frame(removals %>% group_by(run, type) %>% summarise(volume = mean(volume)))
print(values)

write.csv(values, paste0(dataroot,"20240129_harvest_removals_summary.csv"), row.names = TRUE)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
pdf(paste0(dataroot, "20240822_historical_4mng.pdf"), height = 8, width = 12)

#_______________________________________________________________________________
# This tells the colors:

species.we.have<-unique(lnd$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


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

# column diagram
ggplot(removals, aes(year, volume, fill=factor(type, levels=c( "regcut","salvaged","finalcut","thinning"))))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~new_run_name, ncol=3)+
  labs(x = "Year",y="Removed volume m3/ha",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","limegreen","#FFDB5C","#FA6E59"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------

ggplot(lnd, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("volume")+
  facet_wrap(~new_run_name, ncol=3)+
  labs(x = "Year",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,400)+
  theme_bw()

#-------------------------------------------------------------------------------

ggplot(lnd, aes(year,total_carbon_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("total_carbon_kg")+
  facet_wrap(~new_run_name, ncol=3)+
  labs(x = "Year",y="total carbon kg/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,400)+
  theme_bw()

#-------------------------------------------------------------------------------

ggplot(aUnit, aes(year,realizedHarvest, color=case))+
  geom_line(size=1.2, show.legend = F)+
  facet_wrap(~new_run_name, ncol=3)+
  ylim(0,120)+
  ggtitle("Realized Harvest Transitional Period")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

#------------------------------------------------------------
# CUMULATIVE HARVEST 

# Perform cumulative sum operation within each run
harvests <- aUnit %>%
  group_by(new_run_name) %>%
  mutate(cumulative_harvest = cumsum(realizedHarvest))

#head(harvests)
summary(harvests)  # statistics
#dim(harvests)      # dimension of the data frame

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year, cumulative_harvest, color = new_run_name)) +
  geom_line(size = 1.2) +
  ggtitle("Realized Cumulative Harvest for Different Runs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Realized harvest [m3/ha]") +
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 0))
cumHarv

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS FOR KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG LINE 75

area<-landscape$area[1]
ylim.bb <- c(0, 600000)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 1700000) 

# This is needed to mantain the proportion in the two axis
b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
ggplot(damage.all, aes(year,killedVolume/area))+
  geom_col(fill="grey",col="black", size=0.5)+
  geom_line(aes(y = a+ barkbeetle/area*b), data = damage.all, size=1, col="pink") +
  scale_y_continuous(name="Wind damage [m3/ha]", sec.axis = sec_axis(~ (. - a)/b,name = "Barkbeetle [m3/ha]"))+
  xlab("Year")+
  ggtitle("ABSOLUTE DAMAGES")+
  facet_wrap(~new_run_name, ncol=2)+
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS IN RELATIVE KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG

# variant 1
ylim.bb <- c(0, 600000)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 1700000)

b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
ggplot(damage.all,aes(year,killedVolume/area/tot_vol))+
  geom_col(fill="grey",col="black")+
  geom_line(aes(y = a+ barkbeetle/area/tot_vol*b), data = damage.all, size=0.9, col="pink") +
  scale_y_continuous(name="Wind relative damage", sec.axis = sec_axis(~ (. - a)/b,name = "Barkbeetle relative damage"))+
  facet_wrap(~new_run_name, ncol=2)+
  ggtitle("RELATIVE DAMAGES")+
  theme_bw()


# SPECIES specificaly BA:
species.to.keep<-c("piab", "fasy","qupe", "psme")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("basal area") +
  facet_wrap(~new_run_name, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area m2/ha")+  theme_bw()


#-------------------------------------------------------------------------------
# RECOVERY GRAPHS

# NEEDED THIS FUNCTION TO CHARACTERIZED SCENARIOS IN THE PLOT

get_scenario <- function(run) {
  if (grepl("rcp85", run)) {
    return("RCP 8.5")
  } else if (grepl("rcp45", run)) {
    return("RCP 4.5")
  } else {
    return("Ref Clim")  # Handle unexpected cases if any
  }
}

# Add Scenario column to recovery.all based on the run column
recovery.all$Scenario <- sapply(recovery.all$run, get_scenario)

# Print the updated recovery.all data frame
print(recovery.all)


# Ribbon plots
YMIN<-min(recovery.all$Percentage_Change) # Just to check the limit of the graphs
YMIN

#
# Summarize the data
data_summary_Impact <- recovery.all %>% 
  group_by(Scenario, year_of_impact) %>% 
  summarise(min_Impact = min(Percentage_Change), 
            max_Impact = max(Percentage_Change), 
            mean_Impact = mean(Percentage_Change))

data_summary_Impact$Scenario <- factor(data_summary_Impact$Scenario, levels = c("Ref Clim", "RCP 4.5", "RCP 8.5"))

# Print the names and summary of the data_summary_Impact data frame
print(names(data_summary_Impact))
print(summary(data_summary_Impact))

# Create plot all in one
ggplot(data_summary_Impact) +
  geom_ribbon(aes(year_of_impact, ymin = min_Impact, ymax = max_Impact, fill = Scenario), alpha = 0.3) +
  geom_line(aes(year_of_impact, mean_Impact, color = Scenario), lwd = 1) +
  geom_line(aes(year_of_impact, min_Impact, color = Scenario), lwd = 0.5, linetype = "dashed") +
  geom_line(aes(year_of_impact, max_Impact, color = Scenario), lwd = 0.5, linetype = "dashed") +
  scale_fill_manual(values = c("blue", "forestgreen", "chocolate3")) +
  scale_color_manual(values = c("blue", "forestgreen", "chocolate3")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ylim(YMIN, 30) +  # Adjust YMIN to -10 (or any other value as required)
  xlim(0, 31) +
  ylab("Percentage change") +
  xlab("Year") +
  ggtitle("Impact Over Time") +  # Adjust title as needed
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = c(0.15, 0.85))

# Create plot all in one for 100 years run
ggplot(data_summary_Impact) +
  geom_ribbon(aes(year_of_impact, ymin = min_Impact, ymax = max_Impact, fill = Scenario), alpha = 0.3) +
  geom_line(aes(year_of_impact, mean_Impact, color = Scenario), lwd = 1) +
  geom_line(aes(year_of_impact, min_Impact, color = Scenario), lwd = 0.5, linetype = "dashed") +
  geom_line(aes(year_of_impact, max_Impact, color = Scenario), lwd = 0.5, linetype = "dashed") +
  scale_fill_manual(values = c("blue", "forestgreen", "chocolate3")) +
  scale_color_manual(values = c("blue", "forestgreen", "chocolate3")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ylim(YMIN, 30) +  # Adjust YMIN to -10 (or any other value as required)
  xlim(0, 50) +
  ylab("Percentage change") +
  xlab("Year") +
  ggtitle("Impact Over Time") +  # Adjust title as needed
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = c(0.15, 0.85))

#-------------------------------------------------------------------------------
# Separate per management too!

get_scenario_and_management <- function(run) {
  # Extract management type based on the prefixes in the run name
  management_type <- ifelse(grepl("^ADAPTATION", run), "ADAPTATION",
                            ifelse(grepl("^BAU", run), "BAU", 
                                   ifelse(grepl("^BIOECONOMY", run), "BIOECONOMY",
                                          ifelse(grepl("^UNMANAGED", run), "UNMANAGED",
                                          ifelse(grepl("^CONSERVATION", run), "CONSERVATION", "UNKNOWN")))))
  
  
  # Extract scenario based on the presence of "rcp45", "rcp85", or otherwise classify as "Ref Clim"
  scenario <- ifelse(grepl("rcp85", run), "RCP 8.5", 
                     ifelse(grepl("rcp45", run), "RCP 4.5", "Ref Clim"))
  
  return(c(management_type, scenario))  # Return a character vector instead of a list
}

# Step 2: Apply the function and add columns to the data frame
scenario_and_management <- t(sapply(recovery.all$run, get_scenario_and_management))
recovery.all$Management <- scenario_and_management[, 1]
recovery.all$Scenario <- scenario_and_management[, 2]

# Print the updated recovery.all data frame
print(head(recovery.all))

# Step 3: Proceed with summarization and plotting
YMIN <- min(recovery.all$Percentage_Change)

# Summarize the data by both Scenario and Management
data_summary_Impact <- recovery.all %>%
  group_by(Management, Scenario, year_of_impact) %>%
  summarise(min_Impact = min(Percentage_Change), 
            max_Impact = max(Percentage_Change), 
            mean_Impact = mean(Percentage_Change))

data_summary_Impact$Scenario <- factor(data_summary_Impact$Scenario, levels = c("Ref Clim", "RCP 4.5", "RCP 8.5"))

# Check the summary to ensure correct data structure
print(names(data_summary_Impact))
print(summary(data_summary_Impact))


# Plot with separate lines and ribbons for each management type
ggplot(data_summary_Impact, aes(x = year_of_impact, fill = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = min_Impact, ymax = max_Impact), alpha = 0.3) +
  geom_line(aes(y = mean_Impact), lwd = 1) +
  geom_line(aes(y = min_Impact), lwd = 0.5, linetype = "dashed") +
  geom_line(aes(y = max_Impact), lwd = 0.5, linetype = "dashed") +
  scale_fill_manual(values = c("blue", "forestgreen", "chocolate3")) +
  scale_color_manual(values = c("blue", "forestgreen", "chocolate3")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ylim(YMIN, 30) +  # Adjust YMIN to -10 (or any other value as required)
  xlim(0, 50) +
  ylab("Percentage change") +
  xlab("Year") +
  ggtitle("Impact Over Time by Scenario and Management") +  # Adjust title as needed
  facet_wrap(~Management) +  # Separate plots by Management type (ADAPTATION and BAU)
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = c(0.15, 0.85))

  
dev.off()

#________________________________________________________________________THE END
