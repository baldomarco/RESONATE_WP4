# Marco Baldo 
# baldo@fld.czu.cz
# 15/08/2024

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
dataroot <- "I:/iLand/2024/output/20240814/Final_Analysis/select/"

# CREATE NEW EMPTY VECTORS FOR OUR DATASET OF INTEREST (DOT)

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
WP4_table <- c()

# SHANNON INDEX PER RU (1ha)
H <- c()

# SHANNON INDEX AT LANDSCAPE SCALE
H_avg <- c()

#dys <- c() DISACTIVATED BECAUSE GENERATE LARGE AMOUNT OF DATA BUT CAN BE USED!!


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
  landscape <- dbReadTable(db1,"landscape")
  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
  abeUnit <- dbReadTable(db1, "abeUnit")
  landscape_removed <- dbReadTable(db1,"landscape_removed")
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
  
  
  # Compile the WP4 table of variables
  
  # Growing stock
  WP4 = landscape %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
    summarise(Growing_stock = sum(volume_m3),
              .groups = 'drop')
  
  # Net annual increment
  
  # Calculate cumulative increment
  GWL <- landscape %>% 
    group_by(year) %>%
    summarise(Cumulated_increment = sum(gwl_m3)) %>%
    ungroup()
  
  # Calculate annual increment
  GWL <- GWL %>%
    mutate(Annual_increment = Cumulated_increment - lag(Cumulated_increment, default = NA))
  
  
  # TOTAL IMPACT IN M3/ha
  Disturbance_mortality <- data.frame(year=damage[,1],
                                      impact= (damage[,2] + damage[,3])/landscape.area)
  
  # Create a new row with the desired values
  new_row <- data.frame(year = 0, impact = NA)
  
  # Bind the new row to the existing data frame
  Disturbance_mortality <- rbind(new_row, Disturbance_mortality)
  
  # Annual harvest
  AH = data.frame(year= abeUnit[,1],
                  Annual_harvest= abeUnit[,12])
  # Create a new row with the desired values
  new_row <- data.frame(year = 0, Annual_harvest = NA)
  
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
  
  # Create a new row with the desired values
  new_row <- data.frame(year = 0, Natural_mortality = NA)
  
  # Bind the new row to the existing data frame
  NM <- rbind(new_row, NM)
  
  # Create the final data frame
  variables <- data.frame(year=WP4$year,
                          Growing_stock=WP4$Growing_stock,
                          Annual_increment=GWL$Annual_increment,
                          Disturbance_mortality=Disturbance_mortality$impact,
                          Natural_mortality=NM$Natural_mortality,
                          Annual_harvest=AH$Annual_harvest)
  
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
  WP4_table <- rbind(WP4_table, variables)
  
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

write.csv(WP4_table, paste0(dataroot,"RESONATE_WP4_Table.csv"), row.names = TRUE)
write.csv(H, paste0(dataroot,"RESONATE_WP4_H.csv"), row.names = TRUE)
write.csv(H_avg, paste0(dataroot,"RESONATE_WP4_H_avg.csv"), row.names = TRUE)

#-------------------------------------------------------------------------------
# PDF NAME NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE

pdf(paste0(dataroot, "Kostelec_Simulation_Results_visualization.pdf"), height=8, width=12)
# pdf(paste0(dataroot, "20220428_sw.pdf"), height=8, width=12)
# pdf(paste0(dataroot, "20220414c.pdf"), height=10, width=25)



#-------------------------------------------------------------------------------
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

# STARTING PLOTS

#-------------------------------------------------------------------------------
# COLUMN DIAGRAM PLOT ON THE HARVEST

ggplot(removals, aes(year, volume, fill=factor(type, levels=c( "regcut","finalcut","thinning","salvaged"))))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="Removed volume [m3/ha]",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59","#B3C100"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------
# PLOT LANDSCAPE VOLUME PLOT FOR CASES (GEOM AREA)

ggplot(lnd, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Landscape Volume by species")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="Volume [m3/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,450)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#------------------------------------------------------------------------------
# PLOT TOTAL CARBON IN LANDSCAPE

ggplot(lnd, aes(year,total_carbon_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Carbon in Living Biomass")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="Living Carbon [kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,450)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
#------------------------------------------------------------------------------
# Pie chart year 0

# Filter per the year and create the percentage data frame

pie_chart_sp_comp_start <- lnd %>% filter(run=="ADAPTATION HISTORICAL" & year==2021)
pie_chart_sp_comp_start_per<- pie_chart_sp_comp_start %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Present forest composition")

pie_chart_sp_comp_final <- lnd %>% filter(year==2100)
pie_chart_sp_comp_final_per<- pie_chart_sp_comp_final %>% group_by(run) %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol)


# r1wb<-rbind(pie_chart_sp_comp_start_per,
#           pie_chart_sp_comp_final_per)

# Intial species composition
x7wb <- ggplot(pie_chart_sp_comp_start_per, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=1)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in year 2020")+
  theme_bw()
x7wb + theme(plot.title = element_text(hjust = 0.5))

# ALL the scenarios together at year 80
x8wb <- ggplot(pie_chart_sp_comp_final_per, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = F) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~run, ncol=6)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in year 2100 ADP-BAU-BIOEC-CNS")+
  theme_bw()

x8wb + theme(plot.title = element_text(hjust = 0.5))

# write.csv(lnd0_spin_up1_per, paste0(dataroot,"species_per_volume_after1500y_spinup_snapshot_6190.csv"), row.names = TRUE)

# write.csv(lnd0_spin_up2_per, paste0(dataroot,"species_per_volume_after1500y_spinup_snapshot.csv"), row.names = TRUE)

#-------------------------------------------------------------------------------
# (SHOULD BE REALIZED) PLOT 2 "Y" AXIS WITH relationship between realized harvest and volume increasing in the landscape
# Total realized harvest at landscape level in average per ha

ggplot(aUnit, aes(year, realizedHarvest, color = case)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(~run, ncol = 3) +
  ylim(0, 50) +
  ggtitle("Realized Harvest") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Realized harvest [m3/ha]") +
  xlab("Year") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )



#-------------------------------------------------------------------------------
# SPECIES specificaly BA:
#species.to.keep<-c("piab","pisy", "fasy","qupe")
#lnd2 <- lnd %>% filter(species %in% species.to.keep)
#ggplot(data=lnd2, aes(x=year, y=basal_area_m2, colour=species)) + 
#  geom_line(size=1.2)+
#  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
#  ggtitle("Clearcut management in brow pressure 0") +
#  theme(plot.title = element_text(hjust = 0.5))+
#  ylab("Basal area [m2/ha]")+
#  theme_bw()

#-------------------------------------------------------------------------------
# PLOT BASAL AREA GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES SELECTED
# SPECIES specifically BA:

species.to.keep<-c("piab", "fasy","qupe", "pisy")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=0.8)+
  ggtitle("Basal area by dominat species") +
  facet_wrap(~run, ncol=3)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+  
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# PLOT TOTAL AVG BASAL AREA AT LANDSCAPE LEVEL BY SPECIES

ggplot(lnd, aes(year, basal_area_m2, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Basal Area")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="Basal Area [m2/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# PLOT DBH GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES

species.to.keep<-c("piab", "fasy","qupe", "pisy")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=dbh_avg_cm, color=species)) + 
  geom_line(size=0.8)+
  ggtitle("Avarage DBH by dominat species") +
  facet_wrap(~run, ncol=3)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("DBH [cm]")+  
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# PLOT NUMBER OF STEMS GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES

ggplot(data=lnd2, aes(x=year, y=count_ha, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol=3)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Individual stems")+  
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )


#-------------------------------------------------------------------------------
# PLOT NUMBER OF STEMS GEOM_AREA AT LANDSCAPE LEVEL BY SPECIES

ggplot(lnd, aes(x=year, y=count_ha, fill=factor(species, levels=new_order_gg)))+ 
  geom_area(size=1.2)+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="Individual Stems", fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# Total Carbon in Kg (total_carbon_kg	double	total carbon in living biomass (aboveground compartments and roots) of all living trees (including regeneration layer) (kg/ha))

ggplot(lnd, aes(year, total_carbon_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Carbon in Living Biomass")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="[kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# PLOT LAI AT LANDSCAPE LEVEL BY SPECIES

ggplot(lnd, aes(year, LAI, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("LAI index by species")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="LAI index",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )


#-------------------------------------------------------------------------------
# PLOT NPP AT LANDSCAPE LEVEL BY SPECIES

ggplot(lnd, aes(year, NPP_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Net Primary Productivity")+
  facet_wrap(~run, ncol=3)+
  labs(x = "Year",y="NPP [kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS FOR KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG LINE 75

area<-lnd$area[1]
ylim.bb <- c(0, 600000)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 1700000) 


b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
ggplot(damage.all,aes(year,killedVolume/area))+
  geom_col(fill="grey",col="black", size=0.5)+
  geom_line(aes(y = a+ barkbeetle/area*b), data = damage.all, size=1, col="pink") +
  scale_y_continuous(name="Wind damage [m3/ha]", sec.axis = sec_axis(~ (. - a)/b,name = "Barkbeetle [m3/ha]"))+
  xlab("Year")+
  facet_wrap(~run, ncol=3)+
  theme_bw()

# Here gave me the right values
absolute_damage <- (damage.all %>% group_by(run) %>% summarise(mean(barkbeetle/area),mean(na.omit(killedVolume/area))))     # SUMMARISE THE DAMAGE IMPACT IN NUMERIC VALUES AND IN MEAN FOR BOTH BB AD WIND
absolute_damage

#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS IN RELATIVE KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG

# SET THE SCALE OF THE Y AXIS. NAME IS THE NAME OF THE LABEL. FIRST Y AXIS SCALE REFERENCE ONE. SEC_AXIS SET THE SECOND AXIS TILDE ATTACH, (.,A)/B IS THE FORMULA.                                          
rel_damage <- (damage.all %>% 
                 group_by(run) %>% 
                 summarise((barkbeetle/area/tot_vol),
                           (killedVolume/area/tot_vol)))
rel_damage

relative_damage <- (damage.all %>% 
                      group_by(run) %>% 
                      summarise(mean(barkbeetle/area/tot_vol),
                                mean(na.omit(killedVolume/area/tot_vol))))                    # SUMMARISE THE DAMAGE IMPACT IN NUMERIC VALUES AND IN MEAN FOR BOTH BB AD WIND
relative_damage


# variant 1
ylim.bb <- c(0, 600000)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 1700000) 


b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
ggplot(damage.all,aes(year,killedVolume/area/tot_vol))+
  geom_col(fill="grey",col="black")+
  geom_line(aes(y = a+ barkbeetle/area/tot_vol*b), data = damage.all, size=0.9, col="pink") +
  scale_y_continuous(name="Wind relative damage on total volume", sec.axis = sec_axis(~ (. - a)/b,name = "Barkbeetle relative damage on total volume"))+
  facet_wrap(~run, ncol=3)+
  theme_bw()

#---------------------------------------------
# PLOT OF THE WP4 SCENARIOS WITH SPECIFIC PALETTE 


# FIRST WAY AND THE MOST EFFICACIOUS

library(colorspace)

my_palette <- c("#FFB6C1", "#FF1493", "#CD1076",
                "#7FFFD4" ,"#00FF7F", "aquamarine4", 
                "#FFF68F", "#FFD700", "darkgoldenrod1",
                "#63B8FF", "royalblue1", "mediumblue")


#------------------------------------------------------------
# CUMULATIVE HARVEST 

# Perform cumulative sum operation within each run
harvests <- aUnit %>%
  group_by(run) %>%
  mutate(cumulative_harvest = cumsum(realizedHarvest))

#head(harvests)
summary(harvests)  # statistics
#dim(harvests)      # dimension of the data frame


# Create the ggplot with the palette selected
cumHarv <- ggplot(harvests, aes(year, cumulative_harvest, color = run)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = my_palette) +
  ggtitle("Realized Cumulative Harvest in Different Management and RCP scenarios") +
  ylab("Realized harvest [m3/ha]") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.8),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
  )

# Customize text size and angle for axis titles
cumHarv <- cumHarv + 
  theme(plot.title = element_text(lineheight = 2, 
                                  face = "bold", 
                                  color = "black", 
                                  size = 18))

cumHarv  # Display the plot

#-----------------------------------------------
# SECOND WAY USING RColorBrewer
library(RColorBrewer)

# Define the RdYlBu palette with 11 colors
rdylbu_palette <- brewer.pal(11, "RdYlBu")

# Interpolate to generate 12 colors
custom_palette <- colorRampPalette(rdylbu_palette)(12)

# Create the ggplot with the custom palette
cumHarv <- ggplot(harvests, aes(year, cumulative_harvest, color = run)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = custom_palette) +
  ggtitle("Realized Cumulative Harvest in Different Management and RCP scenarios") +
  ylab("Realized harvest [m3/ha]") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
  )

# Customize text size and angle for axis titles
cumHarv <- cumHarv + 
  theme(plot.title = element_text(lineheight = 2, 
                                  face = "bold", 
                                  color = "black", 
                                  size = 18))

cumHarv  # Display the plot


#-------------------------------------------------------------------------------
# Plot of the WP4 tables separated plots per run - facet_wrap

cumHarv <- ggplot(harvests, aes(year, cumulative_harvest, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Realized Cumulative Harvest in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Realized harvest [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
cumHarv


#_______________________________________________________________________________
# Growing Stock

Growing_stock <- ggplot(WP4_table, aes(year, Growing_stock, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Growing Stocks in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Growing Stock [m3/ha]") +
  theme_bw() +
  ylim(0,350)+
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
Growing_stock <- Growing_stock + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Growing_stock


# WITH CUSTUMIZED PALETTE - custom_palette

Growing_stock <- ggplot(WP4_table, aes(year, Growing_stock, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Growing Stocks in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Growing Stock [m3/ha]") +
  theme_bw() +
  ylim(0,350)+
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
Growing_stock <- Growing_stock + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Growing_stock


# Growing Stock with facet_wrap

Growing_stock <- ggplot(WP4_table, aes(year, Growing_stock, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Growing Stocks in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Growing Stock [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

Growing_stock <- Growing_stock + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Growing_stock <- Growing_stock + theme(plot.title = element_text(hjust = 0.5))
Growing_stock <- Growing_stock + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
Growing_stock <- Growing_stock + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
Growing_stock


#_______________________________________________________________________________
# Annual increment my palette

Annual_increment <- ggplot(WP4_table, aes(year, Annual_increment, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Annual Increment in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Growing Stock [m3/ha]") +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
Annual_increment <- Annual_increment + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_increment



# Annual increment custum_palette

Annual_increment <- ggplot(WP4_table, aes(year, Annual_increment, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Annual Increment in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Growing Stock [m3/ha]") +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
Annual_increment <- Annual_increment + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_increment


# Annual increment facet_wrap

Annual_increment <- ggplot(WP4_table, aes(year, Annual_increment, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Annual Increment in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Annual Increment [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

Annual_increment <- Annual_increment + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_increment <- Annual_increment + theme(plot.title = element_text(hjust = 0.5))
Annual_increment <- Annual_increment + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
Annual_increment <- Annual_increment + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
Annual_increment

#_______________________________________________________________________________
# Disturbance mortality

Disturbance_mortality <- ggplot(WP4_table, aes(year, Disturbance_mortality, color = run)) +
  geom_line(size = 0.7) +
  ggtitle(" Disturbance Mortality in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Disturbance Mortality [m3/ha]") +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
Disturbance_mortality <- Disturbance_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Disturbance_mortality



# Disturbance mortality custum_palette
Disturbance_mortality <- ggplot(WP4_table, aes(year, Disturbance_mortality, color = run)) +
  geom_line(size = 0.7) +
  ggtitle(" Disturbance Mortality in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  ylab("Disturbance Mortality [m3/ha]") +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
Disturbance_mortality <- Disturbance_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Disturbance_mortality


# Disturbance mortality facet_wrap

Disturbance_mortality <- ggplot(WP4_table, aes(year, Disturbance_mortality, color = run)) +
  geom_line(size = 0.6) +
  ggtitle(" Disturbance Mortality in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Disturbance Mortality [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

Disturbance_mortality <- Disturbance_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Disturbance_mortality <- Disturbance_mortality + theme(plot.title = element_text(hjust = 0.5))
Disturbance_mortality <- Disturbance_mortality + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
Disturbance_mortality <- Disturbance_mortality + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
Disturbance_mortality


#_______________________________________________________________________________
# Natural mortality

Natural_mortality <- ggplot(WP4_table, aes(year, Natural_mortality, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Natural Mortality in Different Management and RCP scenarios") +
  ylab("Natural Mortality [m3/ha]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
Natural_mortality <- Natural_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Natural_mortality



# Natural mortality custom_palette

Natural_mortality <- ggplot(WP4_table, aes(year, Natural_mortality, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Natural Mortality in Different Management and RCP scenarios") +
  ylab("Natural Mortality [m3/ha]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
Natural_mortality <- Natural_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Natural_mortality


# Natural_mortality - facet_wrap

Natural_mortality <- ggplot(WP4_table, aes(year, Natural_mortality, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Natural Mortality in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Natural Mortality [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

Natural_mortality <- Natural_mortality + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Natural_mortality <- Natural_mortality + theme(plot.title = element_text(hjust = 0.5))
Natural_mortality <- Natural_mortality + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
Natural_mortality <- Natural_mortality + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
Natural_mortality



#_______________________________________________________________________________
# Annual harvest

Annual_harvest <- ggplot(WP4_table, aes(year, Annual_harvest, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Annual Harvest in Different Management and RCP scenarios") +
  ylab("Annual Harvest [m3/ha]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
Annual_harvest <- Annual_harvest + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_harvest


# Annual harvest custom_palette
Annual_harvest <- ggplot(WP4_table, aes(year, Annual_harvest, color = run)) +
  geom_line(size = 0.8) +
  ggtitle("Annual Harvest in Different Management and RCP scenarios") +
  ylab("Annual Harvest [m3/ha]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
Annual_harvest <- Annual_harvest + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_harvest

 
# Annual_harvest - facet_wrap

Annual_harvest <- ggplot(WP4_table, aes(year, Annual_harvest, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Annual Harvest in Different Management and RCP scenarios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Annual Harvest [m3/ha]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

Annual_harvest <- Annual_harvest + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
Annual_harvest <- Annual_harvest + theme(plot.title = element_text(hjust = 0.5))
Annual_harvest <- Annual_harvest + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
Annual_harvest <- Annual_harvest + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
Annual_harvest


#_______________________________________________________________________________
# Shannon BA

H_BA_plot <- ggplot(H_avg, aes(year, shannon_BA_avg, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Shannon Entropy based on Basal Area Tree Species Proportion") +
  ylab("Shannon Index [H]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
H_BA_plot <- H_BA_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_BA_plot


# Shannon BA - custom_palette

H_BA_plot <- ggplot(H_avg, aes(year, shannon_BA_avg, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Shannon Entropy based on Basal Area Tree Species Proportion") +
  ylab("Shannon Index [H]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
H_BA_plot <- H_BA_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_BA_plot


# Shannon BA- facet_wrap

H_BA_plot <- ggplot(H_avg, aes(year, shannon_BA_avg, color = run)) +
  geom_line(size = 0.7) +
  ggtitle("Shannon Entropy based on Basal Area Tree Species Proportion") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Shannon Index [H]") +
  facet_wrap(~run, ncol=3)+
  scale_color_manual(values = custom_palette)+
  theme_bw()

H_BA_plot <- H_BA_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_BA_plot <- H_BA_plot + theme(plot.title = element_text(hjust = 0.5))
H_BA_plot <- H_BA_plot + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
H_BA_plot <- H_BA_plot + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
H_BA_plot


#_______________________________________________________________________________
# Shannon VOLUME

H_VOL_plot <- ggplot(H_avg, aes(year, shannon_VOL_avg, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Shannon Entropy based on VOlume Tree Species Proportion") +
  ylab("Shannon Index [H]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = my_palette)

# Customize text size and angle for axis titles
H_VOL_plot <- H_VOL_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_VOL_plot


# Shannon VOLUME - custom_palette

H_VOL_plot <- ggplot(H_avg, aes(year, shannon_VOL_avg, color = run)) +
  geom_line(size = 0.6) +
  ggtitle("Shannon Entropy based on VOlume Tree Species Proportion") +
  ylab("Shannon Index [H]") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5), angle = 0)) +
  theme_bw() +
  scale_color_manual(values = custom_palette)

# Customize text size and angle for axis titles
H_VOL_plot <- H_VOL_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_VOL_plot


# Shannon VOL - facet_wrap

H_VOL_plot <- ggplot(H_avg, aes(year, shannon_VOL_avg, color = run)) +
  geom_line(size = 0.7) +
  ggtitle("Shannon Entropy based on Volume Tree Species Proportion") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~run, ncol=3)+
  ylab("Shannon Index [H]") +
  scale_color_manual(values = custom_palette)+
  theme_bw()

H_VOL_plot <- H_VOL_plot + theme(plot.title = element_text(lineheight = 2, face = "bold", color = "black", size = 18))
H_VOL_plot <- H_VOL_plot + theme(plot.title = element_text(hjust = 0.5))
H_VOL_plot <- H_VOL_plot + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
H_VOL_plot <- H_VOL_plot + theme(axis.title.x = element_text(size = rel(1.5), angle = 0))
H_VOL_plot



# CLOSE EVERY PLOT


dev.off()

#                                  END

