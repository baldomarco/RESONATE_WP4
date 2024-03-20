# Install required packages if not already installed
# install.packages(c("RSQLite", "dplyr", "ggplot2"))

# Load required libraries
library(RSQLite)
library(dplyr)
library(ggplot2)

#_______________________________________________________________________________
# Path to search the data folder
dataroot <- "C:/iLand/2023/WP4_Resonate/outputs/20240128/Test_BAU_BIOEC_Scheduler/"

# CREATE NEW EMPTY DATAFRAME
removals <- c()
lnd <- c()
aUnit <- c()
bb <-c()
w <- c()
damage.all<-c()

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
}

# Rest of your code remains unchanged

#_______________________________________________________________________________
#### TO SUMMARIZE THE CUTTING ACTIVITIES ######
values <- data.frame(removals %>% group_by(run, type) %>% summarise(volume = mean(volume)))
print(values)

write.csv(values, paste0(dataroot,"20240129_harvest_removals_summary.csv"), row.names = TRUE)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
pdf(paste0(dataroot, "20240118.pdf"), height = 8, width = 12)

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
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Removed volume m3/ha",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","limegreen","#FFDB5C","#FA6E59"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------

ggplot(lnd, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("volume")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)+
  theme_bw()

#-------------------------------------------------------------------------------

ggplot(aUnit, aes(year,realizedHarvest, color=case))+
  geom_line(size=1.2, show.legend = F)+
  facet_wrap(~run, ncol=2)+
  ylim(0,40)+
  ggtitle("Realized Harvest Transitional Period")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

#------------------------------------------------------------
# CUMULATIVE HARVEST 

# Perform cumulative sum operation within each run
harvests <- aUnit %>%
  group_by(run) %>%
  mutate(cumulative_harvest = cumsum(realizedHarvest))

#head(harvests)
summary(harvests)  # statistics
#dim(harvests)      # dimension of the data frame

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year, cumulative_harvest, color = run)) +
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
  facet_wrap(~run, ncol=3)+
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
  facet_wrap(~run, ncol=3)+
  ggtitle("RELATIVE DAMAGES")+
  theme_bw()


# SPECIES specificaly BA:
species.to.keep<-c("piab", "fasy","qupe", "psme")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("basal area") +
  facet_wrap(~run, ncol=3)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area m2/ha")+  theme_bw()


dev.off()

#________________________________________________________________________THE END
