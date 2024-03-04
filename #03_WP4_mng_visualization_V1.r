
# install.packages("RSQLite")
library(RSQLite)
library(dplyr)
library(fields)
#_____________________________________________________
# Path to search the data
dataroot <- ("C:/iLand/2023/WP4_Resonate/Resonate_WP4_exp/output/")

#------------------------------------------------------------
# 1ST 2 DATABASE - BUSINESS AS USUAL + BIOECONOMY MANAGEMENT

BAU_V1 <-paste0(dataroot,"BAU_WP4_20231223.sqlite")   # file to read
BIOEC_V1 <-paste0(dataroot,"BIOECONOMY_WP4_20221223.sqlite")   # file to read

#_____________________________________________________
# connect to the database of BAU
sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = BAU_V1)  # connect to the file
tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
print(tables.in.the.file)

# connect to the database of BIOECONOMY
sqlite.driver <- dbDriver("SQLite")
db2 <- dbConnect(sqlite.driver, dbname = BIOEC_V1)  # connect to the file
tables.in.the.file<-dbListTables(db2)           # explore the tables in the file
print(tables.in.the.file)

#____________________________________________________
# READ IN different tables (present outputs):    (here can read in by table names.... depending on what you have in your outputfile)
# BAU
abeStand_bau <- dbReadTable(db1, "abeStand")
abeStandRemoval_bau <- dbReadTable(db1, "abeStandRemoval")
abeUnit_bau <- dbReadTable(db1, "abeUnit")
barkbeetle_bau <- dbReadTable(db1,"barkbeetle")
carbon_bau <- dbReadTable(db1,"carbon")
carbonflow_bau <- dbReadTable(db1, "carbonflow")
dynamicstand_bau <- dbReadTable(db1, "dynamicstand")
landscape_bau <- dbReadTable(db1,"landscape")
wind_bau <- dbReadTable(db1,"wind")
dbDisconnect(db1)    # close the file

# BIOECONOMY
abeStand_bioec <- dbReadTable(db2, "abeStand")
abeStandRemoval_bioec <- dbReadTable(db2, "abeStandRemoval")
abeUnit_bioec <- dbReadTable(db2, "abeUnit")
barkbeetle_bioec <- dbReadTable(db2,"barkbeetle")
carbon_bioec <- dbReadTable(db2,"carbon")
carbonflow_bioec <- dbReadTable(db2, "carbonflow")
dynamicstand_bioec <- dbReadTable(db2, "dynamicstand")
landscape_bioec <- dbReadTable(db2,"landscape")
wind_bioec <- dbReadTable(db2,"wind")
dbDisconnect(db2)    # close the file
#_____________________________________________________
# 2ND  DATABASE - CONSERVATION + ADAPTATION MANAGEMENT
#------------------------------------------------------------

# 2 Name of the database
CNS_V1 <-paste0(dataroot,"CONSERVATION_WP4_20231223.sqlite")   # file to read
ADP_V1 <-paste0(dataroot,"ADAPTATION_WP4_20231223.sqlite")   # file to read

# connect to the database COSERVATION
sqlite.driver <- dbDriver("SQLite")
db3 <- dbConnect(sqlite.driver, dbname = CNS_V1)  # connect to the file
tables.in.the.file<-dbListTables(db3)           # explore the tables in the file
print(tables.in.the.file)

# connect to the database ADAPTATION
sqlite.driver <- dbDriver("SQLite")
db4 <- dbConnect(sqlite.driver, dbname = ADP_V1)  # connect to the file
tables.in.the.file<-dbListTables(db4)           # explore the tables in the file
print(tables.in.the.file)

#_______________________________________________
# READ IN different tables:    (here can read in by table names.... depending on what you have in your output file)
# CONSERVATION
abeStand_cns <- dbReadTable(db3, "abeStand")
abeStandRemoval_cns <- dbReadTable(db3, "abeStandRemoval")
abeUnit_cns <- dbReadTable(db3, "abeUnit")
barkbeetle_cns <- dbReadTable(db3,"barkbeetle")
carbon_cns <- dbReadTable(db3,"carbon")
carbonflow_cns <- dbReadTable(db3, "carbonflow")
dynamicstand_cns <- dbReadTable(db3, "dynamicstand")
landscape_cns <- dbReadTable(db3,"landscape")
wind_cns <- dbReadTable(db3,"wind")
dbDisconnect(db3)    # close the file

# ADAPTATION
abeStand_adp <- dbReadTable(db4, "abeStand")
abeStandRemoval_adp <- dbReadTable(db4, "abeStandRemoval")
abeUnit_adp <- dbReadTable(db4, "abeUnit")
barkbeetle_adp <- dbReadTable(db4,"barkbeetle")
carbon_adp <- dbReadTable(db4,"carbon")
carbonflow_adp <- dbReadTable(db4, "carbonflow")
dynamicstand_adp <- dbReadTable(db4, "dynamicstand")
landscape_adp <- dbReadTable(db4,"landscape")
wind_adp <- dbReadTable(db4,"wind")
dbDisconnect(db4)    # close the file
#____________________________________________________________
#------------------------------------------------------------
# Make a plot with ggplot, volume, colored by species for the transitional period


# dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/"
pdf(paste0(dataroot, "20231223_mng_wp4_landscape_test.pdf"), height=8, width=12)

#_______________________________________________
library(ggplot2)
library(gridExtra) # To arrange the graphs in a grid

# SET THE COLORS
# this tells the colors:
species.we.have<-unique(landscape_adp$species)

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
            "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900", "psme" = "black"
)

new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab", "psme")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]

#_____________________________________________________________
# PLOT THE TANSITION PERIOD (TP) VOLUME BY SPECIE IN COMPARABLE WAY (MULTI-WINDOW)
# TP IN WIND AND BARK BEETLE REGIME 
#_____________________________________________
g1 <- ggplot(landscape_bau, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("BAU") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,600)

g2 <- ggplot(landscape_bioec, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("BIOECONOMY") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,600)

# grid.arrange(g1,g2,ncol=2)
# grid.arrange(g1,g2,ncol=1)

#______________________________________________
# TP in only wind regime

g3 <- ggplot(landscape_cns, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("CONSERVATION") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,600)

g4 <- ggplot(landscape_adp, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("ADAPTATION") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,600)

#grid.arrange(g1,g2,ncol=2)
#grid.arrange(g3,g4,ncol=2)

#______________________________________________
# All the 4 graphics together

grid.arrange(g1,g2,g3,g4,ncol=2)

#grid.arrange(x2,x4,ncol=2)

#_______________________________________________________________
#------------------------------------------------------------
# HARVEST TIME SERIES IN MANAGEMENT WP4 RESONATE

# BAU - BIOECONOMY
a1<-data.frame(year=abeUnit_bau$year, harvest=abeUnit_bau$realizedHarvest, case="bau")
a2<-data.frame(year=abeUnit_bioec$year, harvest=abeUnit_bioec$realizedHarvest, case="bioec")

head(a1)
head(a2)

harvests<- rbind(a1,a2)
summary(a1)
summary(a2)
summary(harvests)
dim(harvests)

# Two lines in the same graph
x1 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized total harvest in BAU - BIOECO")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,40)+
  theme_bw()
x1 + theme(plot.title = element_text(hjust = 0.5))


# Visualization with facet wrap
x2 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized  total harvest in BAU BIOEC")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,40)+
  theme_bw()
x2 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# Realized harvest in CONSERVATION AND ADAPTATION MANAGEMENT STRATEGY

a3<-data.frame(year=abeUnit_cns$year, harvest=abeUnit_cns$realizedHarvest, case="cns")
a4<-data.frame(year=abeUnit_adp$year, harvest=abeUnit_adp$realizedHarvest, case="adp")

head(a3)
head(a4)

harvests<- rbind(a3,a4)
summary(a3)
summary(a4)
summary(harvests)
dim(harvests)

# MULTI-LINES in the same graph
x3 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized  total harvest in Conservation -Adaptation")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,45)+
  theme_bw()
x3 + theme(plot.title = element_text(hjust = 0.5))

# Visualization with facet wrap
x4 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized total harvest  in Conservation -Adaptation")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,45)+
  theme_bw()
x4 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# Realized harvest in ALL FOUR MANAGEMENT STRATEGY

harvests<- rbind(a1,a2,a3,a4)
summary(harvests)
dim(harvests)

# MULTI-LINES in the same graph
x5 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized  total harvest in the 4 management strategies")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,45)+
  theme_bw()
x5 + theme(plot.title = element_text(hjust = 0.5))

# Visualization with facet wrap
x6 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=2)+
  ggtitle("Realized total harvest in the 4 management strategies")+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,45)+
  theme_bw()
x6 + theme(plot.title = element_text(hjust = 0.5))
#_______________________________________________________________________________

#------------------------------------------------------------
# CUMULATIVE HARVEST 


# BUSINESS AS USUAL AND BIOECONOMY
cs1 <-data.frame(year=abeUnit_bau$year, harvest=cumsum(abeUnit_bau$realizedHarvest), case="bau")
cs2 <-data.frame(year=abeUnit_bioec$year, harvest=cumsum(abeUnit_bioec$realizedHarvest), case="bioec")

harvests <- rbind(cs1,cs2)
#head(h1)

summary(cs1)  # statistics
summary(cs2)
#dim(cs2)      # dimension of the data frame

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest bau bioec")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest bau bioec")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv


#_______________________________________________________________________________
# CONSERVATION AND ADAPTATION

cs3<-data.frame(year=abeUnit_cns$year, harvest=cumsum(abeUnit_cns$realizedHarvest), case="cns")
cs4<-data.frame(year=abeUnit_adp$year, harvest=cumsum(abeUnit_adp$realizedHarvest), case="adp")

harvests <- rbind(cs3,cs4)
#head(h1)

summary(cs3)  # statistics
summary(cs4)
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest CNS ADP")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest cns adp")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

#___________________________________________________________________________
# CUMULATIVE HARVEST ALL FOUR MANAGEMENT TOGETHER

harvests <- rbind(cs1,cs2,cs3,cs4)

summary(harvests)  # statistics
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv
#___________________________________________________________________________
#------------------------------------------------------------
# Yearly increment on the landscape 
landscape_bau_inc <- landscape_bau %>%
  group_by(year) %>%
  summarise(volume_m3 = sum(volume_m3)) %>%
  filter(year < max(year))

landscape_bioec_inc <- landscape_bau %>%
  group_by(year) %>%
  summarise(volume_m3 = sum(volume_m3)) %>%
  filter(year < max(year))


inc_bau<-data.frame(year=landscape_bau_inc$year, volume_m3 = landscape_bau_inc$volume_m3, volume_hrv=abeUnit_bau$realizedHarvest, case="bau")
inc_bioec<-data.frame(year=landscape_bioec_inc$year, volume_m3 = landscape_bioec_inc$volume_m3, volume_hrv=abeUnit_bioec$realizedHarvest, case="bioec")


inc_bau <- inc_bau %>%
  arrange(year) %>% # Ensure the data is sorted by year
  mutate(volume_total = volume_m3 + volume_hrv, # Calculate the total volume for each year
         volume_increment = c(NA, diff(volume_total)), # Calculate the volume increment (difference between consecutive years)
         index = volume_increment / volume_total) # Calculate the index


inc_bioec <- inc_bioec %>%
  arrange(year) %>% # Ensure the data is sorted by year
  mutate(volume_total = volume_m3 + volume_hrv, # Calculate the total volume for each year
         volume_increment = c(NA, diff(volume_total)), # Calculate the volume increment (difference between consecutive years)
         index = volume_increment / volume_total) # Calculate the index

#_______________________________________________________________________________

#------------------------------------------------------------
# PIE CHARTS IN BAU AND BIOECONOMY MANAGEMENT WOOD VOLUME BY SPECIES PROPORTION

landscape_bau_0 <- landscape_bau %>% filter(year==0)
landscape_bau_100 <- landscape_bau %>% filter(year==100)
landscape_bioec_100 <- landscape_bioec %>% filter(year==100)
#landscape_bau_200 <- landscape_bau %>% filter(year==200)
#landscape_bioec_200 <- landscape_bioec %>% filter(year==200)
#landscape_bau_300 <- landscape_bau %>% filter(year==300)
#landscape_bioec_300 <- landscape_bioec %>% filter(year==300)
#landscape_bau_400 <- landscape_bau %>% filter(year==400)
#landscape_bioec_400 <- landscape_bioec %>% filter(year==400)

b0_initinal<-landscape_bau_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial stage")
b1_bau<-landscape_bau_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BAU year 100")
b2_bioec<-landscape_bioec_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BIOEC year 100")
#b3_bau<-landscape_bau_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BAU year 200")
#b4_bioec<-landscape_bioec_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BIOEC year 200")
#b5_bau<-landscape_bau_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BAU year 300")
#b6_bioec<-landscape_bioec_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BIOEC year 300")
#b7_bau<-landscape_bau_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BAU year 400")
#b8_bioec<-landscape_bioec_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="BIOEC year 400")

#summary(b1)
#sum(b1$perc.vol)

# SELETION OF THE INITIAL AND FINAL STAGE BAU AND BIOECONOMY

r1wb<-rbind(b0_initinal,b1_bau,b2_bioec)

x7wb <- ggplot(r1wb, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] BAU and BIOEC")+
  theme_bw()
x7wb + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_wb <-rbind(b0_initinal,b1_bau,b2_bioec,b3_bau,b4_bioec,b5_bau,b6_bioec)

x8wb <- ggplot(r_transition_wb, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in bau bioec")+
  theme_bw()
x8wb + theme(plot.title = element_text(hjust = 0.5))


#_______________________________________________________________________________
# PIE CHARTS IN CONSERVATION AND ADAPTATION WOOD VOLUME BY SPECIES PROPORTION

landscape_cns_0 <- landscape_cns %>% filter(year==0)
landscape_cns_100 <- landscape_cns %>% filter(year==100)
landscape_adp_100 <- landscape_adp %>% filter(year==100)
#landscape_cns_200 <- landscape_cns %>% filter(year==200)
#landscape_adp_200 <- landscape_adp %>% filter(year==200)
#landscape_cns_300 <- landscape_cns %>% filter(year==300)
#landscape_adp_300 <- landscape_adp %>% filter(year==300)
#landscape_cns_400 <- landscape_cns %>% filter(year==399)
#landscape_adp_400 <- landscape_adp %>% filter(year==399)

b0_initial<-landscape_cns_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial_stage")
b1_cns<-landscape_cns_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="CNS year 100")
b2_adp<-landscape_adp_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="ADP year 100")
#b3_cns<-landscape_cns_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="CNS year 200")
#b4_adp<-landscape_adp_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="ADP year 200")
#b5_cns<-landscape_cns_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="CNS year 300")
#b6_adp<-landscape_adp_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="ADP year 300")
#b7_cns<-landscape_cns_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="CNS year 400")
#b8_adp<-landscape_adp_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="ADP year 400")

#summary(b1)
#sum(b1$perc.vol)

# SELETION OF THE INITIAL AND FINAL STAGE CONSERVATION AND ADAPTATION

r1w<-rbind(b0_initinal,b1_cns,b2_adp)

x7w <- ggplot(r1w, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in cns adp ")+
  theme_bw()
x7w + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_w<-rbind(b0_initinal,b1_cns,b2_adp,b3_cns,b4_adp,b5_cns,b6_adp)

x8w <- ggplot(r_transition_w, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in cns adp")+
  theme_bw()
x8w + theme(plot.title = element_text(hjust = 0.5))


#_______________________________________________________________________________
# ALL THE MANAGEMENT TOGETHER


# SELETION OF THE INITIAL AND FINAL STAGE

r1w<-rbind(b0_initinal,b1_bau, b2_bioec, b1_cns, b2_adp)

x7w <- ggplot(r1w, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=4)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha]")+
  theme_bw()
x7w + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_w<-rbind(b0_initinal,b1_bau,b2_bioec,b3_bau,b4_bioec,b5_bau,b6_bioec,
                      b1_cns,b2_adp,b3_cns,b4_adp,b5_cns,b6_adp)

x8w <- ggplot(r_transition_w, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=6)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha]")+
  theme_bw()
x8w + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________

#------------------------------------------------------------
# Understanding the Basal Area dynamic for selected species
# SPECIES specific BA:

# Wind and Bark Beetle REGIME

species.to.keep<-c("piab","pisy", "fasy","qupe")
species.to.keep

landscape_bau_ba <- landscape_bau %>% filter(species %in% species.to.keep)
landscape_bioec_ba <- landscape_bioec %>% filter(species %in% species.to.keep)
#head(landscape_bau2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba1_wb <-ggplot(data=landscape_bau_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("BAU management BASAL AREA INTERESTED SPECIES") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  ylim(0,30)+
  theme_bw()

ba2_wb <-ggplot(data=landscape_bioec_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("BIOEC management BASAL AREA INTERESTED SPECIES")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  ylim(0,30)+
  theme_bw()

grid.arrange(ba1_wb,ba2_wb,ncol=1)

#_______________________________________________________________________________
# IN wind DISTURBANCE REGIME

landscape_cns_ba <- landscape_cns %>% filter(species %in% species.to.keep)
landscape_adp_ba <- landscape_adp %>% filter(species %in% species.to.keep)
#head(landscape_bau2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba3_w <-ggplot(data=landscape_cns_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("CNS management BASAL AREA INTERESTED SPECIES") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  ylim(0,30)+
  theme_bw()

ba4_w <-ggplot(data=landscape_adp_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("ADP management BASAL AREA INTERESTED SPECIES")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  ylim(0,30)+
  theme_bw()

grid.arrange(ba3_w,ba4_w,ncol=1)
#_________________________________________________________________________________

grid.arrange(ba1_wb, ba2_wb, ba3_w, ba4_w, ncol=2)
#_______________________________________________________________________________


#------------------------------------------------------------
# BASAL AREA PROPORTION BASED ON DBH CLASSES -DYNAMIC STAND OUTPUT

#_______________________________________________________________________________
# BASAL AREA PROPORTION BASED ON DBH CLASSES
# Make the distribution of the Basal Area by DBH classes and species composition

area<-landscape_bioec$area[1]
print(area)
new_order_gg2=c("piab", "abal", "lade", "pisy", "fasy", "quro", "acps", "frex", "cabe", "bepe", "qupe", "algl", "potr", "poni", "tico", "saca", "rops","psme")

#_______________________________________________________________________________
# IN BAU AND BIOECONOMY

dynamicstand_bioec_0 <- dynamicstand_bioec %>% filter(year==0)
dynamicstand_bau_100 <- dynamicstand_bau %>% filter(year==100)
dynamicstand_bioec_100 <- dynamicstand_bioec %>% filter(year==100)

ba_dbh_bioec_0 <- dynamicstand_bioec_0[,8:24]
ba_dbh_bau_100 <- dynamicstand_bau_100[,8:24]
ba_dbh_bioec_100 <- dynamicstand_bioec_100[,8:24]

row.names(ba_dbh_bioec_0) <- dynamicstand_bioec_0$species
colnames(ba_dbh_bioec_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_bioec_100) <- dynamicstand_bioec_100$species
colnames(ba_dbh_bioec_100) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_bau_100) <- dynamicstand_bau_100$species
colnames(ba_dbh_bau_100) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_bioec_0<-ba_dbh_bioec_0/area       # divide by the area because of the dynamic stand output
ba_dbh_bioec_100<-ba_dbh_bioec_100/area   # divide by the area because of the dynamic stand output
ba_dbh_bau_100<-ba_dbh_bau_100/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_bioec_0), col = cols[new_order_gg2], ylab = "Basal area [m2/ha]", ylim=c(0,100), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_bau_100), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_bioec_100), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=1,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________

#_______________________________________________________________________________
# IN WIND REGIME

dynamicstand_adp_0 <- dynamicstand_adp %>% filter(year==0)
dynamicstand_cns_400 <- dynamicstand_cns %>% filter(year==400)
dynamicstand_adp_400 <- dynamicstand_adp %>% filter(year==400)

ba_dbh_adp_0 <- dynamicstand_adp_0[,8:24]
ba_dbh_cns_400 <- dynamicstand_cns_400[,8:24]
ba_dbh_adp_400 <- dynamicstand_adp_400[,8:24]

row.names(ba_dbh_adp_0) <- dynamicstand_adp_0$species
colnames(ba_dbh_adp_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_adp_400) <- dynamicstand_adp_400$species
colnames(ba_dbh_adp_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_cns_400) <- dynamicstand_cns_400$species
colnames(ba_dbh_cns_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_adp_0<-ba_dbh_adp_0/area       # divide by the area because of the dynamic stand output
ba_dbh_adp_400<-ba_dbh_adp_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cns_400<-ba_dbh_cns_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_adp_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cns_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC wind", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

barplot(as.matrix(ba_dbh_adp_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW wind", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=0.9,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________

#_______________________________________________________________________________
# IN not disturbance REGIME

dynamicstand_sw_0 <- dynamicstand_sw %>% filter(year==0)
dynamicstand_bau_400 <- dynamicstand_bau %>% filter(year==400)
dynamicstand_sw_400 <- dynamicstand_sw %>% filter(year==400)

ba_dbh_sw_0 <- dynamicstand_sw_0[,8:24]
ba_dbh_bau_400 <- dynamicstand_bau_400[,8:24]
ba_dbh_sw_400 <- dynamicstand_sw_400[,8:24]

row.names(ba_dbh_sw_0) <- dynamicstand_sw_0$species
colnames(ba_dbh_sw_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_sw_400) <- dynamicstand_sw_400$species
colnames(ba_dbh_sw_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_bau_400) <- dynamicstand_bau_400$species
colnames(ba_dbh_bau_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_sw_0<-ba_dbh_sw_0/area       # divide by the area because of the dynamic stand output
ba_dbh_sw_400<-ba_dbh_sw_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_400<-ba_dbh_cc_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cc_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

barplot(as.matrix(ba_dbh_sw_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=0.9,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________
#---------------

library(RColorBrewer)
display.brewer.all()
library(viridis)           
library(colorRamps)

# DISTURBANCE REGIME

area<-landscape_adp$area[1]
print(area)

head(dynamicstand_adp)


dynamicstand_sw_0_w <- dynamicstand_adp %>% filter(year==0)
dynamicstand_cc_400_w <- dynamicstand_cns %>% filter(year==400)
dynamicstand_sw_400_w <- dynamicstand_adp %>% filter(year==400)

ba_dbh_sw_0_w <- dynamicstand_sw_0_w[,8:24]
ba_dbh_cc_400_w <- dynamicstand_cc_400_w[,8:24]
ba_dbh_sw_400_w <- dynamicstand_sw_400_w[,8:24]

row.names(ba_dbh_sw_0_w) <- dynamicstand_sw_0_w$species
colnames(ba_dbh_sw_0_w) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_sw_400_w) <- dynamicstand_sw_400_w$species
colnames(ba_dbh_sw_400_w) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_cc_400_w) <- dynamicstand_cc_400_w$species
colnames(ba_dbh_cc_400_w) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


ba_dbh_sw_0_w<-ba_dbh_sw_0_w/area   # divide by the area because of the dynamic stand output
ba_dbh_sw_400_w<-ba_dbh_sw_400_w/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_400_w<-ba_dbh_cc_400_w/area   # divide by the area because of the dynamic stand output

rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"), space = "rgb")

colors<-rgb.palette(17)

names(colors)<-dynamicstand_sw_0_w$species

colors

par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_0_w), col = colors, ylab = "Basal area", main = "Year 0 in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))

barplot(as.matrix(ba_dbh_sw_400_w), col =colors, ylab = "Basal area", main = "Year 400 BIOEC in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))
barplot(as.matrix(ba_dbh_cc_400_w), col = colors, ylab = "Basal area", main = "Year 400 BAU in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))

legend(21,18, legend = names(colors),cex=1.5,fill = colors, xpd = NA)

#_______________________________________________________________________________

area<-landscape_sw$area[1]
print(area)

head(dynamicstand_sw)


dynamicstand_sw_0 <- dynamicstand_sw %>% filter(year==0)
dynamicstand_cc_400 <- dynamicstand_cc %>% filter(year==399)
dynamicstand_sw_400 <- dynamicstand_sw %>% filter(year==399)

ba_dbh_sw_0 <- dynamicstand_sw_0[,8:24]
ba_dbh_cc_400 <- dynamicstand_cc_400[,8:24]
ba_dbh_sw_400 <- dynamicstand_sw_400[,8:24]

row.names(ba_dbh_sw_0) <- dynamicstand_sw_0$species
colnames(ba_dbh_sw_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_sw_400) <- dynamicstand_sw_400$species
colnames(ba_dbh_sw_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_cc_400) <- dynamicstand_cc_400$species
colnames(ba_dbh_cc_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


ba_dbh_sw_0<-ba_dbh_sw_0/area   # divide by the area because of the dynamic stand output
ba_dbh_sw_400<-ba_dbh_sw_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_400<-ba_dbh_cc_400/area   # divide by the area because of the dynamic stand output

#rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"), space = "rgb")

#colors<-rgb.palette(17)

#names(colors)<-dynamicstand_sw_0$species

#colors

par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_0), col = colors, ylab = "Basal area", main = "Year 0 ", cex.main=2, cex.lab=1.5, ylim=c(0,5))

barplot(as.matrix(ba_dbh_sw_400), col =colors, ylab = "Basal area", main = "Year 400 BIOEC ", cex.main=2, cex.lab=1.5, ylim=c(0,5))
barplot(as.matrix(ba_dbh_cc_400), col = colors, ylab = "Basal area", main = "Year 400 BAU", cex.main=2, cex.lab=1.5, ylim=c(0,5))

legend(22,24, legend = names(colors),cex=1.5,fill = colors, xpd = NA)

#_______________________________________________________________________________

# DEEP UNDERSTANDING BA DBH DYNAMICS

# Working with DPLYR library to filter, group, mutate and arrange

#_______________________________________ CC year 400

dynamicstand_cc_400 <- dynamicstand_cc%>%
  filter(year==399)

ba_dbh_cc_400 <- dynamicstand_cc_400[,8:24]
ba_dbh_cc_400

row.names(ba_dbh_cc_400) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")
ba_dbh_cc_400

colnames(ba_dbh_cc_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_400))

#_________________________________________ CC year 0

dynamicstand_cc_0 <- dynamicstand_cc%>%
  filter(year==0)

ba_dbh_cc_0 <- dynamicstand_cc_0[,8:24]
# ba_dbh_cc_0

row.names(ba_dbh_cc_0) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")
# ba_dbh_cc_0

colnames(ba_dbh_cc_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_0))

#_________________________________________ SW year 400

dynamicstand_sw_400 <- dynamicstand_sw%>%
  filter(year==399)

ba_dbh_sw_400 <- dynamicstand_sw_400[,8:24]
# ba_dbh_cc_0

row.names(ba_dbh_sw_400) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")
# ba_dbh_cc_0

colnames(ba_dbh_sw_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_0))


#_________________________________________ SW year 0

dynamicstand_sw_0 <- dynamicstand_sw%>%
  filter(year==0)

ba_dbh_sw_0 <- dynamicstand_sw_0[,8:24]


row.names(ba_dbh_sw_0) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")


colnames(ba_dbh_sw_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#________________________________________ CC year 100

dynamicstand_cc_100 <- dynamicstand_cc%>%
  filter(year==100)

ba_dbh_cc_100 <- dynamicstand_cc_100[,8:24]

row.names(ba_dbh_cc_100) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca", "rops")

colnames(ba_dbh_cc_100) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_________________________________________ CC year 200

dynamicstand_cc_200 <- dynamicstand_cc%>%
  filter(year==200)

ba_dbh_cc_200 <- dynamicstand_cc_200[,8:24]

row.names(ba_dbh_cc_200) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_cc_200) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_________________________________________ CC year 300

dynamicstand_cc_300 <- dynamicstand_cc%>%
  filter(year==300)

ba_dbh_cc_300 <- dynamicstand_cc_300[,8:24]

row.names(ba_dbh_cc_300) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_cc_300) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 100

dynamicstand_sw_100 <- dynamicstand_sw%>%
  filter(year==100)

ba_dbh_sw_100 <- dynamicstand_sw_100[,8:24]

row.names(ba_dbh_sw_100) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")

colnames(ba_dbh_sw_100) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 200

dynamicstand_sw_200 <- dynamicstand_sw%>%
  filter(year==200)

ba_dbh_sw_200 <- dynamicstand_sw_200[,8:24]

row.names(ba_dbh_sw_200) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca", "rops")

colnames(ba_dbh_sw_200) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 300

dynamicstand_sw_300 <- dynamicstand_sw%>%
  filter(year==300)

ba_dbh_sw_300 <- dynamicstand_sw_300[,8:24]

row.names(ba_dbh_sw_300) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_sw_300) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_______________________________________________________ Plotting in bar chart and using color palettes

# TO ADD THE COLOURS 

library(RColorBrewer)
#display.brewer.all()
library(viridis)           
library(colorRamps)

# Trying other palette

cl <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000", "pink", "darkgreen", "violet", "black")
cl2 <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000", "pink", "brown", "orange", "green")
cl3 <- c("#1B9E77","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")

barplot(as.matrix(ba_dbh_cc_0), col = cl)
barplot(as.matrix(ba_dbh_cc_0), col = viridis(17))
barplot(as.matrix(ba_dbh_cc_0), col = rgb.palette(17))


rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"),
                                space = "rgb")

dev.off()

#   BAR PLOT CC 0 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_0), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 0", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 100 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_100), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 100", cex.main=2, cex.lab=1.5)

#   Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 200 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_200), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 200", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 300 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_300), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 300", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 400 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_400), 
        col = rgb.palette(17), 
        ylab = "Landscape BA (m2)",
        main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 400",
        cex.main=2, cex.lab=1.5)

#   Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 0__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_0), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "BIOEC Landscape Basal Area divided in DBH classes by species Tran. Period year 0", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 100 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_100), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "BIOEC Landscape Basal Area divided in DBH classes by species Tran. Period year 100", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 200__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_200), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "BIOEC Landscape Basal Area divided in DBH classes by species Tran. Period year 200", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 300__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_300), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "BIOEC Landscape Basal Area divided in DBH classes by species Tran. Period year 300", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 400 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_400), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "BIOEC Landscape Basal Area divided in DBH classes by species Tran. Period year 400", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________
#_______________________________________________________________________________
# FINISHED DEEP UNDERSTANDING BA DBH DYNAMICS



# link for edit the labels and the x and y titles

######### IMPORTANT #############  http://www.sthda.com/english/wiki/add-titles-to-a-plot-in-r-software

# legend("top", c("finalcut", "thinning/regcut","salvaged"), cex=1, bty="n", fill=colors, text.font=2)
# leggend, (position, names/variables, size, bty?- contorn?, color filling, text font) 

# barplot(ba_dbh$if_dbh_5_basalarea_0_sum)  to plot only one column with bar charth

# have a look at this website   https://statisticsglobe.com/barplot-in-r


# ggplot(data_ggp, aes(x = group, y = values)) +        # Create barchart with ggplot2
#  geom_bar(stat = "identity")


# hist(dynamicStand_cc$dbh_mean, main= 'Clear Cut V-DBH Transitional Period')

# hist(dynamicstand_sw$dbh_mean, abeUnit_sw$volume, main= 'Shalterwood Vol-DBH Transitional Period')



# barplot(dynamicStand_cc$year, dynamicStand_cc$dbh_mean, main= 'Clear Cut V-DBH Transitional Period')

#_____________________________________________________________________________________________________
#----------------------------------------------------------------
# wind only disturbance impact in the landscape volume in percentages
# BAU AND BIOECONOMY

killed_volume_bau <- sum(wind_bau$killedVolume)                # 1601620 m3
killed_volume_bau
killed_volume_per_year_bau <- killed_volume_bau/101            # 4014.085
killed_volume_per_year_bau
killed_volume_per_year_bau_ha <- killed_volume_per_year_bau/17749.26
killed_volume_per_year_bau_ha                                 # 0.226155 m3

#_____________________________________________

killed_volume_bioec <- sum(wind_bioec$killedVolume)                # 1362593 m3
killed_volume_bioec
killed_volume_per_year_bioec <- killed_volume_bioec/101            # 3415.021
killed_volume_per_year_bioec
killed_volume_per_year_bioec_ha <- killed_volume_per_year_bioec/17749.26
killed_volume_per_year_bioec_ha                                 # 0.1924036 m3                        


dfnew1_bau <- landscape_bau[,c(1,8)]

df_vol_bau = dfnew1_bau %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_bioec <- landscape_bioec[,c(1,8)]

df_vol_bioec = dfnew1_bioec %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_bau <- df_vol_bau %>% mutate(perc.vol=100*0.226155/tot_vol)

prop_killed_vol_ha_year_bioec <- df_vol_bioec %>% mutate(perc.vol=100*0.1924036/tot_vol)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_bau$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by WIND per year in BAU",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("0.5974279 m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_bioec$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by WIND per year in BIOEC",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("0.4682009 m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')


#____________________________________________________________________________________________________________________________________
{### year by year relative proportion  ONLY BARK BEETLES##
  
  # FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR
  
  bb_killvol_ha_bau <- barkbeetle_bau$killedVolume/abeUnit_bau$area
  bb_killvol_ha_bioec <- barkbeetle_bioec$killedVolume/abeUnit_bioec$area
  
  # Add a column of variables in this case volume killed in % of the total ha avg
  dist_per_bau <- abeUnit_bau %>% mutate(perc.vol=100*bb_killvol_ha_bau/abeUnit_bau$volume)
  summary(dist_per_bau)
  
  dist_per_bioec <- abeUnit_bioec %>% mutate(perc.vol=100*bb_killvol_ha_bioec/abeUnit_bioec$volume)
  summary(dist_per_bioec)
  
  par(mfrow = c(1,2))  # 763 m3 of difference in CC than in SW
  
  # natural logarithm is based on the "Euler's number" = e ??? 2,71828183
  
  hist(dist_per_bau$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in CC",cex.main = 1, xlab = " [Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(0,6), ylim = c(0,100))
  
  legend("topright", c("0.655 m3 / 0.240 %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')
  # legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')
  
  hist(dist_per_bioec$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in SW",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(0,6), ylim = c(0,100))
  
  legend("topright", c("0.612 m3 / 0.232 %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')}

#_________________________________________________________________________________________________________________
#   NEW KILLED VOLUME CALCULATION INCLUDING ALL MANAGEMENT wind + bark beetles


# Disturbance impact in the landscape volume in percentages

# BAU
killed_volume_bau_wb <- sum(wind_bau$killedVolume)                            # 5929296 m3
killed_volume_bau_wb
killed_volume_bau_bb_wb <- sum(barkbeetle_bau$killedVolume)                   # 4706904
killed_volume_bau_bb_wb
killed_volume_bau_dist_wb <- killed_volume_bau_wb + killed_volume_bau_bb_wb   # 10636200
killed_volume_bau_dist_wb
killed_volume_per_year_bau <- killed_volume_bau_dist_wb/100                   # 26590.5 
killed_volume_per_year_bau
killed_volume_per_year_bau_ha_wb <- killed_volume_per_year_bau/17749.26
killed_volume_per_year_bau_ha_wb                                              # 1.498119 m3   # 1.747298

# BIOECONOMY

killed_volume_bioec_wb <- sum(wind_bioec$killedVolume)                          # 5407252 m3
killed_volume_bioec_wb
killed_volume_bioec_bb_wb <- sum(barkbeetle_bioec$killedVolume)               # 4388046
killed_volume_bioec_bb_wb
killed_volume_bioec_dist_wb <- killed_volume_bioec_wb + killed_volume_bioec_bb_wb  # 9795297
killed_volume_bioec_dist_wb
killed_volume_per_year_bioec <- killed_volume_bioec_dist_wb/100               # 24488.24
killed_volume_per_year_bioec
killed_volume_per_year_bioec_ha_wb <- killed_volume_per_year_bioec/17749.26
killed_volume_per_year_bioec_ha_wb                                            # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_bau <- landscape_bau[,c(1,8)]

df_vol_bau = dfnew1_bau %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_bioec <- landscape_bioec[,c(1,8)]

df_vol_bioec = dfnew1_bioec %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_bau <- df_vol_bau %>% mutate(perc.vol=100*killed_volume_per_year_bau_ha_wb/tot_vol)
prop_killed_vol_ha_year_bau
summary(prop_killed_vol_ha_year_bau)

prop_killed_vol_ha_year_bioec <- df_vol_bioec %>% mutate(perc.vol=100*killed_volume_per_year_bioec_ha_wb/tot_vol)
prop_killed_vol_ha_year_bioec
summary(prop_killed_vol_ha_year_bioec)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_bau$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in BAU",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("1.131803 m3 / 0.3660 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_bioec$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in BIOECONOMY",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("0.9346 m3 / 0.3111 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

#_________________________________________________________________________________________________
# KILLED VOLUME PROPORTION IN CONSERVATION AND ADAPTATION

{# Only wind 
# Wind disturbance impact in the landscape volume in percentages

killed_volume_cns <- sum(wind_cns$killedVolume)                # 7261776 m3
killed_volume_cns
killed_volume_per_year_cns <- killed_volume_cns/400            # 18154.44
killed_volume_per_year_cns
killed_volume_per_year_bau_ha_w <- killed_volume_per_year_cns/17749.26
killed_volume_per_year_bau_ha_w                                 # 1.022828 m3

#_____________________________________________

killed_volume_adp <- sum(wind_adp$killedVolume)                # 7040383 m3
killed_volume_adp
killed_volume_per_year_adp <- killed_volume_adp/400            # 17600.96
killed_volume_per_year_adp
killed_volume_per_year_bioec_ha_w <- killed_volume_per_year_adp/17749.26
killed_volume_per_year_bioec_ha_w                                   # 0.9916446 m3                        


dfnew1_cns <- landscape_cns[,c(1,8)]

df_vol_cns = dfnew1_cns %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_adp <- landscape_adp[,c(1,8)]

df_vol_adp = dfnew1_adp %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cns <- df_vol_cns %>% mutate(perc.vol=100*killed_volume_per_year_bau_ha_w/tot_vol)
summary(prop_killed_vol_ha_year_cns)

prop_killed_vol_ha_year_adp <- df_vol_adp %>% mutate(perc.vol=100*killed_volume_per_year_bioec_ha_w/tot_vol)
summary(prop_killed_vol_ha_year_adp)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cns$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind per year in CC",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,60))

legend("topright", c("1.023 m3 / 0.344 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_adp$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind per year in SW",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,60))

legend("topright", c("0.992 m3 / 0.350 %"), cex = 0.55, title = "Average killed volume [m3/ha/yea] / avarage % on total volume", text.font = 3, bg='lightpink')}


#_________________________________________________________________________________________________________________
#  KILLED VOLUME CALCULATION CONSERVATION AND ADAPTATION MANAGEMENT wind + bark beetles

# Disturbance impact in the landscape volume in percentages

# CONSERVATION
killed_volume_cns_wb <- sum(wind_cns$killedVolume)                            # 5929296 m3
killed_volume_cns_wb
killed_volume_cns_bb_wb <- sum(barkbeetle_cns$killedVolume)                   # 4706904
killed_volume_cns_bb_wb
killed_volume_cns_dist_wb <- killed_volume_cns_wb + killed_volume_cns_bb_wb   # 10636200
killed_volume_cns_dist_wb
killed_volume_per_year_cns <- killed_volume_cns_dist_wb/100                   # 26590.5 
killed_volume_per_year_cns
killed_volume_per_year_cns_ha_wb <- killed_volume_per_year_cns/17749.26
killed_volume_per_year_cns_ha_wb                                              # 1.498119 m3   # 1.747298

# ADAPTATION

killed_volume_adp_wb <- sum(wind_adp$killedVolume)                          # 5407252 m3
killed_volume_adp_wb
killed_volume_adp_bb_wb <- sum(barkbeetle_adp$killedVolume)               # 4388046
killed_volume_adp_bb_wb
killed_volume_adp_dist_wb <- killed_volume_adp_wb + killed_volume_adp_bb_wb  # 9795297
killed_volume_adp_dist_wb
killed_volume_per_year_adp <- killed_volume_adp_dist_wb/100               # 24488.24
killed_volume_per_year_adp
killed_volume_per_year_adp_ha_wb <- killed_volume_per_year_adp/17749.26
killed_volume_per_year_adp_ha_wb                                            # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_cns <- landscape_cns[,c(1,8)]

df_vol_cns = dfnew1_cns %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_adp <- landscape_adp[,c(1,8)]

df_vol_adp = dfnew1_adp %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cns <- df_vol_cns %>% mutate(perc.vol=100*killed_volume_per_year_cns_ha_wb/tot_vol)
prop_killed_vol_ha_year_cns
summary(prop_killed_vol_ha_year_cns)

prop_killed_vol_ha_year_adp <- df_vol_adp %>% mutate(perc.vol=100*killed_volume_per_year_adp_ha_wb/tot_vol)
prop_killed_vol_ha_year_adp
summary(prop_killed_vol_ha_year_adp)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cns$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CONSERVATION",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("1.6354 m3 / 0.4967 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_adp$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in ADAPTATION",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,40))

legend("topright", c("0.9423 m3 / 0.3280 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')


# ALL THE 4 GRAPHS TOGETHER

par(mfrow = c(2,2))

# BAU
hist(prop_killed_vol_ha_year_bau$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in BAU",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("1.577 m3 / 0.4641 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# BIOECONOMY
hist(prop_killed_vol_ha_year_bioec$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in BIOECONOMY",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("1.193 m3 / 0.381 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# CONSERVATION
hist(prop_killed_vol_ha_year_cns$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CONSERVATION",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("1.679 m3 / 0.4365 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# ADAPTATION
hist(prop_killed_vol_ha_year_adp$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in ADAPTATION",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("1.172 m3 / 0.381 %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

#_______________________________________________________________________________




#-------------------------------------------------------------------------------
# Loop to see into the transitional period but interately

# install.packages("RSQLite")
library(RSQLite)
library(dplyr)
library(ggplot2)
library(gridExtra)   

#_______________________________________________________________________________
# Path to search the data
dataroot <- ("C:/iLand/2023/WP4_Resonate/Resonate_WP4_exp/output/")                        # Root for the selection of the data

# CREATE NEW EMPTY DATAFRAME
removals<-c()
lnd<-c()
aUnit<-c()

# NAMES OF THE DATABESES VARIABLES
cases <-c("BAU_WP4_20231215", "BIOECONOMY_WP4_20221219","CONSERVATION_WP4_20231219","ADAPTATION_WP4_20231219")

#cases <- c("SW9","SW10","SW11","SW12")

# ALTERNATIE WAY
#fs <- c("aaa","bbb","ccc","ddd")                                      # ALTERNATIVE WAY AAA,BBB,CCC,DDD ARE THE NAMES OF THE DBs
#fs <- c("Cz_region_20220325_BAU_bioec_management_brow_0.6")


#_______________________________________________________________________________
# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED


for (i in (1:length(cases)))  {                                        # We read in the files in the loop. The "i" is for the x-> 1:i 
  
  # "for" = for argument, "(in"= in, "1"= first element of the for cycle to analysis, ": length(cases)))" = throgh the length of the object cases  
  # PAY ATTENTION FROM HERE FOR TESTING
  
  # i <- 1                                                                 # to test but remember to don't run also the }
  
  case<-cases[i]                                                      # ORDINATION OF THE CASE TO IMPORT AS DATABASE
  
  
  # Name of the database
  file <-paste0(dataroot, case, ".sqlite")   # file to read here the case is always the actual case in the loop
  
  # "file"= name of the object, "paste0"+ function to create a NAME for a computer path of selection of data/objects
  
  # ALTERNATIVE WAY
  #f <-paste0(dataroot,fs,".sqlite")
  
  print(file)
  
  # connect to the database of clearcut model
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
  tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------
  landscape <- dbReadTable(db1,"landscape")
  abeUnit <- dbReadTable(db1, "abeUnit")
  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
  
  #carbon <- dbReadTable(db1,"carbon")
  # wind <- dbReadTable(db1,"wind")
  #carbonflow <- dbReadTable(db1, "carbonflow")
  dbDisconnect(db1)    # close the file
  
  landscape.area<-landscape$area[1]
  
  
  
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
  
  
  # Collect landscape data:
  landscape<- (landscape %>% mutate(run=case))
  lnd<-rbind(lnd, landscape)
  
  # Collect abeUnit data
  abeUnit<-(abeUnit %>% mutate(run=case))
  aUnit<-rbind(aUnit, abeUnit)
  
  
  
}  # end of loop


{# TO SUMMARIZE THE CUTTING ACTIVITIES
values<-data.frame(removals %>% group_by(run, type) %>% summarise(volume=mean(volume)))
print(values)
values_sum <- values %>%
  filter(run == "case1b", type=="finalcut", type == "thinning", type == "regcut")  # to have the results you have to decide only one type at time

values_sum}


# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE

pdf(paste0(dataroot, "20220420a.pdf"), height=8, width=12)
#pdf(paste0(dataroot, "20220414b.pdf"), height=5, width=15)
#pdf(paste0(dataroot, "20220414c.pdf"), height=10, width=25)



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
  ylim(4.5,15.5)+
  ggtitle("Realized Harvest Transitional Period")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()


#-------------------------------------------------------------------------------

# SPECIES specificaly BA:
species.to.keep<-c("piab", "fasy","qupe", "psme")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("basal area") +
  facet_wrap(~run, ncol=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area m2/ha")+  theme_bw()


dev.off()

#________________________________________________________________________THE END

