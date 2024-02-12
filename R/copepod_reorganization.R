# Reorganizing copepods so that large copepod group is only
# Calanus finmarchicus. This is done to be consistent with 
# the State of the Ecosystem work

# Author: Max Grezlik
# largely following work of Sarah Weisberg in MAB-Rpath

# Load packages -----------------------------------------------------------

library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(dplyr) 
library(tidyverse)
library(here)

# Biomass adjustments -----------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

#load data
plankton <- read.csv(here("data", "EcoMon_Plankton_Data_v3_8.csv"))
#convert data and create year, month, day columns: 
plankton$date <- as.Date(plankton$date, format =  "%d-%b-%y")
plankton$month <- month(plankton$date)
plankton$year <- year(plankton$date)
plankton$day <- day(plankton$date)

#filter for GB only
#combine with the strata dataset 
#load strata
strata <- read_sf(here("data", "EcomonStrata_v4b.shp"))
#set coordinate system
st_crs(strata) = 4326 #this is WGS 1984

#make ecomon spatial with the same defined coordinate system
plankton_sp <- st_as_sf(x = plankton, 
                        coords = c("lon", "lat"),
                        crs = 4326)

#extract stratum values to points
#get a weird error - need to follow these steps: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
plankton_extract <- st_intersection(plankton_sp, strata)
plankton_extract <- as.data.frame(plankton_extract)
plankton <- plankton_extract

#filter for GB only
plankton_extract <- plankton_extract %>% filter(Region %in% c("GB"))
plankton <- plankton_extract

#create season column based on 2 month period
plankton$season <- ifelse(plankton$month == 1 | plankton$month == 2, "JanFeb", ifelse(plankton$month == 3 | plankton$month == 4, "MarApril",
                                                                                      ifelse(plankton$month == 5 | plankton$month == 6, "MayJun",ifelse(plankton$month == 7 | plankton$month == 8, "JulAug",ifelse(plankton$month == 9 | plankton$month == 10, "SepOct",ifelse(plankton$month == 11 | plankton$month == 12, "NovDec", NA))))))

#filter for copepods of interest
#tidy
copes_y<-plankton %>% select(ctyp_10m2,calfin_10m2,mlucens_10m2)
copes_x<-as.data.frame(cbind(plankton$year, plankton$season))
colnames(copes_x)<-c("Year","Season")
copes_y[is.na(copes_y)] <- 0 #NAs should be true 0s
copes <- cbind(copes_x, copes_y)

#pull out species names
copes_ynames <- colnames(copes_y)

#make long dataframe
copes_long <- copes %>% pivot_longer(all_of(copes_ynames), names_to = "spp")

#rename spp
copes_long<- copes_long %>% 
  mutate(spp_long=if_else(spp=="ctyp_10m2","CENTROPAGES_TYPICUS",
                          if_else(spp=="calfin_10m2","CALANUS_FINMARCHICUS", if_else(spp=="mlucens_10m2","METRIDIA_LUCENS",NA))))

#filter for 1980-85
copes_long <- copes_long %>% filter(Year >= 1980 & Year <=1985)

#look at temporal coverage
cruises<-copes_long %>% select(Year,Season) %>% distinct()

#average abundance per cruise
copes_cruise<-copes_long %>% group_by(Year,Season,spp_long) %>% dplyr::summarise(abd = mean(value))
#annual average (across cruises)
copes_annual<-copes_cruise %>% group_by(Year,spp_long) %>% dplyr::summarise(abd=mean(abd))
#average across 1980-85
copes_avg<-copes_annual %>% group_by(spp_long) %>% dplyr::summarise(abd=mean(abd))

#convert to biomass
sizes<-read.csv(here("data/EcoMon_Copepod size.csv"))
sizes<-sizes[-1,]
sizes<-sizes %>% select(c("X","Literature"))
colnames(sizes)<-c("spp_long","length")
sizes$length<-as.numeric(sizes$length)
#use l-w conversion from EMAX (wet weight)
sizes$weight<-0.0881*sizes$length^2.8514

#join back with abd density estimates
#join back with abd estimates
copes_avg<-left_join(copes_avg,sizes,by="spp_long")
#multiply ind * mg/ind
copes_avg$biomass<-copes_avg$abd*copes_avg$weight
#pull out calanus
calfin<-copes_avg %>% filter(spp_long=="CALANUS_FINMARCHICUS")
copes_ratio<- calfin$biomass/sum(copes_avg$biomass)

#adjust starting biomasses
#SmCopepods
sm_new<-GB.rpath.params$model$Biomass[which(GB.rpath.params[["model"]][["Group"]] == "SmCopepods")] + GB.rpath.params$model$Biomass[which(GB.rpath.params[["model"]][["Group"]] == "LgCopepods")] * (1-copes_ratio)
GB.rpath.params$model$Biomass[which(GB.rpath.params[["model"]][["Group"]] == "SmCopepods")] <- sm_new
#LgCopepods
lg_new<-GB.rpath.params$model$Biomass[which(GB.rpath.params[["model"]][["Group"]] == "LgCopepods")] * copes_ratio
GB.rpath.params$model$Biomass[which(GB.rpath.params[["model"]][["Group"]] == "LgCopepods")] <- lg_new


# Diet adjustments --------------------------------------------------------
#get rid of NAs first
GB.rpath.params$diet<-replace(GB.rpath.params$diet,is.na(GB.rpath.params$diet),0)

#Shift AmLobster[1] predation
#Shift 0.5% from Macrobenthos[20] to LgCopepods[17]
GB.rpath.params$diet[20,2]<-GB.rpath.params$diet[20,2]-0.005
GB.rpath.params$diet[17,2]<-GB.rpath.params$diet[17,2]+0.005
#Shift 0.5% from Macrobenthos[20]] to LgCopepods[38]
GB.rpath.params$diet[20,2]<-GB.rpath.params$diet[20,2]-0.005
GB.rpath.params$diet[38,2]<-GB.rpath.params$diet[38,2]+0.005

#Shift AtlMackerel[4] predation
#Remove 5% from Macrobenthos[20]
#Remove 15% from Micronekton[23]
#Remove 10% from SmPelagics[41]
#Remove 30% from LgCopepods[17]
#Move 60% to SmCopepods[38]
GB.rpath.params$diet[20,5]<-GB.rpath.params$diet[20,5]-0.05
GB.rpath.params$diet[23,5]<-GB.rpath.params$diet[23,5]-0.15
GB.rpath.params$diet[41,5]<-GB.rpath.params$diet[41,5]-0.1
GB.rpath.params$diet[17,5]<-GB.rpath.params$diet[17,5]-0.3
GB.rpath.params$diet[38,5]<-GB.rpath.params$diet[38,5]+0.6

#Shift SmCopepod[38] predation
#Move 0.25% from SmCopepods[38] to LgCopepods[17]
GB.rpath.params$diet[38,39]<-GB.rpath.params$diet[38,39]-0.0025
GB.rpath.params$diet[17,39]<-GB.rpath.params$diet[17,39]+0.0025


#For rest of groups, add portion to SmCope consumption
GB.rpath.params$diet[38,c(3:4,5:38,40:50)]<-
  GB.rpath.params$diet[38,c(3:4,5:38,40:50)]+
  GB.rpath.params$diet[17,c(3:4,5:38,40:50)]*(1-copes_ratio)
GB.rpath.params$diet[17,c(3:4,5:38,40:50)]<-
  GB.rpath.params$diet[17,c(3:4,5:38,40:50)]*copes_ratio