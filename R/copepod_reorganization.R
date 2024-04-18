# Reorganizing copepods so that large copepod group is only
# Calanus finmarchicus. This is done to be consistent with 
# the State of the Ecosystem work

# Author: Max Grezlik
# Ratio calculations largely following work of Sarah Weisberg in MAB-Rpath (but GB specific here)
# Biomass adjustments and diet adjustments change input files rather than GB.params

# Order of operations should be:
# 1. Run GBRpath_biomass_pull.R and GBRpath_diet_pull.R in GBRpath>data-raw>R
# 2. Run this script to reapportion copepod groups in alignment with SOE work
# 3. Run GB_initial_model.R in GBRpath>R

# Load packages -----------------------------------------------------------

library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(data.table)
library(dplyr) 
library(tidyverse)
library(here)
library(Rpath)
# Ratio calculations  -----------------------------------------------------

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


#Biomass adjustments -----------------------------------------------------
#load bio.input.rda
load(here("data", "bio.input.rda"))

#SmCopepods
# Biomass should be current biomass + (current biomass of LgCopepods * (1-copes_ratio))
new.SmCopes <- bio.input[RPATH == "SmCopepods", "B"] + (bio.input[RPATH == "LgCopepods", "B"] * (1 - copes_ratio))

#LgCopepods
# Biomass should be current biomass of LgCopepods * copes_ratio
new.LgCopes <- bio.input[RPATH == "LgCopepods", "B"] * copes_ratio

# Replace Biomass for SmCopepods and LgCopepods with new.SmCopes and new.LgCopes
bio.input[RPATH == "SmCopepods", "B"] <- new.SmCopes
bio.input[RPATH == "LgCopepods", "B"] <- new.LgCopes

# Save bio.input -----------------------------------------------------
usethis::use_data(bio.input, overwrite = T)


# Diet adjustments --------------------------------------------------------
# load diet input data
load(here('data', 'diet.input.rda'))

#Shift AmLobster predation
#Shift 0.5% from Macrobenthos to LgCopepods
new.macrobenthos <- diet.input[Rprey == "Macrobenthos" & Rpred == "AmLobster", "preyper"] - 0.005
diet.input[Rprey == "Macrobenthos" & Rpred == "AmLobster", "preyper"] <- new.macrobenthos
# create new entry Rpred = "AmLobster" and Rprey = "LgCopepods", set to 0.005
new.LgCopes <- data.table(
  Rpred = "AmLobster",
  Rprey = "LgCopepods",
  preyper = 0.005
)
# add new entry to diet.input
diet.input <- rbind(diet.input, new.LgCopes)

# Shift AtlHerring Predation
# based on NWACS values
# Shift 22% from Krill to SmCopepods
# Shift 18% from Micronekton to SmCopepods
new.krill <- diet.input[Rprey == "Krill" & Rpred == "AtlHerring", "preyper"] - 0.22
new.micronekton <- diet.input[Rprey == "Micronekton" & Rpred == "AtlHerring", "preyper"] - 0.12
old.krill <- diet.input[Rprey == "Krill" & Rpred == "AtlHerring", "preyper"]
old.micronekton <- diet.input[Rprey == "Micronekton" & Rpred == "AtlHerring", "preyper"]
krill.change <- old.krill - new.krill
micronekton.change <- old.micronekton - new.micronekton
diet.input[Rprey == "Krill" & Rpred == "AtlHerring", "preyper"] <- new.krill
diet.input[Rprey == "Micronekton" & Rpred == "AtlHerring", "preyper"] <- new.micronekton

# create new entry Rpred = "AtlHerring" and Rprey = "SmCopepods", set to new.krill + new.micronekton
new.SmCopes <- data.table(
  Rpred = "AtlHerring",
  Rprey = "SmCopepods",
  preyper =  krill.change + micronekton.change
)
# name columns
setnames(new.SmCopes, c("Rpred", "Rprey", "preyper"))
# add new entry to diet.input
diet.input <- rbind(diet.input, new.SmCopes)

#Shift AtlMackerel predation
#Remove 23% from Macrobenthos and move to SmCopepods
#Remove 5% from Micronekton
#Remove 4% from SilverHake
#Move 32% to SmCopepods
new.macrobenthos <- diet.input[Rprey == "Macrobenthos" & Rpred == "AtlMackerel", "preyper"] - 0.23
new.micronekton <- diet.input[Rprey == "Micronekton" & Rpred == "AtlMackerel", "preyper"] - 0.05
new.silverhake <- diet.input[Rprey == "SilverHake" & Rpred == "AtlMackerel", "preyper"] - 0.04
diet.input[Rprey == "Macrobenthos" & Rpred == "AtlMackerel", "preyper"] <- new.macrobenthos
diet.input[Rprey == "Micronekton" & Rpred == "AtlMackerel", "preyper"] <- new.micronekton
diet.input[Rprey == "SilverHake" & Rpred == "AtlMackerel", "preyper"] <- new.silverhake

# create new entry Rpred = "AtlMackerel" and Rprey = "SmCopepods", set preyper to 0.32
new.SmCopes <- data.table(
  Rpred = "AtlMackerel",
  Rprey = "SmCopepods",
  preyper = 0.32
)
# add new entry to diet.input
diet.input <- rbind(diet.input, new.SmCopes)

#Shift SmCopepod predation
#Move 0.25% from SmCopepods to LgCopepods
new.SmCopes <- diet.input[Rprey == "SmCopepods" & Rpred == "SmCopepods", "preyper"] - 0.0025
new.LgCopes <- diet.input[Rprey == "LgCopepods" & Rpred == "SmCopepods", "preyper"] + 0.0025
diet.input[Rprey == "SmCopepods" & Rpred == "SmCopepods", "preyper"] <- new.SmCopes
diet.input[Rprey == "LgCopepods" & Rpred == "SmCopepods", "preyper"] <- new.LgCopes

#For all other predators of SmCopepods, change diet proportional to copes_ratio

# find species that prey on both small and large copepods
SmCopes.preds <- diet.input[Rprey == "SmCopepods", "Rpred"]
LgCopes.preds <- diet.input[Rprey == "LgCopepods", "Rpred"]
# find overlapping Rpred
Copes.preds <- intersect(SmCopes.preds, LgCopes.preds)

# Reapportion predation by copes_ratio
# SmPelagics
new.SmPelagics.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "SmPelagics", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "SmPelagics", "preyper"] * (1 - copes_ratio))
new.SmPelagics.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "SmPelagics", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "SmPelagics", "preyper"] <- new.SmPelagics.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "SmPelagics", "preyper"] <- new.SmPelagics.Lg
#GelZooplankton
new.GelZooplankton.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "GelZooplankton", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "GelZooplankton", "preyper"] * (1 - copes_ratio))
new.GelZooplankton.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "GelZooplankton", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "GelZooplankton", "preyper"] <- new.GelZooplankton.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "GelZooplankton", "preyper"] <- new.GelZooplankton.Lg
#Krill
new.Krill.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "Krill", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "Krill", "preyper"] * (1 - copes_ratio))
new.Krill.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "Krill", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "Krill", "preyper"] <- new.Krill.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "Krill", "preyper"] <- new.Krill.Lg
#Micronekton
new.Micronekton.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "Micronekton", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "Micronekton", "preyper"] * (1 - copes_ratio))
new.Micronekton.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "Micronekton", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "Micronekton", "preyper"] <- new.Micronekton.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "Micronekton", "preyper"] <- new.Micronekton.Lg
#Macrobenthos
new.Macrobenthos.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "Macrobenthos", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "Macrobenthos", "preyper"] * (1 - copes_ratio))
new.Macrobenthos.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "Macrobenthos", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "Macrobenthos", "preyper"] <- new.Macrobenthos.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "Macrobenthos", "preyper"] <- new.Macrobenthos.Lg
#LgCopepods
new.LgCopes.Sm <- diet.input[Rprey == "SmCopepods" & Rpred == "LgCopepods", "preyper"] + (diet.input[Rprey == "LgCopepods" & Rpred == "LgCopepods", "preyper"] * (1 - copes_ratio))
new.LgCopes.Lg <- diet.input[Rprey == "LgCopepods" & Rpred == "LgCopepods", "preyper"] * copes_ratio
diet.input[Rprey == "SmCopepods" & Rpred == "LgCopepods", "preyper"] <- new.LgCopes.Sm
diet.input[Rprey == "LgCopepods" & Rpred == "LgCopepods", "preyper"] <- new.LgCopes.Lg

# Save diet.input -----------------------------------------------------

usethis::use_data(diet.input, overwrite = T)

