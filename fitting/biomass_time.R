# Purpose: This script summarizes biomass in t/km^2
#           for all RPath groups per year from 1986-2019.
#           Used to generate data for time series fitting routine

# DataFiles: 'NEFSC_BTS_2021_all_seasons.RData'

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/biomass_time.R

#Load required packages -------------------------------------------------------
library(here);library(data.table);library(survdat); 
library(units); library(ggplot2); library(dplyr); library(tidyr)

#Load survey data -------------------------------------------------------------
load(here('data/NEFSC_BTS_2021_all_seasons.RData'))

#Load species codes -----------------------------------------------------------
#Use these codes to translate survey species codes ('SVSPP') to RPATH species codes ('spp')
load(here('data-raw/Species_codes.Rdata'))

#Clam index -------------------------------------------------------------------
load(here('data', 'survdatClams.RData'))

#Use GB clam region to calculate biomass
# clam.index <- clams$data[!is.na(SVSPP) & clam.region == 'GB', 
#                          .(B = mean(BIOMASS.MW, na.rm = T)), by = c('YEAR', 'SVSPP')]

clam.index <- clams$data |> 
      filter(!is.na(SVSPP), clam.region == 'GB') |> 
  group_by(YEAR, SVSPP) |> 
  summarise(Biomass = mean(BIOMASS.MW, na.rm = T), Stdev = sd(BIOMASS.MW, na.rm = T)) |> 
  ungroup()

#Need to expand from kg/tow to mt/km^2
# Clam tows can vary greatly by I'll use an example tow as the expansion
# 0.0039624 (dredge width in km) * 0.374(tow length in km) = 0.00148 
#kg to mt is 0.001
# so conversion is 0.001 / 0.00148 or 0.6757
# clam.index[, B := B * 0.6757]
# clam.index[, SD := SD * 0.6757]
# clam.index[, Units := 'mt km^-2']
# clam.index$RPATH <- ifelse(clam.index$SVSPP == 409, 'OceanQuahog','SurfClam')

clam.index <- clam.index |> 
  mutate(Biomass = Biomass*0.6757) |> 
  mutate(Stdev = Stdev*0.6757) |>
  mutate(RPATH = ifelse(SVSPP == 409, 'OceanQuahog', 'SurfClam')) |> 
  select(-SVSPP)

#clam.index[, RPATH := 'Clams']

# create weight
clam.index <- clam.index |> 
                  mutate(CV = Stdev / Biomass) |> 
                  group_by(RPATH) |>
                  mutate(mean_CV = mean(CV, na.rm = T)) |> 
                  mutate(weight = 1 / mean_CV) |> 
                  dplyr::ungroup()

#Scallop index ---------------------------------------------------------------
#Scallops and clam survey not included in ms-keyrun data set as they are not 
#used in the other models
library(DBI); library(sf); library(survdat)

#Connect to the database
# channel <- dbutils::connect_to_database('sole', 'slucey')

#scall <- survdat::get_survdat_scallop_data(channel, getWeightLength = T)
load(here::here('data', 'survdatScallops.RData'))

#Scallop survey did not record weight prior to 2001 (FSCS) so need to manually
#calculate catch weights
scalldat <- scallops$survdat[, BIOMASS := sum(WGTLEN), by = c('YEAR', 'STATION')]

scalldat <- scallops$survdat |> 
  group_by(YEAR, STATION) |> 
  dplyr::mutate(Stdev = sd(WGTLEN, na.rm = T)) |>
  dplyr::mutate(Biomass = sum(WGTLEN)) |> 
  dplyr::ungroup()

#Calculate scallop index
#use poststrat to assign to EPU
epu <- sf::st_read(dsn = here::here('data-raw','gis', 'EPU_extended.shp'))

scall.mean <- survdat::calc_stratified_mean(scalldat, areaPolygon = epu,
                                            areaDescription = 'EPU',
                                            filterByArea = 'GB',
                                            filterBySeason = 'SUMMER', tidy = T)

scall.index <- scall.mean[variable == 'strat.biomass', .(Biomass = value), by = YEAR]
#Need to expand from kg/tow to mt/km^2
#A tow is approximately 0.0045 km^2 
# 0.001317 (dredge width in nautical miles) * 1.852(convert naut mi to km)
# 1.0 (tow length in nautical miles) * 1.852(convert naut mi to km)
#kg to mt is 0.001
# so conversion is 0.001 / 0.0045 or 0.222
scall.index[, Biomass := Biomass * 0.222]
# scall.index[, Biomass := NULL]
scall.index[, Units := 'mt km^-2']
scall.index[, RPATH := 'AtlScallop']

#Calculate total GB area ------------------------------------------------------
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GB.area<-subset(area, area$STRATUM %in% c(1090, 1130:1210, 1230, 1250, 3460, 3480, 
                                          3490, 3520:3550))
GB.area<-sum(GB.area$Area)

#Calculate swept area biomass -------------------------------------------------
swept<-calc_swept_area(surveyData=survey$survdat, areaPolygon = 'NEFSC strata', 
                       areaDescription = 'STRATA', 
                       filterByArea = c(1090, 1130:1210, 1230, 1250, 3460, 3480, 
                                        3490, 3520:3550), 
                       filterBySeason= "FALL", 
                       groupDescription = "SVSPP", 
                       filterByGroup = "all", 
                       mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)

#Merge with RPATH names -------------------------------------------------------
#RPATH names are the same as in Georges Bank model
#Aggregate groups below 0.05 t/km^2 threshold
spp <- spp[!duplicated(spp$SVSPP),] #SVSPPs are NEFSC species-specific codes
spp <- spp[RPATH == 'RedCrab', RPATH := 'Macrobenthos']
# spp <- spp[RPATH == 'Clams', RPATH := 'Megabenthos']
# spp <- spp[RPATH == 'Scup', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OffHake', RPATH := 'SilverHake']
# spp <- spp[RPATH == 'Bluefish', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'AmShad', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'SmallPelagics', RPATH := 'SmPelagics']
spp <- spp[RPATH == 'OtherShrimp', RPATH := 'OtherShrimps']
spp <- spp[RPATH == 'AtlCroaker', RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'LargePelagics', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'StripedBass', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'Sturgeon', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Weakfish', RPATH := 'OtherDemersals']

#Merge swept area biomass with RPATH names ------------------------------------
swept<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

#Calcuate biomass, sd / area in mt  -------------------------------------
swept <- swept[, biomass.area   := (tot.biomass*.001)/(Fall.q*GB.area)]
# replace 0 variance with NA
swept <- swept[, tot.bio.var := ifelse(tot.bio.var == 0, NA, tot.bio.var)]
swept <- swept[, sd.area   := (sqrt(tot.bio.var)*.001)/(Fall.q*GB.area)]
# remove NAs
swept <- swept |> 
  filter(!is.na(sd.area))

#add biomasses (relevant for aggregate groups) -------------------------------
setkey(swept,RPATH,YEAR)
biomass_fit <- swept[, sum(biomass.area), by = key(swept)]
setnames(biomass_fit, 'V1','Biomass')

#average stderror (relevant for aggregate groups) -----------------------------
#not correcting for area because that shouldn't matter - right??
setkey(swept,RPATH,YEAR)


sd_fit<-swept[,mean(sd.area, na.rm = T), by = key(swept)]
setnames(sd_fit,'V1','Stdev')


#merge biomass with SD --------------------------------------------------------
biomass_fit<-cbind(biomass_fit,sd_fit$Stdev)
setnames(biomass_fit, 'V2','Stdev')

#subset for 1985-2019 ---------------------------------------------------------
biomass_fit<-biomass_fit[YEAR %in% 1985:2022]

#drop units for plotting purposes ---------------------------------------------
biomass_fit$Biomass <- drop_units(biomass_fit$Biomass)
biomass_fit$Stdev <- drop_units(biomass_fit$Stdev)


# biomass_fit<-biomass_fit[!RPATH %in% c(NA, 'Fauna','Freshwater','SouthernDemersals')]

#remove groups not in the model -----------------------------------------------
load(here('data/alternate.GB.bal.rda'))
GBRpath_species <- alternate.GB.bal[["Group"]]

biomass_fit <- biomass_fit |> 
  filter(RPATH %in% GBRpath_species)

#replace bts data with clam and scallop data ----------------------------------
biomass_fit <- biomass_fit |> 
                   filter(RPATH != 'SurfClam') |> 
                   filter(RPATH != 'OceanQuahog') |>
                   filter(RPATH != 'AtlScallop')

clam.index.bind <- clam.index |> 
  filter(YEAR %in% 1985:2022) |> 
  select(YEAR, RPATH, Biomass, Stdev)

scall.index.bind <- scall.index |>
  filter(YEAR %in% 1985:2022) |>
  select(YEAR, RPATH, Biomass) |> 
  mutate(Stdev = NA)

biomass_fit <- rbind(biomass_fit, clam.index.bind, scall.index.bind)

#adjust biomass values for changes made during balancing -----------------------

#pull initial estimates
load(here("data/bio.input.rda"))
#pull estimates from balanced model
load(here( "data/alternate.GB.params.bal.rda"))
biomass_80s_balanced <- as.data.frame(cbind(alternate.GB.params.bal$model$Group[1:60], 
                                            alternate.GB.params.bal$model$Biomass[1:60]))
colnames(biomass_80s_balanced)<-c("RPATH","Biomass_balanced")

#merge
ratio<-left_join(bio.input,biomass_80s_balanced,by="RPATH")
ratio$Biomass_balanced<-as.numeric(ratio$Biomass_balanced) #why does this happen??
ratio<-ratio |> 
        mutate(ratio = Biomass_balanced/B) |> 
        dplyr::select(RPATH,ratio)

#add adjusted biomass column ---------------------------------------------------- 
biomass_adjust<- left_join(biomass_fit,ratio,by="RPATH")  |>  replace_na(list(ratio=1))
biomass_adjust <- biomass_adjust |> mutate(Value = Biomass*ratio)

#rename columns to comply with fitting code -------------------------------------
biomass_adjust <- biomass_adjust  |> 
                    rename(Group = RPATH, Year = YEAR)  |> 
                    dplyr::select(-Biomass, -ratio)
biomass_fit <- biomass_fit  |> 
                  rename(Group = RPATH, Year = YEAR, Value = Biomass)
#set biomass as index or absolute as appropriate --------------------------------
biomass_adjust$Type <- rep("absolute",length(biomass_adjust$Group))
biomass_adjust$Scale <- rep(1,length(biomass_adjust$Group))
biomass_fit$Type <- rep("index",length(biomass_adjust$Group))
biomass_fit$Scale <- rep(1,length(biomass_adjust$Group))

#set weights for fitting --------------------------------------------------------
biomass_weights <- biomass_fit |>
                      # mutate(Stdev = replace(Stdev,
                      #                        Stdev == 0, NA)) |> 
                      mutate(CV = Stdev / Value) |> 
                      group_by(Group) |>
                      mutate(mean_CV = mean(CV, na.rm = T)) |> 
                      mutate(weight = 1 / mean_CV) |> 
                      dplyr::ungroup()

group_B_weights <- biomass_weights |> 
                      select(Group, weight) |> 
                      distinct()

#save file
write.csv(biomass_adjust,"fitting/biomass_fit.csv")
write.csv(biomass_fit,"fitting/biomass_fit_index.csv")

#visualize biomass trends over time
ggplot(biomass_adjust,aes(x=Year, y = Value)) +
  geom_point() +
  facet_wrap(vars(Group),ncol = 4, scale = "free")

# Just Cod
ggplot(biomass_adjust[Group == "Cod",],aes(x=Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Cod Biomass on Georges Bank",
       x = "Year",
       y = "Biomass (t/km^2)")

# Just Haddock
ggplot(biomass_adjust[Group == "Haddock",],aes(x=Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Haddock Biomass on Georges Bank",
       x = "Year",
       y = "Biomass (t/km^2)")

# AtlScallop
ggplot(biomass_adjust[Group == "AtlScallop",],aes(x=Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Scallop Biomass on Georges Bank",
       x = "Year",
       y = "Biomass (t/km^2)")

# OceanQuahog
ggplot(biomass_adjust[Group == "OceanQuahog",],aes(x=Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Quahog Biomass on Georges Bank",
       x = "Year",
       y = "Biomass (t/km^2)")

# SurfClam
ggplot(biomass_adjust[Group == "SurfClam",],aes(x=Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "SurfClam Biomass on Georges Bank",
       x = "Year",
       y = "Biomass (t/km^2)")
