# Purpose: This script summarizes catch in t/km^2
#           for all fished RPath groups per year from 1980-2019.
#           Used to generate data for time series fitting routine

# DataFiles: 'commercial_landings_gom_80_19.RData'

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/catch_time.R

#load required packages and data ---------------------------------------------
library(ggplot2); library(here); library(units); library(data.table); 
library(dplyr); library(survdat)

load(here('data/land.index.rda'))

#load list of groups included in model
load(here('data-raw/Species_codes.Rdata'))

#code below modified from landings_conversion.R
land.index[Fleet =="HMS",Fleet:="HMS Fleet"]

#Calculate total GB area ------------------------------------------------------
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
GB.area<-subset(area, area$STRATUM %in% c(1090, 1130:1210, 1230, 1250, 3460, 3480, 
                                          3490, 3520:3550))
GB.area<-sum(GB.area$Area)

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

#5 NESPP3 codes are matched with 2 RPATH codes
#remove duplicate NESPP3 codes
spp<-distinct(spp,NESPP3,.keep_all=TRUE)

#Sum landings for multispecies groups -----------------------------------------
land.index <- land.index |>  
  group_by(Fleet,RPATH,YEAR) |>  
  summarise(Landings=sum(Landings)) |> 
  ungroup()

#sum all landings per group ---------------------------------------------------
spp.land <- land.index |>  
  group_by(RPATH,YEAR) |>  
  summarise(Landings=sum(Landings)) |> 
  ungroup()

#rename columns to comply with fitting code -----------------------------------
colnames(spp.land)<-c("Group", "Year", "Value")
colnames(land.index)<-c("Fleet", "Group", "Year", "Value")

#add sd, scale columns --------------------------------------------------------
spp.land<-as.data.table(spp.land)
spp.land[,Stdev := Value * 0.1]
spp.land[,Scale := rep(1,length(spp.land$Value))]

land.index<-as.data.table(land.index)
land.index[,Stdev := Value * 0.1]
land.index[,Scale := rep(1,length(land.index$Value))]

#visualize landings trends over time ------------------------------------------
ggplot(spp.land,aes(x=Year, y = Value)) +
  geom_point() +
  facet_wrap(vars(Group),ncol = 4, scales = "free")

#add a column called 'catch?'

#save file
write.csv(spp.land,"fitting/landings_fit.csv")
write.csv(land.index,"fitting/landings_by_gear_fit.csv")