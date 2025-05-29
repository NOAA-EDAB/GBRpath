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
load(here('data/disc.index.rda'))

#load list of groups included in model
load(here('data-raw/Species_codes.Rdata'))

#code below modified from landings_conversion.R
land.index[Fleet =="HMS",Fleet:="HMS Fleet"]
disc.index[Fleet =="HMS",Fleet:="HMS Fleet"]

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

disc.index <- disc.index |>  
  group_by(Fleet,RPATH,YEAR) |>  
  summarise(Discards=sum(Discards)) |> 
  ungroup()

#sum all landings per group ---------------------------------------------------
spp.land <- land.index |>  
  group_by(RPATH,YEAR) |>  
  summarise(Landings=sum(Landings)) |> 
  ungroup()

spp.discard <- disc.index |>  
  group_by(RPATH,YEAR) |>  
  summarise(Discards=sum(Discards)) |> 
  ungroup()

#remove groups not in the model -----------------------------------------------
load(here('data/alternate.GB.bal.rda'))
GBRpath_species <- alternate.GB.bal[["Group"]]

spp.land <- spp.land |> 
  filter(RPATH %in% GBRpath_species)

spp.discard <- spp.discard |>
  filter(RPATH %in% GBRpath_species)

#sum catch and discards by species for total take value -----------------------

spp.take <- spp.land |> 
  left_join(spp.discard, by = c("RPATH", "YEAR"))  |> 
  mutate(Take = coalesce(Landings, 0) + coalesce(Discards, 0))  |> 
  select(RPATH, YEAR, Take)

take.index <- disc.index |> 
  left_join(land.index, by = c("Fleet","RPATH", "YEAR"))  |>
  mutate(Take = coalesce(Discards, 0) + coalesce(Landings, 0))  |>
  select(Fleet,RPATH, YEAR, Take)

#rename columns to comply with fitting code -----------------------------------
colnames(spp.take)<-c("Group", "Year", "Value")
colnames(take.index)<-c("Fleet", "Group", "Year", "Value")

colnames(spp.land)<-c("Group", "Year", "Landings")
colnames(land.index)<-c("Fleet", "Group", "Year", "Value")

#add sd, scale columns --------------------------------------------------------
spp.take<-as.data.table(spp.take)
spp.take[,Stdev := Value * 0.1]
spp.take[,Scale := rep(1,length(spp.take$Value))]

take.index<-as.data.table(take.index)
take.index[,Stdev := Value * 0.1]
take.index[,Scale := rep(1,length(take.index$Value))]

#visualize landings trends over time ------------------------------------------
ggplot(spp.take,aes(x=Year, y = Value)) +
  geom_point() +
  facet_wrap(vars(Group),ncol = 4, scales = "free")

spp.take |> 
  filter(Group == "SurfClam") |>
  ggplot(aes(x=Year, y = Value)) +
  geom_point()

spp.take |> 
  filter(Group == "OceanQuahog") |>
  ggplot(aes(x=Year, y = Value)) +
  geom_point()

# Compare landings to take
spp.compare <- spp.take |> 
  left_join(spp.land, by = c("Group", "Year")) |> 
  select(Group, Year, Value, Landings)
colnames(spp.compare) <- c("Group", "Year", "Take", "Landings")

# Cod
spp.compare |> 
  filter(Group == "Cod") |>
  ggplot(aes(x=Year, y = Take)) +
  geom_point() +
  geom_point(aes(x=Year, y = Landings), color = "red")

# all species, facet wrap by Group
spp.compare |> 
  ggplot(aes(x=Year, y = Take)) +
  geom_point() +
  geom_point(aes(x=Year, y = Landings), color = "red") +
  facet_wrap(vars(Group),ncol = 4, scales = "free")

#AtlScallop

#add a column called 'catch?'

#save file
write.csv(spp.land,"fitting/landings_fit.csv")
write.csv(land.index,"fitting/landings_by_gear_fit.csv")

write.csv(spp.take,"fitting/take_fit.csv")
write.csv(take.index,"fitting/take_by_gear_fit.csv")
