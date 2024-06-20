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
swept <- swept[, sd.area   := (sqrt(tot.bio.var)*.001)/(Fall.q*GB.area)]

#add biomasses (relevant for aggregate groups) -------------------------------
setkey(swept,RPATH,YEAR)
biomass_fit <- swept[, sum(biomass.area), by = key(swept)]
setnames(biomass_fit, 'V1','Biomass')

#average stderror (relevant for aggregate groups) -----------------------------
#not correcting for area because that shouldn't matter - right??
setkey(swept,RPATH,YEAR)
sd_fit<-swept[,mean(sd.area), by = key(swept)]
setnames(sd_fit,'V1','Stdev')

#merge biomass with SD --------------------------------------------------------
biomass_fit<-cbind(biomass_fit,sd_fit$Stdev)
setnames(biomass_fit, 'V2','Stdev')

#subset for 1985-2019 ---------------------------------------------------------
biomass_fit<-biomass_fit[YEAR %in% 1985:2019]

#drop units for plotting purposes ---------------------------------------------
biomass_fit$Biomass <- drop_units(biomass_fit$Biomass)
biomass_fit$Stdev <- drop_units(biomass_fit$Stdev)

biomass_fit<-biomass_fit[!RPATH %in% c(NA, 'Fauna','Freshwater','SouthernDemersals')]

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
#set biomass as index or absolute as appropriate
biomass_adjust$Type <- rep("absolute",length(biomass_adjust$Group))
biomass_adjust$Scale <- rep(1,length(biomass_adjust$Group))
biomass_fit$Type <- rep("index",length(biomass_adjust$Group))
biomass_fit$Scale <- rep(1,length(biomass_adjust$Group))

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
