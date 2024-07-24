#Title: GB Rpath Time Series Fitting

# Purpose: This script fits the Georges Bank Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'landings_fit.csv'

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/fitting.R

#Load packages
remotes::install_github('NOAA-EDAB/Rpath', ref='fit_beta', force = T)
library(Rpath); library(data.table);library(dplyr);library(here)

# #Pull in code from GitHub
# library(devtools)
# source_url('https://github.com/NOAA-EDAB/Rpath/blob/fit_alpha/R/ecofitting.R')

#Create fitting functions
# source(here("fitting/ecofitting.R"))

#Load balanced model
#Load balanced model
load(here("data/alternate.GB.bal.rda"))
load(here("data/alternate.GB.params.bal.rda"))

GB <- alternate.GB.bal 
GB.params <- alternate.GB.params.bal
  
#define fit years
fit.years <- 1985:2019

catch.datafile<- paste("fitting/landings_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("fitting/biomass_fit.csv",sep='')

# Setup Base Ecopath and Base Rsim scenario
basescene85 <- rsim.scenario(GB, GB.params, years = fit.years)
basescene85$params$NoIntegrate[57:58] <- 0
# Done for Bacteria and LgCopepods
scene0 <- basescene85

# Read in fitting data
# Biomass data (e.g. surveys)
scene0 <- read.fitting.biomass(scene0, biomass.datafile)

# Read time series of catch data and re-arrange catch forcing
scene0 <- read.fitting.catch(scene0, catch.datafile) 
# Apply the fit catch as a forcing catch
scene0 <- fitcatch.to.forcecatch(scene0)
# Turn off fishing effort, freeze discards/offal using forced biomass
scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GB), fit.years, value=0.0)
#Turn off catch forcing for groups with high uncertainty
scene0 <- adjust.fishing(scene0, "ForcedCatch", c("SmFlatfishes","OtherShrimps","Mesopelagics"), fit.years, value=0.0)
scene0$fitting$Catch[which(scene0$fitting$Group %in% c("SmFlatfishes","OtherShrimps","Mesopelagics"))]<-0
#scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GOM), fit.years, value=1.0)
#scene0$forcing$ForcedBio[,"Discards"] <- GOM$Biomass["Discards"]

#run active respiration forcing
# source(here("fitting/act_resp_force.R"))

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- rowSums(GB$Landings)  /(GB$Biomass)  #+ rowSums(GOM$Discards))

Equil_species <- c( "Phytoplankton", "Bacteria", "Microzooplankton", "GelZooplankton", "LgCopepods",
                    "SmCopepods", "Micronekton", "OtherCephalopods", "HMS", "Goosefish", "SeaBirds",
                    "Pinnipeds", "BaleenWhales","Odontocetes")
for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}

#force phytoplankton biomass from 1998 onwards
# source(here("fitting/Phyto_time.R"))
# scene0<-adjust.forcing(scene0,"ForcedBio","Phytoplankton",sim.year = 1998:2019,bymonth = F,value=pp_force$force_b[1:23])

# Run model
run0 <- rsim.run(scene0, method='AB', years=fit.years)

# Some Diagnostics 
rsim.fit.table(scene0,run0)

# Species to test 
test_sp <- c("Cod",'Haddock', 'YTFlounder','Pollock', 'AmPlaice', 'WitchFlounder',
             'WhiteHake', 'Windowpane', 'WinterFlounder', 'Redfish', 'OceanPout', 
             'Barndoor', 'WinterSkate', 'LittleSkate', 'OtherSkates')
index_sp<-c("Goosefish","WitchFlounder","YTFlounder","Fourspot","WinterFlounder")
#maybe include "SilverHake"
data_type <- "index"  #"absolute"

# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36

# Set data type for test species
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group %in% index_sp] <- data_type
scene0$fitting$Biomass$Type[!(scene0$fitting$Biomass$Group %in% index_sp)] <- "absolute"

# Set data weighting for species to fit
# scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("Haddock","Redfish")] <- 1
scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("Cod",'Haddock', 'YTFlounder','Pollock', 'AmPlaice', 'WitchFlounder',
                                                              'WhiteHake', 'Windowpane', 'WinterFlounder', 'Redfish', 'OceanPout',
                                                              'Barndoor', 'WinterSkate', 'LittleSkate', 'OtherSkates')] <- 1

# all combined
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
# fit_values   <- c(rep(0.2,length(test_sp)),rep(0.02,length(test_sp)),rep(0.02,length(test_sp)))
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

#Initial fit
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)
par(mfrow=c(1,1))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, run0, test_sp[i])
  # rsim.plot.catch(scene0, run0, test_sp[i])
}

# Run optimization
fit.optim    <- optim(fit_values, rsim.fit.run, #lower=0, #upper=3, 
                      species=fit_species, vartype=fit_vartype, scene=scene0,   
                      run_method='AB', years=fit.years) 
out_values <- fit.optim$par

data.frame(fit_vartype,fit_species,fit_values,out_values)
fit.final  <- rsim.fit.run(out_values, fit_species, fit_vartype, scene0, verbose=T,
                           run_method='AB', years=fit.years) 
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.final, test_sp[i])
  rsim.plot.catch(scene0, fit.final, test_sp[i])
}


