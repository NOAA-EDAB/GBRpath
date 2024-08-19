#Title: GB Rpath Time Series Fitting

# Purpose: This script fits the Georges Bank Rpath model 
#           to time series data from 1985-2019

# DataFiles: 'landings_fit.csv'

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/fitting.R

#Load packages ----------------------------------------------------------------------------
# remotes::install_github('NOAA-EDAB/Rpath', ref='fit_beta', force = T)
library(Rpath); library(data.table);library(dplyr);library(here)

# #Pull in code from GitHub
# library(devtools)
# source_url('https://github.com/NOAA-EDAB/Rpath/blob/fit_alpha/R/ecofitting.R')

#Create fitting functions
# source(here("fitting/ecofitting.R"))

#Load balanced model and params  ----------------------------------------------------------
load(here("data/alternate.GB.bal.rda"))
load(here("data/alternate.GB.params.bal.rda"))

GB <- alternate.GB.bal 
GB.params <- alternate.GB.params.bal
  
#define fit years and files  ---------------------------------------------------------------
fit.years <- 1985:2019

catch.datafile<- paste("fitting/take_fit.csv",sep = "")
#could also accomplish this by running catch_time.R script
biomass.datafile  <- paste("fitting/biomass_fit.csv",sep='')

# Setup Base Ecopath and Base Rsim scenario ------------------------------------------------
basescene85 <- rsim.scenario(GB, GB.params, years = fit.years)
basescene85$params$NoIntegrate[57:58] <- 0
# Done for Bacteria and LgCopepods
scene0 <- basescene85

# Read in fitting data --------------------------------------------------------------------
# Biomass data (e.g. surveys)
scene0 <- read.fitting.biomass(scene0, biomass.datafile)

# Read time series of catch data and re-arrange catch forcing
scene0 <- read.fitting.catch(scene0, catch.datafile) 
# Apply the fit catch as a forcing catch
scene0 <- fitcatch.to.forcecatch(scene0)
# Turn off fishing effort, freeze discards/offal using forced biomass
scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GB), fit.years, value=0.0)
#Turn off catch forcing for groups with high uncertainty
scene0 <- adjust.fishing(scene0, "ForcedCatch", c("OtherShrimps"), fit.years, value=0.0)
scene0$fitting$Catch[which(scene0$fitting$Group %in% c("OtherShrimps"))]<-0
#scene0 <- adjust.fishing(scene0, "ForcedEffort", rpath.gears(GOM), fit.years, value=1.0)
#scene0$forcing$ForcedBio[,"Discards"] <- GOM$Biomass["Discards"]

# For species without catch, reapply Ecopath F (originally through gears) to ForcedFRate
F_equil <- rowSums(GB$Landings)  /(GB$Biomass)  #+ rowSums(GOM$Discards))


Equil_species <- c('SeaBirds', 'Pinnipeds', 'BaleenWhales', 'Odontocetes', 'SmPelagics',
                   'Mesopelagics', 'SmFlatfishes', 'Barndoor', 'Krill', 'Micronekton', 
                   'GelZooplankton', 'Microzooplankton', 'Bacteria', 'LgCopepods', 
                   'SmCopepods','Phytoplankton')

for (sp in Equil_species){
  scene0 <- adjust.fishing(scene0, 'ForcedFRate', sp, fit.years, value=0)
}



# Run model with no fitting data ----------------------------------------------------------
run0 <- rsim.run(scene0, method='AB', years=fit.years)

# Some Diagnostics
rsim.fit.table(scene0,run0)

# Changes from base run -------------------------------------------------------------------

## Add lower trophic level forcing -----------------------------------------
### force phytoplankton biomass from 2001 onwards --------------------------------
source(here("fitting/Phyto_time.R"))
pp_force <- pp_force  |>  filter(Year < 2020)
scene0<-adjust.forcing(scene0,"ForcedBio","Phytoplankton",
                       sim.year = 2001:2019,bymonth = F,value=pp_force$force_b)

### force copepod biomass --------------------------------------------
source(here("fitting/copepods_time.R"))
scene0<-adjust.forcing(scene0,"ForcedBio","SmCopepods",
                       sim.year = sm$Year,bymonth = F,value=sm$force_b)
scene0<-adjust.forcing(scene0,"ForcedBio","LgCopepods",
                       sim.year = lg$Year,bymonth = F,value=lg$force_b)

## Species to test  -----------------------------------------------------------------------
test_sp <- c('Pollock', 'WhiteHake', 'Redfish', 'OceanPout', 'SpinyDogfish', 'YTFlounder', 
             'AmPlaice','Windowpane', 'WinterFlounder', 
             'LittleSkate', 'Barndoor', 'OtherSkates')

# test_sp <- c('Cod', 'Haddock', 'YTFlounder', 'WhiteHake', 'Windowpane', 'WinterFlounder', 
#              'Redfish', 'OceanPout', 'Barndoor', 'OtherSkate')

index_sp<-c('Cod', 'Haddock', 'AtlScallop', 'AtlHerring', 'AtlMackerel', 'WinterSkate', 
            'WitchFlounder', 'Goosefish', 'BlackSeaBass', 'Fourspot', 'SummerFlounder')

data_type <- "index"  #"absolute"

## run active respiration forcing -----------------------------------------------------------
source(here("fitting/act_resp_force.R"))
pp_force <- pp_force  |>
              filter(Year < 2020)
scene0<-adjust.forcing(scene0,"ForcedBio","Phytoplankton",sim.year = 2001:2019,
                       bymonth = F,value=pp_force$force_b)

## Benthic index ----------------------------------------------------------------------------
source(here("fitting/benthic_time.R"))

scene0<-adjust.forcing(scene0,"ForcedBio","Macrobenthos",
                       sim.year = macro_time$Time,bymonth = F,value=macro_time$biomass)
scene0<-adjust.forcing(scene0,"ForcedBio","Megabenthos",
                       sim.year = mega_time$Time,bymonth = F,value=mega_time$biomass)

## Set weights for fitting data -----------------------------------------------------------

### All weights to 1 -----------------------------------------------------------------------
# Doing this to make between scenario comparisons more straight forward
# come back to weighting options once I have an 'optimum' scenario

# scene0$fitting$Biomass$wt[] <- 1
# scene0$fitting$Catch$wt[]   <- 1


### Weighting option 1 --------------------------------------------------------------------
# Set data weightings for all data input low (zeros not allowed)
scene0$fitting$Biomass$wt[] <- 1e-36
scene0$fitting$Catch$wt[]   <- 1e-36

# Set data weighting for species to fit

# scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% c("'Cod', 'Haddock', 'YTFlounder', 'Pollock', 'AmPlaice', 'WitchFlounder',
#              'WhiteHake', 'Windowpane', 'WinterFlounder', 'Redfish', 'OceanPout',
#              'WinterSkate', 'LittleSkate', 'Barndoor', 'OtherSkates', 'Goosefish',
#              'SpinyDogfish')] <- 1

scene0$fitting$Biomass$wt[scene0$fitting$Biomass$Group %in% test_sp] <- 1


### Weighting option 2 --------------------------------------------------------------------
# Trying weight based on pedigree value
# inverse so lower pedigree values are given higher weight
# group.wt <- GB.params$pedigree |>
#               select(Group,Biomass,Trap) |>
#               mutate(B.wt = 1/Biomass) |>
#               mutate(C.wt = 1/Trap)
# 
# # set Biomass weight
# scene0$fitting$Biomass$wt <- group.wt$B.wt[match(scene0$fitting$Biomass$Group,group.wt$Group)]

# # set Catch weight
# scene0$fitting$Catch$wt <- group.wt$C.wt[match(scene0$fitting$Catch$Group,group.wt$Group)]


## Set data type for index species ---------------------------------------------------------
scene0$fitting$Biomass$Type[scene0$fitting$Biomass$Group %in% index_sp] <- data_type
scene0$fitting$Biomass$Type[!(scene0$fitting$Biomass$Group %in% index_sp)] <- "absolute"

## Set variables to allow to change --------------------------------------------------------
# all combined
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
# fit_values   <- c(rep(0.2,length(test_sp)),rep(0.02,length(test_sp)),rep(0.02,length(test_sp)))
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

#Initial fit -----------------------------------------------------------------------------
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)
par(mfrow=c(2,2))
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, run0, test_sp[i])
  rsim.plot.catch(scene0, run0, test_sp[i])
}

# Plot biomass for all species, not just test species
all_sp <- GB.params$pedigree$Group

par(mfrow=c(2,2))
for (i in 1:length(all_sp)){
  rsim.plot.biomass(scene0, run0, all_sp[i])
  rsim.plot.catch(scene0, run0, all_sp[i])
}

# Run optimization ------------------------------------------------------------------------
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

# Analysis of fit -------------------------------------------------------------------------



# All species biomass
par(mfrow=c(2,2))
for(i in 1:length(all_sp)){
rsim.plot.biomass(scene0, fit.final, all_sp[i])
}
par(mfrow=c(2,2))
# all species catch
for (i in 1:length(all_sp)) {
  rsim.plot.catch(scene0, fit.final, all_sp[i])
}

## Save pdf of plots ---------------------------------------------------------------------
# # Save all plots to a single PDF file
# pdf(file = "fitting/plots/scen11_forcePhyto_forceCopes_ActResp_forceBenthic_B.pdf")
# 
# par(mfrow=c(3,3))
# # Loop through all species and create plots
# for (i in 1:length(all_sp)) {
# rsim.plot.biomass(scene0, fit.final, all_sp[i])
# }
# 
# # Close the PDF file
# dev.off()
# 
# # Save all plots to a single PDF file
# pdf(file = "fitting/plots/scen11_forcePhyto_forceCopes_ActResp_forceBenthic_C.pdf")
# 
# par(mfrow=c(3,3))
# # Loop through all species and create plots
# for (i in 1:length(all_sp)) {
#   rsim.plot.catch(scene0, fit.final, all_sp[i])
# }
# 
# # Close the PDF file
# dev.off()



## Sum contribution to sum of squares -----------------------------------------------------
# Can only compare across scenarios when all weights set to 1

# final.fits <- rsim.fit.obj(scene0, fit.final)
# 
# final.B.fits <- final.fits$Biomass |> 
#                   dplyr::group_by(Group) |>
#                   dplyr::summarize(SS = sum(fit))
# B.ss <- sum(final.B.fits$SS)
