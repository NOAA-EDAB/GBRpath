#Georges Bank Rpath model
#Expanded model of Georges Bank
#SML
# Updated by MTG 03/01/2024 and SJW 8/1/24

# Folding Discards into Detritus ----

# Removing discards as a separate group so as to simplify
# network analyses. Discard fate = detritus.



#Required packages--------------------------------------------------------------
library(data.table); library(Rpath); library(here); library(readr); library(dplyr)

#Georges Bank
# Define Groups ---------------------------------------------------------------
groups <- c('SeaBirds', 'Pinnipeds', 'BaleenWhales', 'Odontocetes', 'HMS', 'Sharks', 
            'AtlHerring', 'AtlMackerel', 'RiverHerring', 'Butterfish', 
            'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 'Haddock', 
            'Goosefish', 'SilverHake', 'RedHake', 'WhiteHake', 
            'Redfish', 'Pollock', 'OceanPout', 'BlackSeaBass', 'Bluefish', 'Scup',
            'OtherDemersals', 'SouthernDemersals', 'Fourspot', 'SummerFlounder',
            'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
            'YTFlounder', 'SmFlatfishes', 'SpinyDogfish', 
            'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
            'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 'AmLobster',
            'Macrobenthos', 'Megabenthos', 'AtlScallop', 'OceanQuahog', 'SurfClam',
            'OtherShrimps', 'Krill', 'Micronekton', 'GelZooplankton', 
            'Microzooplankton', 'Bacteria', 'LgCopepods', 'SmCopepods', 'Phytoplankton',
            'Detritus', #'Discards', 
            'ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
            'Trap', 'SmallMesh', 'LargeMesh', 'HMSFleet') #, 'OtherFisheries')

types <- c(rep(0, 58), 1, 2, #2, 
           rep(3, 9))

GB.params <- create.rpath.params(groups, types)


#Enter BioAcc, Unassim, DetInput, and detrital fate
GB.params$model[Type < 3,  BioAcc := 0] 
#There are some BA terms in BA.input.rda if needed to balance
GB.params$model[Type == 0, Unassim := 0.2]
GB.params$model[Type > 0 & Type < 3, Unassim := 0]
GB.params$model[Type == 2, DetInput := 0]
GB.params$model[Type <= 1,  Detritus := 1]
GB.params$model[Type == 2,  Detritus := 0]
GB.params$model[Type == 3, Detritus := 1]
# GB.params$model[Type < 3,  Discards := 0]

# Biomass and EE --------------------------------------------------------------
#Load biomass
load(here('data', 'bio.input.rda'))

for(igroup in GB.params$model[Type < 2, Group]){
  GB.params$model[Group == igroup, Biomass := bio.input[RPATH == igroup, B]]
}

# update values for Large and Small Copepods from EMAX_copepod_params
load(here('data', 'EMAX_copepod_params.rda'))
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), Biomass := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(biomass)]]

#Add EEs for groups without biomass
#Other flatfish biomass shows up as zero so set to NA and let the model calculate

# OtherFlatfish now merged with Otherdemersals
# GB.params$model[Group == 'OtherFlatfish', Biomass := NA]
# GB.params$model[Group == 'OtherFlatfish', EE := 0.8]

#Originally had Seals, PP and Bacteria set to EE but I added EMAX value
#Add NWACS value for pinnepeds
GB.params$model[Group %in% c('Pinnipeds'), Biomass := 0.035]

#Biological Parameters --------------------------------------------------------
load(here('data', 'bioparam.input.rda'))

for(igroup in bioparam.input[, RPATH]){
  GB.params$model[Group == igroup, PB := bioparam.input[RPATH == igroup, PB]]
  GB.params$model[Group == igroup, QB := bioparam.input[RPATH == igroup, QB]]
}

# update values for Large and Small Copepods from EMAX_copepod_params
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), PB := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(pb)]]
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), QB := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(qb)]]


#Phytoplankton needs to be NA
GB.params$model[Group == 'Phytoplankton', QB := NA]


#Landings and Discards --------------------------------------------------------
load(here('data', 'land.input.rda'))

#Need to account for mismatched names
# No catch from "OtherFisheries" so not included
# commented out rather than deleted in case it is needed later
rfleets <- c('ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
             'Trap', 'SmallMesh', 'LargeMesh' , 'HMSFleet') #, 'OtherFisheries') 
rgear   <- c('Scallop Dredge', 'Clam Dredge', 'Other Dredge', 'Fixed Gear', 
             'Pelagic', 'Trap', 'SM Mesh','LG Mesh', 'HMS') #, 'Other') 

for(igear in 1:length(rfleets)){
  gear.landings <- land.input[Fleet == rgear[igear], ]
  setnames(GB.params$model, rfleets[igear], 'gear')
  for(igroup in 1:nrow(gear.landings)){
    GB.params$model[Group == gear.landings[igroup, RPATH], 
                    gear := gear.landings[igroup, Landings]][]
  }
  setnames(GB.params$model, 'gear', rfleets[igear])
}

#Load discards
load(here('data', 'disc.input.rda'))

for(igear in 1:length(rfleets)){
  gear.disc <- disc.input[Fleet == rgear[igear], ]
  setnames(GB.params$model, paste0(rfleets[igear], '.disc'), 'gear')
  #Discards
  for(igroup in 1:nrow(gear.disc)){
    GB.params$model[Group == gear.disc[igroup, RPATH],
                    gear := gear.disc[igroup, Discards]][]
  }
  setnames(GB.params$model, 'gear', paste0(rfleets[igear], '.disc'))
}

# Diet ------------------------------------------------------------------------
load(here('data', 'diet.input.rda'))

# Change predation on discards to detritus
diet.input[Rprey == 'Discards', Rprey := 'Detritus']
diet.input <- diet.input  |> 
  group_by(Rpred, Rprey) |>  # Group by predator and prey
  summarise(preyper = sum(preyper))  |>   # Sum prey consumed per predator-prey pair
  ungroup() |>                     # Remove grouping
  distinct(Rpred, Rprey, .keep_all = TRUE)  # Keep all columns, remove duplicates

preds <- (distinct(diet.input, Rpred)$Rpred)
#remove OffHake from preds and diet.input
preds <- preds[preds != 'OffHake']
diet.input <- diet.input |> 
                filter(Rpred != 'OffHake')

for(ipred in 1:length(preds)){
  setnames(GB.params$diet, preds[ipred], 'Pred')
  pred.diet <- filter(diet.input, Rpred == preds[ipred])
  prey <- filter(pred.diet)$Rprey
  for(iprey in 1:length(prey)){
    GB.params$diet[Group == prey[iprey], 
                   Pred := filter(pred.diet, Rprey == prey[iprey])$preyper]
  }
  setnames(GB.params$diet, 'Pred', preds[ipred])
  GB.params$diet[]
}


check.rpath.params(GB.params)
usethis::use_data(GB.params, overwrite = T)

#Initial unbalanced model

GB.init <- rpath(GB.params, 'Georges Bank', 1)



usethis::use_data(GB.init, overwrite = T)

