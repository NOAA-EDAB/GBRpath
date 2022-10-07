#Georges Bank Rpath model
#Expanded model of Georges Bank
#SML

#Required packages--------------------------------------------------------------
library(data.table); library(Rpath); library(here)

#Georges Bank
groups <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 'Sharks', 
            'AtlHerring', 'AtlMackerel', 'RiverHerring', 'Butterfish', 
            'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 'Haddock', 
            'Goosefish', 'OffHake', 'SilverHake', 'RedHake', 'WhiteHake', 
            'Redfish', 'Pollock', 'OceanPout', 'BlackSeaBass', 'Bluefish', 'Scup',
            'OtherDemersals', 'SouthernDemersals', 'Fourspot', 'SummerFlounder',
            'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
            'YTFlounder', 'OtherFlatfish', 'SmFlatfishes', 'SpinyDogfish', 
            'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
            'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 'AmLobster',
            'Macrobenthos', 'Megabenthos', 'AtlScallop', 'Clams', 'OtherShrimps',
            'Krill', 'Micronekton', 'GelZooplankton', 'Mesozooplankton', 
            'Microzooplankton', 'Bacteria', 'Phytoplankton',
            'Detritus', 'Discards', 
            'ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
            'Trap', 'SmallMesh', 'LargeMesh', 'HMSFleet', 'OtherFisheries')

types <- c(rep(0, 58), 1, 2, 2, rep(3, 10))

GB.params <- create.rpath.params(groups, types)

#Enter BioAcc, Unassim, DetInput, and detrital fate
GB.params$model[Type < 3,  BioAcc := 0] 
#There are some BA terms in BA.input.rda if needed to balance
GB.params$model[Type == 0, Unassim := 0.2]
GB.params$model[Type > 0 & Type < 3, Unassim := 0]
GB.params$model[Type == 2, DetInput := 0]
GB.params$model[Type < 2,  Detritus := 1]
GB.params$model[Type > 1,  Detritus := 0]
GB.params$model[Type == 3, Discards := 1]
GB.params$model[Type < 3,  Discards := 0]

#Load biomass
load(here('data', 'bio.input.rda'))

for(igroup in GB.params$model[Type < 2, Group]){
  GB.params$model[Group == igroup, Biomass := bio.input[RPATH == igroup, B]]
}

#Add EEs for groups without biomass
#Originally had Seals, PP and Bacteria set to EE but I added EMAX value
#Add NWACS value for pinnepeds
GB.params$model[Group %in% c('Seals'), Biomass := 0.035]

#Biological Parameters
load(here('data', 'bioparam.input.rda'))

for(igroup in bioparam.input[, RPATH]){
  GB.params$model[Group == igroup, PB := bioparam.input[RPATH == igroup, PB]]
  GB.params$model[Group == igroup, QB := bioparam.input[RPATH == igroup, QB]]
}

#Phytoplankton needs to be NA
GB.params$model[Group == 'Phytoplankton', QB := NA]

#Load landings
load(here('data', 'land.input.rda'))

#Need to account for mismatched names
rfleets <- c('ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
             'Trap', 'SmallMesh', 'LargeMesh', 'HMSFleet', 'OtherFisheries')
rgear   <- c('Scallop Dredge', 'Clam Dredge', 'Other Dredge', 'Fixed Gear', 
             'Pelagic', 'Trap', 'SM Mesh','LG Mesh', 'HMS', 'Other')

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

#Load Diet
load(here('data', 'diet.input.rda'))

preds <- unique(diet.input[, Rpred])
for(ipred in 1:length(preds)){
  setnames(GB.params$diet, preds[ipred], 'Pred')
  pred.diet <- diet.input[Rpred == preds[ipred], ]
  prey <- pred.diet[, Rprey]
  for(iprey in 1:length(prey)){
    GB.params$diet[Group == prey[iprey], 
                   Pred := pred.diet[Rprey == prey[iprey], preyper]]
  }
  setnames(GB.params$diet, 'Pred', preds[ipred])
  GB.params$diet[]
}

check.rpath.params(GB.params)
usethis::use_data(GB.params, overwrite = T)

#Initial unbalanced model

GB.init <- rpath(GB.params, 'Georges Bank', 1)

usethis::use_data(GB.init, overwrite = T)

