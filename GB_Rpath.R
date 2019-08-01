#Georges Bank Rpath model
#Expanded model of Georges Bank
#SML

#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')

#Required packages--------------------------------------------------------------
library(data.table); library(Rpath)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Georges Bank
groups <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 'Sharks', 
            'AtlHerring', 'AtlMackerel', 'Butterfish', 'SmPelagics', 'Mesopelagics', 
            'OtherPelagics', 'Cod', 'Haddock', 'Goosefish', 'OffHake', 'SilverHake', 
            'RedHake', 'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 'BlackSeaBass', 
            'Bluefish', 'Scup', 'OtherDemersals', 'SouthernDemersals', 'Fourspot', 
            'SummerFlounder', 'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
            'YTFlounder', 'OtherFlatfish', 'SmFlatfishes', 'SpinyDogfish', 
            'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 'OtherSkates',
            'Illex', 'Loligo', 'OtherCephalopods', 'AmLobster', 'Macrobenthos', 
            'Megabenthos', 'AtlScallop', 'Clams', 'OtherShrimps', 'Krill', 'Micronekton', 
            'GelZooplankton', 'Mesozooplankton', 'Microzooplankton', 'Bacteria',
            'Phytoplankton',
            'Detritus', 'Discards', 
            'DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'PotTrap', 
            'OtterTrawlSm', 'OtterTrawlLg', 'Midwater', 'OtherFisheries')

types <- c(rep(0, 57), 1, 2, 2, rep(3, 9))

GB.params <- create.rpath.params(groups, types)

#Enter BioAcc, Unassim, DetInput, and detrital fate
GB.params$model[Type < 3,  BioAcc := 0]
GB.params$model[Type == 0, Unassim := 0.2]
GB.params$model[Type > 0 & Type < 3, Unassim := 0]
GB.params$model[Type == 2, DetInput := 0]
GB.params$model[Type < 2,  Detritus := 1]
GB.params$model[Type > 1,  Detritus := 0]
GB.params$model[Type == 3, Discards := 1]
GB.params$model[Type < 3,  Discards := 0]

#Load biomass
load(file.path(data.dir, 'GB_biomass.RData'))

for(igroup in GB.params$model[Type < 2, Group]){
  group.bio <- GB.biomass[RPATH == igroup, Biomass]
  GB.params$model[Group == igroup, Biomass := group.bio][]
}

#Add EEs for groups without biomass
GB.params$model[Group %in% c('Seals', 'Bacteria', 'Phytoplankton'), EE := 0.8]

#Biological Parameters
load(file.path(data.dir, 'GB_bioparams.RData'))

for(igroup in GB.bioparams[, RPATH]){
  GB.params$model[Group == igroup, PB := GB.bioparams[RPATH == igroup, PB]]
  GB.params$model[Group == igroup, QB := GB.bioparams[RPATH == igroup, QB]][]
}
#Phytoplankton needs to be NA
GB.params$model[Group == 'Phytoplankton', QB := NA]

#Load landings
load(file.path(data.dir, 'GB_landings.RData'))

#Need to account for mismatched names
rfleets <- c('DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'PotTrap', 
             'OtterTrawlSm', 'OtterTrawlLg', 'Midwater', 'OtherFisheries')
rgear   <- c('dredge.sc', 'dredge.cl', 'gillnet', 'longline', 'pot', 'otter.sm',
             'otter.lg', 'midwater', 'other')

for(igear in 1:length(rfleets)){
  gear.landings <- GB.landings[RGear == rgear[igear], ]
  setnames(GB.params$model, rfleets[igear], 'gear')
  for(igroup in 1:nrow(gear.landings)){
    GB.params$model[Group == gear.landings[igroup, RPATH], 
                    gear := gear.landings[igroup, SPPLIVMT]][]
  }
  setnames(GB.params$model, 'gear', rfleets[igear])
}

#Load discards
load(file.path(data.dir, 'GB_discards.RData'))

for(igear in 1:length(rfleets)){
  gear.disc <- GB.disc[RGear == rgear[igear], ]
  setnames(GB.params$model, paste0(rfleets[igear], '.disc'), 'gear')
  #Discards
  for(igroup in 1:nrow(gear.disc)){
    GB.params$model[Group == gear.disc[igroup, RPATH], 
                    gear := gear.disc[igroup, Discards]][]
  }
  setnames(GB.params$model, 'gear', paste0(rfleets[igear], '.disc'))
}

#Load Diet
load(file.path(data.dir, 'GB_diet.RData'))
#Remove predators that weren't present enough to include
GB.diet <- GB.diet[!Rpred %in% c('AmShad', 'AtlHalibut', 'Freshwater', 'LargePelagics',
                                 'RiverHerring', 'StripedBass'), ]
preds <- unique(GB.diet[, Rpred])
for(ipred in 1:length(preds)){
  setnames(GB.params$diet, preds[ipred], 'Pred')
  pred.diet <- GB.diet[Rpred == preds[ipred], ]
  prey <- pred.diet[, Rprey]
  for(iprey in 1:length(prey)){
    GB.params$diet[Group == prey[iprey], Pred := pred.diet[Rprey == prey[iprey], preyper]]
  }
  setnames(GB.params$diet, 'Pred', preds[ipred])
  GB.params$diet[]
}

check.rpath.params(GB.params)

#Balance the model--------------------------------------------------------------
GB <- rpath(GB.params, 'Georges Bank', 1)
#Save initial balance for PreBal
#save(GB, file = file.path(data.dir, 'Unbalanced_GB.RData'))
#save(GB.params, file = file.path(data.dir, 'Input_GB_params.RData'))

#look at worst EEs first
output.GB <- as.data.table(write.Rpath(GB))
setkey(output.GB, EE)

#There are 9 groups with F > 1.  They are also some of the more unbalanced groups.
#Going to remove the biomass estimate and put in an EE and rebalance.
GB.params.2 <- copy(GB.params)
notenoughbio <- c('AtlHerring', 'SmPelagics', 'Redfish', 'Pollock', 'SouthernDemersals', 
                  'AmPlaice', 'WitchFlounder', 'OtherSkates', 'Megabenthos')
GB.params.2$model[Group %in% notenoughbio, Biomass := NA]
GB.params.2$model[Group %in% notenoughbio, EE := 0.8]

GB.2 <- rpath(GB.params.2, 'Georges Bank v2', 1)
#Save initial balance for PreBal
#save(GB.2, file = file.path(data.dir, 'Unbalanced_GB.RData'))
#save(GB.params.2, file = file.path(data.dir, 'Input_GB_params.RData'))

#look at worst EEs first
output.GB <- as.data.table(write.Rpath(GB.2))
setkey(output.GB, EE)

barplot(output.GB[type < 2, EE], log = 'y', names.arg = output.GB[type < 2, Group],
        cex.names = 0.3, las = T)
abline(h=1, col = 'red')
#Balancing Act
# 1 - Added EMAX q's to bring up most biomass values
# 2 - Changed Other Flatfish in diets to SmFlatfishes
ofrac <- GB.params.2$model[Group == 'OtherFlatfish', Biomass] / 
  (GB.params.2$model[Group == 'OtherFlatfish', Biomass] + 
     GB.params.2$model[Group == 'SmFlatfishes', Biomass])
for(i in 2:ncol(GB.params.2$diet)){
  old.name <- colnames(GB.params.2$diet)[i]
  setnames(GB.params.2$diet, old.name, 'switch')
  off.diet <- GB.params.2$diet[Group == 'OtherFlatfish', switch]
  to.oth <- off.diet * ofrac
  to.sm  <- off.diet - to.oth
  GB.params.2$diet[Group == 'SmFlatfishes', switch := switch + to.sm]
  GB.params.2$diet[Group == 'OtherFlatfish', switch := to.oth]
  setnames(GB.params.2$diet, 'switch', old.name)
}

GB.2 <- rpath(GB.params.2, 'Georges Bank', 1)
output.GB <- as.data.table(write.Rpath(GB.2))
setkey(output.GB, EE)
