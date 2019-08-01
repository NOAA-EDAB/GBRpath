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

#Balancing Act
#look at worst EEs first
output.GB <- as.data.table(write.Rpath(GB))
setkey(output.GB, EE)

#1 - Landings > Biomass
# 1.A - Added EMAX q's to bring up most biomass values - included above)

#There were still 2 groups with F > 1.  
# 1.B Top down balancing SmPelagics
GB.params$model[Group == 'SmPelagics', Biomass := NA]
GB.params$model[Group == 'SmPelagics', EE := 0.8]

# 1.C Reducing landings on Redfish because statistical areas overlap Gulf of Maine,
#which is were the landings are probably coming from
#Redfish landings - cut catch in 1/4
GB.params$model[Group == 'Redfish', Gillnet        := Gillnet        / 4]
GB.params$model[Group == 'Redfish', Longline       := Longline       / 4]
GB.params$model[Group == 'Redfish', Midwater       := Midwater       / 4]
GB.params$model[Group == 'Redfish', OtherFisheries := OtherFisheries / 4]
GB.params$model[Group == 'Redfish', OtterTrawlSm   := OtterTrawlSm   / 4]
GB.params$model[Group == 'Redfish', OtterTrawlLg   := OtterTrawlLg   / 4]
GB.params$model[Group == 'Redfish', DredgeScallop.disc  := DredgeScallop.disc  / 4]
GB.params$model[Group == 'Redfish', Gillnet.disc        := Gillnet.disc        / 4]
GB.params$model[Group == 'Redfish', PotTrap.disc        := PotTrap.disc        / 4]
GB.params$model[Group == 'Redfish', Midwater.disc       := Midwater.disc       / 4]
GB.params$model[Group == 'Redfish', OtherFisheries.disc := OtherFisheries.disc / 4]
GB.params$model[Group == 'Redfish', OtterTrawlSm.disc   := OtterTrawlSm.disc   / 4]
GB.params$model[Group == 'Redfish', OtterTrawlLg.disc   := OtterTrawlLg.disc   / 4]

#Deal with worst EEs
#2 Revisit diets to ensure they make sense
# 2.A - Changed Other Flatfish in diets to SmFlatfishes - OtherFlatfish are mostly
#unidentified flatfish in stomachs but biomass is Atl Halibut
ofrac <- GB.params$model[Group == 'OtherFlatfish', Biomass] / 
  (GB.params$model[Group == 'OtherFlatfish', Biomass] + 
     GB.params$model[Group == 'SmFlatfishes', Biomass])
for(i in 2:ncol(GB.params$diet)){
  old.name <- colnames(GB.params$diet)[i]
  setnames(GB.params$diet, old.name, 'switch')
  off.diet <- GB.params$diet[Group == 'OtherFlatfish', switch]
  to.oth <- off.diet * ofrac
  to.sm  <- off.diet - to.oth
  GB.params$diet[Group == 'SmFlatfishes', switch := switch + to.sm]
  GB.params$diet[Group == 'OtherFlatfish', switch := to.oth]
  setnames(GB.params$diet, 'switch', old.name)
}

# 2.B - Fix Red Hake diet - Red Hake way too high in the trophic spectrum
#listed as eating sharks and cod...these must be juveniles/larval versions
#moving to micronekton
misplaced <- GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish',
                                         'SpinyDogfish', 'OtherSkates'), sum(RedHake)]
GB.params$diet[Group == 'Micronekton', RedHake := RedHake + misplaced]
GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish', 
                            'SpinyDogfish', 'OtherSkates'), RedHake := 0]

# 3 - American Plaice EE = 1210 - F was 0.44 on AmPlaice - 
# 3.A - Reduce fishing on AmPlaice
GB.params$model[Group == 'AmPlaice', OtterTrawlSm      := OtterTrawlSm      / 2]
GB.params$model[Group == 'AmPlaice', OtterTrawlLg      := OtterTrawlLg      / 2]
GB.params$model[Group == 'AmPlaice', OtterTrawlSm.disc := OtterTrawlSm.disc / 2]
GB.params$model[Group == 'AmPlaice', OtterTrawlLg.disc := OtterTrawlLg.disc / 2]

# 3.B - Increase Biomass
GB.params$model[Group == 'AmPlaice', Biomass := Biomass * 4]

# 3.C - Increase production
GB.params$model[Group == 'AmPlaice', PB := 0.04]

# 4 - OceanPout
# 4.A - reduce Consumption by Silver Hake
oldval <- GB.params$model[Group == 'SilverHake', QB]
GB.params$model[Group == 'SilverHake', QB := 2.6]

#Check progress
GB <- rpath(GB.params, 'Georges Bank', 1)
output.GB <- as.data.table(write.Rpath(GB))
setkey(output.GB, EE)
output.GB

morts <- as.data.table(write.Rpath(GB, morts = T))
barplot(output.GB[type < 2, EE], log = 'y', names.arg = output.GB[type < 2, Group],
        cex.names = 0.3, las = T)
abline(h=1, col = 'red')

unbal.GB <- as.data.table(write.Rpath(GB))
living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]
bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, Biomass)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, Biomass], living.GB[, Group], cex = .5)
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

#Save initial balance for PreBal
#save(GB.2, file = file.path(data.dir, 'Unbalanced_GB.RData'))
#save(GB.params.2, file = file.path(data.dir, 'Input_GB_params.RData'))
