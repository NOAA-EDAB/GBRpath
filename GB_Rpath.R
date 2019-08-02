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

#Overall Balance - biomass slope ~ -70% according to prebal it should be 5-10%
#Going to drop the biomass of lower trophic levels
GB.params$model[Group %in% c('Phytoplankton', 'Clams', 'Macrobenthos', 'Mesozooplankton',
                              'Microzooplankton', 'AtlScallop', 'Bacteria', 'Micronekton',
                              'GelZooplankton', 'Krill'), Biomass := Biomass / 10]
#That got the slope to -32%
#Raise other groups biomass
GB.params$model[!Group %in% c('Phytoplankton', 'Clams', 'Macrobenthos', 'Mesozooplankton',
                              'Microzooplankton', 'AtlScallop', 'Bacteria', 'Micronekton',
                              'GelZooplankton', 'Krill'), Biomass := Biomass * 4]
#That got it to -14% which is good enough to start
unbal.GB <- as.data.table(write.Rpath(GB))
living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]
bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, Biomass)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, Biomass], living.GB[, Group], cex = .8)
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

bio.slope <- coef(bio.mod)[2]
bio.slope

#Overall production - PreBal suggest similar trend to biomass but mean PBs are 
#on par with other shelf models from Ecobase
ecobase <- data.table(TL = c('1-2', '2-3', '3-4', '4+'), 
                      mean.PB = c(70.91, 27.47, 1.56, 0.605),
                      GB.initialPB = c(91.25, 20.78, 1.28, 0.27))

#1 - Landings > Biomass----
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

# 1.D Increase redfish biomass
GB.params$model[Group == 'Redfish', Biomass := Biomass * 2]

#2 Other Flatfish ----
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

# 2.B - Reduce preadator biomass
#q applied to spiny dogfish way too low (0.199)
#EEs also very low (Spiny dogfish 0.04, Cod 0.51)
orig.spinybio <- GB.params$model[Group == 'SpinyDogfish', Biomass]
orig.cod      <- GB.params$model[Group == 'Cod',          Biomass]
GB.params$model[Group == 'SpinyDogfish', Biomass := 1.64]
GB.params$model[Group == 'Cod',          Biomass := 0.5]

# 2.C - Increase biomass
orig.offbio <- GB.params$model[Group == 'OtherFlatfish', Biomass]
GB.params$model[Group == 'OtherFlatfish', Biomass := Biomass * 4]

#Fix Red Hake diet ---- 
#Red Hake way too high in the trophic spectrum
#listed as eating sharks and cod...these must be juveniles/larval versions
#moving to micronekton
misplaced <- GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish',
                                         'SpinyDogfish', 'OtherSkates'), sum(RedHake)]
GB.params$diet[Group == 'Micronekton', RedHake := RedHake + misplaced]
GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish', 
                            'SpinyDogfish', 'OtherSkates'), RedHake := NA]

# 3 - OceanPout ----
# 3.A Increase Biomass - Not well sampled
orig.opbio <- GB.params$model[Group == 'OceanPout', Biomass]
GB.params$model[Group == 'OceanPout', Biomass := Biomass * 2]

# 3.B Increase production
orig.oppb <- GB.params$model[Group == 'OceanPout', PB]
GB.params$model[Group == 'OceanPout', PB := PB * 3]

# 3.C Increase consumption
orig.opqb <- GB.params$model[Group == 'OceanPout', QB]
GB.params$model[Group == 'OceanPout', PB := QB * 2]

#diagnose(GB.params, 'OceanPout')
#May need to revist DC of OtherDemersal (16%)

# 4 - OtherCephalopods ----
# 4.A Increase biomass - 4x and 6x not enough
GB.params$model[Group == 'OtherCephalopods', Biomass := Biomass * 8]

# 4.B Drop pred biomasses
orig.predbio <- GB.params$model[Group %in% c('Haddock', 'LittleSkate', 'WinterSkate',
                                             'Barndoor', 'SummerFlounder'), Biomass]
GB.params$model[Group %in% c('Haddock', 'LittleSkate', 'WinterSkate', 'Barndoor',
                             'SummerFlounder'), Biomass := Biomass / 10]

# 4.C Reduce QB of summer flounder
orig.sumfqb <- GB.params$model[Group == 'SummerFlounder', QB]
GB.params$model[Group == 'SummerFlounder', QB := 2.5]

# 5 - American Plaice  ----
diagnose(GB.params, 'AmPlaice')

# 5.A - Increase Biomass
orig.ampbio <- GB.params$model[Group == 'AmPlaice', Biomass]
GB.params$model[Group == 'AmPlaice', Biomass := Biomass * 3]

# 5.B - Increase production
orig.amppb <- GB.params$model[Group == 'AmPlaice', PB]
GB.params$model[Group == 'AmPlaice', PB := PB * 6]

# 5.C - Increase consumption
orig.ampqb <- GB.params$model[Group == 'AmPlaice', QB]
GB.params$model[Group == 'AmPlaice', QB := QB * 3]


# 6 - OtherShrimps ----
# 6.A Increase biomass
GB.params$model[Group == 'OtherShrimps', Biomass := Biomass * 4]

# 7 - Megabenthos ----
# 7.A Increase biomass
GB.params$model[Group == 'Megabenthos', Biomass := Biomass * 6]

# 8 - OtherPelagics -----
# 8.A - Fix diet - done in GBRpath_diet_pull
# 8.B - Decrease Silver Hake - did not work cause silver hake are unbalanced
# 8.C - Increase biomass
oldval <- GB.params$model[Group == 'OtherPelagics', Biomass]
GB.params$model[Group == 'OtherPelagics', Biomass := Biomass * 3]

# 9 - OtherSkates----
# 9.A - Move diet in Goosefish to little skate species
tolittle <- GB.params$diet[Group == 'OtherSkates', Goosefish]
GB.params$diet[Group == 'LittleSkate', Goosefish := Goosefish + tolittle]
GB.params$diet[Group == 'OtherSkates', Goosefish := NA]

# 9.B - increase production
oldval <- GB.params$model[Group == 'OtherSkates', PB]
GB.params$model[Group == 'OtherSkates', PB := 0.08]

# 10 - SilverHake ----
# 10.A - probably cause by too much cannibalism - move most of silver hake to red hake
tored <- GB.params$diet[Group == 'SilverHake', SilverHake] / 1.1
GB.params$diet[Group == 'RedHake', SilverHake    := SilverHake + tored]
GB.params$diet[Group == 'SilverHake', SilverHake := SilverHake - tored]

# 10.B - Moving 1/2 of fish biomass to micronekton as they are juveniles
fish.taxa <- c('Cod', 'Haddock', 'WhiteHake', 'OceanPout', 'OtherDemersals', 
               'Fourspot', 'SummerFlounder', 'OtherFlatfish')
for(ifish in 1:length(fish.taxa)){
  tonekton <- GB.params$diet[Group == fish.taxa[ifish], SilverHake] / 2
  GB.params$diet[Group == 'Micronekton', SilverHake    := SilverHake + tonekton]
  GB.params$diet[Group == fish.taxa[ifish], SilverHake := SilverHake - tonekton]
}

# 11 - AtlMackerel ----
# 11.A - reduce predators
oldval <- GB.params$model[Group == 'Haddock', Biomass]
GB.params$model[Group == 'Haddock', Biomass := Biomass / 3]

# 12 - Witch Flounder -----
# 12.A - Increase biomass
oldval <- GB.params$model[Group == 'WitchFlounder', Biomass]
GB.params$model[Group == 'WitchFlounder', Biomass := Biomass * 4]

# 13 - Pollock -----
# 13.A - Increase biomass
oldval <- GB.params$model[Group == 'Pollock', Biomass]
GB.params$model[Group == 'Pollock', Biomass := Biomass * 4]

# 14 - White Hake -----
# 14.A - Increase biomass
oldval <- GB.params$model[Group == 'WhiteHake', Biomass]
GB.params$model[Group == 'WhiteHake', Biomass := Biomass * 4]

# 15 - Offshore Hake -----
# 15.A - Increase biomass
oldval <- GB.params$model[Group == 'OffHake', Biomass]
GB.params$model[Group == 'OffHake', Biomass := Biomass * 4]


#Check progress ----
GB <- rpath(GB.params, 'Georges Bank', 1)
output.GB <- as.data.table(write.Rpath(GB))
setkey(output.GB, EE)
output.GB

morts <- as.data.table(write.Rpath(GB, morts = T))

barplot(output.GB[type < 2, EE], log = 'y', names.arg = output.GB[type < 2, Group],
        cex.names = 0.5, las = T)
abline(h=1, col = 'red')

unbal.GB <- as.data.table(write.Rpath(GB))
living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]
bio.mod <- lm(log(living.GB[!Group %in% c('SouthernDemersals', 'OtherFlatfish'), Biomass], base = 10) 
              ~ living.GB[!Group %in% c('SouthernDemersals', 'OtherFlatfish'), TL])

plot(living.GB[, list(TL, Biomass)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, Biomass], living.GB[, Group], cex = .8)
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)
abline(a = coef(bio.mod)[1], b = -.1, lty = 3, col = 'red')

bio.slope <- coef(bio.mod)[2]
bio.slope

#Save initial balance for PreBal
#save(GB.2, file = file.path(data.dir, 'Unbalanced_GB.RData'))
#save(GB.params.2, file = file.path(data.dir, 'Input_GB_params.RData'))
