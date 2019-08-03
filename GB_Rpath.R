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

#2 - Other Flatfish ----
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
# 6.A Increase production
orig.shmppb <- GB.params$model[Group == 'OtherShrimps', PB]
GB.params$model[Group == 'OtherShrimps', PB := 20]

# 6.B Increase biomass
orig.shmpbio <- GB.params$model[Group == 'OtherShrimps', Biomass]
GB.params$model[Group == 'OtherShrimps', Biomass := Biomass * 2]

# 7 - OtherPelagics -----
# 7.A - Fix diet - done in GBRpath_diet_pull
# 7.A - Increase biomass
orig.opelbio <- GB.params$model[Group == 'OtherPelagics', Biomass]
GB.params$model[Group == 'OtherPelagics', Biomass := Biomass * 4]

# 8 - Micronekton ----
# 8.A - Increase Biomass
orig.mnkbio <- GB.params$model[Group == 'Micronekton', Biomass]
GB.params$model[Group == 'Micronekton', Biomass := Biomass * 2]

# 8.B - Increase production
orig.mnkpb <- GB.params$model[Group == 'Micronekton', PB]
GB.params$model[Group == 'Micronekton', PB := PB * 2]

# 9 - OtherSkates----
# 9.A - increase production
orig.oskpb <- GB.params$model[Group == 'OtherSkates', PB]
GB.params$model[Group == 'OtherSkates', PB := 0.5]

# 10 - Discards ----
# EE is ratio of DetOut to DetIN - need to lower out unless jacking up landings
# This makes sense as discards should be a low portion of diets
sp.todet <- c('AmLobster', 'Megabenthos', 'OtherShrimps', 'SouthernDemersals',
              'OtherFlatfish')
for(isp in 1:length(sp.todet)){
  setnames(GB.params$diet, sp.todet[isp], 'switch')
  todet <- GB.params$diet[Group == 'Discards', switch]
  GB.params$diet[Group == 'Detritus', switch := switch + todet]
  GB.params$diet[Group == 'Discards', switch := NA]
  setnames(GB.params$diet, 'switch', sp.todet[isp])
}

# 11 - Krill ----
# 11.A Increase biomass
orig.krillbio <- GB.params$model[Group == 'Krill', Biomass]
GB.params$model[Group == 'Krill', Biomass := Biomass * 4]

# 11.B Increase productivity
orig.krillpb <- GB.params$model[Group == 'Krill', PB]
GB.params$model[Group == 'Krill', PB := PB * 2]

# 12 - Detritus ----
#Increase unassim to 0.4 for zooplankton
GB.params$model[Group %in% c('Microzooplankton', 'Mesozooplankton'), Unassim := 0.4]

#Increase unassim to 0.3 for other detritavores
GB.params$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos', 'AtlScallop', 
                             'Clams', 'OtherShrimps'), Unassim := 0.3]

#Decrease consumption of Bacteria
orig.bacqb <- GB.params$model[Group == 'Bacteria', QB]
GB.params$model[Group == 'Bacteria', QB := 300]

# 13 - SilverHake ----
# 13.A Increase production
orig.shpb <- GB.params$model[Group == 'SilverHake', PB]
GB.params$model[Group == 'SilverHake', PB := 0.4]

# 13.A - probably cause by too much cannibalism - move most of silver hake to micronekton
tomicro <- GB.params$diet[Group == 'SilverHake', SilverHake] / 1.1
GB.params$diet[Group == 'Micronekton', SilverHake := SilverHake + tomicro]
GB.params$diet[Group == 'SilverHake',  SilverHake := SilverHake - tomicro]

# 13.B Decrease consumption - PQ ratio = 0.09 should be between .1 and .3
orig.shqb <- GB.params$model[Group == 'SilverHake', QB]
GB.params$model[Group == 'SilverHake', QB := 3.055]

#All EE's under 10 need to investigate systematic issues now---------------
#Identified 8 species that look out of place on the trophic spectrum
#Mackerel, Herring, 3 squids, 3 hakes
#Mackerel diet appears spot on based on Bigelow and Shroeder...~10% from other 
#trouble groups.
#Squids look good too.  Mostly micronekton (45%).
#AtlHerring had seabird in its diet (really small but still)...remove
tozoop <- GB.params$diet[Group == 'Seabirds', AtlHerring]
GB.params$diet[Group == 'Mesozooplankton', AtlHerring := AtlHerring + tozoop]
GB.params$diet[Group == 'Seabirds', AtlHerring := NA]
#Changed Excess cannibalism from red hake to micronekton for silver Hake (above)
#Red Hake listed as eating sharks and cod...these must be juveniles/larval versions
#moving to micronekton
misplaced <- GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish',
                                         'SpinyDogfish', 'OtherSkates'), sum(RedHake)]
GB.params$diet[Group == 'Micronekton', RedHake := RedHake + misplaced]
GB.params$diet[Group %in% c('Sharks', 'Cod', 'Haddock', 'Goosefish', 
                            'SpinyDogfish', 'OtherSkates'), RedHake := NA]
#Move 2/3 of fish diet to macrobenthos for redhake
tomacro <- GB.params$diet[Group %in% c('AtlHerring', 'AtlMackerel', 'SmPelagics',
                                        'Mesopelagics', 'SilverHake', 'RedHake', 
                                        'OceanPout', 'OtherDemersals', 'Windowpane',
                                        'OtherFlatfish', 'SmFlatfishes'), 
                          sum(RedHake)] * 2/3
GB.params$diet[Group == 'Macrobenthos', RedHake := RedHake + tomacro]
GB.params$diet[Group %in% c('AtlHerring', 'AtlMackerel', 'SmPelagics', 'Mesopelagics', 
                            'SilverHake', 'RedHake', 'OceanPout', 'OtherDemersals', 
                            'Windowpane', 'OtherFlatfish', 'SmFlatfishes'), 
               RedHake := RedHake / 3]

#According to Bigelow and Schroeder Offshore Hake eat mostly herring, anchovies, 
#and laternfish. Moving 90% from silver hake and redistributing
toothers <- GB.params$diet[Group == 'SilverHake', OffHake] * 0.9
GB.params$diet[Group == 'SmPelagics',   OffHake := toothers * 0.55]
GB.params$diet[Group == 'AtlHerring',   OffHake := toothers * 0.44]
GB.params$diet[Group == 'Mesopelagics', OffHake := toothers * 0.01]
GB.params$diet[Group == 'SilverHake',   OffHake := OffHake - toothers]

# Back to EEs--------
# 14 - Megabenthos ----
# 14.A Increase biomass
orig.megbbio <- GB.params$model[Group == 'Megabenthos', Biomass]
GB.params$model[Group == 'Megabenthos', Biomass := Biomass * 2]

# 14.B Increase PB
orig.megbpb <- GB.params$model[Group == 'Megabenthos', PB]
GB.params$model[Group == 'Megabenthos', PB := PB * 2]

# 14.C Decrease Bio of Pred
orig.hadbio <- GB.params$model[Group == 'Haddock', Biomass]
orig.otdbio <- GB.params$model[Group == 'OtherDemersals', Biomass]
orig.smdbio <- GB.params$model[Group == 'SmoothDogfish', Biomass]
GB.params$model[Group %in% c('Haddock', 'OtherDemersals', 'SmoothDogfish'), 
                Biomass := Biomass / 2]

# 14.D Move portion of SmoothDogfish DC to Macrobenthos
tomacro <- GB.params$diet[Group == 'Megabenthos', SmoothDogfish] * 0.25
GB.params$diet[Group == 'Macrobenthos', SmoothDogfish := SmoothDogfish + tomacro]
GB.params$diet[Group == 'Megabenthos',  SmoothDogfish := SmoothDogfish - tomacro]

# 15 - Witch Flounder -----
# 15.A - Increase biomass
orig.witchbio <- GB.params$model[Group == 'WitchFlounder', Biomass]
GB.params$model[Group == 'WitchFlounder', Biomass := Biomass * 4]

# 15.B Increase production
orig.witchpb <- GB.params$model[Group == 'WitchFlounder', PB]
GB.params$model[Group == 'WitchFlounder', PB := PB * 4]

# 16 - Macrobenthos
# 16.A - Increase production
orig.macbpb <- GB.params$model[Group == 'Macrobenthos', PB]
GB.params$model[Group == 'Macrobenthos', PB := PB * 10]

# 17 - White Hake -----
# 17.A - Increase Production
orig.whpb <- GB.params$model[Group == 'WhiteHake', PB]
GB.params$model[Group == 'WhiteHake', PB := 0.18]

# 17.B - Diet is a little suspect - moving 10% to Loligo and 20% to macrobenthos
# from Silver Hake
toother <- .30
GB.params$diet[Group == 'SilverHake',   WhiteHake := WhiteHake - 0.30]
GB.params$diet[Group == 'Loligo',       WhiteHake := WhiteHake + 0.10]
GB.params$diet[Group == 'Macrobenthos', WhiteHake := WhiteHake + 0.20]

# 17.C - cut landings in half (more of a GoM fishery)
GB.params$model[Group == 'WhiteHake', Gillnet        := Gillnet        / 2]
GB.params$model[Group == 'WhiteHake', Longline       := Longline       / 2]
GB.params$model[Group == 'WhiteHake', Midwater       := Midwater       / 2]
GB.params$model[Group == 'WhiteHake', OtherFisheries := OtherFisheries / 2]
GB.params$model[Group == 'WhiteHake', OtterTrawlSm   := OtterTrawlSm   / 2]
GB.params$model[Group == 'WhiteHake', OtterTrawlLg   := OtterTrawlLg   / 2]
GB.params$model[Group == 'WhiteHake', DredgeScallop.disc  := DredgeScallop.disc  / 2]
GB.params$model[Group == 'WhiteHake', Gillnet.disc        := Gillnet.disc        / 2]
GB.params$model[Group == 'WhiteHake', PotTrap.disc        := PotTrap.disc        / 2]
GB.params$model[Group == 'WhiteHake', Midwater.disc       := Midwater.disc       / 2]
GB.params$model[Group == 'WhiteHake', OtterTrawlSm.disc   := OtterTrawlSm.disc   / 2]
GB.params$model[Group == 'WhiteHake', OtterTrawlLg.disc   := OtterTrawlLg.disc   / 2]

# 17.D - Reduce cannibalism
tomega <- GB.params$diet[Group == 'WhiteHake', WhiteHake] / 2
GB.params$diet[Group == 'WhiteHake', WhiteHake := WhiteHake - tomega]
GB.params$diet[Group == 'Megabenthos', WhiteHake := WhiteHake + tomega]

diagnose(GB.params, 'WhiteHake')
#Pick up here---------------






# 11 - AtlMackerel ----
# 11.A - reduce predators
oldval <- GB.params$model[Group == 'Haddock', Biomass]
GB.params$model[Group == 'Haddock', Biomass := Biomass / 3]


# 13 - Pollock -----
# 13.A - Increase biomass
oldval <- GB.params$model[Group == 'Pollock', Biomass]
GB.params$model[Group == 'Pollock', Biomass := Biomass * 4]


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
