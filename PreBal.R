#Prebalance for Georges Bank
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
library(data.table)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load in pre balanced model
load(file.path(data.dir, 'Input_GB_params.RData'))
load(file.path(data.dir, 'Unbalanced_GB.RData'))
unbal.GB <- as.data.table(write.Rpath(GB.2))

#Need to classify each group
spclass.GB <- data.table(Group = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 
                                   'Sharks', 'AtlHerring', 'AtlMackerel', 'Butterfish', 
                                   'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 
                                   'Haddock', 'Goosefish', 'OffHake', 'SilverHake', 
                                   'RedHake', 'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 
                                   'BlackSeaBass', 'Bluefish', 'Scup', 'OtherDemersals', 
                                   'SouthernDemersals', 'Fourspot', 'SummerFlounder', 
                                   'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
                                   'YTFlounder', 'OtherFlatfish', 'SmFlatfishes', 'SpinyDogfish', 
                                   'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
                                   'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 
                                   'AmLobster', 'Macrobenthos', 'Megabenthos', 'AtlScallop', 
                                   'Clams', 'OtherShrimps', 'Krill', 'Micronekton', 
                                   'GelZooplankton', 'Mesozooplankton', 'Microzooplankton', 
                                   'Phytoplankton'), 
                         type = c('Bird', 'Mammal', 'Whale', 'Whale', 'HMS', 'Shark',
                                  rep('Forage', 6), rep('DemRound', 15), 
                                  rep('DemFlat', 9), 'DemRound', 'DemRound', 
                                  rep('Dem', 4), rep('PelInvert', 3), rep('BenInvert', 5),
                                  rep('PelInvert', 3), rep('Zoop', 3), 'Phyto'),
                         diet = c(rep(NA, 4), 'Pisc', 'Pisc', rep('Plank', 6), 
                                  'Pisc', 'Benth', rep('Pisc', 3), 'Benth', 'Pisc', 
                                  'Benth', 'Pisc', 'Benth', 'Benth', 'Pisc', 
                                  rep('Benth', 4), 'Pisc', rep('Benth', 7),
                                  'Pisc', 'Pisc', rep('Benth', 4), rep(NA, 15)))
                                  
spclass.GB[type %like% 'Round', subtype := 'Round']
spclass.GB[type %like% 'Flat',  subtype := 'Flat']
spclass.GB[type %like% 'Dem',   type := 'Demersal']

living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]

#Biomass Across Taxa (Criteria 1)---------
#Biomass Range
max.bio <- max(living.GB[, Biomass])
min.bio <- min(living.GB[, Biomass])
bio.mag <- round(log(max.bio, base = 10)) - round(log(min.bio, base = 10))
bio.mag

#Slope of Biomass
bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, Biomass)], log = "y")
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

bio.slope <- coef(bio.mod)[2]
bio.slope

#Not from prebal but look at F
living.GB[, F := Removals / Biomass]
living.GB[F > 1, ]

#Biomass Ratios (Criteria 2)----
living.GB <- merge(living.GB, spclass.GB, by = 'Group', all.x = T)
#Predator Prey ratios
living.GB[type == 'Forage', sum(Biomass)] / living.GB[type == 'Zoop', sum(Biomass)]
living.GB[type == 'Zoop', sum(Biomass)] / living.GB[type == 'Phyto', sum(Biomass)]
living.GB[type == 'Forage', sum(Biomass)] / living.GB[type == 'Phyto', sum(Biomass)]
living.GB[type == 'Demersal', sum(Biomass)] / living.GB[type == 'BenInvert', sum(Biomass)]
living.GB[type %in% c('Shark', 'HMS'), sum(Biomass)] / living.GB[type == 'Forage', sum(Biomass)]
living.GB[type %in% c('Mammal', 'Bird'), sum(Biomass)] / living.GB[type == 'Forage', sum(Biomass)]
living.GB[type == 'Whale', sum(Biomass)] / living.GB[type == 'Zoop', sum(Biomass)]
#Energy flows
living.GB[type == 'Demersal', sum(Biomass)] / living.GB[type %in% c('Forage', 'Shark', 'HMS'), sum(Biomass)]
living.GB[subtype == 'Flat', sum(Biomass)] / living.GB[subtype == 'Round', sum(Biomass)]
allfish <- c('Demersal', 'Forage', 'HMS', 'Shark')
living.GB[type == 'Forage', sum(Biomass)] / living.GB[type %in% allfish, sum(Biomass)]
living.GB[type == 'HMS', sum(Biomass)] / living.GB[type %in% allfish, sum(Biomass)]
living.GB[type == 'Shark', sum(Biomass)] / living.GB[type %in% allfish, sum(Biomass)]
living.GB[type == 'Demersal', sum(Biomass)] / living.GB[type %in% allfish, sum(Biomass)]
allinvert <- c('BenInvert', 'Zoop', 'PelInvert', 'Phyto')
living.GB[type == 'BenInvert', sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[type == 'PelInvert', sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[Group == 'GelZooplankton', sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[Group %in% c('OtherShrimps', 'Micronekton'), sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[type == 'Zoop', sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[type == 'Phyto', sum(Biomass)] / living.GB[type %in% allinvert, sum(Biomass)]
living.GB[type == 'Zoop', sum(Biomass)] / living.GB[type == 'BenInvert', sum(Biomass)]
living.GB[diet == 'Benth', sum(Biomass)] / living.GB[diet == 'Pisc', sum(Biomass)]
living.GB[diet == 'Benth', sum(Biomass)] / living.GB[diet == 'Plank', sum(Biomass)]
living.GB[diet == 'Plank', sum(Biomass)] / living.GB[diet == 'Pisc', sum(Biomass)]
living.GB[TL <= 3, sum(Biomass)] / living.GB[TL >= 4, sum(Biomass)]

#Vital Rates (Criteria 3)-----
#QB
cons.mod <- lm(log(living.GB[Group != 'Phytoplankton', QB], base = 10) ~ 
                 living.GB[Group != 'Phytoplankton', TL])

plot(living.GB[Group != 'Phytoplankton', list(TL, QB)], log = "y")
abline(cons.mod)
#+- 1 Standard Error
std <- coef(summary(cons.mod))[, 2]
abline(a = coef(cons.mod)[1] + std[1], b = coef(cons.mod)[2] + std[2], lty = 2)
abline(a = coef(cons.mod)[1] - std[1], b = coef(cons.mod)[2] - std[2], lty = 2)

cons.slope <- coef(cons.mod)[2]
cons.slope

#PB
prod.mod <- lm(log(living.GB[, PB], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, PB)], log = "y")
abline(prod.mod)
#+- 1 Standard Error
std <- coef(summary(prod.mod))[, 2]
abline(a = coef(prod.mod)[1] + std[1], b = coef(prod.mod)[2] + std[2], lty = 2)
abline(a = coef(prod.mod)[1] - std[1], b = coef(prod.mod)[2] - std[2], lty = 2)

prod.slope <- coef(prod.mod)[2]
prod.slope


