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
unbal.GB <- as.data.table(write.Rpath(GB))

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
                                  rep('PelInvert', 3), rep('Zoop', 3), 'Phyto'))
spclass.GB[type %like% 'Round', subtype := 'Round']
spclass.GB[type %like% 'Flat',  subtype := 'Flat']
spclass.GB[type %like% 'Dem',   type := 'Demersal']

living.GB <- unbal.GB[type < 2, list(Group, Biomass, TL)]

#Biomass Across Taxa (Criteria 1)
#Biomass Range
max.bio <- max(living.GB[, Biomass])
min.bio <- min(living.GB[, Biomass])
bio.mag <- round(log(max.bio, base = 10)) - round(log(min.bio, base = 10))

#Slope of Biomass
bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, Biomass)], log = "y")
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

bio.slope <- coef(bio.mod)[2]
