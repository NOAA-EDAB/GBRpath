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
            'GelZooplankton', 'Mesozooplankton', 'Microzooplankton', 'Phytoplankton',
            'Detritus', 'Discards', 
            'DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'Seine', 
            'PotTrap', 'Ottertrawl', 'Midwater', 'OtherFisheries')

types <- c(rep(0, 56), 1, 2, 2, rep(3, 9))

GB.params <- create.rpath.params(groups, types)

#Load biomass
load(file.path(data.dir, 'GB_biomass.RData'))

#Load biomass
for(igroup in GB.params$model[Type < 2, Group]){
  group.bio <- GB.biomass[RPATH == igroup, Biomass]
  GB.params$model[Group == igroup, Biomass := group.bio][]
}
