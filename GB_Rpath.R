#Georges Bank Rpath model
#Expanded model of Georges Bank
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "C:/Users/Sean.Lucey/Desktop/Rpath_code/data"
  out.dir  <- "C:/Users/Sean.Lucey/Desktop/Rpath_code/outputs"
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rpath/data"
  out.dir  <- "/home/slucey/slucey/Rpath/outputs"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(Rpath)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Georges Bank
#This model will have a much less aggregated box structure.
groups <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale','HMS', 
            'AmShad', 'AtlHerring', 'AtlMackerel', 'RiverHerring', 
            'Butterfish', 'SmPelagics', 'Cod', 'Haddock', 'AtlHalibut', 'Goosefish', 
            'OffHake', 'SilverHake', 'RedHake', 'WhiteHake', 'Redfish', 'Pollock', 
            'OceanPout', 'BlackSeaBass', 'Weakfish','Bluefish', 'Scup','OtherDemersals', 
            'Fourspot', 'SummerFlounder', 'AmPlaice', 'Windowpane', 'WinterFlounder', 
            'WitchFlounder', 'YTFlounder', 'SmFlatfishes', 'SpinyDogfish', 
            'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 'SmSkates', 
            'Illex', 'Loligo', 'OtherCephalopods', 'AmLobster', 'Red Crab', 
            'Macrobenthos', 'Megabenthos', 'AtlScallops', 'Clams', 'NShrimp', 
            'OtherShrimp', 'Krill', 'Micronekton', 'GelZooplankton', 
            'Mesozooplankton', 'Microzooplankton', 'Phytoplankton', 
            'Detritus', 'Discards', 
            'DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'Seine', 
            'PotTrap', 'Ottertrawl', 'Midwater', 'OtherFisheries')

types <- c(rep(0, 57), 1, 2, 2, rep(3, 9))

GB.params <- create.rpath.params(groups, types)

#Load GB.input RData
load(file.path(data.dir, 'GB_input.RData'))

#Load biomass
for(igroup in GB.params$model[, Group]){
  group.input <- GB.input[RPATH == igroup, ]
}
