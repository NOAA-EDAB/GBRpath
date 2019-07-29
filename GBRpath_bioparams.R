#Biological parameters for GBRpath
#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')

#Required packages--------------------------------------------------------------
devtools::install_github("ropensci/rfishbase")
library(rfishbase); library(data.table)
load(file.path(data.dir, 'SOE_species_list.RData'))

#User functions-----------------------------------------------------------------
rightcase <- function(x){
  out <- c()
  for(i in 1:length(x)){
    first <- toupper(substring(x[i], 1, 1))
    rest  <- tolower(substring(x[i], 2, nchar(x[i])))
    new   <- paste0(first, rest)
    out   <- c(out, new)
  }
  return(out)
}

fixspace <- function(x){
  for(i in 1:length(x)){
    last <- substring(x[i], nchar(x[i]), nchar(x[i]))
    if(last == ' ') x[i] <- substring(x[i], 1, nchar(x[i]) - 1)
  }
  return(x)
}
#-------------------------------------------------------------------------------
#First build list of fish names
GB.fish <- unique(species[RPATH %in% c('AtlHerring', 'AtlMackerel', 'Butterfish', 
                                       'Cod', 'Haddock', 'Goosefish', 'OffHake', 
                                       'SilverHake', 'RedHake', 'WhiteHake', 'Redfish', 
                                       'Pollock', 'OceanPout', 'BlackSeaBass', 
                                       'Bluefish', 'Scup', 'Fourspot', 'SummerFlounder', 
                                       'AmPlaice', 'Windowpane', 'WinterFlounder', 
                                       'WitchFlounder', 'YTFlounder', 'SpinyDogfish', 
                                       'SmoothDogfish', 'Barndoor', 'WinterSkate', 
                                       'LittleSkate'), list(RPATH, SCINAME)], by = 'SCINAME')
#Fix scinames with extra space
GB.fish[, SCINAME := fixspace(as.character(SCINAME))]

#Fix names for r package
GB.fish[, Sciname := rightcase(as.character(SCINAME))]

#Prime rfishbase
fishbase <- load_taxa(server = "fishbase")

#Validate names
fish <- validate_names(GB.fish[, Sciname])

#Query the data base
pb.fishbase <- as.data.table(species(fish, fields = c('Species', 'LongevityWild')))
pb.fishbase[, PB := 1 / LongevityWild]
qb.fishbase <- as.data.table(popqb(fish, fields = c('Species', 'PopQB')))
qb.fishbase <- qb.fishbase[, mean(PopQB), by = 'Species']
setnames(qb.fishbase, 'V1', 'QB')

fish.params <- merge(pb.fishbase, qb.fishbase, by = 'Species', all = T)
fish.params[, 'LongevityWild' := NULL]

#Add RPATH code back on
fish.params[, SCINAME := toupper(Species)]
fish.params <- merge(fish.params, unique(species[, list(RPATH, SCINAME)]), by = 'SCINAME',
                     all.x = T)
#There are a couple species with extra spaces at the end...need to fix this
fish.params[SCINAME %like% 'PSEUDOPLEURO',  RPATH := 'WinterFlounder']
fish.params[SCINAME %like% 'LIMANDA',       RPATH := 'YTFlounder']
fish.params[SCINAME %like% 'HIPPOGLOSSINA', RPATH := 'Fourspot']
fish.params[, c('SCINAME', 'Species') := NULL]

#Prime rfishbase
sealife <- load_taxa(server = "sealifebase")
#This works but data not available for almost every species - just use EMAX