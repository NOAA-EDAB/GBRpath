#Diet pull for GBRpath
#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')
gis.dir  <- file.path(main.dir, 'gis')

#Required packages--------------------------------------------------------------
library(data.table); library(rgdal)
load(file.path(data.dir, 'SOE_species_list.RData'))

#User functions-----------------------------------------------------------------

#Food Habits--------------------------------------------------------------------
#Assign prey to Rpath nodes
prey <- as.data.table(read.csv(file = file.path(data.dir, 'SASPREY12B.csv')))

#Start with 1:1
prey[PYCOMNAM == 'ATLANTIC HERRING',        RPATH := 'AtlHerring']
prey[PYCOMNAM == 'ATLANTIC MACKEREL',       RPATH := 'AtlMackerel']
prey[PYCOMNAM == 'BUTTERFISH',              RPATH := 'Butterfish']
prey[PYCOMNAM == 'BUTTERFISH OTOLITHS',     RPATH := 'Butterfish']
prey[PYCOMNAM == 'ATLANTIC COD',            RPATH := 'Cod']
prey[PYCOMNAM == 'HADDOCK',                 RPATH := 'Haddock']
prey[PYCOMNAM == 'GOOSEFISH',               RPATH := 'Goosefish']
prey[PYCOMNAM == 'OFFSHORE HAKE',           RPATH := 'OffHake']
prey[PYCOMNAM == 'SILVER HAKE',             RPATH := 'SilverHake']
prey[PYCOMNAM == 'SILVER HAKE OTOLITHS',    RPATH := 'SilverHake']
prey[PYCOMNAM == 'RED HAKE',                RPATH := 'RedHake']
prey[PYCOMNAM == 'WHITE HAKE',              RPATH := 'WhiteHake']
prey[PYCOMNAM == 'ACADIAN REDFISH',         RPATH := 'Redfish']
prey[PYCOMNAM == 'POLLOCK',                 RPATH := 'Pollock']
prey[PYCOMNAM == 'OCEAN POUT',              RPATH := 'OceanPout']
prey[PYCOMNAM == 'BLACK SEA BASS',          RPATH := 'BlackSeaBass']
prey[PYCOMNAM == 'BLUEFISH',                RPATH := 'Bluefish']
prey[PYCOMNAM == 'SCUP',                    RPATH := 'Scup']
prey[PYCOMNAM == 'FOURSPOT FLOUNDER',       RPATH := 'Fourspot']
prey[PYCOMNAM == 'SUMMER FLOUNDER',         RPATH := 'SummerFlounder']
prey[PYCOMNAM == 'AMERICAN PLAICE',         RPATH := 'AmPlaice']
prey[PYCOMNAM == 'WINDOWPANE',              RPATH := 'Windowpane']
prey[PYCOMNAM == 'WINTER FLOUNDER',         RPATH := 'WinterFlounder']
prey[PYCOMNAM == 'WITCH FLOUNDER',          RPATH := 'WitchFlounder']
prey[PYCOMNAM == 'YELLOWTAIL FLOUNDER',     RPATH := 'YTFlounder']
prey[PYCOMNAM == 'SPINY DOGFISH',           RPATH := 'SpinyDogfish']
prey[PYCOMNAM == 'SMOOTH DOGFISH',          RPATH := 'SmoothDogfish']
prey[PYCOMNAM == 'LITTLE SKATE',            RPATH := 'LittleSkate']
prey[PYCOMNAM == 'NORTHERN SHORTFIN SQUID', RPATH := 'Illex']
prey[PYNAM    == 'ILLEX SP',                RPATH := 'Illex']
prey[PYCOMNAM == 'AMERICAN LOBSTER',        RPATH := 'AmLobster']
prey[PYCOMNAM == 'KRILL',                   RPATH := 'Krill'] 
prey[PYCOMNAM == 'EMPTY STOMACH',           RPATH := 'Empty']
prey[PYCOMNAM == 'BLOWN STOMACH',           RPATH := 'Blown']
prey[PYCOMNAM == 'NORTHERN SHRIMP',         RPATH := 'NShrimp']
prey[PYCOMNAM == 'HORSESHOE CRAB',          RPATH := 'Megabenthos']
#Easy many to ones
prey[PYCOMNAM %in% c('SEA SCALLOP', 'SEA SCALLOP VISCERA'), RPATH := 'AtlScallop']
prey[PYCOMNAM %in% c('ATLANTIC SURFCLAM', 'SURFCLAM VISCERA', 'OCEAN QUAHOG', 
                     'OCEAN QUAHOG VISCERA', 'OCEAN QUAHOG SHELL'), RPATH := 'Clams']
prey[PYCOMNAM %in% c('LONGFIN SQUID', 'LOLIGO SP PEN'), RPATH := 'Loligo']
prey[PYNAM    %in% c('LOLIGO SP', 'LOLIGO SP BEAKS'),   RPATH := 'Loligo']

#Use higher level categories
#A majority of unassigned prey are in the MODCAT BENINV - whittle out those that
#don't go into macrobenthos then assign the rest

#Megabenthos - Asteroids, mantis shrimp, crabs other than hermits, lobsters
prey[is.na(RPATH) & AnalCom == 'STARFISH', RPATH := 'Megabenthos']
prey[is.na(RPATH) & AnalCom == 'MANTIS SHRIMPS', RPATH := 'Megabenthos']
prey[is.na(RPATH) & Collcom %in% c('CANCER CRABS', 'DECAPODA CRAB', 'DECAPODA', 
                                   'DECAPODA LARVAE', 'LOBSTER', 'BLUE CRAB', 
                                   'SLIPPER LOBSTERS'), RPATH := 'Megabenthos']

#GelZooplankton/Cephalopods/Shrimp
prey[is.na(RPATH) & AnalCom == 'SQUIDS, OCTOPUS', RPATH := 'OtherCephalopods']
prey[is.na(RPATH) & Collcom %in% c('JELLYFISH', 'CNIDARIA', 'HYDROZOA'), RPATH := 'GelZooplankton']       
prey[is.na(RPATH) & Collcom %in% c('PANDALIDAE', 'PENAEIDAE', 'DECAPODA SHRIMP', 
                                   'CRAGONID SHRIMP'), RPATH := 'OtherShrimp']

#Macrobenthos
prey[is.na(RPATH) & MODCAT == 'BENINV', RPATH := 'Macrobenthos']

#MODCAT PELINV
prey[is.na(RPATH) & Collcom == 'COMB JELLIES', RPATH := 'GelZooplankton']
prey[PYCOMNAM == 'ROTIFERS', RPATH := 'Microzooplankton']
prey[is.na(RPATH) & Collcom == 'KRILL', RPATH := 'Krill'] 
prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'Mesozooplankton']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfish']
prey[is.na(RPATH) & ANALCAT == 'RAJORD', RPATH := 'OtherSkates']
prey[is.na(RPATH) & ANALCAT %in% c('LUTFAM', 'SCAFAM', 'SCIFAM', 'SPAFAM', 'SERFA3'), RPATH := 'SouthernDemersals']
prey[is.na(RPATH) & ANALCAT == 'SHARK', RPATH := 'Sharks']
prey[is.na(RPATH) & ANALCAT == 'MACFAM', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & ANALCAT %in% c('PLEFAM', 'PLEORD'), RPATH := 'OtherFlatfish']
prey[is.na(RPATH) & MODCAT == 'LDEM', RPATH := 'OtherDemersals']
#Need to revisit ANALCAT GADFAM - Gadidae, urophycis sp

#MODCAT LPEL
prey[PYCOMNAM %in% c('BOA DRAGONFISH', 'VIPERFISH'), RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL' & ANALCAT %in% c('CARFAM', 'POMFAM', 'SALSAL',
                                                      'SCOFAM', 'OTHFIS'), 
     RPATH := 'OtherPelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL', RPATH := 'SmPelagics']

#MODCAT SDEM
prey[PYCOMNAM == 'DRAGONET FISH', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & AnalCom == 'GREENEYES', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SDEM', RPATH := 'OtherDemersals']

#MODCAT SPEL
#Note - RiverHerring biomass was not present enough on Georges Bank during 2012-2016
#They are included as other pelagics here
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'LANTERNFISHES', RPATH := 'Mesopelagics']
prey[PYABBR == 'MAUWEI', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'HERRINGS', RPATH := 'OtherPelagics']
prey[is.na(RPATH) & MODCAT == 'SPEL', RPATH := 'SmPelagics']

#Fish Larvae
prey[is.na(RPATH) & MODCAT == 'FISLAR', RPATH := 'Micronekton']

#Ignoring eggs for now
prey[is.na(RPATH) & MODCAT == 'FISEGG', RPATH := 'NotUsed']

#Miscellaneous - Mostly trash (plastic, twine, rubber)
prey[PYABBR %in% c('POLLAR', 'AMPTUB'), RPATH := 'Macrobenthos']
prey[is.na(RPATH) & MODCAT == 'MISC', RPATH := 'NotUsed']

#Other
prey[PYABBR %in% c('INVERT', 'ARTHRO', 'CRUSTA', 'CRUEGG', 'INSECT', 'UROCHO'),
     RPATH := 'Macrobenthos']
prey[PYABBR %in% c('MARMAM', 'MARMA2', 'DELDEL', 'GLOBSP'), RPATH := 'ToothWhale']
prey[PYABBR %in% c('AVES', 'AVEFEA'), RPATH := 'Seabirds']
prey[PYABBR %in% c('PLANKT', 'DIATOM'), RPATH := 'Phytoplankton']
prey[is.na(RPATH) & MODCAT == 'OTHER', RPATH := 'NotUsed'] #Plants and Parasites

#Leftovers
prey[AnalCom == 'SAND LANCES', RPATH := 'SmPelagics']
prey[PYABBR %in% c('PERORD', 'MYOOCT'), RPATH := 'OtherDemersals']
prey[PYABBR %in% c('CLUSCA', 'CLUHA2'), RPATH := 'AtlHerring']

#Unidentified Stuff
prey[MODCAT == 'AR', RPATH := 'AR']
prey[is.na(RPATH) & AnalCom == 'OTHER FISH', RPATH := 'UNKFish']
prey[PYABBR == 'FISSCA', RPATH := 'UNKFish']
prey[PYABBR == 'CHONDR', RPATH := 'UNKSkate']
prey[PYABBR == 'PRESER', RPATH := 'NotUsed']



load(file.path(data.dir, 'allfh.RData'))
allfh <- as.data.table(allfh)

#ID GB
x <- copy(allfh)
#Use only stations
stations <- unique(x[!is.na(declon), list(cruise6, stratum, station, declat, declon)], 
                   by = c('cruise6', 'stratum', 'station'))
stations[, declon := declon * -1]

#Convert to spatial points data fram
coordinates(stations) <- ~declon+declat
stations@proj4string <- CRS('+init=epsg:4326') #Lat/Lon code
lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
stations <- spTransform(stations, lcc)

#Grab strata
epu <- readOGR(gis.dir, 'EPU_extended')

#Identify tows within new strata
epu <- spTransform(epu, lcc)
stations$EPU <- over(stations, epu)[, 'EPU']

#Output data (convert spatial data frame back to lat/lon)
stations <- spTransform(stations, CRS('+init=epsg:4326'))
sta.data <- as.data.table(as.data.frame(stations))
sta.data[, c('declat', 'declon') := NULL]
x <- merge(x, sta.data, by = c('cruise6', 'stratum', 'station'))

GB.fh <- x[EPU == 'GB' & !pynam %in% c('EMPTY', 'BLOWN'), 
           list(cruise6, stratum, station, svspp, pdid, pdgutv, pynam, pyamtv)]

#Assign Rpath codes to pred
rpath.code <- unique(species[, list(SVSPP, RPATH)])
setnames(rpath.code, 'SVSPP', 'svspp')
GB.fh <- merge(GB.fh, rpath.code, by = 'svspp', all.x = T)
setnames(GB.fh, 'RPATH', 'Rpred')

#Calculate Percent weight using a cluster sampling design
cluster <- c('cruise6', 'stratum', 'station', 'svspp') #Cluster

#Pred data
pred <- unique(GB.fh, by = c(cluster, 'pdid'))
pred[, c('pynam', 'pyamtv') := NULL]
pred[, Mi := length(pdid), by = cluster]
pred[, wi := sum(pdgutv), by = cluster]

#Reduce to cluster data
sta <- unique(pred, by = cluster)
sta[, c('pdid', 'pdgutv') := NULL]
sta[, sumMi := sum(Mi), by = 'svspp']

#prey data
prey <- GB.fh[, sum(pyamtv), by = c(cluster, 'pynam')]
setnames(prey, 'V1', 'wik')

#merge prey/sta data
preysta <- merge(prey, sta, by = cluster)
preysta[, Miqik := Mi * (wik / wi)]

perwt <- preysta[, sum(Miqik), by = c('svspp', 'pynam')]
setnames(perwt, 'V1', 'sumMiqik')
perwt <- merge(perwt, unique(sta[, list(svspp, sumMi)], by = 'svspp'))
perwt[, perwt := sumMiqik / sumMi]

#Need to combine pret groups to Rpath groups
unique.prey <- unique(perwt[, pynam])
#write.csv(unique.prey, file = file.path(out.dir, 'GB_prey.csv'))

#Pull back in manual categorization
unique.prey <- read.csv(file.path(out.dir, 'GB_prey.csv'))
#Assign to Rpath groups
perwt <- merge(perwt, unique.prey, by = 'pynam')
rpath.diet <- perwt[, sum(perwt), by = c('svspp', 'RPATH')]
setnames(rpath.diet, c('svspp', 'RPATH', 'V1'), c('SVSPP', 'prey', 'DC'))
#Add in Predtaor Rpath groups
rpath.diet <- merge(unique(spp[, list(SVSPP, RPATH)], by = 'SVSPP'), rpath.diet, by = 'SVSPP')
setnames(rpath.diet, 'RPATH', 'pred')
rpath.diet[, SVSPP := NULL]

save(rpath.diet, file = file.path(out.dir, 'GB_diet.RData'))
#Deal with unclassified fish and AR
#