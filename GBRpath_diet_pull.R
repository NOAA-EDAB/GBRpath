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

library(RODBC)
uid <- 'slucey'
cat("Oracle Password: ")
pwd <- scan(stdin(), character(), n = 1)

channel <- odbcConnect('sole', uid, pwd)

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
prey[PYCOMNAM == 'NORTHERN SHRIMP',         RPATH := 'OtherShrimp'] #Change to NShrimp for GOM
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

#Pull diet data
allfh.qry <- "select year, season, cruise6, station, stratum, tow, declat, declon,
             svspp, pdid, pdgutw, pdgutv,
             pynam, pyamtw, pyamtv, perpyw, perpyv
             from FHDBS.ALLFH_FEAST"

allfh <- as.data.table(sqlQuery(channel, allfh.qry))

#Subset GB stomachs
#Use only stations
stations <- unique(allfh[!is.na(DECLON), list(CRUISE6, STRATUM, STATION, TOW, DECLAT, DECLON)], 
                   by = c('CRUISE6', 'STRATUM', 'STATION', 'TOW'))
stations[, DECLON := DECLON * -1]

#Convert to spatial points data fram
coordinates(stations) <- ~DECLON+DECLAT
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
sta.data[, c('DECLAT', 'DECLON') := NULL]
allfh <- merge(allfh, sta.data, by = c('CRUISE6', 'STRATUM', 'STATION', 'TOW'))

GB.fh <- allfh[EPU == 'GB' & !PYNAM %in% c('EMPTY', 'BLOWN'), ]

#Assign Rpath codes to pred
rpath.code <- unique(species[, list(SVSPP, RPATH)])
GB.fh <- merge(GB.fh, rpath.code, by = 'SVSPP', all.x = T)
setnames(GB.fh, 'RPATH', 'Rpred')

#Stomach table
stomachs <- unique(GB.fh[, list(CRUISE6, STRATUM, STATION, SVSPP, PDID, Rpred)])
table(stomachs[ , Rpred])

#Assign Rpath codes to prey
GB.fh <- merge(GB.fh, prey[, list(PYNAM, RPATH)], by = 'PYNAM', all.x = T)
#Missing two prey for some reason
GB.fh[PYNAM == 'SELENE SETAPINNIS', RPATH := 'SmPelagics']
GB.fh[PYNAM == 'EPIGONUS PANDIONIS', RPATH := 'Mesopelagics']
setnames(GB.fh, 'RPATH', 'Rprey')

#Remove NotUsed, AR, UNKFish and UNKSkate - Talk to Sarah about how to deal with these
GB.fh <- GB.fh[!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'), ]

#Merge prey items
setkey(GB.fh, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW, Rpred, PDID, Rprey)
GB.fh2 <- GB.fh[, sum(PYAMTW), by = key(GB.fh)]
setnames(GB.fh2, 'V1', 'PYAMTW')

#Calculate Percent weight using a cluster sampling design (Nelson 2014)
#Clusters are station/Rpred combos
cluster <- c('CRUISE6', 'STRATUM', 'STATION', 'TOW', 'Rpred')

#Calculate numbers of fish per cluster
GB.pred <- unique(GB.fh2, by = c(cluster, 'PDID'))
GB.pred[, Mi := length(PDID), by = cluster]
GB.pred <- unique(GB.pred[, list(CRUISE6, STRATUM, STATION, TOW, Rpred, Mi)])
GB.pred[, sumMi := sum(Mi), by = Rpred]
GB.fh2 <- merge(GB.fh2, GB.pred, by = cluster)

#Sum prey weight per stomach
GB.fh2[, yij := sum(PYAMTW), by = c(cluster, 'Rprey')]
GB.fh2[, mu := yij / Mi]
GB.cluster <- unique(GB.fh2, by = c(cluster, 'Rprey'))
GB.cluster[, c('PYAMTW', 'yij') := NULL]

#Calculate weighted contribution
GB.cluster[, Miu := Mi * mu]
GB.cluster[, rhat := Miu / sumMi]

#Grab unique rows
GB.diet <- unique(GB.cluster[, list(Rpred, Rprey, rhat)], by = c('Rpred', 'Rprey'))

#Convert to percentages
GB.diet[, tot.preyw := sum(rhat), by = Rpred]
GB.diet[, preyper := rhat / tot.preyw]
GB.diet[, c('rhat', 'tot.preyw') := NULL]
setkey(GB.diet, Rpred, preyper)

save(GB.diet, file = file.path(data.dir, 'GB_diet.RData'))
#Deal with unclassified fish and AR
