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
prey[PYCOMNAM == 'NORTHERN SHRIMP',         RPATH := 'OtherShrimps'] #Change to NShrimp for GOM
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
                                   'CRAGONID SHRIMP'), RPATH := 'OtherShrimps']

#Macrobenthos
prey[is.na(RPATH) & MODCAT == 'BENINV', RPATH := 'Macrobenthos']

#MODCAT PELINV
prey[is.na(RPATH) & Collcom == 'COMB JELLIES', RPATH := 'GelZooplankton']
prey[PYCOMNAM == 'ROTIFERS', RPATH := 'Microzooplankton']
prey[is.na(RPATH) & Collcom == 'KRILL', RPATH := 'Krill'] 
prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'Mesozooplankton']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfishes']
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
#stomachs <- unique(GB.fh[, list(CRUISE6, STRATUM, STATION, SVSPP, PDID, Rpred)])
#table(stomachs[ , Rpred])

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

#Add diet for groups not surveyed
Rpred.missing <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 'Sharks',
                   'SmPelagics', 'SouthernDemersals', 'OtherFlatfish', 'OtherCephalopods',
                   'AmLobster', 'Macrobenthos', 'Megabenthos', 'AtlScallops', 
                   'Clams', 'OtherShrimps', 'Krill', 'Micronekton', 'GelZooplankton',
                   'Mesozooplankton', 'Microzooplankton')

#Use biomass to disaggregate EMAX prey groups where necessary
convert.table <- data.table(RPATH = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 
                                      'HMS', 'Sharks', 'AtlHerring', 'AtlMackerel', 
                                      'Butterfish', 'SmPelagics', 'Mesopelagics', 
                                      'OtherPelagics', 'Cod', 'Haddock', 'Goosefish', 
                                      'OffHake', 'SilverHake', 'RedHake', 'WhiteHake', 
                                      'Redfish', 'Pollock', 'OceanPout', 'BlackSeaBass', 
                                      'Bluefish', 'Scup', 'OtherDemersals', 
                                      'SouthernDemersals', 'Fourspot', 'SummerFlounder', 
                                      'AmPlaice', 'Windowpane', 'WinterFlounder', 
                                      'WitchFlounder', 'YTFlounder', 'OtherFlatfish', 
                                      'SmFlatfishes', 'SpinyDogfish', 'SmoothDogfish', 
                                      'Barndoor', 'WinterSkate', 'LittleSkate', 
                                      'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 
                                      'AmLobster', 'Macrobenthos', 'Macrobenthos',
                                      'Macrobenthos', 'Macrobenthos', 'Megabenthos', 
                                      'AtlScallop', 'Clams', 'OtherShrimps', 'Krill', 
                                      'Micronekton', 'Micronekton', 'GelZooplankton', 
                                      'Mesozooplankton', 'Mesozooplankton', 'Microzooplankton', 
                                      'Phytoplankton', 'Bacteria', 'Discards', 'Detritus'),
                            EMAX = c('Sea Birds', 'Pinnipeds', 'Baleen Whales', 
                                     'Odontocetes', 'HMS', 'Sharks- pelagics', 
                                     'Small Pelagics- commercial', 'Small Pelagics- commercial',
                                     'Small Pelagics- commercial', 'Small Pelagics- other',
                                     'Mesopelagics', 'Medium Pelagics- (piscivores & other)',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- piscivores', 'Demersals- piscivores',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- omnivores', 'Medium Pelagics- (piscivores & other)',
                                     'Demersals- benthivores', 'Demersals- benthivores',
                                     'Demersals- benthivores', 'Demersals- benthivores',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- benthivores', 'Demersals- benthivores',
                                     'Demersals- benthivores', 'Demersals- benthivores',
                                     'Demersals- benthivores', 'Demersals- benthivores',
                                     'Demersals- piscivores', 'Demersals- piscivores',
                                     'Demersals- omnivores', 'Demersals- omnivores',
                                     'Demersals- omnivores', 'Demersals- omnivores',
                                     'Small Pelagics- squid', 'Small Pelagics- squid',
                                     'Small Pelagics- squid', 'Megabenthos- other',
                                     'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                                     'Macrobenthos- molluscs', 'Macrobenthos- other',
                                     'Megabenthos- other', 'Megabenthos- filterers', 
                                     'Megabenthos- filterers', 'Shrimp et al.', 
                                     'Micronekton', 'Micronekton', 'Larval-juv fish- all', 
                                     'Gelatinous Zooplankton', 'Large Copepods', 
                                     'Small copepods', 'Microzooplankton', 
                                     'Phytoplankton- Primary Producers', 'Bacteria',
                                     'Discard', 'Detritus-POC'))
                                     
load(file.path(data.dir, 'GB_biomass.RData'))
#Add biomass to convert.table
groups <- unique(convert.table[, RPATH])
for(igroup in 1:length(groups)){
  if(nrow(GB.biomass[RPATH == groups[igroup], ]) > 0){
    convert.table[RPATH == groups[igroup], 
                  Biomass := GB.biomass[RPATH == groups[igroup], Biomass]][]
  }
}
#Calculate proportions of biomass per EMAX group
convert.table[, EMAX.tot := sum(Biomass), by = EMAX]
convert.table[, Rpath.prop := Biomass / EMAX.tot]

#Fix NAs
convert.table[RPATH %in% c('Bacteria', 'Discards', 'Detritus'), Rpath.prop := 1]

#Whale diet from Laurel/new bird data - used in Sarah's GOM
balwhale <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Small Pelagics- commercial',
                                'Small Pelagics- other', 'Small Pelagics- squid',
                                'Demersals- benthivores', 'Demersals- omnivores',
                                'Demersals- piscivores'),
                       DC = c(0.15392800, 0.50248550, 0.15509720, 0.08101361, 
                              0.03142953, 0.01532696, 0.01102390, 0.04969525))
balwhale <- merge(balwhale, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
balwhale[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
balwhale <- balwhale[, sum(preyper), by = RPATH]
balwhale[, Rpred := 'BalWhale']
setnames(balwhale, c('RPATH', 'V1'), c('Rprey', 'preyper'))

tooth <- data.table(EMAX = c('Micronekton', 'Macrobenthos- crustaceans',
                             'Macrobenthos- molluscs', 'Shrimp et al.', 
                             'Small Pelagics- commercial', 'Small Pelagics- other',
                             'Small Pelagics- squid', 'Demersals- benthivores',
                             'Demersals- omnivores', 'Demersals- piscivores'),
                    DC = c(0.000274618, 0.002591398, 0.002591398, 0.000274618, 
                           0.228811900, 0.065838490, 0.365222300, 0.111440100,
                           0.045656540, 0.177298600))
tooth <- merge(tooth, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
tooth[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
tooth <- tooth[, sum(preyper), by = RPATH]
tooth[, Rpred := 'ToothWhale']
setnames(tooth, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(balwhale, tooth))

birds <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Shrimp et al.',
                             'Larval-juv fish- all', 'Small Pelagics- commercial',
                             'Small Pelagics- other', 'Small Pelagics- squid',
                             'Small Pelagics- anadromous', 'Medium Pelagics- (piscivores & other)',
                             'Demersals- benthivores', 'Demersals- omnivores', 
                             'Demersals- piscivores', 'Discard'),
                    DC = c(0.034274510, 0.140525600, 0.021573060, 0.100000000, 
                           0.311898300, 0.268483800, 0.031822620, 0.020179870,
                           0.000353094, 0.020000000, 0.013226180, 0.028562110,
                           0.009100700))
birds <- merge(birds, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', 
               all.x = T)
#Fix NAs
birds[EMAX == 'Small Pelagics- anadromous', RPATH := 'SmPelagics'] #no river herring in this model
birds[EMAX == 'Small Pelagics- anadromous', Rpath.prop := 1]
birds[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
birds <- birds[, sum(preyper), by = RPATH]
birds[, Rpred := 'Seabirds']
setnames(birds, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, birds))

seals <- data.table(EMAX = c('Micronekton', 'Macrobenthos- crustaceans',  
                             'Macrobenthos- molluscs', 'Shrimp et al.',
                             'Small Pelagics- commercial', 'Small Pelagics- other',
                             'Small Pelagics- squid', 'Demersals- benthivores',
                             'Demersals- omnivores', 'Demersals- piscivores'),
                    DC = c(0.003135690, 0.005412052, 0.005412052, 0.003135690,
                           0.135976100, 0.256553000, 0.065668290, 0.251599000,
                           0.024498740, 0.248609400))
seals <- merge(seals, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
seals[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
seals <- seals[, sum(preyper), by = RPATH]
seals[, Rpred := 'Seals']
setnames(seals, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, seals))

#From here down are directly from GB EMAX model
hms <- data.table(EMAX = c('Gelatinous Zooplankton', 'Small Pelagics- commercial',
                           'Small Pelagics- other', 'Small Pelagics- squid'),
                  DC = c(0.103, 0.135, 0.740, 0.022))
hms <- merge(hms, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
hms[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
hms <- hms[, sum(preyper), by = RPATH]
hms[, Rpred := 'HMS']
setnames(hms, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, hms))

sharks <- data.table(EMAX = c('Large Copepods', 'Gelatinous Zooplankton', 'Mesopelagics',
                              'Macrobenthos- crustaceans', 'Macrobenthos- other',
                              'Small Pelagics- commercial', 'Small Pelagics- other',
                              'Small Pelagics- squid', 'Small Pelagics- anadromous',
                              'Medium Pelagics- (piscivores & other)', 'Demersals- benthivores',
                              'Demersals- omnivores', 'Demersals- piscivores', 
                              'Sharks- pelagics', 'HMS', 'Baleen Whales', 'Odontocetes',
                              'Sea Birds', 'Detritus-POC'),
                     DC = c(0.031, 0.010, 0.007, 0.010, 0.010, 0.217, 0.082, 0.166,
                            0.001, 0.124, 0.051, 0.082, 0.072, 0.014, 0.010, 0.010,
                            0.021, 0.031, 0.051))
sharks <- merge(sharks, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
#Fix NAs
sharks[EMAX == 'Small Pelagics- anadromous', RPATH := 'SmPelagics'] #no river herring in this model
sharks[EMAX == 'Small Pelagics- anadromous', Rpath.prop := 1]
sharks[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
sharks <- sharks[, sum(preyper), by = RPATH]
sharks[, Rpred := 'Sharks']
setnames(sharks, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, sharks))

smpel <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Small copepods',
                             'Large Copepods', 'Gelatinous Zooplankton', 'Micronekton',
                             'Macrobenthos- crustaceans', 'Macrobenthos- molluscs',
                             'Macrobenthos- other', 'Larval-juv fish- all', 'Detritus-POC'),
                    DC = c(0.15800, 0.11500, 0.57400, 0.10200, 0.04200, 0.00069,
                           0.00048, 0.00024, 0.00700, 0.00070))
smpel <- merge(smpel, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
smpel[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
smpel <- smpel[, sum(preyper), by = RPATH]
smpel[, Rpred := 'SmPelagics']
setnames(smpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, smpel))

ceph <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Macrobenthos- crustaceans',
                            'Macrobenthos- other', 'Shrimp et al.', 'Larval-juv fish- all',
                            'Small Pelagics- commercial', 'Small Pelagics- other',
                            'Small Pelagics- squid', 'Small Pelagics- anadromous'),
                   DC = c(0.1290, 0.4560, 0.0990, 0.0180, 0.0110, 0.1760, 0.0160,
                          0.0180, 0.0770, 0.0001))
ceph <- merge(ceph, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
ceph[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
ceph <- ceph[, sum(preyper), by = RPATH]
ceph[, Rpred := 'OtherCephalopods']
setnames(ceph, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, ceph))

shrimp <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria',
                              'Micronekton', 'Macrobenthos- crustaceans',
                              'Macrobenthos- other', 'Shrimp et al.', 'Discard',
                              'Detritus-POC'),
                     DC = c(0.06200, 0.36500, 0.12300, 0.00900, 0.01300, 0.00054,
                            0.06200, 0.36500))
shrimp <- merge(shrimp, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
shrimp[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
shrimp <- shrimp[, sum(preyper), by = RPATH]
shrimp[, Rpred := 'OtherShrimps']
setnames(shrimp, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, shrimp))

jelly <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria',
                             'Microzooplankton', 'Small copepods', 'Large Copepods',
                             'Gelatinous Zooplankton', 'Larval-juv fish- all',
                             'Small Pelagics- commercial', 'Small Pelagics- other',
                             'Small Pelagics- squid', 'Small Pelagics- anadromous',
                             'Detritus-POC'),
                    DC = c(0.087000, 0.020000, 0.051000, 0.335000, 0.367000,
                           0.021000, 0.010000, 0.005000, 0.002000, 0.000430,
                           0.000016, 0.102000))
jelly <- merge(jelly, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
jelly[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
jelly <- jelly[, sum(preyper), by = RPATH]
jelly[, Rpred := 'GelZooplankton']
setnames(jelly, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, jelly))

micro <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria',
                             'Microzooplankton', 'Detritus-POC'),
                    DC = c(0.216, 0.160, 0.120, 0.504))
micro <- merge(micro, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
micro[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micro <- micro[, sum(preyper), by = RPATH]
micro[, Rpred := 'Microzooplankton']
setnames(micro, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, micro))

#EMAX groups that apply to more than one Rpath
#Southern Demrsals and Otherflatfish will use Demersal- benthivore diet
dems <- data.table(EMAX = c('Gelatinous Zooplankton', 'Micronekton', 'Macrobenthos- polychaetes',
                            'Macrobenthos- crustaceans', 'Macrobenthos- molluscs',
                            'Macrobenthos- other', 'Megabenthos- filterers',
                            'Megabenthos- other', 'Shrimp et al.', 'Small Pelagics- commercial',
                            'Small Pelagics- other', 'Small Pelagics- squid',
                            'Demersals- benthivores', 'Demersals- omnivores',
                            'Demersals- piscivores', 'Discard', 'Detritus-POC'),
                   DC = c(0.005, 0.001, 0.111, 0.130, 0.111, 0.133, 0.104, 0.133,
                          0.006, 0.110, 0.001, 0.010, 0.076, 0.029, 0.018, 0.011,
                          0.011))
dems <- merge(dems, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
dems[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
dems <- dems[, sum(preyper), by = RPATH]
#Two Rpred
south  <- copy(dems[, Rpred := 'SouthernDemersals'][])
otflat <- copy(dems[, Rpred := 'OtherFlatfish'][])
setnames(south,  c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(otflat, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, south))
GB.diet.plus <- rbindlist(list(GB.diet.plus, otflat))

#AmLobster and Megabenthos will use Megabenthos- other
mega <- data.table(EMAX = c('Bacteria', 'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans', 
                            'Macrobenthos- molluscs', 'Macrobenthos- other', 
                            'Megabenthos- filterers', 'Megabenthos- other', 'Demersals- benthivores',
                            'Demersals- omnivores', 'Demersals- piscivores', 'Discard',
                            'Detritus-POC'),
                   DC = c(1.76e-01, 7.80e-02, 9.80e-02, 3.20e-02, 2.94e-01, 4.80e-02,
                          4.50e-02, 3.00e-03, 2.00e-03, 2.40e-07, 4.80e-02, 1.76e-01))
mega <- merge(mega, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
mega[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
mega <- mega[, sum(preyper), by = RPATH]
#Two Rpred
lob  <- copy(mega[, Rpred := 'AmLobster'][])
mega <- copy(mega[, Rpred := 'Megabenthos'][])
setnames(lob,  c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(mega, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, lob))
GB.diet.plus <- rbindlist(list(GB.diet.plus, mega))

#AtlScallops and Clams will use Megabenthos- filters
filter <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria',
                              'Detritus-POC'),
                     DC = c(0.69, 0.08, 0.23))
filter <- merge(filter, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
filter[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
filter <- filter[, sum(preyper), by = RPATH]
#Two Rpred
scal <- copy(filter[, Rpred := 'AtlScallop'][])
clam <- copy(filter[, Rpred := 'Clams'][])
setnames(scal, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(clam, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, scal))
GB.diet.plus <- rbindlist(list(GB.diet.plus, clam))

#Krill and Micronekton will use Micronekton
micro <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Small copepods',
                             'Large Copepods', 'Micronekton', 'Detritus-POC'),
                    DC = c(0.162, 0.308, 0.326, 0.041, 0.163))
micro <- merge(micro, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
micro[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micro <- micro[, sum(preyper), by = RPATH]
#Two Rpred
krill <- copy(micro[, Rpred := 'Krill'][])
micro <- copy(micro[, Rpred := 'Micronekton'][])
setnames(krill, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(micro, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, krill))
GB.diet.plus <- rbindlist(list(GB.diet.plus, micro))

#Need to merge multiple EMAX groups for macrobenthos and mesozooplankton
#Weight DCs by biomass
macro <- data.table(Group = c('Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                              'Macrobenthos- molluscs', 'Macrobenthos- other'),
                    Biomass = c(11.4, 10.9, 9.9, 40.0))
macro[, macro.prop := Biomass / sum(Biomass)]

#Pull individual DCs
poly <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria', 
                            'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                            'Macrobenthos- molluscs', 'Macrobenthos- other', 
                            'Megabenthos- filterers', 'Megabenthos- other', 'Discard',
                            'Detritus-POC'),
                   DC = c(0.128000, 0.308000, 0.015000, 0.000980, 0.000579,
                          0.003540, 0.003560, 0.000190, 0.005930, 0.534000),
                   macro.prop = macro[Group == 'Macrobenthos- polychaetes', macro.prop])
crus <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria', 
                            'Small copepods', 'Large Copepods', 'Macrobenthos- polychaetes',
                            'Macrobenthos- crustaceans', 'Macrobenthos- molluscs',
                            'Macrobenthos- other', 'Megabenthos- filterers',
                            'Megabenthos- other', 'Demersals- benthivores', 
                            'Demersals- omnivores', 'Discard', 'Detritus-POC'),
                   DC = c(0.21200, 0.18700, 0.01900, 0.03700, 0.01500, 0.00800,
                          0.00800, 0.02600, 0.01800, 0.00029, 0.00085, 0.00051,
                          0.00900, 0.45900),
                   macro.prop = macro[Group == 'Macrobenthos- crustaceans', macro.prop])
moll <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria', 
                            'Macrobenthos- molluscs', 'Macrobenthos- other', 
                            'Megabenthos- filterers', 'Megabenthos- other', 
                            'Discard', 'Detritus-POC'),
                   DC = c(0.43300, 0.19900, 0.00400, 0.00300, 0.01300, 0.00043,
                          0.00600, 0.34200),
                   macro.prop = macro[Group == 'Macrobenthos- molluscs', macro.prop])
oth  <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria', 
                            'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                            'Macrobenthos- molluscs', 'Macrobenthos- other', 
                            'Megabenthos- filterers', 'Megabenthos- other', 
                            'Demersals- benthivores', 'Demersals- omnivores', 
                            'Demersals- piscivores', 'Discard', 'Detritus-POC'),
                   DC = c(2.15e-01, 2.31e-01, 1.70e-02, 2.50e-02, 1.60e-02, 5.70e-02,
                          6.00e-03, 3.00e-03, 8.50e-04, 4.20e-04, 2.40e-07, 1.00e-02,
                          4.19e-01),
                   macro.prop = macro[Group == 'Macrobenthos- other', macro.prop])
combo <- rbindlist(list(poly, crus, moll, oth)) 
combo[, newDC := DC * macro.prop]
combo <- merge(combo, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
combo[, preyper := newDC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
combo <- combo[, sum(preyper), by = RPATH]
combo[, Rpred := 'Macrobenthos']
setnames(combo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, combo))

#Weight DCs by biomass
meso <- data.table(Group = c('Small copepods', 'Large Copepods'),
                    Biomass = c(13.0, 7.0))
meso[, meso.prop := Biomass / sum(Biomass)]

sm <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Microzooplankton',
                          'Small copepods', 'Detritus-POC'),
                 DC = c(0.724, 0.080, 0.065, 0.131),
                 meso.prop = meso[Group == 'Small copepods', meso.prop])
lg <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Microzooplankton',
                          'Small copepods', 'Large Copepods', 'Gelatinous Zooplankton',
                          'Macrobenthos- crustaceans', 'Macrobenthos- other', 
                          'Detritus-POC'),
                 DC = c(0.547000, 0.043900, 0.174000, 0.122000, 0.053100, 0.000192,
                        0.000102, 0.059400),
                 meso.prop = meso[Group == 'Large Copepods', meso.prop])
combo <- rbindlist(list(sm, lg)) 
combo[, newDC := DC * meso.prop]
combo <- merge(combo, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
combo[, preyper := newDC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
combo <- combo[, sum(preyper), by = RPATH]
combo[, Rpred := 'Mesozooplankton']
setnames(combo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, combo))

#Merge diet.plus with regular diet
GB.diet <- rbindlist(list(GB.diet, GB.diet.plus), use.names = T)

#Need to add Bacteria
bact <- data.table(Rpred = 'Bacteria', Rprey = 'Detritus', preyper = 1.000)
GB.diet <- rbindlist(list(GB.diet, bact))

save(GB.diet, file = file.path(data.dir, 'GB_diet.RData'))


