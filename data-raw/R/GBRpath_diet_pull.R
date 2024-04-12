#Diet pull for GBRpath

library(data.table); library(here); library(dbutils); library(tidyverse)

# channel <- dbutils::connect_to_database('sole', 'slucey')

# Call in data ----------------------------------------------------------------
load(here('data-raw', 'allfh.RData'))
load(here('data-raw', 'Species_codes.RData'))

#Food Habits--------------------------------------------------------------------
#Assign prey to Rpath nodes
prey <- as.data.table(read.csv(file = here('data-raw', 'SASPREY12B.csv')))

#Start with 1:1
prey[PYCOMNAM == 'ATLANTIC HERRING',        RPATH := 'AtlHerring']
prey[PYCOMNAM == 'ATLANTIC MACKEREL',       RPATH := 'AtlMackerel']
prey[PYCOMNAM == 'BUTTERFISH',              RPATH := 'Butterfish']
prey[PYCOMNAM == 'BUTTERFISH OTOLITHS',     RPATH := 'Butterfish']
prey[PYCOMNAM == 'ATLANTIC COD',            RPATH := 'Cod']
prey[PYCOMNAM == 'HADDOCK',                 RPATH := 'Haddock']
prey[PYCOMNAM == 'GOOSEFISH',               RPATH := 'Goosefish']
prey[PYCOMNAM == 'OFFSHORE HAKE',           RPATH := 'SilverHake']
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
prey[PYCOMNAM %in% c('SEA SCALLOP', 'SEA SCALLOP VISCERA','SCALLOP,UNC'), RPATH := 'AtlScallop']
prey[PYCOMNAM %in% c('ATLANTIC SURFCLAM', 'SURFCLAM VISCERA'), RPATH := 'SurfClam']
prey[PYCOMNAM %in% c('OCEAN QUAHOG', 'OCEAN QUAHOG VISCERA', 'OCEAN QUAHOG SHELL'), 
                    RPATH := 'OceanQuahog']
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
# prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'Mesozooplankton']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfishes']
prey[is.na(RPATH) & ANALCAT == 'RAJORD', RPATH := 'OtherSkates']
prey[is.na(RPATH) & ANALCAT %in% c('LUTFAM', 'SCAFAM', 'SCIFAM', 'SPAFAM', 'SERFA3'), RPATH := 'SouthernDemersals']
prey[is.na(RPATH) & ANALCAT == 'SHARK', RPATH := 'Sharks']
prey[is.na(RPATH) & ANALCAT == 'MACFAM', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & ANALCAT %in% c('PLEFAM', 'PLEORD'), RPATH := 'OtherDemersals']
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
# allfh.qry <- "select year, season, cruise6, station, stratum, tow, declat, declon,
#              svspp, pdid, pdgutw, pdgutv,
#              pynam, pyamtw, pyamtv, perpyw, perpyv
#              from FHDBS.ALLFH_FEAST"

# allfh <- as.data.table(DBI::dbGetQuery(channel, allfh.qry))


# now referencing local data
# allfh select colouns in allfh.qry
# capitalize column names to match later code
allfh <- allfh |> 
          select(one_of(c('year', 'season', 'cruise6', 'station', 'stratum', 'tow', 'declat', 'declon',
                          'svspp', 'pdid', 'pdgutw', 'pdgutv',
                          'pynam', 'pyamtw', 'pyamtv', 'perpyw', 'perpyv'))) |> 
          rename_with(toupper)

#Subset GB stomachs
#Use only stations
# stations <- unique(allfh[!is.na(DECLON), list(CRUISE6, STRATUM, STATION, TOW, DECLAT, DECLON)], 
#                    by = c('CRUISE6', 'STRATUM', 'STATION', 'TOW'))
# stations[, DECLON := DECLON * -1]
# setnames(stations, c('DECLAT', 'DECLON'), c('LAT', 'LON'))

# Doing the commented code above using tidyverse instead of data.table
# extract unique rows from the allfh data.table where:
# DECLON is not missing.
# The combination of CRUISE6, STRATUM, STATION, and TOW is unique.
#
# reverse the sign of DECLON
# change names of DECLAT, DECLON to LAT, LON

stations <- allfh |> 
            filter(!is.na(DECLON)) |> 
            distinct(CRUISE6, STRATUM, STATION, TOW, DECLAT, DECLON) |> 
            mutate(DECLON = DECLON * -1) |> 
            rename(LAT = DECLAT, LON = DECLON)

#Grab GB shapefile
GB <- sf::read_sf(here('data-raw', 'gis'), 'GB_SOE_strata')

#Post stratify
stations <- survdat::post_strat(stations, GB, 'EPU')

#Merge back
# allfh <- merge(allfh, stations, by = c('CRUISE6', 'STRATUM', 'STATION', 'TOW'),
#                all.x = T)

# Doing the commented code above using tidyverse instead of data.table
allfh <- allfh |> 
  left_join(stations, by = c('CRUISE6', 'STRATUM', 'STATION', 'TOW'))

# GB.fh <- allfh[EPU == 'GB' & !PYNAM %in% c('EMPTY', 'BLOWN') & SEASON == 'FALL', ]
# Doing the commented code above using tidyverse instead of data.table
GB.fh <- allfh  |> 
  filter(EPU == 'GB' & !PYNAM %in% c('EMPTY', 'BOWN') & SEASON == 'FALL')


#Assign Rpath codes to pred
rpath.code <- unique(spp[, list(SVSPP, RPATH)])
#Fix Rpath names that didn't make the Georges Bank model
rpath.code[RPATH == 'AtlCroaker',  RPATH := 'SouthernDemersals']
rpath.code[RPATH == 'AtlHalibut',  RPATH := 'OtherDemersals']
rpath.code[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
rpath.code[RPATH == 'AmShad',      RPATH := 'RiverHerring']
rpath.code[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
rpath.code[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
rpath.code[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
rpath.code[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']

GB.fh <- merge(GB.fh, rpath.code, by = 'SVSPP', all.x = T)
setnames(GB.fh, 'RPATH', 'Rpred')

#Stomach table
#stomachs <- unique(GB.fh[, list(CRUISE6, STRATUM, STATION, SVSPP, PDID, Rpred)])
#table(stomachs[ , Rpred])

#Assign Rpath codes to prey
GB.fh <- merge(GB.fh, prey[, list(PYNAM, RPATH)], by = 'PYNAM', all.x = T)
#Missing two prey for some reason
# GB.fh[PYNAM == 'SELENE SETAPINNIS', RPATH := 'SmPelagics']
# GB.fh[PYNAM == 'EPIGONUS PANDIONIS', RPATH := 'Mesopelagics']
# setnames(GB.fh, 'RPATH', 'Rprey')

# Doing the commented code above using tidyverse instead of data.table
GB.fh <- GB.fh |> 
  mutate(
    RPATH = case_when(
      PYNAM == 'SELENE SETAPINNIS' ~ 'SmPelagics',
      PYNAM == 'EPIGONUS PANDIONIS' ~ 'Mesopelagics',
      TRUE ~ RPATH  # Keep existing values for other PYNAM
    )
  ) |> 
  rename(Rprey = RPATH)


#Remove NotUsed, AR, UNKFish and UNKSkate - Talk to Sarah about how to deal with these
# GB.fh <- GB.fh[!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'), ]
# data.table translated to tidyverse
GB.fh <- GB.fh |> 
  filter(!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'))

#Merge prey items
# setkey(GB.fh, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW, Rpred, PDID, Rprey)
# GB.fh2 <- GB.fh[, .(PYAMTW = sum(PYAMTW)), by = key(GB.fh)]
# data.table translated to tidyverse
GB.fh2 <- GB.fh |> 
  group_by(YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW, Rpred, PDID, Rprey) |> 
  summarize(PYAMTW = sum(PYAMTW))


# Cluster Sampling ------------------------------------------------------------
#Calculate Percent weight using a cluster sampling design (Nelson 2014)
#Clusters are station/Rpred combos
cluster <- c('CRUISE6', 'STRATUM', 'STATION', 'TOW', 'Rpred')

#Calculate numbers of fish per cluster
# GB.pred <- unique(GB.fh2, by = c(cluster, 'PDID'))
# GB.pred[, Mi := length(PDID), by = cluster]
# GB.pred <- unique(GB.pred[, list(CRUISE6, STRATUM, STATION, TOW, Rpred, Mi)])
# GB.pred[, sumMi := sum(Mi), by = Rpred]
# GB.fh2 <- merge(GB.fh2, GB.pred, by = cluster)
# data.table to tidyverse translation

# Unique by CRUISE6, STRATUM, STATION, TOW, Rpred and PDID
GB.pred <- GB.fh2 |> 
  group_by(CRUISE6, STRATUM, STATION, TOW, Rpred, PDID) |> 
  distinct()

# Calculate Mi (count of unique PDID) by CRUISE6, STRATUM, STATION, TOW, Rpred
GB.pred <- GB.pred |> 
  group_by(CRUISE6, STRATUM, STATION, TOW, Rpred) |> 
  mutate(Mi = n_distinct(PDID))

# select only unique combinations of CRUISE6, STRATUM, STATION, TOW, Rpred, Mi
GB.pred <- GB.pred |> 
  distinct(CRUISE6, STRATUM, STATION, TOW, Rpred, Mi)

# Calculate sumMi by Rpred
GB.pred <- GB.pred |> 
  group_by(Rpred) |> 
  mutate(sumMi = sum(Mi))

# Merge GB.fh2 with GB.pred
# GB.fh2 <- merge(GB.fh2, GB.pred, by = cluster)
GB.fh2 <- GB.fh2 |> 
  left_join(GB.pred, by = cluster)


#Sum prey weight per stomach
# GB.fh2[, yij := sum(PYAMTW), by = c(cluster, 'Rprey')]
# GB.fh2[, mu := yij / Mi]
# GB.cluster <- unique(GB.fh2, by = c(cluster, 'Rprey'))
# GB.cluster[, c('PYAMTW', 'yij') := NULL]

# translate data.table to tidyverse
# Calculate yij and Mi
GB.fh2 <- GB.fh2 |> 
  group_by(CRUISE6, STRATUM, STATION, TOW, Rpred, Rprey) |> 
  mutate(
    yij = sum(PYAMTW)
  )

# Calculate mu
GB.fh2 <- GB.fh2 |> 
  mutate(mu = yij / Mi)

# Unique by cluster and Rprey
GB.cluster <- GB.fh2 |> 
  unique(by = c(cluster, 'Rprey'))

# Remove PYAMTW and yij columns
GB.cluster <- GB.cluster |> 
  select(-PYAMTW, -yij)  # Select all columns except PYAMTW and yij


#Calculate weighted contribution
# GB.cluster[, Miu := Mi * mu]
# GB.cluster[, rhat := Miu / sumMi]
# GB.cluster[, sum.rhat := sum(rhat), by = .(Rpred, Rprey)]
# data.table to tidyverse translation

# Calculate Miu
GB.cluster <- GB.cluster |> 
  mutate(Miu = Mi * mu)

# Calculate rhat
GB.cluster <- GB.cluster |> 
  mutate(rhat = Miu / sumMi)

# Calculate sum.rhat
GB.cluster <- GB.cluster |> 
  group_by(Rpred, Rprey) |> 
  mutate(sum.rhat = sum(rhat))


#Grab unique rows
# GB.diet <- unique(GB.cluster[, list(Rpred, Rprey, sum.rhat)], by = c('Rpred', 'Rprey'))
# data.table to tidyverse translation

GB.diet <- GB.cluster |> 
  distinct(Rpred, Rprey, sum.rhat)


#Convert to percentages
# GB.diet[, tot.preyw := sum(sum.rhat), by = Rpred]
# GB.diet[, preyper := sum.rhat / tot.preyw]
# GB.diet[, c('sum.rhat', 'tot.preyw') := NULL]
# setkey(GB.diet, Rpred, preyper)
# data.table to tidyverse translation

GB.diet <- GB.diet |> 
  group_by(Rpred) |> 
  mutate(tot.preyw = sum(sum.rhat)) |> 
  mutate(preyper = sum.rhat / tot.preyw) |> 
  select(-sum.rhat, -tot.preyw)  # Select all columns except sum.rhat and tot.preyw


# Unsurveyed Groups -----------------------------------------------------------
#Add diet for groups not surveyed
Rpred.missing <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 'Sharks',
                   'SmPelagics', 'SouthernDemersals', 'OtherCephalopods',
                   'AmLobster', 'Macrobenthos', 'Megabenthos', 'AtlScallops', 
                   'OceanQuahog','SurfClam', 'OtherShrimps', 'Krill', 'Micronekton', 
                   'GelZooplankton', 'Microzooplankton')

#Use biomass to disaggregate EMAX prey groups where necessary
convert.table <- data.table(RPATH = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 
                                      'HMS', 'Sharks', 'AtlHerring', 'RiverHerring',
                                      'AtlMackerel', 'Butterfish', 'SmPelagics', 
                                      'Mesopelagics', 'OtherPelagics', 'Cod', 
                                      'Haddock', 'Goosefish', 
                                      'SilverHake', 'RedHake', 'WhiteHake', 
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
                                      'AtlScallop', 'OceanQuahog','SurfClam', 'OtherShrimps', 'Krill', 
                                      'Micronekton', 'Micronekton', 'GelZooplankton', 
                                      'LgCopepods', 'SmCopepods', 'Microzooplankton', 
                                      'Phytoplankton', 'Bacteria', 'Discards', 'Detritus'),
                            EMAX = c('Sea Birds', 'Pinnipeds', 'Baleen Whales', 
                                     'Odontocetes', 'HMS', 'Sharks- pelagics', 
                                     'Small Pelagics- commercial', 
                                     'Small Pelagics- anadromous', 
                                     'Small Pelagics- commercial',
                                     'Small Pelagics- commercial', 'Small Pelagics- other',
                                     'Mesopelagics', 'Medium Pelagics- (piscivores & other)',
                                     'Demersals- piscivores', 'Demersals- benthivores',
                                     'Demersals- piscivores',
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
                                     'Megabenthos- filterers', 'Megabenthos- filterers',
                                     'Shrimp et al.', 
                                     'Micronekton', 'Micronekton', 'Larval-juv fish- all', 
                                     'Gelatinous Zooplankton', 'Large Copepods', 
                                     'Small copepods', 'Microzooplankton', 
                                     'Phytoplankton- Primary Producers', 'Bacteria',
                                     'Discard', 'Detritus-POC'))

#remove row with OtherFlatfish
convert.table <- convert.table[RPATH != 'OtherFlatfish']
                                     
load(here('data', 'bio.input.rda'))
#Add biomass to convert.table
groups <- unique(convert.table[, RPATH])
for(igroup in 1:length(groups)){
  if(nrow(bio.input[RPATH == groups[igroup], ]) > 0){
    convert.table[RPATH == groups[igroup], 
                  Biomass := bio.input[RPATH == groups[igroup], B]][]
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

#OtherPelagics
# GB.diet <- GB.diet[!Rpred == 'OtherPelagics', ]
# data.table to tidyverse translation
GB.diet <- GB.diet |> 
  filter(Rpred != 'OtherPelagics')

medpel <- data.table(EMAX = c('Gelatinous Zooplankton',  'Mesopelagics', 
                              'Macrobenthos- crustaceans', 'Macrobenthos- other',
                              'Megabenthos- filterers', 'Megabenthos- other', 
                              'Shrimp et al.', 'Larval-juv fish- all', 
                              'Small Pelagics- commercial', 'Small Pelagics- other',
                              'Small Pelagics- squid', 'Small Pelagics- anadromous',
                              'Medium Pelagics- (piscivores & other)', 'Demersals- benthivores',
                              'Demersals- omnivores', 'Demersals- piscivores', 'Detritus-POC'),
                     DC = c(0.001, 0.003, 0.013, 0.011, 0.003, 0.018, 0.001, 0.002,
                            0.577, 0.044, 0.114, 0.010, 0.011, 0.098, 0.014, 0.079,
                            0.001))
medpel <- merge(medpel, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
#Fix NAs
medpel[EMAX == 'Small Pelagics- anadromous', RPATH := 'SmPelagics'] #no river herring in this model
medpel[EMAX == 'Small Pelagics- anadromous', Rpath.prop := 1]
medpel[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
medpel <- medpel[, sum(preyper), by = RPATH]
medpel[, Rpred := 'OtherPelagics']
setnames(medpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, medpel))

#EMAX groups that apply to more than one Rpath
#Southern Demrsals and Otherflatfish will use Demersal- benthivore diet
# Otherflatfish combined with OtherDemersals to better align with GOM and MAB models
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
# otflat <- copy(dems[, Rpred := 'OtherFlatfish'][])
setnames(south,  c('RPATH', 'V1'), c('Rprey', 'preyper'))
# setnames(otflat, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, south))
# GB.diet.plus <- rbindlist(list(GB.diet.plus, otflat))

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

#AtlScallops, OceanQuahog and SurfClam will use Megabenthos- filters
filter <- data.table(EMAX = c('Phytoplankton- Primary Producers', 'Bacteria',
                              'Detritus-POC'),
                     DC = c(0.69, 0.08, 0.23))
filter <- merge(filter, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
filter[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
filter <- filter[, sum(preyper), by = RPATH]
#Two Rpred
scal <- copy(filter[, Rpred := 'AtlScallop'][])
quahog <- copy(filter[, Rpred := 'OceanQuahog'][])
surfclam <- copy(filter[, Rpred := 'SurfClam'][])
setnames(scal, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(quahog, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(surfclam, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, scal))
GB.diet.plus <- rbindlist(list(GB.diet.plus, quahog))
GB.diet.plus <- rbindlist(list(GB.diet.plus, surfclam))

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

#Illex, Loligo, and other cephalopods
#Need to drop illex and loligo from diet pull
# GB.diet <- GB.diet[!Rpred %in% c('Illex', 'Loligo'), ]
# data.table to tidyverse translation
GB.diet <- GB.diet |> 
  filter(!Rpred %in% c('Illex', 'Loligo'))
ceph <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Macrobenthos- crustaceans',
                            'Macrobenthos- other', 'Shrimp et al.', 'Larval-juv fish- all',
                            'Small Pelagics- commercial', 'Small Pelagics- other',
                            'Small Pelagics- squid', 'Small Pelagics- anadromous'),
                   DC = c(0.1290, 0.4560, 0.0990, 0.0180, 0.0110, 0.1760, 0.0160,
                          0.0180, 0.0770, 0.0001))
ceph <- merge(ceph, convert.table[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX', all.x = T)
#Fix NAs
ceph[EMAX == 'Small Pelagics- anadromous', RPATH := 'SmPelagics'] #no river herring in this model
ceph[EMAX == 'Small Pelagics- anadromous', Rpath.prop := 1]
ceph[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
ceph <- ceph[, sum(preyper), by = RPATH]
#3 Rpath groups
illex  <- copy(ceph[, Rpred := 'Illex'][])
loligo <- copy(ceph[, Rpred := 'Loligo'][])
ceph[, Rpred := 'OtherCephalopods']
setnames(ceph, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(illex, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(loligo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, illex))
GB.diet.plus <- rbindlist(list(GB.diet.plus, loligo))
GB.diet.plus <- rbindlist(list(GB.diet.plus, ceph))

#Need to merge multiple EMAX groups for macrobenthos
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
setnames(combo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

GB.diet.plus <- rbindlist(list(GB.diet.plus, combo))

#Merge diet.plus with regular diet
#using SouthernDemersal and OtherFlatfish diets from EMAX
# GB.diet <- rbindlist(list(GB.diet[!Rpred %in% c('SouthernDemersals', 'OtherFlatfish')],
#                           GB.diet.plus), use.names = T)
# data.table to tidyverse translation

GB.diet <- GB.diet |> 
  filter(!Rpred == 'SouthernDemersals') |> 
  bind_rows(GB.diet.plus)

#Need to add Bacteria
bact <- data.table(Rpred = 'Bacteria', Rprey = 'Detritus', preyper = 1.000)

GB.diet <- rbindlist(list(GB.diet, bact))

#Remove predators not used for Georges Bank (not enough stomachs)
GB.diet <- GB.diet[!Rpred %in% c('Freshwater', 'LargePelagics'), ]

#Fix decimal error from external inputs
diet.input <- copy(GB.diet)
diet.sum <- GB.diet[, .(DC = sum(preyper)), by = Rpred]
DC.fix <- diet.sum[DC != 1, Rpred]
for(ipred in 1:length(DC.fix)){
  DC.diff <- 1 - diet.sum[Rpred == DC.fix[ipred], DC]
  pred.diet <- setorder(GB.diet[Rpred == DC.fix[ipred], ], -preyper)[1]
  diet.input[Rpred == pred.diet[, Rpred] & Rprey == pred.diet[, Rprey],
             preyper := pred.diet[, preyper] + DC.diff]
}

#Precision is a bit overkill - rounding to 4 decimal places
diet.input[, preyper := round(preyper, digits = 4)]
diet.input <- diet.input[preyper != 0,]

#Decided to switch OtherDemersals to diet like southern demersals
demersal <- diet.input[Rpred == 'SouthernDemersals', ]
demersal[, Rpred := 'OtherDemersals']
diet.input <- rbindlist(list(diet.input[Rpred != 'OtherDemersals', ], demersal))

# # Copepod changes -----------------------------------------------------
#Load EMAX diet matrix
EMAX_diet <- read_csv("data/Georges Bank-Diet composition.csv")
# remove last column
EMAX_diet <- EMAX_diet[1:33,1:30]

species_converg <- EMAX_diet[1:31,1:2]
colnames(species_converg) <- c("group_no", "species")

# Set EMAX column names to species names by group_no
colnames(EMAX_diet) <- c('group_no','Rprey',species_converg$species[2:29])

# Remove column 'group_no'
EMAX_diet <- EMAX_diet[,-1]
# Remove final row 'sum'
EMAX_diet <- EMAX_diet[1:32,]

# Pivot longer to table with columns 'Rpred', 'Rprey', 'preyper'
# 'Rpred' is the column name
# 'Rprey'column stays the same
# 'preyper' is the value of the column
EMAX_diet <- EMAX_diet |>
  pivot_longer(cols = -Rprey, names_to = 'Rpred', values_to = 'preyper')

# remove rows where preyper = 0
EMAX_diet <- EMAX_diet[EMAX_diet$preyper != 0,]

# Rename data in columns 'Rprey' and 'Rpred' to RPATH names
EMAX_names <- convert.table$EMAX
RPATH_names <- convert.table$RPATH

EMAX_diet <- EMAX_diet |>
  mutate(Rprey = case_when(
    Rprey %in% EMAX_names ~ RPATH_names[match(Rprey, EMAX_names)],
    TRUE ~ Rprey
  )) |>
  mutate(Rpred = case_when(
    Rpred %in% EMAX_names ~ RPATH_names[match(Rpred, EMAX_names)],
    TRUE ~ Rpred
  ))

# Select just the diet of SmCopepods and LgCopepods
copes_diet <-  EMAX_diet |>
  filter(Rpred %in% c('SmCopepods', 'LgCopepods'))

# Add copes_diet to diet.input
diet.input <- rbind(diet.input, copes_diet)

# Save diet.input -----------------------------------------------------

usethis::use_data(diet.input, overwrite = T)

# # select just the diet of SmCopepods and LgCopepods
# copes_diet <- EMAX_diet[, c(2, 5, 6)]
# # give column headers of SmCopepods and LgCopepods
# setnames(copes_diet, c("4","5"),c("SmCopepods", "LgCopepods"))
# # remove 0 values
# copes_diet <- copes_diet[copes_diet$SmCopepods != 0 | copes_diet$LgCopepods != 0,]
# #remove last two rows
# copes_diet <- copes_diet[1:(nrow(copes_diet)-2),]
# # get in the format of diet.input
# #SmCopepods
# # select first two columns
# smcopes_diet <- copes_diet[, c(1, 2)]
# # rename columns
# setnames(smcopes_diet, c("Prey \\ predator", 'SmCopepods'), c("Rprey", "preyper"))
# # create new column 'Rpred' and set to 'SmCopepods'
# smcopes_diet <- data.table(smcopes_diet, Rpred = 'SmCopepods')
# #LgCopepods
# # select first and third columns
# lgcopes_diet <- copes_diet[, c(1, 3)]
# # rename columns
# setnames(lgcopes_diet, c("Prey \\ predator", 'LgCopepods'), c("Rprey", "preyper"))
# # create new column 'Rpred' and set to 'LgCopepods'
# lgcopes_diet <- data.table(lgcopes_diet, Rpred = 'LgCopepods')
# 
# # combine smcopes_diet and lgcopes_diet
# new_copes_diet <- rbind(smcopes_diet, lgcopes_diet)
# # change Rprey to Rpath names
# new_copes_diet[Rprey == 'Phytoplankton- Primary Producers', Rprey := 'Phytoplankton']
# new_copes_diet[Rprey == 'Gelatinous Zooplankton', Rprey := 'GelZooplankton']
# new_copes_diet[Rprey == 'Macrobenthos- crustaceans', Rprey := 'Macrobenthos']
# new_copes_diet[Rprey == 'Macrobenthos- other', Rprey := 'Macrobenthos']
# new_copes_diet[Rprey == 'Detritus-POC', Rprey := 'Detritus']
# new_copes_diet[Rprey == 'Small copepods', Rprey := 'SmCopepods']
# new_copes_diet[Rprey == 'Large Copepods', Rprey := 'LgCopepods']
# # sum the Macrobenthos Rprey by Rpred
# new_copes_diet <- new_copes_diet[, .(preyper = sum(preyper)), by = .(Rprey, Rpred)]
# 
# #combine diet.input and new_copes_diet
# diet.input <- rbind(diet.input, new_copes_diet)
# 
# # Proportion predation on Messozooplankton by Small and Large Copepods by biomass
# copes.b <- sum(bio.input[RPATH %in% c('SmCopepods', 'LgCopepods'), B])
# s.copes.ratio <- bio.input[RPATH == 'SmCopepods', B] / copes.b
# l.copes.ratio <- bio.input[RPATH == 'LgCopepods', B] / copes.b
# 
# 
# # create new Rprey = 'SmCopepods' with preyper set to preyper for Mesozooplankton * s.copes.ratio
# sm.copes.prey <- diet.input |>
#   filter(Rprey == 'Mesozooplankton') |>
#   mutate(Rprey := 'SmCopepods') |>
#   mutate(preyper := preyper * s.copes.ratio)
# 
# # LgCopepods
# lg.copes.prey <- diet.input |>
#   filter(Rprey == 'Mesozooplankton') |>
#   mutate(Rprey := 'LgCopepods') |>
#   mutate(preyper := preyper * l.copes.ratio)
# 
# 
# # SmPelagics not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# SmPelagics_diet <- diet.input |>
#   filter(Rpred == 'SmPelagics')
# 
# 
# remaining_diet <- 1 - sum(SmPelagics_diet$preyper)
# 
# large <- remaining_diet * 0.7
# small <- remaining_diet * 0.3
# 
# # create new row in sm.copes.prey
# # rpred is SmPelagics and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'SmPelagics', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is SmPelagics and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'SmPelagics', Rprey = 'LgCopepods', preyper = large))
# 
# 
# # GelZooplankton not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# GelZooplankton_diet <- diet.input |>
#   filter(Rpred == 'GelZooplankton')
# 
# remaining_diet <- 1 - sum(GelZooplankton_diet$preyper)
# 
# large <- remaining_diet * 0.4
# small <- remaining_diet * 0.6
# 
# # create new row in sm.copes.prey
# # rpred is GelZooplankton and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'GelZooplankton', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is GelZooplankton and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'GelZooplankton', Rprey = 'LgCopepods', preyper = large))
# 
# # Seabirds not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# Seabirds_diet <- diet.input |>
#   filter(Rpred == 'Seabirds')
# 
# remaining_diet <- 1 - sum(Seabirds_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is GelZooplankton and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'Seabirds', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is GelZooplankton and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'Seabirds', Rprey = 'LgCopepods', preyper = large))
# 
# 
# # BalWhale not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# BalWhale_diet <- diet.input |>
#   filter(Rpred == 'BalWhale')
# 
# remaining_diet <- 1 - sum(BalWhale_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is BalWhale and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'BalWhale', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is BalWhale and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'BalWhale', Rprey = 'LgCopepods', preyper = large))
# 
# 
# # Sharks not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# Sharks_diet <- diet.input |>
#   filter(Rpred == 'Sharks')
# 
# remaining_diet <- 1 - sum(Sharks_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is Sharks and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'Sharks', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is Sharks and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'Sharks', Rprey = 'LgCopepods', preyper = large))
# 
# # Illex not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# Illex_diet <- diet.input |>
#   filter(Rpred == 'Illex')
# 
# remaining_diet <- 1 - sum(Illex_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is Illex and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'Illex', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is Illex and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'Illex', Rprey = 'LgCopepods', preyper = large))
# 
# # Loligo not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# Loligo_diet <- diet.input |>
#   filter(Rpred == 'Loligo')
# 
# remaining_diet <- 1 - sum(Loligo_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is Loligo and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'Loligo', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is Loligo and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'Loligo', Rprey = 'LgCopepods', preyper = large))
# 
# # OtherCephalopods not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# OtherCephalopods_diet <- diet.input |>
#   filter(Rpred == 'OtherCephalopods')
# 
# remaining_diet <- 1 - sum(OtherCephalopods_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is OtherCephalopods and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'OtherCephalopods', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is Loligo and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'OtherCephalopods', Rprey = 'LgCopepods', preyper = large))
# 
# # Macrobenthos not feeding on LgCopepods or SmCopepods but are in
# # the GOM model. Proportioning the missing diet to these groups
# # similar to their proportion in the GOM model
# Macrobenthos_diet <- diet.input |>
#   filter(Rpred == 'Macrobenthos')
# 
# remaining_diet <- 1 - sum(Macrobenthos_diet$preyper)
# 
# large <- remaining_diet * 0.77
# small <- remaining_diet * (1-0.77)
# 
# # create new row in sm.copes.prey
# # rpred is Macrobenthos and rprey is SmCopepods
# # value is small
# sm.copes.prey <- rbind(sm.copes.prey,
#                        data.table(Rpred = 'Macrobenthos', Rprey = 'SmCopepods', preyper = small))
# 
# # create new row in lg.copes.prey
# # rpred is Macrobenthos and rprey is LgCopepods
# # value is large
# lg.copes.prey <- rbind(lg.copes.prey,
#                        data.table(Rpred = 'Macrobenthos', Rprey = 'LgCopepods', preyper = large))
# 
# # combine sm.copes.prey and lg.copes.prey with diet.input
# diet.input <- rbind(diet.input, sm.copes.prey, lg.copes.prey)
# # remove rows with Mesozooplankton in Rprey or Rpred
# diet.input <- diet.input[!(Rprey == 'Mesozooplankton' | Rpred == 'Mesozooplankton')]

# # krill and micronekton should have micronekton diet from EMAX diet
# # remove entries from diet.input where Rpred is Micronekton or krill
# diet.input <- diet.input[!(Rpred == 'Micronekton' | Rpred == 'Krill')]
# # create new krill entries with Rprey = c('Phytoplankton' 'SmCopepods',
# # 'LgCopepods', 'Micronekton', 'Detritus')
# krill_diet <- data.table(Rpred = 'Krill',
#                          Rprey = c('Phytoplankton', 'SmCopepods', 'LgCopepods', 'Micronekton', 'Detritus'),
#                          preyper = c(0.162, 0.308, 0.326, 0.041, 0.163))
# # create new micronekton entries with Rprey = c('Phytoplankton' 'SmCopepods',
# # 'LgCopepods', 'Micronekton', 'Detritus')
# micronekton_diet <- data.table(Rpred = 'Micronekton',
#                                Rprey = c('Phytoplankton', 'SmCopepods', 'LgCopepods', 'Micronekton', 'Detritus'),
#                                preyper = c(0.162, 0.308, 0.326, 0.041, 0.163))
# # combine krill_diet and micronekton_diet with diet.input
# diet.input <- rbind(diet.input, krill_diet, micronekton_diet)





