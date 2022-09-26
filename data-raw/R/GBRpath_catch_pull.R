#Landings/Discard pull for GBRpath
library(data.table); library(usethis); library(here); library(dbutils);
library(comlandr)

#Connect to database
channel <- dbutils::connect_to_database('sole', 'slucey')

GB.stat.areas <- c(521, 522, 525:526, 537, 551, 552, 561, 562)
landings <- comlandr::get_comland_data(channel, filterByArea = GB.stat.areas,
                                       aggGear = T, aggArea = T, unkVar = 'EPU',
                                       knStrata = c('NESPP3', 'YEAR', 'HY', 'QY',
                                                    'MONTH', 'Fleet', 'TONCL1',
                                                    'EPU'))
#Merge Rpath groups
load(here('data-raw', 'Species_codes.RData'))
#Fix a couple of duplicate codes
spp[COMNAME %in% c('MORAY UNCL', 'ROCK SEA BASS', 'PUFFER UNCL'), 
    RPATH := 'OtherDemersals']
spp[COMNAME == 'SILVERSIDES', RPATH := 'SmPelagics']
spp[COMNAME == 'GRAY TRIGGERFISH', RPATH := 'SouthernDemersals']

land <- merge(landings$comland[EPU == 'GB', ], unique(spp[!is.na(NESPP3), 
                                                          list(NESPP3, RPATH)]), 
              by = 'NESPP3')

land.index <- land[, .(Landings = sum(SPPLIVMT)), by = c('RPATH', 'Fleet', 'YEAR')]

#Divide by the area of Georges Bank
epu <- sf::st_read(dsn = here::here('gis', 'EPU_extended.shp'))
epu.area <- survdat::get_area(epu, 'EPU')
land.index[, Landings := Landings / as.numeric(epu.area[STRATUM == 'GB', Area])]

#Clean up data set
land.index[, Units := 'mt km^-2']
land.index[is.na(Fleet), Fleet := 'Other']

#Input landings
land.input <- land.index[YEAR %in% 1981:1985, .(Landings = mean(Landings, na.rm = T)),
                         by = c('RPATH', 'Fleet')]
land.input[, Units := 'mt km^-2']

#Move to data-raw folder
usethis::use_data(land.input, overwrite = T)
usethis::use_data(land.index, overwrite = T)

#Discards-----------------------------------------------------------------------
#Observer data base is on nova not sole
channel <- dbutils::connect_to_database('nova', 'slucey')

discards <- comlandr::get_comdisc_data(channel, landings, aggArea = T, aggGear = T)

disc <- merge(discards$comdisc[EPU == 'GB', ], unique(spp[!is.na(NESPP3), 
                                                          list(NESPP3, RPATH)]), 
              by = 'NESPP3')

disc.index <- disc[, .(Discards = sum(DISMT, rm.na = T)), by = c('RPATH', 'Fleet',
                                                                 'YEAR')]
disc.index <- disc.index[!is.na(Discards), ]

#Divide by the area of Georges Bank
disc.index[, Discards := Discards / as.numeric(epu.area[STRATUM == 'GB', Area])]

#Clean up data set
disc.index[, Units := 'mt km^-2']
disc.index[is.na(Fleet), Fleet := 'Other']

#Input landings
disc.input <- disc.index[YEAR %in% 1981:1985, .(Discards = mean(Discards, na.rm = T)),
                         by = c('RPATH', 'Fleet')]
disc.input[, Units := 'mt km^-2']

#Move to data-raw folder
usethis::use_data(disc.input, overwrite = T)
usethis::use_data(disc.index, overwrite = T)


