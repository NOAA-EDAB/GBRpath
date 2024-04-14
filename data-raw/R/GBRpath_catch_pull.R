#Landings/Discard pull for GBRpath
library(data.table); library(usethis); library(here); library(dbutils);
library(comlandr)

#Connect to database
channel <- dbutils::connect_to_database('sole', 'slucey')

## need to figure out unkVar and knStrata fields
GB.stat.areas <- c(521, 522, 525:526, 537, 551, 552, 561, 562)
landings <- comlandr::get_comland_data(channel, filterByYear = 1981:2022 ,filterByArea = GB.stat.areas,
                                       aggGear = T, aggArea = T, 
                                       unkVar = c('MONTH', 'Fleet', 'EPU'),
                                       knStrata = c('HY', 'QY', 'MONTH', 'Fleet', 'TONCL2',
                                                    'EPU'))
landings <- comlandr::get_comland_data(channel, filterByYear = 1981:2022 ,filterByArea = GB.stat.areas,
                                       aggGear = F, aggArea = T, 
                                       unkVar = c('MONTH', 'NEGEAR', 'EPU'),
                                       knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2',
                                                    'EPU'))


saveRDS(landings,here::here("data-raw/landingsNEGEAR.rds"))
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
epu <- sf::st_read(dsn = here::here('data-raw/gis', 'EPU_extended.shp'))
epu.area <- survdat::get_area(epu, 'EPU')
land.index[, Landings := Landings / as.numeric(epu.area[STRATUM == 'GB', Area])]

#Clean up data set
land.index[, Units := 'mt km^-2']
land.index[is.na(Fleet), Fleet := 'Other']

#Input landings
land.input <- land.index[YEAR %in% 1981:1985, .(Landings = mean(Landings, na.rm = T)),
                         by = c('RPATH', 'Fleet')]
land.input[, Units := 'mt km^-2']

#Fix species that didn't make the cut for GB model
land.index[RPATH == 'AtlCroaker',  RPATH := 'SouthernDemersals']
land.index[RPATH == 'AtlHalibut',  RPATH := 'OtherDemersals']
land.index[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
land.index[RPATH == 'AmShad',      RPATH := 'RiverHerring']
land.index[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
land.index[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
land.index[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
land.index[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']
land.input[RPATH == 'AtlCroaker',  RPATH := 'SouthernDemersals']
land.input[RPATH == 'AtlHalibut',  RPATH := 'OtherDemersals']
land.input[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
land.input[RPATH == 'AmShad',      RPATH := 'RiverHerring']
land.input[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
land.input[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
land.input[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
land.input[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']

land.index <- land.index[, .(Landings = sum(Landings)), 
                         by = c('RPATH', 'Fleet', 'YEAR', 'Units')]
setcolorder(land.index, c('RPATH', 'Fleet', 'YEAR', 'Landings', 'Units'))
land.input <- land.input[, .(Landings = sum(Landings)), 
                         by = c('RPATH', 'Fleet', 'Units')]
setcolorder(land.input, c('RPATH', 'Fleet', 'Landings', 'Units'))

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

#Fix species that didn't make the cut for GB model
disc.index[RPATH == 'AtlCroaker',  RPATH := 'SouthernDemersals']
disc.index[RPATH == 'AtlHalibut',  RPATH := 'OtherDemersals']
disc.index[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
disc.index[RPATH == 'AmShad',      RPATH := 'RiverHerring']
disc.index[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
disc.index[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
disc.index[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
disc.index[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']
disc.input[RPATH == 'AtlCroaker',  RPATH := 'SouthernDemersals']
disc.input[RPATH == 'AtlHalibut',  RPATH := 'OtherDemersals']
disc.input[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
disc.input[RPATH == 'AmShad',      RPATH := 'RiverHerring']
disc.input[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
disc.input[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
disc.input[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
disc.input[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']

disc.index <- disc.index[, .(Discards = sum(Discards)), 
                         by = c('RPATH', 'Fleet', 'YEAR', 'Units')]
setcolorder(disc.index, c('RPATH', 'Fleet', 'YEAR', 'Discards', 'Units'))
disc.input <- disc.input[, .(Discards = sum(Discards)), 
                         by = c('RPATH', 'Fleet', 'Units')]
setcolorder(disc.input, c('RPATH', 'Fleet', 'Discards', 'Units'))

#Need to add a row for clam discards (set to zero)
quahog.disc <- data.table(RPATH = 'OceanQuahog', Fleet = 'Clam Dredge', Discards = 0,
                        Units = 'mt km^-2')
disc.input <- rbindlist(list(disc.input, quahog.disc))

surfclam.disc <- data.table(RPATH = 'SurfClam', Fleet = 'Clam Dredge', Discards = 0,
                          Units = 'mt km^-2')
disc.input <- rbindlist(list(disc.input, surfclam.disc))

#Move to data-raw folder
usethis::use_data(disc.input, overwrite = T)
usethis::use_data(disc.index, overwrite = T)


