#Biomass pull for GBRpath
library(here); library(data.table); library(mskeyrun); library(usethis)

#Bottom Trawl Survey-----------------
#Grab species list
load(here('data-raw', 'Species_codes.RData'))

# Replace Rpath values of 'OtherFlatfish' with 'OtherDemersals'
spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
# Replace Rpath values of 'OffHake' with 'SilverHake'
spp[RPATH == 'OffHake', RPATH := 'SilverHake']

#Grab biomass data from ms-keyrun
bio <- mskeyrun::surveyIndexAll

#Merge with Rpath groups
bio <- merge(bio, unique(spp[, list(SVSPP, RPATH)]), by = 'SVSPP')

bio.index <- bio[variable == 'strat.biomass' & SEASON == 'FALL', 
                 .(Biomass = sum(value, na.rm = T)), by = c('RPATH', 'YEAR')]

#Need to expand from kg/tow to mt/km^2
#A tow is standardized to 0.0384 km^2
#kg to mt is 0.001
# so conversion is 0.001 / 0.0384 or 0.02604
bio.index[, B := Biomass * 0.02604]
bio.index[, Biomass := NULL]
bio.index[, Units := 'mt km^-2']
bio.index <- bio.index[!is.na(RPATH), ][]

#Add q's from EMAX
emax.q <- spp[, .(q = mean(Fall.q)), by = RPATH]
emax.q[is.na(q), q := 1]
bio.index <- merge(bio.index, emax.q, by = 'RPATH', all.x = T)
bio.index[, B := B / q]
bio.index[, q := NULL]

#Input biomass
bio.input <- bio.index[YEAR %in% 1981:1985, .(B = mean(B, na.rm = T)), by = RPATH]

#Shellfish surveys--------------------------------------------------------------
#Scallops and clam survey not included in ms-keyrun data set as they are not 
#used in the other models
library(DBI); library(sf); library(survdat)

#Connect to the database
# channel <- dbutils::connect_to_database('sole', 'slucey')

#scall <- survdat::get_survdat_scallop_data(channel, getWeightLength = T)
load(here::here('data', 'survdatScallops.RData'))

#Scallop survey did not record weight prior to 2001 (FSCS) so need to manually
#calculate catch weights
scalldat <- scallops$survdat[, BIOMASS := sum(WGTLEN), by = c('YEAR', 'STATION')]

#Calculate scallop index
#use poststrat to assign to EPU
epu <- sf::st_read(dsn = here::here('data-raw','gis', 'EPU_extended.shp'))

scall.mean <- survdat::calc_stratified_mean(scalldat, areaPolygon = epu,
                                            areaDescription = 'EPU',
                                            filterByArea = 'GB',
                                            filterBySeason = 'SUMMER', tidy = T)

scall.index <- scall.mean[variable == 'strat.biomass', .(Biomass = value), by = YEAR]

#Need to expand from kg/tow to mt/km^2
#A tow is approximately 0.0045 km^2 
# 0.001317 (dredge width in nautical miles) * 1.852(convert naut mi to km)
# 1.0 (tow length in nautical miles) * 1.852(convert naut mi to km)
#kg to mt is 0.001
# so conversion is 0.001 / 0.0045 or 0.222
scall.index[, B := Biomass * 0.222]
scall.index[, Biomass := NULL]
scall.index[, Units := 'mt km^-2']
scall.index[, RPATH := 'AtlScallop']

#Input biomass
scall.input <- scall.index[YEAR %in% 1981:1985, .(B = mean(B, na.rm = T)),
                           by = RPATH]

#Replace scallop biomass from bottomtrawl survey
bio.input[RPATH == 'AtlScallop', B := scall.input[, B]]


#Clam survey--------------------------------------------------------------------
#clam <- survdat::get_survdat_clam_data(channel)
load(here::here('data', 'survdatClams.RData'))

#Use GB clam region to calculate biomass
clam.index <- clams$data[!is.na(SVSPP) & clam.region == 'GB', 
                       .(B = mean(BIOMASS.MW, na.rm = T)), by = c('YEAR', 'SVSPP')]

#Need to expand from kg/tow to mt/km^2
# Clam tows can vary greatly by I'll use an example tow as the expansion
# 0.0039624 (dredge width in km) * 0.374(tow length in km) = 0.00148 
#kg to mt is 0.001
# so conversion is 0.001 / 0.00148 or 0.6757
clam.index[, B := B * 0.6757]
clam.index[, Units := 'mt km^-2']
clam.index$RPATH <- ifelse(clam.index$SVSPP == 409, 'OceanQuahog','SurfClam')
#clam.index[, RPATH := 'Clams']

#Input biomass
clam.input <- clam.index[YEAR %in% 1981:1985, .(B = mean(B, na.rm = T)),
                           by = RPATH]
#Add clam biomass 
bio.input <- rbindlist(list(bio.input, clam.input))

#Non Survey groups--------------------------------------------------------------
#Add groups not available in database using EMAX
#Remove macrobenthos from survey
emax <- data.table(RPATH = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 
                             'Sharks', 'Macrobenthos', 'Krill', 'Micronekton', 
                             'GelZooplankton', 'LgCopepods','SmCopepods', 'Microzooplankton', 
                             'Phytoplankton', 'Bacteria'),
                   B = c(0.015, NA, 0.416, 0.122, 0.035, 0.024, 104, 0.6457, 3.1593, 
                         5.24, 6.981,12.99, 3.1, 19.773, 3.456))



# bind to bio.input
bio.input <- rbindlist(list(bio.input[RPATH != 'Macrobenthos'], emax))

#No other cephalopods in 81 - 85
bio.input <- rbindlist(list(bio.input, data.table(RPATH = 'OtherCephalopods',
                                                  B = 0.001)))

#Fix species that didn't make the cut for GB model
bio.index[RPATH == 'AtlHalibut',  RPATH := 'OtherFlatfish']
bio.index[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
bio.index[RPATH == 'AmShad',      RPATH := 'RiverHerring']
bio.index[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
bio.index[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
bio.index[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
bio.index[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']
bio.input[RPATH == 'AtlHalibut',  RPATH := 'OtherFlatfish']
bio.input[RPATH == 'Weakfish',    RPATH := 'SouthernDemersals']
bio.input[RPATH == 'AmShad',      RPATH := 'RiverHerring']
bio.input[RPATH == 'StripedBass', RPATH := 'OtherDemersals']
bio.input[RPATH == 'Tilefish',    RPATH := 'SouthernDemersals']
bio.input[RPATH == 'RedCrab',     RPATH := 'Megabenthos']
bio.input[RPATH == 'NShrimp',     RPATH := 'OtherShrimps']

bio.index <- bio.index[, .(B = sum(B)), by = c('RPATH', 'YEAR', 'Units')]
setcolorder(bio.index, c('RPATH', 'YEAR', 'B', 'Units'))
bio.input <- bio.input[, .(B = sum(B)), by = 'RPATH']

#Move to data-raw folder
usethis::use_data(bio.input, overwrite = T)
usethis::use_data(bio.index, overwrite = T)

