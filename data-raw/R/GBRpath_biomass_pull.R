#Biomass pull for GBRpath
library(here); library(data.table); library(mskeyrun); library(usethis)

#Bottom Trawl Survey-----------------
#Grab species list
load(here('data-raw', 'Species_codes.RData'))

#Grab biomass data from ms-keyrun
bio <- mskeyrun::surveyIndexAll

#Merge with Rpath groups
bio <- merge(bio, unique(spp[, list(SVSPP, RPATH)]), by = 'SVSPP')

bio.index <- bio[variable == 'strat.biomass' & SEASON == 'FALL', 
                 .(Biomass = sum(value)), by = c('RPATH', 'YEAR')]

#Need to expand from kg/tow to mt/km^2
#A tow is standardized to 0.0384 km^2
#kg to mt is 0.001
# so conversion is 0.001 / 0.0384 or 0.02604
bio.index[, B := Biomass * 0.02604]
bio.index[, Biomass := NULL]
bio.index[, Units := 'mt km^-2']

#Add q's from EMAX
emax.q <- spp[, .(q = mean(Fall.q)), by = RPATH]
bio.index <- merge(bio.index, emax.q, by = 'RPATH', all.x = T)
bio.index[, B := B / q]
bio.index[, q := NULL]

#Input biomass
bio.input <- bio.index[YEAR %in% 1981:1985, .(B = mean(B, na.rm = T)), by = RPATH]

#Move to data-raw folder
usethis::use_data(bio.input)

#Shellfish surveys--------------------------------------------------------------
#Scallops and clam survey not included in ms-keyrun data set as they are not 
#used in the other models
library(dbutils); library(DBI); library(sf); library(survdat)

#Connect to the database
channel <- dbutils::connect_to_database('sole', 'slucey')

#user function
sqltext <- function(x){
  out <- x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      out <- paste(out, x[i], sep = "','")
    }
  }
  out <- paste("'", out, "'", sep = '')
  return(out)
}

#Scallops
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 60
               order by year, cruise6"

cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
cruise <- cruise[!is.na(CRUISE6), ]

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Station data
station.qry <- paste("select unique cruise6, svvessel, station, stratum, 
                      decdeg_beglat as lat, decdeg_beglon as lon,
                      avgdepth as depth
                      from Union_fscs_svsta
                      where cruise6 in (", cruise6, ")
                      and SHG <= 136
                      order by cruise6, station", sep='')

station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))

scalldat <- merge(cruise, station, by = c('CRUISE6', 'SVVESSEL'))

#Catch data
catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, 
                    expcatchnum as abundance, expcatchwt as biomass
                    from UNION_FSCS_SVCAT
                    where cruise6 in (", cruise6, ")
                    and svspp = '401'
                    order by cruise6, station, svspp", sep='')

catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))

#merge with scalldat
scalldat <- merge(scalldat, catch, by = c('CRUISE6', 'STATION', 'STRATUM'), 
                  all.x = T)

#use poststrat to assign to EPU
epu <- sf::st_read(dsn = here('gis', 'EPU_extended.shp'))

scalldat.epu <- survdat::post_strat(scalldat, epu, 'EPU')

#Subset for Georges Bank
GB.scall <- scalldat.epu[EPU == 'GB', ]

#Using a simple means within strata but can still use survdat functions
GB.scall.prep <- survdat::strat_prep(GB.scall, epu, 'EPU')
GB.scall.mean <- stratmean(GB.scall.prep, strat.col = 'EPU', poststrat = T)

#Remove NAs and add RPATH column
GB.scall.mean <- GB.scall.mean[!is.na(SVSPP), ]
GB.scall.mean[, RPATH := 'AtlScallop']
GB.scall.mean[, SVSPP := NULL]

#Calculate total biomass/abundance estimates
a.scall <- (0.001317 * 1.852) * (1.0 * 1.852) #convert dredge area/tow distance to km from nm
GB.scall.mean[, prop.swept := A / a.scall]
GB.scall.mean[, swept.bio := strat.biomass * prop.swept]

#Calculate total estimate variance
GB.scall.mean[, swept.var := prop.swept^2 * biomass.var]

#Input for GBRpath
#Biomass needs to be in mt km^-2
#Convert swept.bio to metric tons
GB.scall.mean[, swept.bio.mt := swept.bio * 10^-3]

GB.scall.biomass <- GB.scall.mean[YEAR %in% 2013:2015, mean(swept.bio.mt) / A, by = RPATH]
setnames(GB.scall.biomass, 'V1', 'Biomass')

#Current biomass
GB.scall.current <- GB.scall.mean[YEAR %in% 2016:2018, mean(swept.bio.mt) / A, by = RPATH]
setnames(GB.scall.current, 'V1', 'Biomass')

#Replace scallop biomass from bottomtrawl survey
GB.biomass[RPATH == 'AtlScallop', Biomass := GB.scall.biomass[, Biomass]]
GB.current[RPATH == 'AtlScallop', Biomass := GB.scall.current[, Biomass]]

#Clam survey--------------------------------------------------------------------
#Generate cruise list
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 50
               and year >= 2012
               order by year, cruise6"

cruise <- as.data.table(sqlQuery(channel, cruise.qry))

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Station data
station.qry <- paste("select unique cruise6, svvessel, station, stratum, 
                      decdeg_beglat as lat, decdeg_beglon as lon,
                      avgdepth as depth
                      from Union_fscs_svsta
                      where cruise6 in (", cruise6, ")
                      and SHG <= 136
                      order by cruise6, station", sep='')

station <- as.data.table(sqlQuery(channel, station.qry))
setkey(station, CRUISE6, SVVESSEL)

#merge cruise and station
clamdat <- merge(cruise, station, by = key(station))

#remove station with bad lat/lon
clamdat <- clamdat[!is.na(LON), ]

#Catch data
catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, 
                    expcatchnum as abundance, expcatchwt as biomass
                    from UNION_FSCS_SVCAT
                    where cruise6 in (", cruise6, ")
                    and svspp in ('403', '409')
                    order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))
setkey(catch, CRUISE6, STATION, STRATUM)

#merge with clamdat
clamdat <- merge(clamdat, catch, by = key(catch), all.x = T)

#use poststrat to assign to EPU
clamdat.epu <- poststrat(clamdat, epu)
setnames(clamdat.epu, 'newstrata', 'EPU')

#Subset for Georges Bank
GB.clam <- clamdat.epu[EPU == 'GB', ]

#Using a simple means within strata but can still use survdat functions
GB.clam.prep <- stratprep(GB.clam, epu.area, strat.col = 'EPU', area.col = 'Area')
GB.clam.mean <- stratmean(GB.clam.prep, strat.col = 'EPU', poststrat = T)

#Remove NAs
GB.clam.mean <- GB.clam.mean[!is.na(SVSPP), ]

#Subset for 2013 and 2016
GB.clam.mean <- GB.clam.mean[YEAR %in% c(2013, 2016), ]

#Get mean per species and sum for "clam" group
#Calculate total biomass/abundance estimates
a.clam <- 0.0039624 * 0.374 
GB.clam.mean[, prop.swept := A / a.clam]
GB.clam.mean[, swept.bio := strat.biomass * prop.swept]

#Calculate total estimate variance
GB.clam.mean[, swept.var := prop.swept^2 * biomass.var]

#Biomass needs to be in mt km^-2
#Convert swept.bio to metric tons
GB.clam.mean[, swept.bio.mt := swept.bio * 10^-3]

GB.clam.biomass <- GB.clam.mean[, mean(swept.bio.mt) / A, by = SVSPP]
setnames(GB.clam.biomass, 'V1', 'Biomass')

#Now merge
GB.clam.biomass[, RPATH := 'Clams']
GB.clam.biomass[, SVSPP := NULL]
GB.clam.biomass <- GB.clam.biomass[, sum(Biomass), by = RPATH]

#Add clam biomass from bottomtrawl survey
GB.biomass <- rbindlist(list(GB.biomass, data.table(RPATH = 'Clams', 
                                                    Biomass = GB.clam.biomass[, V1])))
#Current is not different
GB.current <- rbindlist(list(GB.current, data.table(RPATH = 'Clams', 
                                                    Biomass = GB.clam.biomass[, V1])))

#Non Survey groups--------------------------------------------------------------
#Add groups not available in database using EMAX
#Remove macrobenthos from survey
GB.biomass <- GB.biomass[RPATH != 'Macrobenthos', ]
emax <- data.table(RPATH = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 
                             'Macrobenthos', 'Krill', 'Micronekton', 'GelZooplankton', 
                             'Mesozooplankton', 'Microzooplankton', 'Phytoplankton'),
                   Biomass = c(0.015, NA, 0.416, 0.122, 0.035, 104, 3, 4.6, 5.24, 
                               14.25, 3.1, 20))

GB.biomass <- rbindlist(list(GB.biomass, emax))

#Merge raw biomass data to use for biomass accumulation
GB.raw <- rbindlist(list(GB.mean, GB.scall.mean), fill = T)

save(GB.biomass, file = file.path(data.dir, 'GB_biomass.RData'))
save(GB.raw,     file = file.path(data.dir, 'GB_Biomass_raw.RData'))
save(GB.current, file = file.path(data.dir, 'GB_biomass_current.RData'))
