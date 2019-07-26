#Biomass pull for GBRpath
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
library(Survdat); library(data.table); library(rgdal); library(fitdistrplus)
load(file.path(data.dir, 'SOE_species_list.RData'))

#User functions-----------------------------------------------------------------
#Convert output to text for RODBC query
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

#Bottom trawl-------------------------------------------------------------------
#Generate cruise list
cruise.qry <- "select unique year, cruise6, season
              from mstr_cruise
              where purpose_code = 10
              and year >= 2012
              and year <= 2016
              and season = 'FALL'
              order by year, cruise6"

cruise <- as.data.table(sqlQuery(channel, cruise.qry))

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Station data
station.qry <- paste("select unique cruise6, svvessel, station, stratum,
                      tow, decdeg_beglat as lat, decdeg_beglon as lon, 
                      begin_est_towdate as est_towdate
                      from union_fscs_svsta
                      where cruise6 in (", cruise6, ")
                      and TOGA <= 1324
                      order by cruise6, station", sep='')
  
station <- as.data.table(sqlQuery(channel, station.qry))

#Grab swept area
sweptarea.qry <- paste("select unique cruise6, station, area_swept_wings_mean_km2 as sweptarea
                     from tow_evaluation
                     where cruise6 in (", cruise6, ")", sep = '')

sweptarea <- as.data.table(sqlQuery(channel, sweptarea.qry))

station <- merge(station, sweptarea, by = c('CRUISE6', 'STATION'))

#Replace sweptarea NAs with average sweptarea
station[, mean.swept := mean(SWEPTAREA, na.rm = T), by = c('CRUISE6', 'STRATUM')]
station[is.na(SWEPTAREA), SWEPTAREA := mean.swept]
#2013 strata 01250 had no sweptareas so using average for the year
sweptarea.13 <- mean(station[CRUISE6 == 201304, SWEPTAREA], na.rm = T)
station[CRUISE6 == 201304 & STRATUM == 1250, SWEPTAREA := sweptarea.13]

#merge cruise and station
survdat <- merge(cruise, station, by = 'CRUISE6')
setkey(survdat, CRUISE6, STATION, STRATUM, TOW)

#Catch data
catch.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex, 
                   expcatchnum as abundance, expcatchwt as biomass
                   from UNION_FSCS_SVCAT
                   where cruise6 in (", cruise6, ")
                   and stratum not like 'YT%'
                   order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))

#merge with survdat
survdat <- merge(survdat, catch, by = key(survdat), all = T)
#Remove catch from non-rep tows
survdat <- survdat[!is.na(LON), ]

#Assign Rpath species designations
svspp.rpath <- unique(species[!is.na(SVSPP), list(SVSPP, RPATH)])

survdat <- merge(survdat, svspp.rpath, by = 'SVSPP', all.x = T)

#Assign catch to EPUs
#Grab strata
epu <- readOGR(gis.dir, 'EPU_extended')

#Generate area table
epu.area <- getarea(epu, 'EPU')

#use poststrat to assign to EPU
survdat.epu <- poststrat(survdat, epu)
setnames(survdat.epu, 'newstrata', 'EPU')

#Subset for Georges Bank
GB <- survdat.epu[EPU == 'GB', ]

#Using a simple means within strata but can still use survdat functions
GB.prep <- stratprep(GB, epu.area, strat.col = 'EPU', area.col = 'Area')
GB.mean <- stratmean(GB.prep, group.col = 'RPATH', strat.col = 'EPU', poststrat = T)

#Calculate minimum swept area biomass estimates (i.e. q = 1)
#Georges Bank area
A <- epu.area[EPU == 'GB', Area]
#Using wings swept area per tow
mean.swept <- GB[, mean(SWEPTAREA), by = YEAR]
setnames(mean.swept, 'V1', 'mean.swept')
mean.swept[, A := A]
mean.swept[, prop.swept := A / mean.swept]

#Calculate minimum swept area estimate
GB.mean <- merge(GB.mean, mean.swept[, list(YEAR, prop.swept)], by = 'YEAR')
GB.mean[, swept.bio := strat.biomass * prop.swept]

#Calculate total estimate variance
GB.mean[, swept.var := prop.swept^2 * biomass.var]

#Input for GBRpath
#Biomass needs to be in mt km^-2
#Convert swept.bio to metric tons
GB.mean[, swept.bio.mt := swept.bio * 10^-3]

GB.biomass <- GB.mean[YEAR %in% 2013:2015, mean(swept.bio.mt) / A, by = RPATH]
setnames(GB.biomass, 'V1', 'Biomass')

#Shellfish surveys--------------------------------------------------------------
#Scallops
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 60
               and year >= 2012
               and year <= 2016
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

scalldat <- merge(cruise, station, by = c('CRUISE6', 'SVVESSEL'))

#Catch data
catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, 
                    expcatchnum as abundance, expcatchwt as biomass
                    from UNION_FSCS_SVCAT
                    where cruise6 in (", cruise6, ")
                    and svspp = '401'
                    order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))

#merge with scalldat
setkey(catch, CRUISE6, STATION, STRATUM)
scalldat <- merge(scalldat, catch, by = key(catch), all.x = T)

#use poststrat to assign to EPU
scalldat.epu <- poststrat(scalldat, epu)
setnames(scalldat.epu, 'newstrata', 'EPU')

#Subset for Georges Bank
GB.scall <- scalldat.epu[EPU == 'GB', ]

#Using a simple means within strata but can still use survdat functions
GB.scall.prep <- stratprep(GB.scall, epu.area, strat.col = 'EPU', area.col = 'Area')
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

#Replace scallop biomass from bottomtrawl survey
GB.biomass[RPATH == 'AtlScallop', Biomass := GB.scall.biomass[, Biomass]]

#Clam survey--------------------------------------------------------------------
#Generate cruise list
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 50
               and year >= 2012
               and year <= 2016
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
setcolorder(GB.scall.mean, colnames(GB.mean))
GB.raw <- rbindlist(list(GB.mean, GB.scall.mean))

save(GB.biomass, file = file.path(data.dir, 'GB_biomass.RData'))
save(GB.raw,     file = file.path(data.dir, 'GB_Biomass_raw.RData'))
