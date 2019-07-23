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



#GB.tows <- unique(GB.prep[, list(YEAR, ntows)])

#Sum biomass by Rpath groups
GB.sums <- GB[, sum(BIOMASS), by = c('YEAR', 'RPATH', 'STATION', 'SWEPTAREA')]
setnames(GB.sums, 'V1', 'sta.bio')
GB.sums[, n.pos := length(STATION), by = c('YEAR', 'RPATH')]
GB.sums[, tot.bio := sum(sta.bio), by = c('YEAR', 'RPATH')]

#Calculate yearly mean
GB.sums <- merge(GB.sums, GB.tows, by = 'YEAR')
GB.sums[, mean.bio := tot.bio / ntows]

#Calculate variance - Need to account for zero tows
GB.sums[, zero.tow := ntows - n.pos]
GB.sums[, zero.var := zero.tow * (0 - mean.bio)^ 2]
GB.sums[, pos.var  := sum((sta.bio - mean.bio)^ 2), by = c('YEAR', 'RPATH')]
GB.sums[, var.bio  := (zero.var + pos.var) / (ntows - 1)]

#Calculate minimum swept area biomass estimates (i.e. q = 1)
#Using wings swept area per tow

#Georges Bank area
A <- epu.area[EPU == 'GB', Area]
#Calculate minimum swept area estimate
GB.sums[, swept.bio := mean.bio * A / SWEPTAREA]
#Calculate total estimate variance
GB.sums[, swept.var := (A / SWEPTAREA)^2 * var.bio]

GB.biomass <- unique(GB.sums[, list(YEAR, RPATH, swept.bio, swept.var)], 
                     by = c('YEAR', 'RPATH'))

#Input for GBRpath
#Biomass needs to be in mt km^-2
#Convert swept.bio to metric tons
GB.biomass[, swept.bio.mt := swept.bio * 10^-3]

GB.input <- GB.biomass[YEAR %in% 2013:2015, mean(swept.bio.mt) / A, by = RPATH]
setnames(GB.input, 'V1', 'Biomass')
save(GB.input, file = file.path(out.dir, 'GB_Rpath_inputs.RData'))

#Biomass Accumulation-----------------------------------------------------------
#Use lm test for significant trend - only add significant terms
groups <- GB.input[, RPATH]
BA.all <- c()
for(igroup in groups){
  species <- GB.biomass[RPATH == igroup, ]
  #Default is BA = 0 unless significant
  BA.sp <- data.table(RPATH = igroup, BA = 0)
  #Test significance using lm
  if(length(species[, swept.bio.mt]) > 2){
    spLM <- lm(swept.bio.mt ~ YEAR, data = species)
    spF <- summary(spLM)$fstatistic
    spP <- pf(spF[1], spF[2], spF[3], lower = F)
    #If significant replace 0 with slope divided by GB area (scaled)
    if(spP <= 0.05) BA.sp[, BA := spLM$coefficients[2] / A]
  }
  BA.all <- rbindlist(list(BA.all, BA.sp))
}

#Double check graphically
BA.sig <- BA.all[BA != 0, RPATH]
for(isp in 1:length(BA.sig)){
  plot(GB.biomass[RPATH == BA.sig[isp], list(YEAR, swept.bio.mt)])
  mtext(3, text = BA.sig[isp])
}
#Merge BA with other inputs
GB.input <- merge(GB.input, BA.all, all.x = T, by = 'RPATH')
save(GB.input, file = file.path(out.dir, 'GB_Rpath_inputs.RData'))

#Biological Parameters----------------------------------------------------------
#Get QB/PB from FishBase
library(rfishbase)

#First build list of fish names
fish <- unique(spp[RPATH %in% c('AmPlaice', 'AmShad', 'AtlHalibut', 'AtlHerring',
                         'AtlMackerel', 'Barndoor', 'BlackSeaBass', 'Bluefish',
                         'Butterfish', 'Cod', 'Fourspot', 'Goosefish', 'Haddock',
                         'LittleSkate', 'OceanPout', 'OffHake', 'Pollock',
                         'RedHake', 'Redfish', 'RiverHerring', 'Scup', 'SilverHake',
                         'SmoothDogfish', 'SpinyDogfish', 'StripedBass', 
                         'SummerFlounder', 'Weakfish', 'WhiteHake', 'Windowpane',
                         'WinterFlounder', 'WinterSkate', 'WitchFlounder',
                         'YTFlounder'), list(RPATH, SCINAME)], by = 'SCINAME')

#Validate names
fish <- validate_names(as.character(fish[, SCINAME]))

#Query the data base
pb.fishbase <- as.data.table(species(fish, fields = c('sciname', 'LongevityWild')))
pb.fishbase[, PB := 1 / LongevityWild]
qb.fishbase <- as.data.table(popqb(fish, fields = c('sciname', 'PopQB')))
qb.fishbase <- qb.fishbase[, mean(PopQB), by = 'sciname']
setnames(qb.fishbase, 'V1', 'QB')

fish.params <- merge(pb.fishbase, qb.fishbase, by = 'sciname', all = T)
fish.params[, c('LongevityWild', 'SpecCode') := NULL]

#Add RPATH code back on
fish.params[, sciname := toupper(sciname)]
setnames(fish.params, 'sciname', 'SCINAME')
fish.params <- merge(fish.params, unique(spp[, list(RPATH, SCINAME)], by = 'SCINAME'),
                     all.x = T)
#There are a couple species with extra spaces at the end...need to fix this
fish.params[SCINAME %like% 'PSEUDOPLEURO',  RPATH := 'WinterFlounder']
fish.params[SCINAME %like% 'LIMANDA',       RPATH := 'YTFlounder']
fish.params[SCINAME %like% 'HIPPOGLOSSINA', RPATH := 'Fourspot']

#A couple have more than one species - take average for now
fish.params[, PB := mean(PB, na.rm = T), by = RPATH]
fish.params[, QB := mean(QB, na.rm = T), by = RPATH]
fish.params <- unique(fish.params[, list(RPATH, PB, QB)], by = 'RPATH')

#Merge params with other inputs
GB.input <- merge(GB.input, fish.params, all.x = T, by = 'RPATH')

#Fisheries Data-----------------------------------------------------------------
#Pull landings from 2013 to 2015
landings.qry <- paste0("select year, month, NEGEAR, mesh, nespp3, nespp4, area, spplivlb, link
                     from stockeff.MV_CF_Landings
                     where YEAR in (2013, 2014, 2015)")

landings <- as.data.table(sqlQuery(channel, landings.qry))
save(landings, file = file.path(out.dir, 'landings_13_15.RData'))
#load(file = file.path(out.dir, 'landings_13_15.RData'))

#Create small/large mesh gear categories
#July 2015 - minimum mesh size = 6.5 inches
#https://www.greateratlantic.fisheries.noaa.gov/regs/infodocs/small_mesh_exemption.pdf
otter.trawl <- landings[NEGEAR %in% 50:59, ]

#Assign large vs small based on mesh size
otter.trawl[MESH < 6.5,  Cat := factor('Small', levels = c('Small', 'Large'))]
otter.trawl[MESH >= 6.5, Cat := 'Large']

#Assign unknown mesh size by species composition by area/gear
areas <- unique(otter.trawl[, AREA])
for(iarea in 1:length(areas)){
  area.land <- otter.trawl[AREA == areas[iarea], ]
  gear <- unique(area.land[, NEGEAR])
  for(igear in 1:length(gear)){
    gear.land <- area.land[NEGEAR == gear[igear], ]
    sp <- unique(gear.land[is.na(Cat), NESPP3])
    for(isp in 1:length(sp)){
      sp.land <- gear.land[NESPP3 == sp[isp], ]
      prop.small <- sp.land[Cat == 'Small', sum(SPPLIVLB)] / sp.land[!is.na(Cat), sum(SPPLIVLB)]
    }
  }
}

#fw <- fitdist(sp.land[Cat == 'Small', SPPLIVLB], "pois")
#fg <- fitdist(sp.land[Cat == 'Small', SPPLIVLB], "norm")
fln <- fitdist(sp.land[Cat == 'Small', SPPLIVLB], "lnorm")
fln.lg <- fitdist(sp.land[Cat == 'Large', SPPLIVLB], "lnorm")

sm.meanlog <- fln$estimate[1]
sm.sdlog <- fln$estimate[2]
lg.meanlog <- fln.lg$estimate[1]
lg.sdlog <- fln.lg$estimate[2]

#test a very small value
plnorm(5, sm.meanlog, sm.sdlog)
plnorm(5, lg.meanlog, lg.sdlog)

#test a small value
plnorm(25, sm.meanlog, sm.sdlog, lower.tail = F)
plnorm(25, lg.meanlog, lg.sdlog, lower.tail = F)

#test medium
plnorm(150, sm.meanlog, sm.sdlog, lower.tail = F)
plnorm(150, lg.meanlog, lg.sdlog, lower.tail = F)

#test large
plnorm(500, sm.meanlog, sm.sdlog, lower.tail = F)
plnorm(500, lg.meanlog, lg.sdlog, lower.tail = F)


par(mfrow = c(2, 2))
plot.legend <- c("pois", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

plnorm(x, meanlog, sdlog) #From fitdist


#Convert landings to metric tons
landings[, SPPLIVMT := SPPLIVLB * 0.00045359237]
landings[, SPPLIVLB := NULL]

#Remove market categories of parts
landings <- landings[!NESPP4 %in% c(119, 123, 125, 127, 812, 819, 828, 829, 1731, 2351,
                                  2690, 2699, 3472, as.numeric(paste(348:359, 8, sep = '')), 
                                  3868, as.numeric(paste(469:471, 4, sep = '')), 
                                  as.numeric(paste(480:499, 8, sep ='')), 5018, 5039, 
                                  5261, 5265), ]

#Merge Rpath groups
load(file.path(data.dir, 'SOE_species_list.RData'))
nespp.rpath <- unique(species[!is.na(NESPP3), list(NESPP3, RPATH)])

landings <- merge(landings, nespp.rpath, by = 'NESPP3', all.x = T)

#Need to figure out gear categories
#Broad Gear Categories
gcat <- c('otter', 'dredge.sc', 'pot', 'longline', 'seine', 'gillnet', 'midwater',
          'dredge.o')
otter     <- 50:59
dredge.sc <- 131:132
pot       <- c(180:190, 200:219, 300, 301)
longline  <- c(10, 40)
seine     <- c(70:79, 120:129, 360)
gillnet   <- c(100:119, 500, 510, 520)
midwater  <- c(170, 370)
dredge.o  <- c(281, 282, 380:400)

gcat.all <- c()
for(icat in 1:length(gcat)){
  gcat.land <- landings[NEGEAR %in% get(gcat[icat]), ]
  gcat.sums <- gcat.land[, sum(SPPLIVMT), by = NEGEAR]
  setnames(gcat.sums, 'V1', 'IndGear')
  gcat.sums[, CatTot := sum(IndGear)]
  gcat.sums[, GearProp := IndGear / CatTot]
  setorder(gcat.sums, -GearProp)
  gcat.sums[, Cat := gcat[icat]]
  gcat.all <- rbindlist(list(gcat.all, gcat.sums))
}

#Look into species specific gears
spland.all <- c()



#Need to divide landings by area of stat areas not GB
stat.areas <- readOGR(gis.dir, 'Statistical_Areas_2010')

#Generate area table
stat.areas.area <- getarea(stat.areas, 'Id')
GB.com.area <- stat.areas.area[Id %in% c(521:526, 551, 552, 561, 562), sum(Area)]

#merge rpath codes
GB.lands <- merge(GB.lands, unique(spp[, list(NESPP3, RPATH)]), all.x = T)
setkey(GB.lands, YEAR, GEAR, RPATH)
GB.lands.sum <- GB.lands[, sum(SPPLIVMT), by = key(GB.lands)]
GB.lands.sum[, Landings := V1 / GB.com.area]

#Merge with other inputs
gear <- unique(GB.lands.sum[, GEAR])
for(i in 1:length(gear)){
  gear.landings <- GB.lands.sum[GEAR == gear[i], ]
  gear.mean <- gear.landings[, mean(Landings), by = RPATH]
  setnames(gear.mean, 'V1', paste0(gear[i], '_Landings'))
  GB.input <- merge(GB.input, gear.mean, by = 'RPATH', all.x = T)
}

#Grab discards
load(file.path(data.dir2, 'Comdisc.RData'))
GB.disc <- comdisc[YEAR %in% 2010:2012 & EPU == 'GB', ]

#merge rpath codes
GB.disc <- merge(GB.disc, unique(spp[, list(NESPP3, RPATH)]), all.x = T)
setkey(GB.disc, YEAR, GEAR, RPATH)
GB.disc.sum <- GB.disc[, sum(DISC), by = key(GB.disc)]
GB.disc.sum[, Discards := V1 / GB.com.area]

#Merge with other inputs
gear <- unique(GB.disc.sum[, GEAR])
for(i in 1:length(gear)){
  gear.discards <- GB.disc.sum[GEAR == gear[i], ]
  gear.mean <- gear.discards[, mean(Discards), by = RPATH]
  setnames(gear.mean, 'V1', paste0(gear[i], '_Discards'))
  GB.input <- merge(GB.input, gear.mean, by = 'RPATH', all.x = T)
}

save(GB.input, file = file.path(out.dir, 'GB_input.RData'))
#-------------------------------------------------------------------------------
#Add groups not available in database using EMAX
emax <- data.table(RPATH = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 
                             'Macrobenthos', 'Krill', 'Micronekton', 'GelZooplankton', 
                             'Mesozooplankton', 'Microzooplankton', 'Phytoplankton'),
                   Biomass = c(0.015, NA, 0.416, 0.122, 0.035, 104, 3, 4.6, 5.24, 
                               14.25, 3.1, 20),
                   BA = rep(0, 12))

#Add PB from Ecobase
load(file.path(out.dir, 'Ecobase_model_parameters.RData'))
ecobase <- parameters #change in ecobase pull code later
ecobase[, PB := as.numeric(PB)]
ecobase[, QB := as.numeric(QB)]
ecobase[, Group := tolower(Group)]

birds <- ecobase[Group %like% 'bird', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seabirds', PB := birds[, PB]]
emax[RPATH == 'Seabirds', QB := birds[, QB]]

seals <- ecobase[Group %like% 'grey seal', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := seals[, PB]]
emax[RPATH == 'Seals', QB := seals[, QB]]

baleen <- ecobase[Group %like% 'baleen', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

tooth <- ecobase[Group %like% 'toothed', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

HMS <- ecobase[Group %like% 'tuna', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

macro <- ecobase[Group %like% 'macrobenthos', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

krill <- ecobase[Group %like% 'euph', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

micronek <- ecobase[Group %like% 'micronekton', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

gel <- ecobase[Group %like% 'gelat' | Group %like% 'jelly', lapply(.SD, mean), 
               .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

mesozoo <- ecobase[Group %like% 'mesozoo' | Group %like% 'cope', lapply(.SD, mean), 
                   .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

microzoo <- ecobase[Group %like% 'microzoo', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]

phyto <- ecobase[Group %like% 'phytoplankton', lapply(.SD, mean), .SDcols = c('PB', 'QB')]
phyto[, QB := 0]
emax[RPATH == 'Seals', PB := birds[, PB]]
emax[RPATH == 'Seals', QB := birds[, QB]]
#Diet Data----------------------------------------------------------------------
library(dplyr); library(rerddap)

food_habits <- sprintf("http://comet.nefsc.noaa.gov/erddap/tabledap/%s.csv",
                       c("food_habits_2012_v1",
                         "food_habits_2013_v1",
                         "food_habits_2014_v1",
                         "food_habits_2015_v1",
                         "food_habits_2016_v1")) %>% 
  purrr::map(function(x) {
    readr::read_csv(url(x))
  })

#Convert to data.table
food.habits <- as.data.table(rbindlist(list(food_habits[[1]], food_habits[[2]], 
                                            food_habits[[3]], food_habits[[4]],
                                            food_habits[[5]])))

save(food.habits, file = file.path(out.dir, 'GB_Rpath_diet.RData'))
#Diet input
load(file.path(out.dir, 'allfh.RData'))
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

#Identify tows within new strata
epu <- spTransform(epu, lcc)
stations$EPU <- over(stations, epu)[, 'EPU']

#Output data (convert spatial data frame back to lat/lon)
stations <- spTransform(stations, CRS('+init=epsg:4326'))
sta.data <- as.data.table(as.data.frame(stations))
sta.data[, c('declat', 'declon') := NULL]
x <- merge(x, sta.data, by = c('cruise6', 'stratum', 'station'))

GB.fh <- x[EPU == 'GB' & year %in% 2010:2012 & !pynam %in% c('EMPTY', 'BLOWN'), 
           list(cruise6, stratum, station, svspp, pdid, pdgutv, pynam, pyamtv)]

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