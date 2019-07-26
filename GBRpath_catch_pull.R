#Landings/Discard pull for GBRpath
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

#Landings-----------------------------------------------------------------------
landings.qry <- paste0("select year, month, NEGEAR, mesh, nespp3, nespp4, area, spplivlb, link
                     from stockeff.MV_CF_Landings
                     where YEAR in (2013, 2014, 2015)")

landings <- as.data.table(sqlQuery(channel, landings.qry))

#Create small/large mesh gear categories
#July 2015 - minimum mesh size = 6.5 inches
#https://www.greateratlantic.fisheries.noaa.gov/regs/infodocs/small_mesh_exemption.pdf
otter.trawl <- landings[NEGEAR %in% 50:59, ]

#Assign large vs small based on mesh size
otter.trawl[MESH < 6.5,  Cat := factor('Small', levels = c('Small', 'Large'))]
otter.trawl[MESH >= 6.5, Cat := 'Large']

#Proportion unknown mesh size landings by proportion of known landings
#Get Proportions
lg.species <- otter.trawl[Cat == 'Large', sum(SPPLIVLB), by = NESPP3]
sm.species <- otter.trawl[Cat == 'Small', sum(SPPLIVLB), by = NESPP3]
setnames(lg.species, 'V1', 'Lg')
setnames(sm.species, 'V1', 'Sm')
otter.known <- merge(lg.species, sm.species, by = 'NESPP3', all = T)
otter.known[is.na(Lg), Lg := 0]
otter.known[is.na(Sm), Sm := 0]
otter.known[, Total := Lg + Sm]
otter.known[, Prop.Lg := Lg / Total]
otter.known[, Prop.Sm := Sm / Total]

#Separate out unknown mesh
otter.unk <- otter.trawl[is.na(Cat), ]
otter.unk <- merge(otter.unk, otter.known[, list(NESPP3, Prop.Lg, Prop.Sm)], 
                   by = 'NESPP3')
#Apply proportion and form to look like full data set
#Large
otter.unk.lg <- copy(otter.unk)
otter.unk.lg[, SPPLIVLB := SPPLIVLB * Prop.Lg]
otter.unk.lg[, Cat := 'Large']
otter.unk.lg <- otter.unk.lg[SPPLIVLB > 0, ]
otter.unk.lg[, c('Prop.Lg', 'Prop.Sm') := NULL]
#Small
otter.unk.sm <- copy(otter.unk)
otter.unk.sm[, SPPLIVLB := SPPLIVLB * Prop.Sm]
otter.unk.sm[, Cat := 'Small']
otter.unk.sm <- otter.unk.sm[SPPLIVLB > 0, ]
otter.unk.sm[, c('Prop.Lg', 'Prop.Sm') := NULL]
#Merge together
otter.unk <- rbindlist(list(otter.unk.sm, otter.unk.lg))

#Remove previous no mesh records and insert new ones
otter.trawl <- rbindlist(list(otter.trawl[!is.na(Cat), ], otter.unk), use.names = T)

#Add back into landings
landings[, Cat := NA]
landings <- rbindlist(list(landings[!NEGEAR %in% 50:59, ], otter.trawl), use.names = T)

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
#Fix known errors
species[COMNAME == 'EEL UNCL', RPATH := 'SouthernDemersals']
species[COMNAME == 'GRAY TRIGGERFISH', RPATH := 'SouthernDemersals']
species[COMNAME == 'SEA BASS,NK', RPATH := 'SouthernDemersals']
species[COMNAME == 'SILVERSIDES', RPATH := 'SmPelagics']
species[COMNAME == 'SMOOTH PUFFER', RPATH := 'SouthernDemersals']          
nespp.rpath <- unique(species[!is.na(NESPP3), list(NESPP3, RPATH)])

landings <- merge(landings, nespp.rpath, by = 'NESPP3', all.x = T)

#Subset landings for Georges Bank
GB.stat.areas <- c(521:526, 551, 552, 561, 562)
GB.landings <- landings[AREA %in% GB.stat.areas, ]

#Sum landings by Rpath node and gear
#Fleet Categories
#Note there will be a small and large mesh otter trawl fishery
gcat <- c('otter', 'dredge.sc', 'pot', 'longline', 'seine', 'gillnet', 'midwater',
          'dredge.cl')
otter     <- 50:59
dredge.sc <- 131:132
pot       <- c(180:190, 200:219, 300, 301)
longline  <- c(10, 40)
seine     <- c(70:79, 120:129, 360)
gillnet   <- c(100:119, 500, 510, 520)
midwater  <- c(170, 370)
dredge.cl  <- c(382, 386, 400)

#Assign gear category
for(igear in 1:length(gcat)){
  GB.landings[NEGEAR %in% get(gcat[igear]), RGear := gcat[igear]][]
}

#Fix otter trawls
GB.landings[Cat == 'Small' & RGear == 'otter', RGear := 'otter.sm']
GB.landings[Cat == 'Large' & RGear == 'otter', RGear := 'otter.lg']

#Assign Other Fisheries
GB.landings[is.na(RGear), RGear := 'other']

#Sum by Rpath group
GB.sums <- GB.landings[, sum(SPPLIVMT), by = c('YEAR', 'RPATH', 'RGear')]
setnames(GB.sums, 'V1', 'SPPLIVMT')

#Get three year mean
GB.mean <- GB.sums[, mean(SPPLIVMT), by = c('RPATH', 'RGear')]
setnames(GB.mean, 'V1', 'SPPLIVMT')

#Need to divide landings by area of stat areas not GB
stat.areas <- readOGR(gis.dir, 'Statistical_Areas_2010')

#Generate area table
stat.areas.area <- getarea(stat.areas, 'Id')
GB.com.area <- stat.areas.area[Id %in% c(521:526, 551, 552, 561, 562), sum(Area)]

GB.landings <- GB.mean[, SPPLIVMT := SPPLIVMT / GB.com.area][]
save(GB.landings, file = file.path(data.dir, 'GB_landings.RData'))


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