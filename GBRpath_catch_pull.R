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
landings.qry <- paste0("select year, month, NEGEAR, mesh, nespp3, nespp4, area, spplivlb, utilcd
                     from stockeff.MV_CF_Landings
                     where YEAR in (2013, 2014, 2015)")

landings <- as.data.table(sqlQuery(channel, landings.qry))

#Need to assign unknown skates to species
#Food = Winter Skate
#Bait = Little Skate
landings[NESPP3 == 365 | NESPP3 == 373 & UTILCD == 0, NESPP3 := 367]
landings[NESPP3 == 365 | NESPP3 == 373 & UTILCD == 7, NESPP3 := 366]
landings[, UTILCD := NULL]

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

#Discards-----------------------------------------------------------------------
#Observer data base is on nova not sole
channel <- odbcConnect('nova', uid, pwd)

ob.qry <- "select year, month, area, negear, nespp4, hailwt, catdisp, 
          drflag, tripid, haulnum, link1, link3
          from OBSPP
          where obsrflag = 1
          and year in (2013, 2014, 2015)
          and program not in ('127', '900', '250', '160')
          union
          select year, month, area, negear, nespp4, hailwt, catdisp, drflag, 
          tripid, haulnum, link1, link3
          from ASMSPP
          where obsrflag = 1
          and year in (2013, 2014, 2015)
          and program not in ('127', '900', '250', '160')"

ob <- as.data.table(sqlQuery(channel, ob.qry))

#Grab mesh size for small/large mesh otter trawl
mesh.qry <- "select link1, link3, link4, negear, codmsize 
             from OBOTGH
             where year in (2013, 2014, 2015)
             union
             select link1, link3, link4, negear, codmsize
             from ASMOTGH
             where year in (2013, 2014, 2015)"
mesh <- as.data.table(sqlQuery(channel, mesh.qry))
#Remove missing entries
mesh <- mesh[!is.na(CODMSIZE), ]
#Convert cod mesh from mm to inches
mesh[, mesh := CODMSIZE * 0.0393701]
#Assign small/large
mesh[mesh < 6.5,  Cat := 'Small']
mesh[mesh >= 6.5, Cat := 'Large']
#Drop extra columns
mesh[, c('CODMSIZE', 'mesh') := NULL]
#Merge duplicates
mesh <- unique(mesh)

#Merge with ob
ob <- merge(ob, mesh, by = c('LINK1', 'LINK3', 'NEGEAR'), all.x = T)
ob[, LINK4 := NULL]

#Add protected species here
mammal.qry <- "select distinct a.year, a.month, b.area, b.negear, a.nespp4, 
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum, 
               a.link1, a.link3
               from obinc a, obspp b
               where a.tripid = b.tripid
               and a.year in (2013, 2014, 2015)
               union
               select distinct a.year, a.month, b.area, b.negear, a.nespp4, 
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum, 
               a.link1, a.link3
               from asminc a, asmspp b
               where a.tripid = b.tripid
               and a.year in (2013, 2014, 2015)"

mammal <- as.data.table(sqlQuery(channel, mammal.qry))
#Added for merge
mammal[, Cat := NA] 

ob <- rbindlist(list(ob, mammal), use.names = T)

#Clean up data set
#Remove those with unknown disposition
ob <- ob[CATDISP != 9, ]

#remove record if weight is missing
ob <- ob[!is.na(HAILWT), ]    

#remove non-living items (clappers and stomach contents) and unknown living matter
ob <- ob[!(NESPP4 %in% c(0, 6800:6802, 6805, 6810, 6820, 6830, 6850:6857, 6882, 6883, 6894:6897))]  

#Convert weights
convert.qry <- "select nespp4_obs, catdisp_code, drflag_code, cf_lndlb_livlb, cf_rptqty_lndlb  
                from obspecconv"
convert <- as.data.table(sqlQuery(channel, convert.qry))

setnames(convert,
         c('NESPP4_OBS', 'CATDISP_CODE', 'DRFLAG_CODE'),
         c('NESPP4',     'CATDISP',      'DRFLAG'))

setkey(convert,
       NESPP4,
       CATDISP,
       DRFLAG)

ob.code <- merge(ob, convert, by = key(convert), all.x = T) 

#missing cf's will be set to 1 Assume living
ob.code[is.na(CF_LNDLB_LIVLB), CF_LNDLB_LIVLB := 1]
ob.code[is.na(CF_RPTQTY_LNDLB), CF_RPTQTY_LNDLB := 1]

ob.code[, C.HAILWT := HAILWT * CF_RPTQTY_LNDLB * CF_LNDLB_LIVLB] 

#Grab common name and PR flags
comname.qry <- "select NESPP4, comname, sciname, cetacean, turtle, pinniped
                from obspec"

comname <- as.data.table(sqlQuery(channel, comname.qry))
comname[CETACEAN == 1 | TURTLE == 1 | PINNIPED == 1, PR := 1]
comname[is.na(PR), PR := 0]
comname[, c('CETACEAN', 'TURTLE', 'PINNIPED') := NULL]

ob.code <- merge(comname, ob.code, by = 'NESPP4')

#Convert to metric tons to align with commercial landings data
ob.code[PR == 0, C.HAILWT := C.HAILWT * 0.00045359237]

#Change to NESPP3 to combine market categories
ob.code[NESPP4 < 100,                NESPP3 := as.numeric(substring(NESPP4, 1, 1))]
ob.code[NESPP4 > 99 & NESPP4 < 1000, NESPP3 := as.numeric(substring(NESPP4, 1, 2))]
ob.code[(NESPP4 > 999  & NESPP4 < 6100) | (NESPP4 > 6579 & NESPP4 < 6901) |
          (NESPP4 > 6999 & NESPP4 < 8090) |  NESPP4 > 8180, 
        NESPP3 := as.numeric(substring(NESPP4, 1, 3))]
#Birds, mammals, etc don't have unique NESPP3 codes
ob.code[is.na(NESPP3), NESPP3 := NESPP4] 

ob.code[NESPP4 < 100, MKTCAT := as.numeric(substring(NESPP4, 2, 2))]
ob.code[NESPP4 > 99 & NESPP4 < 1000, MKTCAT := as.numeric(substring(NESPP4, 3, 3))]
ob.code[NESPP4 > 999, MKTCAT := as.numeric(substring(NESPP4, 4, 4))]

#drop NESPP4
ob.code[, NESPP4 := NULL]

#Subset landings for Georges Bank
GB.stat.areas <- c(521:526, 551, 552, 561, 562)
GB.observed <- ob.code[AREA %in% GB.stat.areas, ]

#Sum observations by Rpath node, gear, status
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
  GB.observed[NEGEAR %in% get(gcat[igear]), RGear := gcat[igear]][]
}

#Fix otter trawls
GB.observed[Cat == 'Small' & RGear == 'otter', RGear := 'otter.sm']
GB.observed[Cat == 'Large' & RGear == 'otter', RGear := 'otter.lg']

#Assign Other Fisheries
GB.observed[is.na(RGear), RGear := 'other']

GB.observed[, c('DRFLAG', 'MONTH', 'AREA', 'NEGEAR', 
            'HAILWT', 'CF_LNDLB_LIVLB', 'CF_RPTQTY_LNDLB') := NULL]

#Assign Rpath groups
GB.observed <- merge(GB.observed, nespp.rpath, by = 'NESPP3', all.x = T)

#Assign groups not in nespp.rpath (mostly protected species)
GB.observed[NESPP3 %in% c(524, 660, 666, 667, 678, 679), RPATH := 'OtherDemersals']
GB.observed[NESPP3 %in% c(685, 687, 689), RPATH := 'Macrobenthos']
GB.observed[NESPP3 >= 6100 & NESPP3 < 6500, RPATH := 'Seabirds']
GB.observed[NESPP3 >= 6936 & NESPP3 <= 6942 | NESPP3 %in% c(6960, 6992, 6997), RPATH := 'ToothWhale']
GB.observed[NESPP3 %in% c(6945, 6993, 6999), RPATH := 'BalWhale']
GB.observed[NESPP3 %in% c(6981, 6994:6996), RPATH := 'Seals']
#Note there were 16 turtles in >500K records so I did not include them
GB.observed <- GB.observed[!is.na(RPATH), ]


#Pick up here----------------

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






setkey(ob.code, YEAR, RGear, NESPP3, CATDISP))
ob.sums <- ob.code[, sum(C.HAILWT), by = key(ob.code)]

#Calculate kept and discards
ob.discard <- ob.sums[CATDISP == 0, ]

setnames(ob.discard,
         "V1",
         "DISCARD")

setkeyv(ob.sums, strat.var)

ob.kept <- ob.sums[CATDISP == 1, sum(V1), by = key(ob.sums)]

setnames(ob.kept,
         "V1",
         "KEPT.ALL")

ob.all <- merge(ob.kept, ob.discard, by = key(ob.sums))

ob.all[, CATDISP := NULL]

ob.all[, DK := DISCARD / KEPT.ALL]
ob.all[is.na(DK), DK := 1.0]
ob.all[, c('KEPT.ALL', 'DISCARD') := NULL]

#Get landings
load(file.path(data.dir, landings.file))

setkeyv(comland, strat.var)

tot.land <- comland[, sum(SPPLIVMT), by = key(comland)]

setnames(tot.land,
         "V1",
         "TOT.LAND")

comdisc <- merge(ob.all, tot.land, by = key(comland))

comdisc[, DISC := DK * TOT.LAND]

#Variance
#Need to add back individual trip data
rm(ob) #Free up memory
setkeyv(comdisc, c(strat.var, 'NESPP3'))

disc.var <- unique(comdisc, by = key(comdisc))

#Trip kept all
setkeyv(ob.code, c(strat.var, 'TRIPID'))

trip.kept <- ob.code[CATDISP == 1, sum(C.HAILWT), by = key(ob.code)]                
setnames(trip.kept, "V1", "trip.k")

#Trip discard by species
setkeyv(ob.code, c(strat.var, 'TRIPID', 'NESPP3'))

trip.disc <- ob.code[CATDISP == 0, sum(C.HAILWT), by = key(ob.code)]                
setnames(trip.disc, "V1", "trip.d")

trip.all <- merge(trip.disc, trip.kept, by = c(strat.var, 'TRIPID'), all = T)
trip.all[is.na(trip.k), trip.k := 0] 

disc.var <- merge(disc.var, trip.all, by = c(strat.var, 'NESPP3'))

#Calculate the number of observed trips
setkeyv(ob.code, c(strat.var, 'TRIPID'))

trips <- unique(ob.code, by = key(ob.code))

trip.count <- trips[, count(TRIPID), by = strat.var]

setnames(trip.count, "V1", "n")

disc.var <- merge(disc.var, trip.count, by = strat.var)

#Calculate the total number of trips
#CFDBS is on sole - need to switch connection
odbcClose(channel)
if(Sys.info()['sysname']=="Windows"){
  channel <- odbcDriverConnect()
} else {
  channel <- odbcConnect('sole', uid, pwd)
}

tables <- c(paste('WODETS',  89:93, sep = ''), 
            paste('CFDETS',  1994:endyear, 'AA', sep = ''))

comtrip.qry <- "select year, month, area, negear, count(link) as N
                from WODETS89
                group by year, month, area, negear"
comtrip <- as.data.table(sqlQuery(channel, comtrip.qry))

for(i in 2:length(tables)){
  tripyr.qry <- paste("select year, month, area, negear, count(link) as N
                       from", tables[i],
                      "group by year, month, area, negear")
  tripyr <- as.data.table(sqlQuery(channel, tripyr.qry))
  
  comtrip <- rbindlist(list(comtrip, tripyr))
}

comtrip[AREA %in% gom, EPU := 'GOM']
comtrip[AREA %in% gb,  EPU := 'GB']
comtrip[AREA %in% mab, EPU := 'MAB']
comtrip[AREA %in% ss,  EPU := 'SS']
comtrip[is.na(EPU),    EPU := 'OTHER']
comtrip[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

comtrip[YEAR < 100, YEAR := YEAR + 1900]

comtrip[MONTH %in% 1:3,   QY := 1]
comtrip[MONTH %in% 4:6,   QY := 2]
comtrip[MONTH %in% 7:9,   QY := 3]
comtrip[MONTH %in% 10:12, QY := 4]

comtrip[NEGEAR %in% otter,     GEAR := 'otter']
comtrip[NEGEAR %in% dredge.sc, GEAR := 'dredge.sc']
comtrip[NEGEAR %in% pot,       GEAR := 'pot']
comtrip[NEGEAR %in% longline,  GEAR := 'longline']
comtrip[NEGEAR %in% seine,     GEAR := 'seine']
comtrip[NEGEAR %in% gillnet,   GEAR := 'gillnet']
comtrip[NEGEAR %in% midwater,  GEAR := 'midwater']
comtrip[NEGEAR %in% dredge.o,  GEAR := 'dredge.o']
comtrip[is.na(GEAR),           GEAR := 'other']
comtrip[, GEAR := as.factor(GEAR)]

setkeyv(comtrip, strat.var)

comtrip.count <- comtrip[, sum(N), by = key(comtrip)]

setnames(comtrip.count, "V1", "N")

disc.var <- merge(disc.var, comtrip.count, by = key(comtrip), all.x = T)

#Fix groups that don't line up properly - actual value of N not that important only relative size
N.avg <- disc.var[, mean(N, na.rm = T)]
disc.var[is.na(N), N := N.avg]

#Calculate variance
#Need to expand so zero discards by species are represented
setkeyv(disc.var, c(strat.var, 'TRIPID'))
var.trips <- unique(disc.var, by = key(disc.var))
#drop species specific data
var.trips[, c('NESPP3', 'DK', 'DISC', 'trip.d') := NULL]

#Get list of species
spp <- unique(disc.var[, NESPP3])
all.spp.var <- c()
for(i in 1:length(spp)){
  spp.trip <- disc.var[NESPP3 == spp[i], ]  
  #Get rid of extra data
  spp.trip[, c('TOT.LAND', 'DISC', 'trip.k', 'n', 'N') := NULL]
  
  spp.var <- merge(var.trips, spp.trip, by = c(strat.var, 'TRIPID'), all.x = T)
  
  #Fix NAs
  spp.var[is.na(NESPP3), NESPP3 := spp[i]]
  spp.var[is.na(trip.d), trip.d := 0]
  
  #Merge in DK ratios
  setkeyv(spp.trip, strat.var)
  spp.dk <- unique(spp.trip, by = key(spp.trip))
  spp.var[, DK := NULL]
  spp.dk[, c('NESPP3', 'TRIPID', 'trip.d') := NULL]
  spp.var <- merge(spp.var, spp.dk, by = strat.var, all.x = T)
  spp.var[is.na(DK), DK := 0]
  
  spp.var[, step.1 := (sum(trip.d^2 + DK^2 * trip.k^2 - 2 * DK * trip.d * trip.k)/(n - 1)), by = strat.var]
  
  setkeyv(spp.var, strat.var)
  spp.var <- unique(spp.var, by = key(spp.var))
  spp.var[, c('TRIPID', 'trip.d', 'trip.k', 'DK') := NULL]
  
  spp.var[, DISC.VAR :=  TOT.LAND^2 * ((N - n)/n*N) * (1/(TOT.LAND/n)^2) * step.1]
  spp.var[, c('TOT.LAND', 'n', 'N', 'step.1') := NULL]
  
  all.spp.var <- rbindlist(list(all.spp.var, spp.var))
}
comdisc <- merge(comdisc, all.spp.var, by = c(strat.var, 'NESPP3'), all.x = T)

#Add species names
#Change to NESPP3 to combine market categories
comname[NESPP4 < 100,                        NESPP3 := as.numeric(substring(NESPP4, 1, 1))]
comname[NESPP4 > 99 & NESPP4 < 1000,         NESPP3 := as.numeric(substring(NESPP4, 1, 2))]
comname[(NESPP4 > 999 & NESPP4 < 6100) | 
          NESPP4 %in% c(7100:7109, 8020:8029), NESPP3 := as.numeric(substring(NESPP4, 1, 3))]
#Birds, mammals, etc don't have unique NESPP3 codes
comname[NESPP4 > 6099 & !NESPP4 %in% c(7100:7109, 8020:8029), NESPP3 := NESPP4]

setkey(comname, NESPP3)
comname <- unique(comname, by = key(comname))
comname[, c('NESPP4', 'SCINAME') := NULL]

comdisc <- merge(comname, comdisc, by = 'NESPP3')

save(comdisc, file = file.path(out.dir, "Comdisc.RData"))







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