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

land <- merge(landings$comland, unique(spp[!is.na(NESPP3), list(NESPP3, RPATH)]), 
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

disc <- comlandr::get_

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
               a.anmlcond, a.estlen, a.link1, a.link3
               from obinc a, obspp b
               where a.tripid = b.tripid
               and a.year in (2013, 2014, 2015)
               union
               select distinct a.year, a.month, b.area, b.negear, a.nespp4, 
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum, 
               a.anmlcond, a.estlen, a.link1, a.link3
               from asminc a, asmspp b
               where a.tripid = b.tripid
               and a.year in (2013, 2014, 2015)"

mammal <- as.data.table(sqlQuery(channel, mammal.qry))

#Only retain animals that were killed
mammal <- mammal[ANMLCOND >= 10 & ANMLCOND <= 14, ]
mammal[, c('ANMLCOND', 'ESTLEN') := NULL]

#Add mesh size
mammal <- merge(mammal, mesh, by = c('LINK1', 'LINK3', 'NEGEAR'), all.x = T)
mammal[, LINK4 := NULL]

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

#Convert to metric tons from pounds to align with commercial landings data
ob.code[PR == 0, C.HAILWT := C.HAILWT * 0.00045359237]

#Apply average weight of Protected resources
#Values from Trites and Pauly 1998
#Averaged male and female values
mammal.size <- data.table(COMNAME = c('WHALE, FINBACK', 'WHALE, HUMPBACK', 'DOLPHIN, WHITESIDED',
                                      'DOLPHIN, COMMON (OLD SADDLEBACK)', 'DOLPHIN, BOTTLENOSE',
                                      'DOLPHIN, RISSOS', 'WHALE, MINKE', 'PORPOISE, HARBOR',
                                      'SEAL, HARP', 'WHALE, PILOT, NK', 'WHALE, BALEEN, NK',
                                      'SEAL, NK', 'SEAL, HARBOR', 'SEAL, GRAY', 
                                      'DOLPHIN, NK (MAMMAL)', 'PORPOISE/DOLPHIN, NK'),
                          meanwt = c(55590, 30408, 92, 80, 188, 224, 6566, 31,
                                     92, 747, 30855, 105, 64, 160, 146, 227))

ob.code <- merge(ob.code, mammal.size, by = 'COMNAME', all.x = T)
ob.code[PR == 1 & !is.na(meanwt), C.HAILWT := meanwt * 0.001]

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

#Sum by Rpath group, gear, disposition
GB.sums <- GB.observed[, sum(C.HAILWT), by = c('YEAR', 'RPATH', 'RGear', 'CATDISP', 'PR')]
setnames(GB.sums, 'V1', 'SPPLIVMT')

#Calculate kept and discards
GB.discard <- GB.sums[CATDISP == 0, ]
setnames(GB.discard, 'SPPLIVMT', 'DISCARD')

GB.kept <- GB.sums[CATDISP == 1, sum(SPPLIVMT), by = c('YEAR', 'RGear')]
setnames(GB.kept, 'V1', 'KEPT.ALL')

GB.all <- merge(GB.kept, GB.discard, by = c('YEAR', 'RGear'))

GB.all[, CATDISP := NULL]

GB.all[, DK := DISCARD / KEPT.ALL]
GB.all[, c('KEPT.ALL', 'DISCARD') := NULL]

#Calculate mean ratio
GB.mean <- GB.all[, mean(DK), by = c('RGear', 'RPATH')]
setnames(GB.mean, 'V1', 'DK')

#Expand ratio by landings
#Get landings
load(file.path(data.dir, 'GB_landings.RData'))

#Merge landings to discard ratios
GB.disc <- merge(GB.mean, GB.landings, by = c('RPATH', 'RGear'), all = T)

#Fix NAs
GB.disc <- GB.disc[!is.na(RPATH), ]
GB.disc[is.na(DK),  DK  := 0]
GB.disc[is.na(SPPLIVMT), SPPLIVMT := 0]

#Calculate gear landings
GB.disc[, Gear.Tot := sum(SPPLIVMT), by = RGear]
GB.disc[, SPPLIVMT := NULL]

GB.disc[, Discards := DK  * Gear.Tot]

GB.disc[, c('DK', 'Gear.Tot') := NULL]
save(GB.disc, file = file.path(data.dir, 'GB_discards.RData'))



