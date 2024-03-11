#Biological parameters for GBRpath


#Required packages--------------------------------------------------------------
#remotes::install_github("ropensci/rfishbase")
library(rfishbase); library(data.table)
load(here('data-raw', 'Species_codes.RData'))

#User functions-----------------------------------------------------------------
rightcase <- function(x){
  out <- c()
  for(i in 1:length(x)){
    first <- toupper(substring(x[i], 1, 1))
    rest  <- tolower(substring(x[i], 2, nchar(x[i])))
    new   <- paste0(first, rest)
    out   <- c(out, new)
  }
  return(out)
}

fixspace <- function(x){
  for(i in 1:length(x)){
    last <- substring(x[i], nchar(x[i]), nchar(x[i]))
    if(last == ' ') x[i] <- substring(x[i], 1, nchar(x[i]) - 1)
  }
  return(x)
}

#Buchheister--------------------------------------------------------------------
NWACS <- data.table(Group = c('Phytoplankton', 'Other primary producers', 'Bacteria',
                              'Microzooplankton', 'Small Copepods', 'Large Copepods',
                              'Gelatinous zooplankton', 'Micronekton', 
                              'Macrobenthos- polychaetes', 'Macrobenthos- crustaceans',
                              'Macrobenthos- molluscs', 'Macrobenthos- other', 
                              'Megabenthos- filterers', 'Megabenthos- other',
                              'Shrimp and Similar Species', 'Mesopelagics',
                              'Atlantic herring', 'Alosines', 'Atlantic menhaden (S)',
                              'Atlantic menhaden (M)', 'Atlantic menhaden (L)', 
                              'Anchovies', 'Atlantic mackerel', 'Squid', 'Butterfish',
                              'Small pelagics- other', 'Bluefish (S)', 'Bluefish (M)',
                              'Bluefish (L)', 'Striped bass (S)', 'Striped bass (M)',
                              'Striped bass (L)', 'Weakfish (S)', 'Weakfish (M)', 
                              'Weakfish (L)', 'Spiny dogfish (S)', 'Spiny dogfish (L)',
                              'Atlantic cod (S)', 'Atlantic cod (M)', 'Atlantic Cod (L)',
                              'Haddock', 'Hake', 'Atlantic croaker', 
                              'Yellowtail flounder (S)', 'Yellowtail flounder (L)',
                              'Summer flounder (S)', 'Summer flounder (L)', 'Skates',
                              'Demersal benthivores - other', 'Demersal piscivores - other',
                              'Demersal omnivores - other', 'Medium pelagics - other',
                              'Sharks - coastal', 'Sharks - pelagic', 'Large pelagics (HMS)', 
                              'Pinnipeds', 'Baleen whales', 'Odontocetes',
                              'Seabirds', 'Nearshore pisc birds', 'Detritus'),
                    Biomass = c(30.000, 1.605, 7.700, 7.000, 16.000, 17.966, 6.349, 
                                7.654, 17.452, 7.000, 8.340, 21.000, 5.500, 4.498, 
                                0.470, 0.090, 1.650, 0.200, 0.371, 2.048, 1.200,
                                1.100, 1.740, 1.267, 1.488, 1.400, 0.017, 0.160, 
                                0.509, 0.003, 0.022, 0.022, 0.006, 0.038, 0.036,
                                0.337, 0.800, 0.055, 0.144, 0.277, 0.254, 1.100,
                                0.350, 0.007, 0.187, 0.010, 0.159, 1.000, 2.300,
                                1.300, 1.100, 0.021, 0.008, 0.016, 0.070, 0.035,
                                0.464, 0.060, 0.007, 0.007, 52.6),
                    PB = c(180.700, 55.570, 91.250, 85.000, 46.000, 46.000, 40.000,
                           14.250, 2.500, 3.600, 2.200, 2.000, 1.200, 2.300, 2.000,
                           1.100, 1.100, 1.300, 1.900, 1.309, 0.756, 2.200, 0.550,
                           5.720, 1.312, 1.200, 3.900, 0.900, 0.310, 1.500, 0.526, 
                           0.317, 3.300, 0.900, 1.000, 0.321, 0.321, 1.087, 1.125, 
                           0.700, 0.700, 1.296, 0.994, 2.700, 0.850, 2.200, 1.050,
                           0.250, 0.550, 0.450, 0.550, 0.450, 0.200, 0.113, 0.579, 
                           0.075, 0.040, 0.040, 0.279, 0.279, NA),
                    QB = c(0.000, 0.000, 380.208, 283.400, 140.000, 150.000, 145.326,
                           85.497, 17.500, 21.000, 13.949, 16.059, 6.660, 15.533,
                           6.660, 3.700, 3.700, 4.400, 15.860, 6.643, 3.785, 7.333,
                           2.170, 19.000, 4.230, 4.000, 20.935, 6.093, 3.139, 
                           10.265, 3.429, 1.820, 13.520, 4.689, 2.803, 3.519, 1.810,
                           5.059, 2.603, 1.500, 3.000, 3.850, 3.550, 12.168, 2.900, 
                           10.283, 2.900, 0.900, 1.833, 1.500, 1.833, 1.838, 1.247,
                           0.690, 6.794, 5.581, 3.217, 14.301, 80.000, 80.000, NA),
                    RPATH = c('Phytoplankton', 'Phytoplankton', 'Bacteria', 
                              'Microzooplankton', 'SmCopepods', 'LgCopepods',
                              'GelZooplankton', 'Micronekton', 
                              rep('Macrobenthos', 4), 'AtlScallop', 'AmLobster',
                              'OtherShrimps', 'Mesopelagics', 'AtlHerring', 'RiverHerring',
                              rep('OtherPelagics', 3), 'SmPelagics', 'AtlMackerel',
                              'Illex', 'Butterfish', 'SmPelagics', rep('Bluefish', 3),
                              rep('OtherPelagics', 3), rep('SouthernDemersals', 3),
                              rep('SpinyDogfish', 2), rep('Cod', 3), 'Haddock',
                              'RedHake', 'SouthernDemersal', rep('YTFlounder', 2),
                              rep('SummerFlounder', 2), 'WinterSkate', 
                              rep('OtherDemersals', 3), 'OtherPelagics', rep('Sharks', 2),
                              'HMS', 'Seals', 'BalWhale', 'ToothWhale', 'Seabirds',
                              'Seabirds', 'Detritus'))
#Calculate weighted mean
NWACS[, Rpath.bio := sum(Biomass), by = RPATH]
NWACS[, PB.weight := Biomass * PB]
NWACS[, new.PB := sum(PB.weight) / Rpath.bio, by = RPATH]
NWACS[, QB.weight := Biomass * QB]
NWACS[, new.QB := sum(QB.weight) / Rpath.bio, by = RPATH]

#Drop extra columns and rename
GB.params <- unique(NWACS[, list(RPATH, new.PB, new.QB)])
setnames(GB.params, c('new.PB', 'new.QB'), c('PB', 'QB'))

#Used fishbase for skates and hakes
GB.params <- GB.params[!RPATH %in% c('RedHake', 'WinterSkate'), ]

#Several parameters need to be duplicated as their parameters will be similar to others
GB.double <- GB.params[RPATH %in% c('AtlScallop', 'AmLobster', 'Illex', 'Micronekton'), ]
GB.double[RPATH == 'Micronekton', RPATH := 'Krill']
GB.double[RPATH == 'AtlScallop',  RPATH := 'OceanQuahog']
GB.double[RPATH == 'AmLobster',   RPATH := 'Megabenthos']
GB.double[RPATH == 'Illex', RPATH := 'Loligo']
ceph <- copy(GB.double[RPATH == 'Loligo', ])
ceph[, RPATH := 'OtherCephalopods']
GB.double <- rbindlist(list(GB.double, ceph))
surf <- copy(GB.double[RPATH == 'OceanQuahog', ])
surf[, RPATH := 'SurfClam']
GB.double <- rbindlist(list(GB.double, surf))

GB.params <- rbindlist(list(GB.params, GB.double))

#FishBase-----------------------------------------------------------------------
#Fill-in groups not in Buchheister
GB.fish <- unique(spp[RPATH %in% c('Goosefish', 'OffHake', 'SilverHake', 'RedHake', 
                                   'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 
                                   'BlackSeaBass', 'Scup', 'Fourspot','AmPlaice', 
                                   'Windowpane', 'WinterFlounder', 'WitchFlounder', 
                                   'SmoothDogfish', 'Barndoor', 'WinterSkate',
                                   'LittleSkate'), list(RPATH, SCINAME)], 
                  by = 'SCINAME')
#Fix scinames with extra space
GB.fish[, SCINAME := fixspace(as.character(SCINAME))]

#Fix names for r package
GB.fish[, Sciname := rightcase(as.character(SCINAME))]

#Prime rfishbase
fishbase <- load_taxa(server = "fishbase")

#Validate names
fish <- validate_names(GB.fish[, Sciname])

#Query the data base
pb.fishbase <- as.data.table(species(fish, fields = c('Species', 'LongevityWild')))
pb.fishbase[, PB := 1 / LongevityWild]
qb.fishbase <- as.data.table(popqb(fish, fields = c('Species', 'PopQB')))
qb.fishbase <- qb.fishbase[, .(QB = mean(PopQB, na.rm = T)), by = 'Species']

fish.params <- merge(pb.fishbase, qb.fishbase, by = 'Species', all = T)
fish.params[, 'LongevityWild' := NULL]

#Add RPATH code back on
fish.params[, SCINAME := toupper(Species)]
fish.params <- merge(fish.params, unique(spp[, list(RPATH, SCINAME)]), by = 'SCINAME',
                     all.x = T)
#There are a couple species with extra spaces at the end...need to fix this
fish.params[SCINAME %like% 'PSEUDOPLEURO',  RPATH := 'WinterFlounder']
fish.params[SCINAME %like% 'HIPPOGLOSSINA', RPATH := 'Fourspot']
fish.params[SCINAME %like% 'LEUCORAJA ERINACEUS', RPATH := 'LittleSkate']
fish.params[, c('SCINAME', 'Species') := NULL]

#Use EMAX values for aggregate group if still missing
fish.params <- merge(fish.params, unique(spp[, list(RPATH, EMAX)]), by = 'RPATH')
EMAX <- data.table(agg = c('Demersals- benthivores', 'Demersals- piscivores',
                            'Demersals- omnivores'),
                   PB.agg = 0.45, QB.agg = c(0.92, 2.44, 0.83))
fish.params[is.na(QB) & substr(EMAX, 1, 1) == 'B', QB := 0.92]
fish.params[is.na(QB) & substr(EMAX, 1, 1) == 'P', QB := 2.44]
fish.params[is.na(QB) & substr(EMAX, 1, 1) == 'O', QB := 0.83]
fish.params[is.na(PB), PB := 0.45]
fish.params[, EMAX := NULL]

#Add OtherFlatfish and SmFlatfish....

GB.bioparams <- rbindlist(list(GB.params, fish.params), use.names = T)

#Need to add OtherFlatfish and SmFlatfish
#Use OtherDemersals for OtherFlatfish and SmPelagics for SmFlatfish
GB.add <- copy(GB.bioparams[RPATH %in% c('OtherDemersals', 'SmPelagics'), ])
GB.add[RPATH == 'OtherDemersals', RPATH := 'OtherFlatfish']
GB.add[RPATH == 'SmPelagics', RPATH := 'SmFlatfishes']
#Need an OtherSkates as well...going to take a mean of Winter and Little
GB.skate <- copy(GB.bioparams[RPATH %in% c('WinterSkate', 'LittleSkate'), ])
GB.skate <- GB.skate[, lapply(list(PB, QB), mean)]
setnames(GB.skate, c('V1', 'V2'), c('PB', 'QB'))
GB.skate[, RPATH := 'OtherSkates']
GB.add <- rbindlist(list(GB.add, GB.skate), use.names = T)
bioparam.input <- rbindlist(list(GB.bioparams, GB.add), use.names = T)

usethis::use_data(bioparam.input, overwrite = T)

