#Generate prebal table
groups1 <- data.table(Group = c('AtlHerring', 'AtlMackerel', 'Butterfish', 
                                'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 
                                'Haddock', 'Goosefish', 'SilverHake', 
                                'RedHake', 'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 
                                'BlackSeaBass', 'Bluefish', 'Scup', 'OtherDemersals', 
                                'SouthernDemersals', 'Fourspot', 'SummerFlounder', 
                                'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
                                'YTFlounder', 'SmFlatfishes', 'SpinyDogfish', 
                                'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
                                'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 
                                'AmLobster', 'AtlScallop', 
                                'OceanQuahog', 'SurfClam'), 
                      'Data Source' = c(rep('NEFSC BTS', 38), 'NEFSC Scallop', 
                                        (rep('NEFSC Clam',2)) ),
                      Classification = c(rep('Pelagic (Small; Round)', 5),
                                         'Pelagic (Medium; Round)', 
                                         rep('Demersal (Round)', 10), 
                                         'Pelagic (Medium; Round)', 'Pelagic (Small; Round)',
                                         'Demersal (Round)', 'Demersal (Round)',
                                         rep('Demersal (Flat)', 8), 'Demersal (Round)',
                                         'Demersal (Round)', rep('Demersal (Flat)', 4),
                                         rep('Invertebrate (Pelagic)', 3),
                                         rep('Invertebrate (Benthic)', 4)))

groups2 <- data.table(Group = c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 
                                'Sharks', 'Macrobenthos', 'Megabenthos', 
                                'OtherShrimps', 'Krill', 'Micronekton', 
                                'GelZooplankton', 'Mesozooplankton', 'Microzooplankton', 
                                'Phytoplankton'), 
                      Classification = c('Bird', 'Mammal', 'Whale', 'Whale', 'HMS', 
                                         'Shark',rep('Invertebrate (Benthic)', 2),
                                         rep('Invertebrate (Pelagic)', 4), 
                                         rep('Zooplankton', 2), 'Primary Producer'))


spclass.GB <- rbindlist(list(groups1[, .(Group, Classification)], groups2))

spclass.GB[Classification %like% 'Round', RF := 'Round']
spclass.GB[Classification %like% 'Flat',  RF := 'Flat']
spclass.GB[Classification %like% 'Dem',   type := 'Demersal']
spclass.GB[Classification %like% 'Pel' & !Classification %like% 'Invert', 
           type := 'Pelagic']
spclass.GB[Classification %in% c('Demersal (Round)', 'Demersal (Flat)', 'Demersal',
                                 'Pelagic (Small; Round)', 'Pelagic (Medium; Round)',
                                 'HMS', 'Shark'), fish := T]
spclass.GB[Classification %in% c('Invertebrate (Pelagic)', 'Invertebrate (Benthic)', 
                                 'Primary Producer'), invert := T]
spclass.GB[Group %in% c('Mesozooplankton', 'Microzooplankton', 'GelZooplankton', 
                        'Micronekton'), Zoop := T]

plank <- c('AtlHerring', 'AtlMackerel', 'SmPelagics')
pisc  <- c('Barndoor', 'Bluefish', 'Cod', 'Fourspot', 'Goosefish', 'HMS', 
           'SilverHake', 'Sharks', 'SpinyDogfish', 'SummerFlounder', 'OtherPelagics')

spclass.GB[Group %in% plank, diet := 'Plank']
spclass.GB[Group %in% pisc,  diet := 'Pisc']
spclass.GB[is.na(diet) & fish == T, diet := 'Benth']

usethis::use_data(spclass.GB, overwrite = T)
