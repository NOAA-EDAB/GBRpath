GB <- rpath(GB.params)
GB.bal <- as.data.table(write.Rpath(GB))
living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]

groups1 <- data.table(Group = c('AtlHerring', 'AtlMackerel', 'Butterfish', 
                                'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 
                                'Haddock', 'Goosefish', 'OffHake', 'SilverHake', 
                                'RedHake', 'WhiteHake', 'Redfish', 'Pollock', 'OceanPout', 
                                'BlackSeaBass', 'Bluefish', 'Scup', 'OtherDemersals', 
                                'SouthernDemersals', 'Fourspot', 'SummerFlounder', 
                                'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
                                'YTFlounder', 'OtherFlatfish', 'SmFlatfishes', 'SpinyDogfish', 
                                'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
                                'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 
                                'AmLobster', 'AtlScallop', 
                                'Clams'), 
                      'Data Source' = c(rep('NEFSC BTS', 40), 'NEFSC Scallop', 
                                        'NEFSC Clam'),
                      Classification = c(rep('Pelagic (Small; Round)', 5),
                                         'Pelagic (Medium; Round)', 
                                         'Demersal (Round)', 'Demersal (Round)',
                                         'Demersal', rep('Demersal (Round)', 6),
                                         'Demersal', 'Demersal (round)', 
                                         'Pelagic (Medium; Round)', 'Pelagic (Small; Round)',
                                         'Demersal (Round)', 'Demersal (round)',
                                         rep('Demersal (Flat)', 9), 'Demersal (Round)',
                                         'Demersal (Round)', rep('Demersal (Flat)', 4),
                                         rep('Invertebrate (Pelagic)', 3),
                                         rep('Invertebrate (Benthic)', 3)))

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

living.GB <- merge(living.GB, spclass.GB, by = 'Group', all.x = T)

plank <- c('AtlHerring', 'AtlMackerel', 'SmPelagics')
pisc  <- c('Barndoor', 'Bluefish', 'Cod', 'Fourspot', 'Goosefish', 'HMS', 'OffHake', 
           'SilverHake', 'Sharks', 'SpinyDogfish', 'SummerFlounder', 'OtherPelagics')

living.GB[Group %in% plank, diet := 'Plank']
living.GB[Group %in% pisc,  diet := 'Pisc']
living.GB[is.na(diet) & fish == T, diet := 'Benth']


living.GB[, Q := Biomass * QB]
living.GB[, P := Biomass * PB]
living.GB[, sum(P)]
living.GB[, sum(Q)]
living.GB[, sum(Biomass)]

living.GB[Group %in% c('Phytoplankton', 'Microzooplankton', 'Mesozooplankton', 'Micronekton'),
          sum(P)] / living.GB[, sum(P)]

living.GB[Group %in% c('Seals', 'ToothWhale', 'BaleenWhale'),
          sum(P)] / living.GB[, sum(P)]
living.GB[Classification == 'Invertebrate (Benthic)', sum(Biomass)] / living.GB[, sum(Biomass)]
