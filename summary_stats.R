GB <- rpath(GB.params)
GB.bal <- as.data.table(write.Rpath(GB))

GB.living <- merge(GB.bal[type < 2, ], spclass.GB, by = 'Group')

plank <- c('AtlHerring', 'AtlMackerel', 'SmPelagics')
pisc  <- c('Barndoor', 'Bluefish', 'Cod', 'Fourspot', 'Goosefish', 'HMS', 'OffHake', 
           'SilverHake', 'Sharks', 'SpinyDogfish', 'SummerFlounder', 'OtherPelagics')

GB.living[Group %in% plank, diet := 'Plank']
GB.living[Group %in% pisc,  diet := 'Pisc']
GB.living[is.na(diet) & fish == T, diet := 'Benth']

GB.living[, Q := Biomass * QB]
GB.living[, P := Biomass * PB]
GB.living[, Tot.Q := sum(Q), by = Classification]
GB.living[, Tot.P := sum(P), by = Classification]
GB.living[, Tot.B := sum(Biomass), by = Classification]

GB.sum <- unique(GB.living[, .(Classification, Tot.B, Tot.Q, Tot.P)])
GB.sum[Classification %in% c('Mammal', 'Seal'), sum(Tot.P)]

GB.living[diet == 'Pisc', sum(Biomass)]
