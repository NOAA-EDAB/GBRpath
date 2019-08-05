#Prebalance for Georges Bank
#SML

#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')

#Required packages--------------------------------------------------------------
library(data.table)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load in pre balanced model
load(file.path(data.dir, 'Input_GB_params.RData'))
load(file.path(data.dir, 'Unbalanced_GB.RData'))
unbal.GB <- as.data.table(write.Rpath(GB))


living.GB <- unbal.GB[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]

#Biomass Across Taxa (Criteria 1)---------
#Biomass Range
max.bio <- max(living.GB[, Biomass])
min.bio <- min(living.GB[, Biomass])
bio.mag <- round(log(max.bio, base = 10)) - round(log(min.bio, base = 10))
bio.mag

#Slope of Biomass
bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, Biomass)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, Biomass], living.GB[, Group], cex = .5)
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

bio.slope <- coef(bio.mod)[2]
bio.slope

#Trophic Level
TL.level <- c(1, seq(2, 5.5, .25))
for(iTL in 1:(length(TL.level) - 1)){
  living.GB[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.group := TL.level[iTL]]
  living.GB[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.bio := sum(Biomass)][]
}
TL.model <- unique(living.GB[, .(TL.group, TL.bio)])
plot(TL.model[, TL.group], TL.model[, TL.bio])

TL.mod <- lm(log(TL.model[, TL.bio], base = 10) ~ TL.model[, TL.group])

opar <- par(mar = c(4, 6, 2, 2))
plot(TL.model[, .(TL.group, TL.bio)], log = "y", pch = 19, axes = F, xlab = '',
     ylab = '')
abline(TL.mod)
#+- 1 Standard Error
std <- coef(summary(TL.mod))[, 2]
abline(a = coef(TL.mod)[1] + std[1], b = coef(TL.mod)[2] + std[2], lty = 2)
abline(a = coef(TL.mod)[1] - std[1], b = coef(TL.mod)[2] - std[2], lty = 2)

axis(1)
axis(2, at = axTicks(2), labels = c(0.5, 1.00, 2.0, 5.0, 10.0, 20.0), las = T)
box(lwd = 2)
mtext(1, text = 'Trophic Level', line = 2)
mtext(2, text = expression('Biomass, kg km'^-2 * '(log scale)'), line = 3)
par(opar)

TL.slope <- coef(TL.mod)[2]
TL.slope

#Not from prebal but look at F
living.GB[, F := Removals / Biomass]
living.GB[F > 1, ]

#Biomass Ratios (Criteria 2)----
#Need to classify each group
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

#Predator Prey ratios
PredPrey <- data.table(Ratio = c('SP:ZP', 'ZP:PP', 'SP:PP', 'DM:BI', 'SH:SP', 
                                 'MB:SP', 'W:ZP'),
                       Value = c(living.GB[Classification == 'Pelagic (Small; Round)', 
                                           sum(Biomass)] /
                                   living.GB[Zoop == T, sum(Biomass)],
                                 living.GB[Zoop == T, sum(Biomass)] / 
                                   living.GB[Group == 'Phytoplankton', sum(Biomass)],
                                 living.GB[Classification == 'Pelagic (Small; Round)', 
                                           sum(Biomass)] /
                                   living.GB[Group == 'Phytoplankton', sum(Biomass)],
                                 living.GB[type == 'Demersal', sum(Biomass)] /
                                   living.GB[Classification == 'Invertebrate (Benthic)', 
                                             sum(Biomass)],
                                 living.GB[Classification %in% c('Shark', 'HMS'), 
                                           sum(Biomass)] /
                                   living.GB[Classification == 'Pelagic (Small; Round)', 
                                             sum(Biomass)],
                                 living.GB[Classification %in% c('Mammal', 'Bird'), 
                                           sum(Biomass)] / 
                                   living.GB[Classification == 'Pelagic (Small; Round)', 
                                             sum(Biomass)],
                                 living.GB[Classification == 'Whale', sum(Biomass)] / 
                                   living.GB[Zoop == T, sum(Biomass)]))

#Energy flows
Fish <- data.table(Ratio = c('DM:PL', 'FF:RF', 'SP:AF', 'HMS:AF', 'Sharks:AF', 
                             'DM:AF'),
                   Value = c(living.GB[type == 'Demersal', sum(Biomass)] /
                               living.GB[Classification %in% c('Pelagic (Small; Round)', 
                                                               'Shark', 
                                                               'HMS',
                                                               'Pelagic (Medium; Round'),
                                         sum(Biomass)],
                             living.GB[RF == 'Flat', sum(Biomass)] / 
                               living.GB[RF == 'Round', sum(Biomass)],
                             living.GB[Classification == 'Pelagic (Small; Round)', 
                                       sum(Biomass)] /
                               living.GB[fish == T, sum(Biomass)],
                             living.GB[Classification == 'HMS', sum(Biomass)] / 
                               living.GB[fish == T, sum(Biomass)],
                             living.GB[Classification == 'Shark', sum(Biomass)] / 
                               living.GB[fish == T, sum(Biomass)],
                             living.GB[type == 'Demersal', sum(Biomass)] / 
                               living.GB[fish == T, sum(Biomass)]))

Invert <- data.table(Ratio = c('BI:AI', 'PI:AI', 'GZ:AI', 'SM:AI', 'ZP:AI', 'PP:AI', 
                               'ZP:BI'),
                     Value = c(living.GB[Classification == 'Invertebrate (Benthic)', 
                                         sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Classification == 'Invertebrate (Pelagic)', 
                                         sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Group == 'GelZooplankton', sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Group %in% c('OtherShrimps', 'Micronekton', 'Krill'), 
                                         sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Zoop == T, sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Group == 'Phytoplankton', sum(Biomass)] / 
                                 living.GB[invert == T, sum(Biomass)],
                               living.GB[Zoop == T, sum(Biomass)] / 
                                 living.GB[Classification == 'Invertebrate (Benthic)', 
                                           sum(Biomass)]))

plank <- c('AtlHerring', 'AtlMackerel', 'SmPelagics')
pisc  <- c('Barndoor', 'Bluefish', 'Cod', 'Fourspot', 'Goosefish', 'HMS', 'OffHake', 
           'SilverHake', 'Sharks', 'SpinyDogfish', 'SummerFlounder', 'OtherPelagics')

living.GB[Group %in% plank, diet := 'Plank']
living.GB[Group %in% pisc,  diet := 'Pisc']
living.GB[is.na(diet) & fish == T, diet := 'Benth']

Diet <- data.table(Ratio = c('Pisc:Benth', 'Benth:Plank', 
                             'Pisc:Plank', 'TL > 4:TL < 3'),
                   Value = c(living.GB[diet == 'Pisc', sum(Biomass)] / 
                               living.GB[diet == 'Benth', sum(Biomass)],
                             living.GB[diet == 'Benth', sum(Biomass)] / 
                               living.GB[diet == 'Plank', sum(Biomass)],
                             living.GB[diet == 'Pisc', sum(Biomass)] / 
                               living.GB[diet == 'Plank', sum(Biomass)],
                             living.GB[TL >= 4, sum(Biomass)] / 
                               living.GB[TL <= 3, sum(Biomass)]))

opar <- par(mfrow = c(2, 2), mar = c(5, 0, 0, 2), oma = c(2, 4, 2, 0))
barplot(PredPrey[, Value], names.arg = PredPrey[, Ratio], las = 2)
legend('topright', legend = 'A', bty = 'n', cex = 2)
barplot(Fish[, Value], names.arg = Fish[, Ratio], las = 2)
legend('topright', legend = 'B', bty = 'n', cex = 2)
barplot(Invert[, Value], names.arg = Invert[, Ratio], las = 2)
legend('topright', legend = 'C', bty = 'n', cex = 2)
barplot(Diet[, Value], names.arg = Diet[, Ratio], las = 2)
legend('topright', legend = 'D', bty = 'n', cex = 2)
mtext(2, outer = T, text = 'Ratio', line = 2.5)

#Vital Rates (Criteria 3)-----
#QB
cons.mod <- lm(log(living.GB[Group != 'Phytoplankton', QB], base = 10) ~ 
                 living.GB[Group != 'Phytoplankton', TL])

plot(living.GB[Group != 'Phytoplankton', list(TL, QB)], log = "y", typ = 'n')
text(living.GB[Group != 'Phytoplankton', TL], living.GB[Group != 'Phytoplankton', QB], 
     living.GB[, Group], cex = .5)
abline(cons.mod)
#+- 1 Standard Error
std <- coef(summary(cons.mod))[, 2]
abline(a = coef(cons.mod)[1] + std[1], b = coef(cons.mod)[2] + std[2], lty = 2)
abline(a = coef(cons.mod)[1] - std[1], b = coef(cons.mod)[2] - std[2], lty = 2)

cons.slope <- coef(cons.mod)[2]
cons.slope

#PB
prod.mod <- lm(log(living.GB[, PB], base = 10) ~ living.GB[, TL])

plot(living.GB[, list(TL, PB)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, PB], living.GB[, Group], cex = .5)
abline(prod.mod)
#+- 1 Standard Error
std <- coef(summary(prod.mod))[, 2]
abline(a = coef(prod.mod)[1] + std[1], b = coef(prod.mod)[2] + std[2], lty = 2)
abline(a = coef(prod.mod)[1] - std[1], b = coef(prod.mod)[2] - std[2], lty = 2)

prod.slope <- coef(prod.mod)[2]
prod.slope


