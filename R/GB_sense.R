#Georges Bank Rpath Ecosense
#SML

#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Load balanced parameters and model
load(here('data', 'GB.params.bal.rda'))
load(here('data', 'GB.bal.rda'))

#Need to fix GB pedigree file - bigger issue to fix eventually!
GB.params.bal$pedigree <- GB.params.bal$pedigree[!Group %in% c('ScallopDredge',
                                                               'ClamDredge',
                                                               'OtherDredge',
                                                               'FixedGear',
                                                               'Pelagic',
                                                               'Trap',
                                                               'SmallMesh',
                                                               'LargeMesh',
                                                               'HMSFleet',
                                                               'OtherFisheries'), ]


#Assign pedigrees - currently based off Lucey 2019 values - need to  revisit
GB.params.bal$pedigree$Biomass <- c(0.5, 0.8, 0.5, 0.5, 0.5, 0.4, 0.2, 0.2, 0.5,
                                    0.2, 0.5, 0.4, 0.5, 0.2, 0.2, 0.4, 0.4, 0.2,
                                    0.2, 0.2, 0.4, 0.4, 0.4, 0.4, 0.2, 0.2, 0.2,
                                    0.4, 0.2, 0.2, 0.4, 0.2, 0.2, 0.4, 0.2, 0.5,
                                    0.5, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                                    0.5, 0.2, 0.5, 0.2, 0.2, 0.2, 0.2, 0.5, 0.5,
                                    0.5, 0.5, 0.5, 0.8, 0.5, 0.8, 0.8)

GB.params.bal$pedigree$PB <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2, 0.2,
                               0.5, 0.5, 0.5, 0.2, 0.2, 0.7, 0.6, 0.7, 0.6, 0.7,
                               0.6, 0.7, 0.7, 0.7, 0.2, 0.6, 0.5, 0.5, 0.6, 0.2,
                               0.7, 0.7, 0.7, 0.7, 0.2, 0.8, 0.8, 0.2, 0.6, 0.6,
                               0.7, 0.7, 0.2, 0.6, 0.6, 0.6, 0.6, 0.5, 0.6, 0.6,
                               0.6, 0.5, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0,
                               0.0)

GB.params.bal$pedigree$QB <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2, 0.2,
                               0.5, 0.5, 0.5, 0.2, 0.2, 0.6, 0.6, 0.7, 0.6, 0.7,
                               0.6, 0.7, 0.6, 0.6, 0.2, 0.6, 0.5, 0.5, 0.6, 0.2,
                               0.6, 0.6, 0.6, 0.6, 0.2, 0.8, 0.8, 0.2, 0.6, 0.6,
                               0.6, 0.6, 0.2, 0.6, 0.6, 0.6, 0.6, 0.5, 0.6, 0.6,
                               0.6, 0.5, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0,
                               0.0)

GB.params.bal$pedigree$Diet <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2, 
                                 0.2, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2, 0.2, 0.2,
                                 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.5,
                                 0.5, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.8,
                                 0.8, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.6, 0.6,
                                 0.6, 0.6, 0.5, 0.6, 0.6, 0.6, 0.5, 0.6, 0.5,
                                 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0)
#Test dynamic run
# GB.scene <- rsim.scenario(GB, GB.params, years = 2014:2113)
# GB.scene$params$NoIntegrate[2:6] <- 0
# GB.testrun <- rsim.run(GB.scene, method = 'AB', years = 2014:2113)
# rsim.plot(GB.testrun, GB.params$model[Type < 3, Group])

#Set up sense runs
all_years <- 1981:2031
scene <- rsim.scenario(GB.bal, GB.params.bal, years = all_years)

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 300
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(123)
for (irun in 1:NUM_RUNS){
  GBsense <- copy(scene) 
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GBsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(GBsense, GB.params.bal)	# Replace the base params with Ecosense params  
  GBsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GBsense$params <- parlist[[irun]]
  GBtest <- rsim.run(GBsense, method = "RK4", years = all_years)
  failList <- which(is.na(GBtest$end_state$Biomass))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",GBtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
# 34 / 300 = 11.3%
GB.sense <- parlist[KEPT]
save(GB.sense, file = file.path(data.dir, 'GB_ecosense_valid.RData'))


#This is still old code below here


#Run scenario-----
#Set 1----
#Atlantic herring
#Determine new effort
species <- 'AtlHerring'
gears <- c('DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'PotTrap', 'OtterTrawlSm',
           'OtterTrawlLg', 'Midwater', 'OtherFisheries')
RGear <- c('dredge.sc', 'dredge.cl', 'gillnet', 'longline', 'pot', 'otter.sm',
           'otter.lg', 'midwater', 'other')
deltaE <- data.table(Group = species, rpath.bio = GB.params$model[Group == species, Biomass],
                     RGear = gears)

rpath.land <-  data.table(RGear = gears, GearCode = RGear, 
                          rpath.land = as.numeric(GB.params$model[Group == species, .SD, .SDcols = gears]))
deltaE <- merge(deltaE, rpath.land, by = 'RGear')
deltaE <- deltaE[rpath.land > 0]

current <- GB.cur.land[RPATH == species]
current[, Biomass := GB.current[RPATH == species, Biomass]]
setnames(current, c('RGear', 'SPPLIVMT', 'Biomass'), c('GearCode', 'new.land', 'new.bio'))
deltaE <- merge(deltaE, current, by = 'GearCode', all = T)
#Note - 2016 station 298 caught 4,204 kg of herring, most ever on the Bigelow!  Removed
#Tow similar to stock assessment and reaverage numbers (4.70)
#Multiple by Rpath change table (base x4).
deltaE[, new.bio := 18.824]

deltaE[, rpath.q := rpath.land / rpath.bio]
deltaE[, newE := new.land / (rpath.q * new.bio)]

bio.output.1 <- c()
catch.output.1 <- c()
set.seed(123)
for(irun in 1:length(GB.sense)){
  run.scene <- rsim.scenario(GB, GB.params, years = all_years)
  run.scene$params <- GB.sense[[irun]]
  run.scene <- adjust.fishing(run.scene, parameter = 'ForcedEffort', group = 'Midwater',
                              sim.year = 2020:2063, value = 0.156)
  run <- rsim.run(run.scene, years = all_years)
  
  #Save average of final 10 years
  bio <- as.data.table(run$annual_Biomass)
  bio.mean <- bio[41:50, lapply(.SD, mean), .SDcols = names(bio)]
  bio.mean[, run := irun]
  bio.output.1 <- rbindlist(list(bio.output.1, bio.mean))
  
  catch <- as.data.table(run$annual_Catch)
  catch.mean <- catch[41:50, lapply(.SD, mean), .SDcols = names(catch)]
  catch.mean[, run := irun]
  catch.output.1 <- rbindlist(list(catch.output.1, catch.mean))
  
  #Counter
  cat(irun,": processed\n")
  flush.console()
}
save(bio.output.1,   file = file.path(data.dir, 'Bio_output_set1.RData'))
save(catch.output.1, file = file.path(data.dir, 'Catch_output_set1.RData'))

#Explore results
target <- unique(deltaE[Group == 'AtlHerring', new.bio])
low    <- target / 2
high   <- target * 2

target.c <- deltaE[, sum(new.land, na.rm = T)]
low.c    <- target.c / 2
high.c   <- target.c * 2

boxplot(bio.output.1[, AtlHerring], ylim = c(low, high))
abline(h = target, col = 'red')
abline(h = high, col = 'red', lty = 2)
abline(h = low, col = 'red', lty = 2)

manage.set.bio <- bio.output.1[AtlHerring >= low & AtlHerring <= high, .(run, AtlHerring)]
setnames(manage.set.bio, 'AtlHerring', 'Biomass')
manage.set.catch <- catch.output.1[AtlHerring >= low.c & AtlHerring <= high.c, .(run, AtlHerring)]
setnames(manage.set.catch, 'AtlHerring', 'Catch')
manage.set <- merge(manage.set.bio, manage.set.catch, by = 'run')
#388 out of 1104 ~35.1%
run.set1 <- manage.set[, run]

#MS plots set 1----
#All species
opar <- par(mfrow = c(2, 1), mar = c(0, 0, 1, 1), oma = c(7, 6, 0, 0))
plot.all <- names(bio.output.1)[which(!names(bio.output.1) %in% c('Outside', 'Detritus', 'Discards', 'run'))]
boxplot(bio.output.1[, 2:59], log = 'y', range = 0, axes = F)
axis(1, at = 1:58, labels = F, las = 2, cex.axis = .8)
axis(2, cex.axis = 0.8, las = T)
box(lwd = 2)
legend('topleft', legend = 'A', bty = 'n')

boxplot(bio.output.1[run.set1, 2:59], log = 'y', range = 0, axes = F, ylim = c(1e-10, 1e10))
axis(1, at = 1:58, labels = names(bio.output.1)[2:59], las = 2, cex.axis = .8)
axis(2, cex.axis = 0.8, las = T)
box(lwd = 2)
mtext(2, text = expression('Biomass, t km'^-2 * '(log)'), line = 2.8, outer = T)
legend('topleft', legend = 'B', bty = 'n')
par(opar)

#Herring
opar <- par(mfcol = c(1, 2), mar = c(4, 0, 4, 2), oma = c(2, 5, 2, 5))
boxplot(manage.set[, Biomass], axes = F)
lines(c(0.7, 1.3), c(target, target), col = 'blue', lty = 2)
axis(2, las = T)
box(lwd = 2)
mtext(2, text = expression('Biomass, t km'^-2), line = 2.5, cex = 1.4)
legend('topleft', legend = 'A', bty = 'n')

boxplot(manage.set[, Catch], axes = F)
lines(c(0.7, 1.3), c(target.c, target.c), col = 'blue', lty = 2)
axis(4, las = T)
box(lwd = 2)
mtext(4, text = expression('Landings, t km'^-2), line = 3.2, cex = 1.4)
legend('topleft', legend = 'B', bty = 'n')
par(opar)

#Set 2------
#Cod, Haddock, and Yellowtail
species <- c('Cod', 'Haddock', 'YTFlounder')
gears <- c('DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'PotTrap', 'OtterTrawlSm',
           'OtterTrawlLg', 'Midwater', 'OtherFisheries')
RGear <- c('dredge.sc', 'dredge.cl', 'gillnet', 'longline', 'pot', 'otter.sm',
           'otter.lg', 'midwater', 'other')
deltaE <- data.table(GearCode = NA, RGear = NA, Group = NA, rpath.bio = NA, rpath.land = NA,
                     RPATH = NA, new.land = NA, new.bio = NA)
for(i in 1:3){
  deltaE.sp <- data.table(Group = species[i], rpath.bio = GB.params$model[Group == species[i], Biomass],
                          RGear = gears)
  
  rpath.land <-  data.table(RGear = gears, GearCode = RGear, 
                            rpath.land = as.numeric(GB.params$model[Group == species[i], .SD, .SDcols = gears]))
  deltaE.sp <- merge(deltaE.sp, rpath.land, by = 'RGear')
  deltaE.sp <- deltaE.sp[rpath.land > 0]
  
  current <- GB.cur.land[RPATH == species[i]]
  current[, Biomass := GB.current[RPATH == species[i], Biomass]]
  setnames(current, c('RGear', 'SPPLIVMT', 'Biomass'), c('GearCode', 'new.land', 'new.bio'))
  deltaE.sp <- merge(deltaE.sp, current, by = 'GearCode', all = T)
  deltaE <- rbindlist(list(deltaE, deltaE.sp))
}
deltaE <- deltaE[!is.na(Group), ]

#Multiple by Rpath change table (base x4).
deltaE[, new.bio := new.bio * 4]
deltaE[Group == 'Cod', new.bio := new.bio * 0.12]
deltaE[Group == 'Haddock', new.bio := new.bio * 0.05]

deltaE[, rpath.q := rpath.land / rpath.bio]
deltaE[, newE := new.land / (rpath.q * new.bio)]

E.otter.lg <- deltaE[RGear == 'OtterTrawlLg', mean(newE)]
E.otter.sm <- deltaE[RGear == 'OtterTrawlSm', mean(newE)]
E.gillnet  <- deltaE[RGear == 'Gillnet',      mean(newE)]

bio.output.2 <- c()
catch.output.2 <- c()
set.seed(123)
for(irun in 1:length(GB.sense)){
  run.scene <- rsim.scenario(GB, GB.params, years = all_years)
  run.scene$params <- GB.sense[[irun]]
  run.scene <- adjust.fishing(run.scene, parameter = 'EFFORT', group = 'OtterTrawlSm',
                              sim.year = 2020:2063, value = E.otter.sm)
  run.scene <- adjust.fishing(run.scene, parameter = 'EFFORT', group = 'OtterTrawlLg',
                              sim.year = 2020:2063, value = E.otter.lg)
  run.scene <- adjust.fishing(run.scene, parameter = 'EFFORT', group = 'Gillnet',
                              sim.year = 2020:2063, value = E.gillnet)
  run <- rsim.run(run.scene, years = all_years)
  
  #Save average of final 10 years
  bio <- as.data.table(run$annual_Biomass)
  bio.mean <- bio[41:50, lapply(.SD, mean), .SDcols = names(bio)]
  bio.mean[, run := irun]
  bio.output.2 <- rbindlist(list(bio.output.2, bio.mean))
  
  catch <- as.data.table(run$annual_Catch)
  catch.mean <- catch[41:50, lapply(.SD, mean), .SDcols = names(catch)]
  catch.mean[, run := irun]
  catch.output.2 <- rbindlist(list(catch.output.2, catch.mean))
  
  #Counter
  cat(irun,": processed\n")
  flush.console()
}
save(bio.output.2,   file = file.path(data.dir, 'Bio_output_set2.RData'))
save(catch.output.2, file = file.path(data.dir, 'Catch_output_set2.RData'))

bio.output.2[, agg.bio := sum(Cod, Haddock, YTFlounder), by = run]
catch.output.2[, agg.catch := sum(Cod, Haddock, YTFlounder), by = run]

#Explore results
target.cod <- unique(deltaE[Group == 'Cod', new.bio])
low.cod    <- target.cod / 2
high.cod   <- target.cod * 2

target.had <- unique(deltaE[Group == 'Haddock', new.bio])
low.had    <- target.had / 2
high.had   <- target.had * 2

target.ytf <- unique(deltaE[Group == 'YTFlounder', new.bio])[!is.na(unique(deltaE[Group == 'YTFlounder', new.bio]))]
low.ytf    <- target.ytf / 2
high.ytf   <- target.ytf * 2

target.c.cod <- deltaE[Group == 'Cod', sum(new.land, na.rm = T)]
low.c.cod    <- target.c.cod / 2
high.c.cod   <- target.c.cod * 2

target.c.had <- deltaE[Group == 'Haddock', sum(new.land, na.rm = T)]
low.c.had    <- target.c.had / 2
high.c.had   <- target.c.had * 2

target.c.ytf <- deltaE[Group == 'YTFlounder', sum(new.land, na.rm = T)]
low.c.ytf    <- target.c.ytf / 2
high.c.ytf   <- target.c.ytf * 2

target.agg <- sum(unique(deltaE[, new.bio]), na.rm = T)
low.agg  <- target.agg / 2
high.agg <- target.agg * 2

target.c.agg <- sum(unique(deltaE[, new.land]), na.rm = T)
low.c.agg  <- target.c.agg / 2
high.c.agg <- target.c.agg * 2

boxplot(bio.output.2[, Cod], log = 'y')
abline(h = target.cod, col = 'red')
abline(h = high.cod, col = 'red', lty = 2)
abline(h = low.cod, col = 'red', lty = 2)

boxplot(bio.output.2[, Haddock], log = 'y')
abline(h = target.had, col = 'red')
abline(h = high.had, col = 'red', lty = 2)
abline(h = low.had, col = 'red', lty = 2)

boxplot(bio.output.2[, YTFlounder], log = 'y')
abline(h = target.ytf, col = 'red')
abline(h = high.ytf, col = 'red', lty = 2)
abline(h = low.ytf, col = 'red', lty = 2)

boxplot(bio.output.2[, agg.bio], log = 'y')
abline(h = target.agg, col = 'red')
abline(h = high.agg, col = 'red', lty = 2)
abline(h = low.agg, col = 'red', lty = 2)

boxplot(catch.output.2[, agg.catch], log = 'y')
abline(h = target.c.agg, col = 'red')
abline(h = high.c.agg, col = 'red', lty = 2)
abline(h = low.c.agg, col = 'red', lty = 2)


manage.set.bio <- bio.output.2[agg.bio >= low.agg & agg.bio <= high.agg, .(run, agg.bio)]
setnames(manage.set.bio, 'agg.bio', 'Biomass')
manage.set.catch <- catch.output.2[agg.catch >= low.c.agg & agg.catch <= high.c.agg, .(run, agg.catch)]
setnames(manage.set.catch, 'agg.catch', 'Catch')
manage.set <- merge(manage.set.bio, manage.set.catch, by = 'run')
#only 3 out of 1104 ~0.27% if setting limit per species
#354 of 1104 ~32% when done in aggregate

run.set2 <- manage.set[, run]
set.ind.bio <- bio.output.2[run %in% run.set2, .(run, Cod, Haddock, YTFlounder, agg.bio)]
setnames(set.ind.bio, c('Cod', 'Haddock', 'YTFlounder'), 
         c('Cod.Biomass', 'Haddock.Biomass', 'YTFlounder.Biomass'))
set.ind.catch <- catch.output.2[run %in% run.set2, .(run, Cod, Haddock, YTFlounder, agg.catch)]
setnames(set.ind.catch, c('Cod', 'Haddock', 'YTFlounder'), 
         c('Cod.Catch', 'Haddock.Catch', 'YTFlounder.Catch'))
set.ind <- merge(set.ind.bio, set.ind.catch, by = 'run')


boxplot(set.ind[, .(Cod.Biomass, Haddock.Biomass, YTFlounder.Biomass)])
abline(h = target.cod, col = 'red')
abline(h = high.cod, col = 'red', lty = 2)
abline(h = low.cod, col = 'red', lty = 2)

abline(h = target.had, col = 'green')
abline(h = high.had, col = 'green', lty = 2)
abline(h = low.had, col = 'green', lty = 2)

abline(h = target.ytf, col = 'orange')
abline(h = high.ytf, col = 'orange', lty = 2)
abline(h = low.ytf, col = 'orange', lty = 2)

abline(h = target.agg, col = 'grey')
abline(h = high.agg, col = 'grey', lty = 2)
abline(h = low.agg, col = 'grey', lty = 2)


boxplot(set.ind[, .(Cod.Catch, Haddock.Catch, YTFlounder.Catch)])
abline(h = target.c.cod, col = 'red')
abline(h = high.c.cod, col = 'red', lty = 2)
abline(h = low.c.cod, col = 'red', lty = 2)

abline(h = target.c.had, col = 'green')
abline(h = high.c.had, col = 'green', lty = 2)
abline(h = low.c.had, col = 'green', lty = 2)

abline(h = target.c.ytf, col = 'orange')
abline(h = high.c.ytf, col = 'orange', lty = 2)
abline(h = low.c.ytf, col = 'orange', lty = 2)

abline(h = target.c.agg, col = 'grey')
abline(h = high.c.agg, col = 'grey', lty = 2)
abline(h = low.c.agg, col = 'grey', lty = 2)

#MS plots set 2----
#All species
opar <- par(mfrow = c(2, 1), mar = c(0, 0, 1, 1), oma = c(7, 6, 0, 0))
plot.all <- names(bio.output.2)[which(!names(bio.output.1) %in% c('Outside', 'Detritus', 'Discards', 'run'))]
boxplot(bio.output.2[, 2:59], log = 'y', range = 0, axes = F)
axis(1, at = 1:58, labels = F, las = 2, cex.axis = .8)
axis(2, cex.axis = 0.8, las = T)
box(lwd = 2)
legend('topleft', legend = 'A', bty = 'n')

boxplot(bio.output.2[run.set2, 2:59], log = 'y', range = 0, axes = F, ylim = c(1e-10, 1e10))
axis(1, at = 1:58, labels = names(bio.output.2)[2:59], las = 2, cex.axis = .8)
axis(2, cex.axis = 0.8, las = T)
box(lwd = 2)
mtext(2, text = expression('Biomass, t km'^-2 * '(log)'), line = 2.8, outer = T)
legend('topleft', legend = 'B', bty = 'n')
par(opar)

#Cod, Haddock, YT
opar <- par(mfcol = c(2, 1), mar = c(0, 0, 2, 0), oma = c(4, 5, 2, 5))
boxplot(set.ind[, .(Cod.Biomass, Haddock.Biomass, YTFlounder.Biomass, agg.bio)], axes = F)
lines(c(0.5, 1.5), c(target.cod, target.cod), col = 'blue', lty = 2)
lines(c(1.5, 2.5), c(target.had, target.had), col = 'blue', lty = 2)
lines(c(2.5, 3.5), c(target.ytf, target.ytf), col = 'blue', lty = 2)
lines(c(3.5, 4.5), c(target.agg, target.agg), col = 'blue', lty = 2)
axis(2, las = T)
box(lwd = 2)
mtext(2, text = expression('Biomass, t km'^-2), line = 2.8, cex = 1.4)
legend('topleft', legend = 'A', bty = 'n', cex = 2, adj = c(2, -2))

boxplot(set.ind[, .(Cod.Catch, Haddock.Catch, YTFlounder.Catch, agg.catch)], axes = F)
lines(c(0.5, 1.5), c(target.c.cod, target.c.cod), col = 'blue', lty = 2)
lines(c(1.5, 2.5), c(target.c.had, target.c.had), col = 'blue', lty = 2)
lines(c(2.5, 3.5), c(target.c.ytf, target.c.ytf), col = 'blue', lty = 2)
lines(c(3.5, 4.5), c(target.c.agg, target.c.agg), col = 'blue', lty = 2)
axis(1, at = 1:4, labels = c('Cod', 'Haddock', 'YTFlounder', 'Aggregate'), cex.axis = 1.4)
axis(2, las = T)
box(lwd = 2)
mtext(2, text = expression('Landings, t km'^-2), line = 2.8, cex = 1.4)
legend('topleft', legend = 'B', bty = 'n', cex = 2, adj = c(2, -2))
par(opar)




#Haddock catch low/ Cod and YT high
#Fleet structure doesn't catch the nuaices of actually fishing - different efforts required between the 3 species


