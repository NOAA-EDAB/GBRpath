#Biomass accumulation for GBRpath
#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')
gis.dir  <- file.path(main.dir, 'gis')

#Required packages--------------------------------------------------------------
library(data.table); library(fitdistrplus)
load(file.path(data.dir, 'GB_biomass_raw.RData'))

#User functions-----------------------------------------------------------------

#Script-------------------------------------------------------------------------
#Need area for slope
#Grab strata
epu <- readOGR(gis.dir, 'EPU_extended')

#Generate area table
epu.area <- getarea(epu, 'EPU')
A <- epu.area[EPU == 'GB', Area]

#Use lm test for significant trend - only add significant terms
groups <- unique(GB.raw[, RPATH])
BA.all <- c()
for(igroup in groups){
  species <- GB.raw[RPATH == igroup, ]
  #Default is BA = 0 unless significant
  BA.sp <- data.table(RPATH = igroup, BA = 0, m = 0, b = 0)
  #Test significance using lm
  if(length(species[, swept.bio.mt]) > 2){
    spLM <- lm(swept.bio.mt ~ YEAR, data = species)
    spF <- summary(spLM)$fstatistic
    spP <- pf(spF[1], spF[2], spF[3], lower = F)
    #If significant replace 0 with slope divided by GB area (scaled)
    if(spP <= 0.05) BA.sp[, BA := spLM$coefficients[2] / A]
    #Add slope and intercept term for plots
    BA.sp[, m := spLM$coefficients[2]]
    BA.sp[, b := spLM$coefficients[1]]
  }
  BA.all <- rbindlist(list(BA.all, BA.sp))
}
BA.sig <- BA.all[BA != 0, RPATH]

#Double check graphically
opar <- par(mfrow = c(5, 11), mar = c(0, 0, 0, 0), oma = c(4, 8, 2, 2))
for(isp in 1:nrow(BA.all)){
  bio.sp <- GB.raw[RPATH == groups[isp], list(YEAR, swept.bio.mt)]
  y.max <- max(bio.sp[, swept.bio.mt]) + 0.1 * max(bio.sp[, swept.bio.mt])
  y.min <- min(bio.sp[, swept.bio.mt]) - 0.1 * min(bio.sp[, swept.bio.mt])
  
  plot(bio.sp, xlim = c(2011, 2017), ylim = c(y.min, y.max), axes = F, pch = 19,
       typ = 'b')
  abline(a = BA.all[RPATH == groups[isp], b], b = BA.all[RPATH == groups[isp], m],
         col = 'red', lwd = 1)
  if(groups[isp] %in% BA.sig){
    abline(a = BA.all[RPATH == groups[isp], b], b = BA.all[RPATH == groups[isp], m],
           col = 'red', lwd = 3)
  }
  #axis(1, labels = F)
  #axis(2, labels = F)
  box()
  text(par('usr')[1], par('usr')[4], labels = groups[isp], adj = c(0, 1), cex = 0.7)
}


save(BA.all, file = file.path(data.dir, 'GB_biomass_accumulation.RData'))
