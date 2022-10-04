#Biomass accumulation for GBRpath
#Required packages--------------------------------------------------------------
library(data.table); library(here); library(fitdistrplus)

#Load biomass index
load(file.path(here('data', 'bio.index.rda')))
bio.index <- bio.index[!is.na(B), ]

#Use lm test for significant trend - only add significant terms
groups <- unique(bio.index[, RPATH])

#Test on multiple windows
time <- list(1968:2019, 1978:1987, 1979:1988, 1981:1985)

BA.all <- c()
for(igroup in 1:length(groups)){
  for(itime in 1:length(time)){

    t.win <- time[[itime]]
    
    species <- bio.index[RPATH == groups[igroup] & YEAR %in% t.win, ]
    #Default is BA = 0 unless significant
    BA.sp <- data.table(RPATH = groups[igroup], BA = 0, m = 0, b = 0)
    #Test significance using lm
    if(nrow(species) > 2){
      spLM <- lm(B ~ YEAR, data = species)
      spF <- summary(spLM)$fstatistic
      spP <- pf(spF[1], spF[2], spF[3], lower = F)
      #If significant replace 0 with slope
      if(spP <= 0.05) BA.sp[, BA := spLM$coefficients[2]]
      #Add slope and intercept term for plots
      BA.sp[, m := spLM$coefficients[2]]
      BA.sp[, b := spLM$coefficients[1]]
    }
    BA.sp[, time := itime]
    BA.all <- rbindlist(list(BA.all, BA.sp))
  }
}
BA.sig <- BA.all[BA != 0, list(RPATH, time)]

#Change time variable into a category
BA.all[time == 1, timecat := '68 - 19']
BA.all[time == 2, timecat := '78 - 87']
BA.all[time == 3, timecat := '79 - 88']
BA.all[time == 4, timecat := '81 - 85']
BA.sig[time == 1, timecat := '68 - 19']
BA.sig[time == 2, timecat := '78 - 87']
BA.sig[time == 3, timecat := '79 - 88']
BA.sig[time == 4, timecat := '81 - 85']

#Double check graphically
library(ggplot2)

ba.plot <- ggplot(data = bio.index[RPATH %in% unique(BA.sig[, RPATH]), ],
                  aes(x = YEAR, y = B)) +
  geom_line() +
  facet_wrap(~RPATH, scales = 'free') +
  geom_abline(data = BA.all[RPATH %in% unique(BA.sig[, RPATH]) & BA != 0, ],
              aes(slope = m, intercept = b, col = timecat)) +
  annotate("rect", fill = 'grey', alpha = 0.4,
                    xmin = 1981 , xmax = 1985,
                    ymin = -Inf, ymax = Inf)

  

plot(ba.plot)

#Using groups with both short-term and long-term declines
BA.rpath <- c('Cod', 'Goosefish', 'RedHake', 'YTFlounder')

BA.input <- BA.all[RPATH %in% BA.rpath & timecat == '68 - 19', list(RPATH, BA)]

usethis::use_data(BA.input, overwrite = T)
