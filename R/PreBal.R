#Pre-balance functions
#SML

library(data.table); library(ggplot2)

load(here('data', 'GB.params.rda'))
load(here('data', 'GB.init.rda'))

prebal <- function(rpath.obj, spClasses){
  model <- data.table::as.data.table(Rpath::write.Rpath(rpath.obj))
  living <- model[type < 2, ]
  
  #Biomass Range----
  max.bio <- max(living[, Biomass])
  min.bio <- min(living[, Biomass])
  bio.mag <- round(log(max.bio, base = 10)) - round(log(min.bio, base = 10))
  
  #Slope of Biomass----
  bio.mod <- lm(log(living[, Biomass], base = 10) ~ living[, TL])
  
  #Slope by Trophic level----
  TL.level <- c(1, seq(2, 5.5, .25))
  for(iTL in 1:(length(TL.level) - 1)){
    living[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.group := TL.level[iTL]]
    living[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.bio := sum(Biomass)][]
  }
  TL.model <- unique(living[, .(TL.group, TL.bio)])
  
  TL.mod <- lm(log(TL.model[, TL.bio], base = 10) ~ TL.model[, TL.group])
  
  #Not from prebal but look at F----
  living[, Fmort := Removals / Biomass]
  Bad.F <- living[Fmort > 1, .(Group, Biomass, Removals, Fmort)]
  
  #Predator Prey ratios----
  ratio.tab <- merge(living[, .(Group, Biomass, TL)], spClasses, by = 'Group', 
                     all.x = T)
  
  PredPrey <- data.table(Ratio = c('SP:ZP', 'ZP:PP', 'SP:PP', 'DM:BI', 'SH:SP', 
                                   'MB:SP', 'W:ZP'),
                         Value = c(ratio.tab[Classification == 'Pelagic (Small; Round)', 
                                             sum(Biomass)] /
                                     ratio.tab[Zoop == T, sum(Biomass)],
                                   ratio.tab[Zoop == T, sum(Biomass)] / 
                                     ratio.tab[Group == 'Phytoplankton', sum(Biomass)],
                                   ratio.tab[Classification == 'Pelagic (Small; Round)', 
                                             sum(Biomass)] /
                                     ratio.tab[Group == 'Phytoplankton', sum(Biomass)],
                                   ratio.tab[type == 'Demersal', sum(Biomass)] /
                                     ratio.tab[Classification == 'Invertebrate (Benthic)', 
                                               sum(Biomass)],
                                   ratio.tab[Classification %in% c('Shark', 'HMS'), 
                                             sum(Biomass)] /
                                     ratio.tab[Classification == 'Pelagic (Small; Round)', 
                                               sum(Biomass)],
                                   ratio.tab[Classification %in% c('Mammal', 'Bird'), 
                                             sum(Biomass)] / 
                                     ratio.tab[Classification == 'Pelagic (Small; Round)', 
                                               sum(Biomass)],
                                   ratio.tab[Classification == 'Whale', sum(Biomass)] / 
                                     ratio.tab[Zoop == T, sum(Biomass)]))
  
  #Energy flows
  Fish <- data.table(Ratio = c('DM:PL', 'FF:RF', 'SP:AF', 'HMS:AF', 'Sharks:AF', 
                               'DM:AF'),
                     Value = c(ratio.tab[type == 'Demersal', sum(Biomass)] /
                                 ratio.tab[Classification %in% c('Pelagic (Small; Round)', 
                                                                 'Shark', 
                                                                 'HMS',
                                                                 'Pelagic (Medium; Round'),
                                           sum(Biomass)],
                               ratio.tab[RF == 'Flat', sum(Biomass)] / 
                                 ratio.tab[RF == 'Round', sum(Biomass)],
                               ratio.tab[Classification == 'Pelagic (Small; Round)', 
                                         sum(Biomass)] /
                                 ratio.tab[fish == T, sum(Biomass)],
                               ratio.tab[Classification == 'HMS', sum(Biomass)] / 
                                 ratio.tab[fish == T, sum(Biomass)],
                               ratio.tab[Classification == 'Shark', sum(Biomass)] / 
                                 ratio.tab[fish == T, sum(Biomass)],
                               ratio.tab[type == 'Demersal', sum(Biomass)] / 
                                 ratio.tab[fish == T, sum(Biomass)]))
  
  Invert <- data.table(Ratio = c('BI:AI', 'PI:AI', 'GZ:AI', 'SM:AI', 'ZP:AI', 'PP:AI', 
                                 'ZP:BI'),
                       Value = c(ratio.tab[Classification == 'Invertebrate (Benthic)', 
                                           sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Classification == 'Invertebrate (Pelagic)', 
                                           sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Group == 'GelZooplankton', sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Group %in% c('OtherShrimps', 'Micronekton', 'Krill'), 
                                           sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Zoop == T, sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Group == 'Phytoplankton', sum(Biomass)] / 
                                   ratio.tab[invert == T, sum(Biomass)],
                                 ratio.tab[Zoop == T, sum(Biomass)] / 
                                   ratio.tab[Classification == 'Invertebrate (Benthic)', 
                                             sum(Biomass)]))
  
  Diet <- data.table(Ratio = c('Pisc:Benth', 'Benth:Plank', 
                               'Pisc:Plank', 'TL > 4:TL < 3'),
                     Value = c(ratio.tab[diet == 'Pisc', sum(Biomass)] / 
                                 ratio.tab[diet == 'Benth', sum(Biomass)],
                               ratio.tab[diet == 'Benth', sum(Biomass)] / 
                                 ratio.tab[diet == 'Plank', sum(Biomass)],
                               ratio.tab[diet == 'Pisc', sum(Biomass)] / 
                                 ratio.tab[diet == 'Plank', sum(Biomass)],
                               ratio.tab[TL >= 4, sum(Biomass)] / 
                                 ratio.tab[TL <= 3, sum(Biomass)]))
  
  #Vital Rates (Criteria 3)-----
  #QB
  cons.mod <- lm(log(living[Group != 'Phytoplankton', QB], base = 10) ~ 
                   living[Group != 'Phytoplankton', TL])
  cons.slope <- cons.mod$coef[2]
  
  #PB
  prod.mod <- lm(log(living[, PB], base = 10) ~ living[, TL])
  prod.slope <- prod.mod$coef[2]
  
  return(list('Biomass Span' = bio.mag,
              'Biomass Slope' = as.numeric(bio.mod$coef[2]),
              'Trophic Level Slope' = as.numeric(TL.mod$coef[2]),
              'Too High F' = Bad.F,
              'Predator Prey' = PredPrey,
              'Energy Flows - Fish' = Fish,
              'Energy Flows - Invert' = Invert,
              'Energy Flows - Diet' = Diet,
              'Vital Rates - QB' = cons.slope,
              'Vital Rates - PB' = prod.slope))
}

slopePlot <- function(rpath.obj, type = 'Biomass', ref = T){
  model <- data.table::as.data.table(Rpath::write.Rpath(rpath.obj))
  living <- model[type < 2, ]
  
  if(type == 'Biomass'){
    #Slope of Biomass
    lm.mod <- lm(log(living[, Biomass], base = 10) ~ living[, TL])
    #+- 1 Standard Error
    std <- coef(summary(lm.mod))[, 2]
    
    #Plot basics
    slope <- ggplot(data = living,
                    aes(x = TL, y = Biomass)) +
      geom_label(aes(label = Group)) 
  }
  
  if(type == 'TL'){
    #Slope by Trophic level
    TL.level <- c(1, seq(2, 5.5, .25))
    for(iTL in 1:(length(TL.level) - 1)){
      living[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.group := TL.level[iTL]]
      living[TL >= TL.level[iTL] & TL < TL.level[iTL + 1], TL.bio := sum(Biomass)][]
    }
    TL.model <- unique(living[, .(TL.group, TL.bio)])
    
    lm.mod <- lm(log(TL.model[, TL.bio], base = 10) ~ TL.model[, TL.group])
    #+- 1 Standard Error
    std <- coef(summary(lm.mod))[, 2]
    
    #Plot basics
    slope <- ggplot(data = TL.model,
                    aes(x = TL.group, y = TL.bio)) +
      geom_point() 
  }
  
  if(type == 'Order'){
    data.table::setkey(living, TL)
    living[, TL.order := 1:nrow(living)]
    
    lm.mod <- lm(log(living[, Biomass], base = 10) ~ living[, TL.order])
    #+- 1 Standard Error
    std <- coef(summary(lm.mod))[, 2]
    
    #Plot basics
    slope <- ggplot(data = living,
                    aes(x = TL.order, y = Biomass)) +
      geom_bar(stat = "identity") 
     
  }
  
  if(type == 'TL Order'){
    data.table::setkey(TL.model, TL.group)
    TL.model[, TL.order := 1:nrow(TL.model)]
    
    lm.mod <- lm(log(TL.model[, TL.bio], base = 10) ~ TL.model[, TL.order])
    #+- 1 Standard Error
    std <- coef(summary(lm.mod))[, 2]
    
    #Plot basics
    slope <- ggplot(data = TL.model,
                    aes(x = TL.order, y = TL.bio)) +
      geom_bar(stat = "identity") 
    print(lm.mod$coef[2])
  }
  
  #Add slope line with standard deviations
  slope <- slope +
    scale_y_log10() +
    geom_abline(intercept = lm.mod$coef[1], 
                slope = lm.mod$coef[2]) +
    geom_abline(intercept = lm.mod$coef[1] + std[1], 
                slope = lm.mod$coef[2] + std[2], lty = 2) +
    geom_abline(intercept = lm.mod$coef[1] - std[1], 
                slope = lm.mod$coef[2] - std[2], lty = 2)
  
  
  #Add reference lines for 5 and 10%
  if(ref){
    slope <- slope +
      geom_abline(intercept = lm.mod$coef[1],
                  slope = -0.1, col = 'red') +
      geom_abline(intercept = lm.mod$coef[1],
                  slope = -0.05, col = 'red')
  }
  

  plot(slope)
}


taxaplots <- function()
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


#QB slope
plot(living.GB[Group != 'Phytoplankton', list(TL, QB)], log = "y", typ = 'n')
text(living.GB[Group != 'Phytoplankton', TL], living.GB[Group != 'Phytoplankton', QB], 
     living.GB[, Group], cex = .5)
abline(cons.mod)
#+- 1 Standard Error
std <- coef(summary(cons.mod))[, 2]
abline(a = coef(cons.mod)[1] + std[1], b = coef(cons.mod)[2] + std[2], lty = 2)
abline(a = coef(cons.mod)[1] - std[1], b = coef(cons.mod)[2] - std[2], lty = 2)

#PB
plot(living.GB[, list(TL, PB)], log = "y", typ = 'n')
text(living.GB[, TL], living.GB[, PB], living.GB[, Group], cex = .5)
abline(prod.mod)
#+- 1 Standard Error
std <- coef(summary(prod.mod))[, 2]
abline(a = coef(prod.mod)[1] + std[1], b = coef(prod.mod)[2] + std[2], lty = 2)
abline(a = coef(prod.mod)[1] - std[1], b = coef(prod.mod)[2] - std[2], lty = 2)





