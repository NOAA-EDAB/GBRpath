#Check progress ----
diagnose <- function(rpath.params, group){
  model <- rpath(rpath.params)
  output <- as.data.table(write.Rpath(model))
  setkey(output, EE)
  
  morts <- as.data.table(write.Rpath(model, morts = T))
  
  opar <- par(mfrow = c(3, 1), mar = c(3, 4, 1, 1))
  
  living <- output[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]
  bio.mod <- lm(log(living.GB[, Biomass], base = 10) ~ living.GB[, TL])
  
  plot(living[, list(TL, Biomass)], log = "y", typ = 'n')
  text(living[, TL], living[, Biomass], living[, Group], cex = .8)
  abline(bio.mod)
  #+- 1 Standard Error
  std <- coef(summary(bio.mod))[, 2]
  abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
  abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)
  points(living[Group == group, TL], living[Group == group, Biomass], pch = 19, col = 'red')
  
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
  points(living[Group == group, TL], living[Group == group, QB], pch = 19, col = 'red')
  
  #PB
  prod.mod <- lm(log(living.GB[, PB], base = 10) ~ living.GB[, TL])
  
  plot(living.GB[, list(TL, PB)], log = "y", typ = 'n')
  text(living.GB[, TL], living.GB[, PB], living.GB[, Group], cex = .5)
  abline(prod.mod)
  #+- 1 Standard Error
  std <- coef(summary(prod.mod))[, 2]
  abline(a = coef(prod.mod)[1] + std[1], b = coef(prod.mod)[2] + std[2], lty = 2)
  abline(a = coef(prod.mod)[1] - std[1], b = coef(prod.mod)[2] - std[2], lty = 2)
  points(living[Group == group, TL], living[Group == group, PB], pch = 19, col = 'red')
  
  par(opar)
  
  #Stuff to look at
  mort.table <- data.table(Source = colnames(morts), Mort = as.numeric(morts[Group == group, ]))
  mort.table <- mort.table[!Source %in% c('Group', 'type'), ]
  prey.table <- data.table(Rpred = colnames(rpath.params$diet), 
                           DC = as.numeric(rpath.params$diet[Group == group, ]))
  prey.table <- prey.table[Rpred != 'Group', ]
  out <- list(Balance = output[Group == group, ], Mortalities = mort.table,
              AsPred = rpath.params$diet[, .(Group, get(group))], 
              AsPrey = prey.table)
  return(out)
}
