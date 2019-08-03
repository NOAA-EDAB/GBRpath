#Check progress ----
GB <- rpath(GB.params, 'Georges Bank', 1)
output.GB <- as.data.table(write.Rpath(GB))
setkey(output.GB, EE)
output.GB[, Q := QB * Biomass]
output.GB[, PQ := PB / QB]
output.GB[, .(Group, EE)]
morts <- as.data.table(write.Rpath(GB, morts = T))


diagnose <- function(rpath.params, group){
  model <- rpath(rpath.params)
  output <- as.data.table(write.Rpath(model))
  setkey(output, EE)
  
  output[, Q := QB * Biomass]
  output[, PQ := PB / QB]
  
  morts <- as.data.table(write.Rpath(model, morts = T))
  
  #Stuff to look at
  mort.table <- data.table(Source = colnames(morts), Mort = as.numeric(morts[Group == group, ]))
  mort.table <- mort.table[!Source %in% c('Group', 'type'), ]
  prey.table <- data.table(Rpred = colnames(rpath.params$diet), 
                           DC = as.numeric(rpath.params$diet[Group == group, ]))
  prey.table <- prey.table[Rpred != 'Group', ]
  top.preds  <- mort.table[Source != 'PB' & Mort > 1, substr(Source, 4, nchar(Source))]
  if(group %in% top.preds) pt.col <- 'blue' else pt.col <- 'green'
  
  #Plots
  opar <- par(mfrow = c(3, 1), mar = c(3, 4, 1, 1))
  
  living <- output[type < 2, list(Group, Biomass, Removals, TL, PB, QB)]
  bio.mod <- lm(log(living[, Biomass], base = 10) ~ living[, TL])
  
  plot(living[, list(TL, Biomass)], log = "y", typ = 'n')
  text(living[, TL], living[, Biomass], living[, Group], cex = .8)
  abline(bio.mod)
  #+- 1 Standard Error
  std <- coef(summary(bio.mod))[, 2]
  abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
  abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)
  points(living[Group %in% top.preds, TL], living[Group %in% top.preds, Biomass],
         pch = 19, col = 'red')
  points(living[Group == group, TL], living[Group == group, Biomass], 
         pch = 19, col = pt.col)
  
  #QB
  cons.mod <- lm(log(living[Group != 'Phytoplankton', QB], base = 10) ~ 
                   living[Group != 'Phytoplankton', TL])
  
  plot(living[, list(TL, QB)], log = "y", typ = 'n')
  text(living[, TL], living[, QB], living[, Group], cex = .8)
  abline(cons.mod)
  #+- 1 Standard Error
  std <- coef(summary(cons.mod))[, 2]
  abline(a = coef(cons.mod)[1] + std[1], b = coef(cons.mod)[2] + std[2], lty = 2)
  abline(a = coef(cons.mod)[1] - std[1], b = coef(cons.mod)[2] - std[2], lty = 2)
  points(living[Group %in% top.preds, TL], living[Group %in% top.preds, QB],
         pch = 19, col = 'red')
  points(living[Group == group, TL], living[Group == group, QB], pch = 19, col = pt.col)
  
  #PB
  prod.mod <- lm(log(living[, PB], base = 10) ~ living[, TL])
  
  plot(living[, list(TL, PB)], log = "y", typ = 'n')
  text(living[, TL], living[, PB], living[, Group], cex = .8)
  abline(prod.mod)
  #+- 1 Standard Error
  std <- coef(summary(prod.mod))[, 2]
  abline(a = coef(prod.mod)[1] + std[1], b = coef(prod.mod)[2] + std[2], lty = 2)
  abline(a = coef(prod.mod)[1] - std[1], b = coef(prod.mod)[2] - std[2], lty = 2)
  points(living[Group == group, TL], living[Group == group, PB], pch = 19, col = 'green')
  
  par(opar)
  
  
  out <- list(Mortalities = mort.table,
              AsPred = rpath.params$diet[, .(Group, get(group))], 
              AsPrey = prey.table, Balance = output[Group == group, ])
  return(out)
}
