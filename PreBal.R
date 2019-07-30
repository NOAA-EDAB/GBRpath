#PreBal functions
library(data.table)
classify <- function(groups, class1, class2){
  #Available classifications
  class.type.1 <- c('Dem', 'MedPel', 'SmPel', 'Zoo', 'Phyto', 'BenInvert', 'Shark', 'HMS',
                    'Mammal', 'Whale', 'Bird')
  class.type.2 <- c('Flat', 'Round')
  for(iclass in 1:length(class1)){
    if(!class1[iclass] %in% class.type.1){
      errorCondition(message = paste(class1[iclass] 'not valid'))
    }
  }
  out <- data.table(Group = groups, Class1 = class1, Class2 = class2)
  return(out)
}

prebal <- function(rpath, )
  #For now REco is the same as rpath
x <- copy(REco)
output.table <- data.table(Group = x$Group, type = x$type, TL = x$TL, 
                           Biomass = x$Biomass)
living.table <- output.table[type < 2, ]
setkey(living.table, TL)

max.bio <- max(living.table[, Biomass])
min.bio <- min(living.table[, Biomass])
group.num <- 1:nrow(living.table)

bio.mag <- round(log(max.bio, base = 10)) - round(log(min.bio, base = 10))
bio.mod <- lm(log(living.table[, Biomass], base = 10) ~ group.num)

plot(living.table[, Biomass], log = "y")
abline(bio.mod)
#+- 1 Standard Error
std <- coef(summary(bio.mod))[, 2]
abline(a = coef(bio.mod)[1] + std[1], b = coef(bio.mod)[2] + std[2], lty = 2)
abline(a = coef(bio.mod)[1] - std[1], b = coef(bio.mod)[2] - std[2], lty = 2)

bio.slope <- coef(bio.mod)[2]
