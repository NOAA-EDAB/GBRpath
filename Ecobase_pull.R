#Code from Ecobase.ecopath.org to access Ecobase
#To get the list of available Ewe models
library(RCurl); library(XML); library(plyr)

#To obtain the list of available model
h=basicTextGatherer()
curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)
liste_mod <- as.data.table(ldply(xmlToList(data),data.frame))

models <- liste_mod[, list(model.model_number, model.model_name, model.country, 
                           model.ecosystem_type)]
shelf.mods <- models[model.ecosystem_type %in% c('continental shelf', 'Continental shelf'), ]

all.mod <- c()
for(imod in 1:nrow(shelf.mods)){
  mod.num <- as.numeric(as.character(shelf.mods[imod, model.model_number]))
  h <- basicTextGatherer()
  curlPerform(url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',
                          mod.num), writefunction = h$update, verbose = T)
  data <- xmlTreeParse(h$value(), useInternalNodes = T)
  model1 <- xpathSApply(data, '//group', xmlToList)
  if(length(model1 > 0)){
    ind.mod <- c()
    for(igroup in 1:ncol(model1)){
      sp.mod <- data.table(Group   = model1[['group_name', igroup]],
                           TL      = as.numeric(model1[['tl', igroup]]),
                           Biomass = as.numeric(model1[['biomass', igroup]]),
                           PB      = as.numeric(model1[['pb', igroup]]),
                           QB      = as.numeric(model1[['qb', igroup]]))
      ind.mod <- rbindlist(list(ind.mod, sp.mod))
    }
    ind.mod[, ModNum := mod.num]
    all.mod <- rbindlist(list(all.mod, ind.mod))
  }
}

#Double check PBs
all.mod[TL == 1 & PB > 0, mean(PB)]
all.mod[Group == 'Bacteria', mean(PB)]
all.mod[Group == 'Bacteria', mean(QB)]
all.mod[Group %like% 'Gel', mean(PB)]
all.mod[Group %like% 'Gel', mean(QB)]
all.mod[!Group %like% 'Gel' & Group %like% 'Zoo', mean(PB)]
all.mod[!Group %like% 'Gel' & Group %like% 'Zoo', mean(QB)]
