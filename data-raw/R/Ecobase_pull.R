#Code from Ecobase.ecopath.org to access Ecobase
#To get the list of available Ewe models
library(RCurl); library(XML); library(plyr); library(tidyr); library(data.table); library(janitor)

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

all.mod[Group %like% 'hake',]

all.mod[TL > 1 & TL <=2, mean(PB)]
all.mod[TL > 2 & TL <=3, mean(PB)]
all.mod[TL > 3 & TL <=4, mean(PB)]
all.mod[TL > 4, mean(PB)]

all.mod[TL > 1 & TL <=2, mean(QB)]
all.mod[TL > 2 & TL <=3, mean(QB)]
all.mod[TL > 3 & TL <=4, mean(QB)]
all.mod[TL > 4, mean(QB)]


## Grab GB EMAX values for small and large copepods to aide in splitting from mesozooplankton

# pull starting values for Georges Bank EMAX model. model.model_number == 705

h=basicTextGatherer()
mymodel<-705
curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)


data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

input1<-xpathSApply(data,'//group',function(x) xmlToList(x)) 

# make each row of input1 its own column in a new data.table
input1 <- as.data.table(input1, keep.rownames = T)
input2 <- as.data.table(t(input1))

# make row 1 the column names
input2 <- row_to_names(input2, 1)

# select only group_name Large Copepods and Small copepods
EMAX_copepod_params <- input2[group_name %in% c('Large Copepods', 'Small copepods'),]
# add column for RPATH values 'LgCopepods' and 'SmCopepods'
EMAX_copepod_params[, RPATH := c('LgCopepods', 'SmCopepods')]

# save to data folder
usethis::use_data(EMAX_copepod_params, overwrite = T)
