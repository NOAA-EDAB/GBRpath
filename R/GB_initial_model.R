#Georges Bank Rpath model
#Expanded model of Georges Bank
#SML
# Updated by MTG 03/01/2024

#Required packages--------------------------------------------------------------
library(data.table); library(Rpath); library(here); library(readr); library(dplyr)

#Georges Bank
groups <- c('Seabirds', 'Seals', 'BalWhale', 'ToothWhale', 'HMS', 'Sharks', 
            'AtlHerring', 'AtlMackerel', 'RiverHerring', 'Butterfish', 
            'SmPelagics', 'Mesopelagics', 'OtherPelagics', 'Cod', 'Haddock', 
            'Goosefish', 'OffHake', 'SilverHake', 'RedHake', 'WhiteHake', 
            'Redfish', 'Pollock', 'OceanPout', 'BlackSeaBass', 'Bluefish', 'Scup',
            'OtherDemersals', 'SouthernDemersals', 'Fourspot', 'SummerFlounder',
            'AmPlaice', 'Windowpane', 'WinterFlounder', 'WitchFlounder', 
            'YTFlounder', 'OtherFlatfish', 'SmFlatfishes', 'SpinyDogfish', 
            'SmoothDogfish', 'Barndoor', 'WinterSkate', 'LittleSkate', 
            'OtherSkates', 'Illex', 'Loligo', 'OtherCephalopods', 'AmLobster',
            'Macrobenthos', 'Megabenthos', 'AtlScallop', 'OceanQuahog', 'SurfClam',
            'OtherShrimps', 'Krill', 'Micronekton', 'GelZooplankton', 
            'Microzooplankton', 'Bacteria', 'LgCopepods', 'SmCopepods', 'Phytoplankton',
            'Detritus', 'Discards', 
            'ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
            'Trap', 'SmallMesh', 'LargeMesh', 'HMSFleet') #, 'OtherFisheries')

types <- c(rep(0, 60), 1, 2, 2, rep(3, 9))

GB.params <- create.rpath.params(groups, types)

#Enter BioAcc, Unassim, DetInput, and detrital fate
GB.params$model[Type < 3,  BioAcc := 0] 
#There are some BA terms in BA.input.rda if needed to balance
GB.params$model[Type == 0, Unassim := 0.2]
GB.params$model[Type > 0 & Type < 3, Unassim := 0]
GB.params$model[Type == 2, DetInput := 0]
GB.params$model[Type < 2,  Detritus := 1]
GB.params$model[Type > 1,  Detritus := 0]
GB.params$model[Type == 3, Discards := 1]
GB.params$model[Type < 3,  Discards := 0]

#Load biomass
load(here('data', 'bio.input.rda'))

for(igroup in GB.params$model[Type < 2, Group]){
  GB.params$model[Group == igroup, Biomass := bio.input[RPATH == igroup, B]]
}

# update values for Large and Small Copepods from EMAX_copepod_params
load(here('data', 'EMAX_copepod_params.rda'))
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), Biomass := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(biomass)]]

#Add EEs for groups without biomass
#Other flatfish biomass shows up as zero so set to NA and let the model calculate
GB.params$model[Group == 'OtherFlatfish', Biomass := NA]
GB.params$model[Group == 'OtherFlatfish', EE := 0.8]
#Originally had Seals, PP and Bacteria set to EE but I added EMAX value
#Add NWACS value for pinnepeds
GB.params$model[Group %in% c('Seals'), Biomass := 0.035]

#Biological Parameters
load(here('data', 'bioparam.input.rda'))

for(igroup in bioparam.input[, RPATH]){
  GB.params$model[Group == igroup, PB := bioparam.input[RPATH == igroup, PB]]
  GB.params$model[Group == igroup, QB := bioparam.input[RPATH == igroup, QB]]
}

# update values for Large and Small Copepods from EMAX_copepod_params
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), PB := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(pb)]]
GB.params$model[Group %in% c('LgCopepods', 'SmCopepods'), QB := EMAX_copepod_params[RPATH %in% c('LgCopepods', 'SmCopepods'), as.numeric(qb)]]


#Phytoplankton needs to be NA
GB.params$model[Group == 'Phytoplankton', QB := NA]


#Load landings
load(here('data', 'land.input.rda'))

#Need to account for mismatched names
# No catch from "OtherFisheries" so not included
# commented out rather than deleted in case it is needed later
rfleets <- c('ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
             'Trap', 'SmallMesh', 'LargeMesh' , 'HMSFleet') #, 'OtherFisheries') 
rgear   <- c('Scallop Dredge', 'Clam Dredge', 'Other Dredge', 'Fixed Gear', 
             'Pelagic', 'Trap', 'SM Mesh','LG Mesh', 'HMS') #, 'Other') 

for(igear in 1:length(rfleets)){
  gear.landings <- land.input[Fleet == rgear[igear], ]
  setnames(GB.params$model, rfleets[igear], 'gear')
  for(igroup in 1:nrow(gear.landings)){
    GB.params$model[Group == gear.landings[igroup, RPATH], 
                    gear := gear.landings[igroup, Landings]][]
  }
  setnames(GB.params$model, 'gear', rfleets[igear])
}

#Load discards
load(here('data', 'disc.input.rda'))

for(igear in 1:length(rfleets)){
  gear.disc <- disc.input[Fleet == rgear[igear], ]
  setnames(GB.params$model, paste0(rfleets[igear], '.disc'), 'gear')
  #Discards
  for(igroup in 1:nrow(gear.disc)){
    GB.params$model[Group == gear.disc[igroup, RPATH], 
                    gear := gear.disc[igroup, Discards]][]
  }
  setnames(GB.params$model, 'gear', paste0(rfleets[igear], '.disc'))
}

#Load Diet
load(here('data', 'diet.input.rda'))
#Load EMAX diet matrix
EMAX_diet <- read_csv("data/Georges Bank-Diet composition.csv")
# select just the diet of SmCopepods and LgCopepods
copes_diet <- EMAX_diet[, c(2, 5, 6)]
# give column headers of SmCopepods and LgCopepods
setnames(copes_diet, c("4","5"),c("SmCopepods", "LgCopepods"))
# remove 0 values
copes_diet <- copes_diet[copes_diet$SmCopepods != 0 | copes_diet$LgCopepods != 0,]
#remove last two rows
copes_diet <- copes_diet[1:(nrow(copes_diet)-2),]
# get in the format of diet.input
#SmCopepods
# select first two columns
smcopes_diet <- copes_diet[, c(1, 2)]
# rename columns
setnames(smcopes_diet, c("Prey \\ predator", 'SmCopepods'), c("Rprey", "preyper"))
# create new column 'Rpred' and set to 'SmCopepods'
smcopes_diet <- data.table(smcopes_diet, Rpred = 'SmCopepods')
#LgCopepods
# select first and third columns
lgcopes_diet <- copes_diet[, c(1, 3)]
# rename columns
setnames(lgcopes_diet, c("Prey \\ predator", 'LgCopepods'), c("Rprey", "preyper"))
# create new column 'Rpred' and set to 'LgCopepods'
lgcopes_diet <- data.table(lgcopes_diet, Rpred = 'LgCopepods')

# combine smcopes_diet and lgcopes_diet
new_copes_diet <- rbind(smcopes_diet, lgcopes_diet)
# change Rprey to Rpath names
new_copes_diet[Rprey == 'Phytoplankton- Primary Producers', Rprey := 'Phytoplankton']
new_copes_diet[Rprey == 'Gelatinous Zooplankton', Rprey := 'GelZooplankton']
new_copes_diet[Rprey == 'Macrobenthos- crustaceans', Rprey := 'Macrobenthos']
new_copes_diet[Rprey == 'Macrobenthos- other', Rprey := 'Macrobenthos']
new_copes_diet[Rprey == 'Detritus-POC', Rprey := 'Detritus']
new_copes_diet[Rprey == 'Small copepods', Rprey := 'SmCopepods']
new_copes_diet[Rprey == 'Large Copepods', Rprey := 'LgCopepods']
# sum the Macrobenthos Rprey by Rpred
new_copes_diet <- new_copes_diet[, .(preyper = sum(preyper)), by = .(Rprey, Rpred)]

#combine diet.input and new_copes_diet
diet.input <- rbind(diet.input, new_copes_diet)

# Proportion predation on Messozooplankton by Small and Large Copepods by biomass
copes.b <- sum(bio.input[RPATH %in% c('SmCopepods', 'LgCopepods'), B])
s.copes.ratio <- bio.input[RPATH == 'SmCopepods', B] / copes.b
l.copes.ratio <- bio.input[RPATH == 'LgCopepods', B] / copes.b


# create new Rprey = 'SmCopepods' with preyper set to preyper for Mesozooplankton * s.copes.ratio
sm.copes.prey <- diet.input |> 
                  filter(Rprey == 'Mesozooplankton') |>
                  mutate(Rprey := 'SmCopepods') |>
                  mutate(preyper := preyper * s.copes.ratio)

# LgCopepods
lg.copes.prey <- diet.input |> 
                  filter(Rprey == 'Mesozooplankton') |>
                  mutate(Rprey := 'LgCopepods') |>
                  mutate(preyper := preyper * l.copes.ratio)


# SmPelagics not feeding on LgCopepods or SmCopepods but are in
# the GOM model. Proportioning the missing diet to these groups
# similar to their proportion in the GOM model
SmPelagics_diet <- diet.input |> 
                  filter(Rpred == 'SmPelagics')


remaining_diet <- 1 - sum(SmPelagics_diet$preyper)

large <- remaining_diet * 0.7
small <- remaining_diet * 0.3

# create new row in sm.copes.prey
# rpred is SmPelagics and rprey is SmCopepods
# value is small
sm.copes.prey <- rbind(sm.copes.prey, 
                       data.table(Rpred = 'SmPelagics', Rprey = 'SmCopepods', preyper = small))

# create new row in lg.copes.prey
# rpred is SmPelagics and rprey is LgCopepods
# value is large
lg.copes.prey <- rbind(lg.copes.prey, 
                       data.table(Rpred = 'SmPelagics', Rprey = 'LgCopepods', preyper = large))


# GelZooplankton not feeding on LgCopepods or SmCopepods but are in
# the GOM model. Proportioning the missing diet to these groups
# similar to their proportion in the GOM model
GelZooplankton_diet <- diet.input |> 
                  filter(Rpred == 'GelZooplankton')

remaining_diet <- 1 - sum(GelZooplankton_diet$preyper)

large <- remaining_diet * 0.4
small <- remaining_diet * 0.6

# create new row in sm.copes.prey
# rpred is GelZooplankton and rprey is SmCopepods
# value is small
sm.copes.prey <- rbind(sm.copes.prey, 
                       data.table(Rpred = 'GelZooplankton', Rprey = 'SmCopepods', preyper = small))

# create new row in lg.copes.prey
# rpred is GelZooplankton and rprey is LgCopepods
# value is large
lg.copes.prey <- rbind(lg.copes.prey, 
                       data.table(Rpred = 'GelZooplankton', Rprey = 'LgCopepods', preyper = large))


# combine sm.copes.prey and lg.copes.prey with diet.input
diet.input <- rbind(diet.input, sm.copes.prey, lg.copes.prey)
# remove rows with Mesozooplankton in Rprey or Rpred
diet.input <- diet.input[!(Rprey == 'Mesozooplankton' | Rpred == 'Mesozooplankton')]

# krill and micronekton should have micronekton diet from EMAX diet
# remove entries from diet.input where Rpred is Micronekton or krill
diet.input <- diet.input[!(Rpred == 'Micronekton' | Rpred == 'Krill')]
# create new krill entries with Rprey = c('Phytoplankton' 'SmCopepods',
# 'LgCopepods', 'Micronekton', 'Detritus')
krill_diet <- data.table(Rpred = 'Krill', 
                         Rprey = c('Phytoplankton', 'SmCopepods', 'LgCopepods', 'Micronekton', 'Detritus'),
                         preyper = c(0.162, 0.308, 0.326, 0.041, 0.163))
# create new micronekton entries with Rprey = c('Phytoplankton' 'SmCopepods',
# 'LgCopepods', 'Micronekton', 'Detritus')
micronekton_diet <- data.table(Rpred = 'Micronekton', 
                         Rprey = c('Phytoplankton', 'SmCopepods', 'LgCopepods', 'Micronekton', 'Detritus'),
                         preyper = c(0.162, 0.308, 0.326, 0.041, 0.163))
# combine krill_diet and micronekton_diet with diet.input
diet.input <- rbind(diet.input, krill_diet, micronekton_diet)

# filter for the trouble groups
# Seabirds, Balwhale, Sharks, Illex, Loligo, Other Cephalopods, Macrobenthos
not_one <- diet.input |> 
                filter(Rpred %in% c('Seabirds', 'BalWhale', 'Sharks', 'Illex', 
                                    'Loligo', 'OtherCephalopods', 'Macrobenthos'))

# The remaining issues are very close to one
# increasing the value of preyper equal to the current proportion of the diet
not_one <- not_one |> 
            group_by(Rpred) |>
            mutate(proportion = preyper / sum(preyper))

# removing the preyper column and replacing it with the proportion
not_one <- not_one |> 
            select(-preyper) |>
            rename(preyper = proportion)

# replace values in diet.input with the new values
# from not_one
diet.input <- diet.input |> 
                filter(!(Rpred %in% c('Seabirds', 'BalWhale', 'Sharks', 'Illex', 
                                    'Loligo', 'OtherCephalopods', 'Macrobenthos'))) |>
                rbind(not_one)


preds <- unique(diet.input[, Rpred])
for(ipred in 1:length(preds)){
  setnames(GB.params$diet, preds[ipred], 'Pred')
  pred.diet <- diet.input[Rpred == preds[ipred], ]
  prey <- pred.diet[, Rprey]
  for(iprey in 1:length(prey)){
    GB.params$diet[Group == prey[iprey], 
                   Pred := pred.diet[Rprey == prey[iprey], preyper]]
  }
  setnames(GB.params$diet, 'Pred', preds[ipred])
  GB.params$diet[]
}

check.rpath.params(GB.params)


usethis::use_data(GB.params, overwrite = T)

#Initial unbalanced model

GB.init <- rpath(GB.params, 'Georges Bank', 1)

usethis::use_data(GB.init, overwrite = T)

