#Steps to balancing the Georges Bank model
library(data.table); library(here); library(Rpath); library(dplyr); library(ggplot2)

#Load prebal functions
source(here('R', 'PreBal.R'))

#Load data
load(here('data', 'GB.params.rda'))
load(here('data', 'GB.init.rda'))
load(here('data', 'spclass.GB.rda'))
load(here('data', 'BA.input.rda'))

#Initial prebal diagnostics
prebal(GB.init, spclass.GB)

#Biomass Span - 6x
#Biomass Slope -0.6911775
# PREBAL recomends a biomass slope of -0.05 to -0.1

# There is an issue with this metric - PreBal pub uses ranked order to 
# plot not raw TL - I can live with this for now
# should try to reduce slope during balancing

#Several of these are pelagics and many of the prebal metrics suggest that 
#pelagics are too low.  Also know that the survey is remarkably poor at sampling
#pelagics.

#look at worst EEs first
check.ee(GB.init)

#Copy parameter set 
GB.params.adj <- copy(GB.params)
GB.new <- rpath(GB.params, 'Georges Bank')

# Non-Balancing changes to starting parameters -------------


## Bioparams for EMAX groups ---------
# Call in GOM params
load(here::here('data', 'GOM.params.rda'))

# Call in EMAX groups
# Values come from GB specific EMAX ecopath model accessed via ecobase
# QB for SeaBirds informed by Heymans, 2001
EMAX.groups <- data.table(Group = c("Bacteria", "BaleenWhales", "GelZooplankton", "HMS" ,"LgCopepods",
                                    "Micronekton", "Microzooplankton","Odontocetes", "Phytoplankton", "SeaBirds",  "SmCopepods"),
                          PB = c(91.24998, 0.03802086, 40, 0.682623, 54.63586, 
                                 14.25, 72.00002, 0.04, 166.1342, 0.275, 41.66504),
                          QB = c(380.2082, 4.5, 143.08, 2.053014, 109.5,
                                 36.5, 242.4243, 13.82976, NA, 76.2, 127.75)
)

# Groups with P/Q > 0.3 are HMS, LgCopepods, Micronekton, SmCopepods
# For HMS EMAX pedigree suggested higher confidence in PB than QB
# For all other groups estimates are equally uncertain

# Increase QB for HMS
EMAX.groups[Group == 'HMS', QB := 2.3]
# Decrease PB and Increase QB for LgCopepods, Micronekton, SmCopepods
EMAX.groups[Group == 'SmCopepods', PB := PB *0.90]
EMAX.groups[Group == 'SmCopepods', QB := QB *1.1]

EMAX.groups[Group == 'Micronekton', PB := PB *0.85]
EMAX.groups[Group == 'Micronekton', QB := QB *1.15]

EMAX.groups[Group == 'LgCopepods', PB := PB *0.75]
EMAX.groups[Group == 'LgCopepods', QB := QB *1.25]

# Groups with P/Q < 0.1 are BaleenWhales, Odontocetes, and SeaBirds
# all warm blooded so I think it is fine.

# Change PB and QB for these groups in GB.params.adj to EMAX values
for(igroup in 1:nrow(EMAX.groups)) {
  GB.params.adj$model[Group == EMAX.groups$Group[igroup], "PB"] <- EMAX.groups$PB[igroup]
  GB.params.adj$model[Group == EMAX.groups$Group[igroup], "QB"] <- EMAX.groups$QB[igroup]
}

# PB and QB for GB.params.adj should be set to values from GOM.params
# Except for EMAX groups which will keep EMAX values
# match by Group
# pull out PB and QB from GB.params.adj
GB.PB.QB <- GB.params.adj$model |> 
  select(Group, PB, QB) |> 
  setnames(c('Group', 'PB', 'QB'), c('Group', 'GB.PB', 'GB.QB'))
# same for GOM
GOM.PB.QB <- GOM.params$model |>
  select(Group, PB, QB) |>
  setnames(c('Group', 'PB', 'QB'), c('Group', 'GOM.PB', 'GOM.QB'))


# merge
GB.GOM.PB.QB <- merge(GB.PB.QB, GOM.PB.QB, by = 'Group')

# remove GB values and rename to 'Group', 'PB', 'QB'
GB.GOM.PB.QB <- GB.GOM.PB.QB |> 
  select(Group, GOM.PB, GOM.QB) |>
  setnames(c('Group', 'PB', 'QB'))

GB.GOM.groups <- GB.GOM.PB.QB$Group
# Only groups not found in EMAX.groups
GB.GOM.groups <- GB.GOM.groups[!(GB.GOM.groups %in% EMAX.groups$Group)]

# Set PB in GB.params.adj to GB.GOM.PB.QB$PB
for(igroup in GB.GOM.groups) {
  GB.params.adj$model[GB.params.adj$model$Group == igroup, "PB"] <- GB.GOM.PB.QB[GB.GOM.PB.QB$Group == igroup, "PB"]
}
# Same for QB
for(igroup in GB.GOM.groups) {
  GB.params.adj$model[GB.params.adj$model$Group == igroup, "QB"] <- GB.GOM.PB.QB[GB.GOM.PB.QB$Group == igroup, "QB"]
}

# save new starting params for comparison after balancing
# This will inform pedigree values
GB.params <- copy(GB.params.adj)

GB.new <- rpath(GB.params.adj, eco.name = 'Georges Bank')
check.rpath.params(GB.params.adj)
check.ee(GB.new)


#Step 01 - Pelagics and aggregate groups----
#Several of the main offenders are pelagic or aggregate groups
#Set EE to 0.8 for agg groups due to lack of data
#Multiple pelagics (Mack/Herring) by 10

aggEE <- c('OtherPelagics', 'SmPelagics', 'Megabenthos', 'OtherCephalopods', 
           'OtherShrimps', 'Mesopelagics', 'OtherSkates', 'SmFlatfishes')
GB.params.adj$model[Group %in% aggEE, Biomass := NA]
GB.params.adj$model[Group %in% aggEE, EE := 0.8]

GB.params.adj$model[Group %in% c('AtlHerring', 'AtlMackerel'), 
                    Biomass := Biomass * 10]

# Step 02 - Ocean Quahog -------------------------------------
GB.new <- rpath(GB.params.adj, eco.name = 'Georges Bank')
check.mort(GB.new, 'OceanQuahog')

# Set PB and QB to match MAB model values
GB.params.adj$model[Group == 'OceanQuahog', PB := 0.05]
GB.params.adj$model[Group == 'OceanQuahog', QB := 0.3]

# Macrobenthos and Megabenthos. Some AmLobster and OtherDemersals
# Move predation on OceanQuahog to AtlScallop and SurfClam
GB.params.adj$diet[Group == 'OceanQuahog', Macrobenthos := 0.0002] # removed 0.005
GB.params.adj$diet[Group == 'AtlScallop', Macrobenthos := Macrobenthos + 0.0025]
GB.params.adj$diet[Group == 'SurfClam', Macrobenthos := Macrobenthos + 0.0025]

GB.params.adj$diet[Group == 'OceanQuahog', Megabenthos := Megabenthos - 0.028]
GB.params.adj$diet[Group == 'SurfClam', Megabenthos := Megabenthos + 0.028]

GB.params.adj$diet[Group == 'OceanQuahog', AmLobster := AmLobster - 0.02]
GB.params.adj$diet[Group == 'SurfClam', AmLobster := AmLobster + 0.01]
GB.params.adj$diet[Group == 'Detritus', AmLobster := AmLobster + 0.01]

GB.params.adj$diet[Group == 'OceanQuahog', OtherDemersals := OtherDemersals - 0.06]
GB.params.adj$diet[Group == 'OtherDemersals', OtherDemersals := OtherDemersals + 0.06]


#Step 02 - Deal with OceanPout, AmPlaice, and WitchFlounder----
#OceanPout
#Increase Biomass - Not well sampled
GB.params.adj$model[Group == 'OceanPout', Biomass := Biomass * 3]

#Increase production
#Ocean pout live 12-14 years
# pbcalc(12) #0.208
# pbcalc(14) #0.179
# #Split the difference
# GB.params.adj$model[Group == 'OceanPout', PB := 0.193]
# Changed this. Now set to match value in GOM model

#Predation
check.mort(GB.new, 'OceanPout')
#Main predator is spiny dogfish - moving 5% of spiny dogfish diet into import
#due to the migratory nature of the species
GB.params.adj$diet[, SpinyDogfish := SpinyDogfish - (SpinyDogfish * 0.5)]
GB.params.adj$diet[Group == 'Import', SpinyDogfish := 0.5]

#American Plaice
#Increase biomass - flatfish poorly sampled by gear
GB.params.adj$model[Group == 'AmPlaice', Biomass := Biomass * 4]

#Live about 20 years
# pbcalc(20) #0.125
# GB.params.adj$model[Group == 'AmPlaice', PB := 0.125]
# Changed this. Now set to match value in GOM model

#Predation
check.mort(GB.new, 'AmPlaice')
#Main pred - SpinyDogfish
#Large F from LargeMesh as well
#May revisit but not making any changes for now

#Witch Flounder
#Increase biomass - flatfish poorly sampled by gear
GB.params.adj$model[Group == 'WitchFlounder', Biomass := Biomass * 5]

#Live about 20 years
# GB.params.adj$model[Group == 'WitchFlounder', PB := pbcalc(20)]
# Changed this. Now set to match value in GOM model

#Predation
check.mort(GB.new, 'WitchFlounder')
#Main pred - OtherSkates
#Large F from LargeMesh and SmallMesh
#Drop 50% of large and small mesh catch - Shelf break species

mesh.fleet <- c('SM Mesh', 'SM Mesh.disc', 'LG Mesh', 'LG Mesh.disc')

for(imesh in 1:length(mesh.fleet)){
  setnames(GB.params.adj$model, mesh.fleet[imesh], 'fleet')
  GB.params.adj$model[Group == 'WitchFlounder', fleet := fleet - 0.5 * fleet]
  setnames(GB.params.adj$model, 'fleet', mesh.fleet[imesh])
}

#Step 03 - Lobster----
# lscalc(GB.params.adj$model[Group == 'AmLobster', PB]) #1.08
# GB.params.adj$model[Group == 'AmLobster', PB := pbcalc(15)]
# GB.params.adj$model[Group == 'AmLobster', QB := QB / 12]

# Changed this. Now set to match GOM model values

check.mort(GB.new, 'AmLobster') #Macrobenthos, Megabenthos

#Moving DC into Megabenthos
GB.params.adj$diet[Group == 'AmLobster', Macrobenthos := NA]
GB.params.adj$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 0.0018]

GB.params.adj$diet[Group == 'AmLobster', Megabenthos := 0.001] #removed 0.0436
GB.params.adj$diet[Group == 'Megabenthos', Megabenthos := Megabenthos + 0.0437]

GB.params.adj$diet[Group == 'AmLobster', OtherDemersals := 0.01] #removed 0.1218
GB.params.adj$diet[Group == 'Megabenthos',
                   OtherDemersals := OtherDemersals + 0.122]

#Step 04 - Atlantic Mackerel----
check.mort(GB.new, 'AtlMackerel')

#Biomass and production seem ok
#Biggest Predators are spiny dogfish, silver hake, pollock, winter skate, cod
#Cutting DC and moving to import - assume they are eating mackerel
#off the bank
DC.mack <- data.table::melt(GB.params.adj$diet[Group == 'AtlMackerel', ],
                            id.var = 'Group')
DC.mack <- DC.mack[!is.na(value), ]
DC.mack[, new.value := value / 10]

mack.pred <- as.character(DC.mack[, variable])

for(ipred in 1:length(mack.pred)){
  new.value <- DC.mack[variable == mack.pred[ipred], new.value]
  setnames(GB.params.adj$diet, mack.pred[ipred], 'pred')
  GB.params.adj$diet[Group == 'AtlMackerel', pred := new.value]
  GB.params.adj$diet[Group == 'Import', pred := sum(c(pred, 9 * new.value), na.rm = T)]
  setnames(GB.params.adj$diet, 'pred', mack.pred[ipred])
}

#Cut Spinydogfish and winterskate biomasses
GB.params.adj$model[Group %in% c('SpinyDogfish', 'WinterSkate'),
                    Biomass := Biomass / 3]

#Also fixed PB and QB for these predators later

#Reduce catch in half
# for(ifleet in 1:length(fleets)){
#   setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
#   GB.params.adj$model[Group == 'AtlMackerel', fleet := fleet / 2]
#   setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
# }

# Trying not to change landings too much. Might revisit

#Step 05 - Reduce F on Southern Dems, Redfish, and Barndoors----
fleets <- GB.params.adj$model[Type == 3, Group]
#Southern Dems
check.mort(GB.new, 'SouthernDemersals')

#Reduce catch by an order of magnitude
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'SouthernDemersals', fleet := fleet / 10]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

#increase biomass
GB.params.adj$model[Group == 'SouthernDemersals', Biomass := Biomass * 2]

#Redfish - more of a Gulf of Maine species
check.mort(GB.new, 'Redfish')

#Reduce catch in half
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'Redfish', fleet := fleet / 2]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

#Barndoors
#Reduce catch in half
# for(ifleet in 1:length(fleets)){
#   setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
#   GB.params.adj$model[Group == 'Barndoor', fleet := fleet / 2]
#   setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
# }


# Step 06 - Butterfish -----------------
check.mort(GB.new, 'Butterfish') #LowerTroph/CharsmaticMF

#Look at GelZoop and OtherPel
#GelZoop highly productive with high QB
# GB.params.adj$model[Group == 'GelZooplankton', PB := PB / 2]
# GB.params.adj$model[Group == 'GelZooplankton', QB := 100]
# Now set to match GOM model values

#Still very high mort - reducing DC and moving to Micronekton
GB.params.adj$diet[Group == 'Butterfish', GelZooplankton := 0.001]
GB.params.adj$diet[Group == 'Micronekton', GelZooplankton := 0.0137]

#Fix other pelagics
# GB.params.adj$model[Group == 'OtherPelagics', PB := PB / 2]
# GB.params.adj$model[Group == 'OtherPelagics', QB := QB / 3]
# Now set to match GOM model values

# EE still at 3.04. Bumping B

GB.params.adj$model[Group == 'Butterfish', Biomass := Biomass * 3.5]

# Step 07 - Windowpane -----------------
check.mort(GB.new, 'Windowpane')
# Low amounts of mortality from a wide variety of species and fleets
# Bumping B
GB.params.adj$model[Group == 'Windowpane', Biomass := Biomass * 3.5]

# Still above 1. Macrobenthos main predator and causes issues with other groups.
# Droping macrobenthos biomass
GB.params.adj$model[Group == 'Macrobenthos', Biomass := Biomass / 3]


#Step 08 - OtherDemersals----
#DC looks too much like piscivores
#Changed DC to look like SouthernDemersals which are more omnivorous
check.mort(GB.new, 'OtherDemersals')

#Cod, Winterskate, LittleSkate, SpinyDogfish are the major culprits -- again

#Top down - 0.9
GB.params.adj$model[Group == 'OtherDemersals', Biomass := NA]
GB.params.adj$model[Group == 'OtherDemersals', EE := 0.9]

# Step 09 - Redfish -----------------
check.mort(GB.new, 'Redfish') #Fisheries
#Reduce catch by 30%
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'Redfish', fleet := fleet - fleet * 0.3]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}
#Still off so bumping biomass
GB.params.adj$model[Group == 'Redfish', Biomass := Biomass * 3.5]

# Step 10 - AtlHerring -----------------
check.mort(GB.new, 'AtlHerring')
# Top predators SilverHake, SpinyDogfish, and Pollock

# #Spinydogfish
# lscalc(GB.params.adj$model[Group == 'SpinyDogfish', PB]) #7.8
# GB.params.adj$model[Group == 'SpinyDogfish', PB := pbcalc(31)]
# 
# #New production pushes the PQ ratio very low ... need to decrease Q
# GB.params.adj$model[Group == 'SpinyDogfish', QB := QB / 4]
# 
# #Check Pollock
# lscalc(GB.params.adj$model[Group == 'Pollock', PB]) #62.5!
# GB.params.adj$model[Group == 'Pollock', PB := pbcalc(25)]
# #New production pushes the PQ ratio very low ... need to decrease Q
# GB.params.adj$model[Group == 'Pollock', QB := QB / 5]

# PBs and QBs now set to match GOM model values

# Herring EE still at 3.37. Bumping B
GB.params.adj$model[Group == 'AtlHerring', Biomass := Biomass * 4]


# Step 11 - Illex -----------------
check.mort(GB.new, 'Illex')
# Spinydogfish, Loligo top preds. Bumping B
GB.params.adj$model[Group == 'Illex', Biomass := Biomass * 2.5]

#Step 12 - BlackSeaBass -------------------
#Black Sea Bass
# lscalc(GB.params.adj$model[Group == 'BlackSeaBass', PB]) #75
# GB.params.adj$model[Group == 'BlackSeaBass', PB := pbcalc(20)]
# Changed this. Now set to match GOM model value

check.mort(GB.new, 'BlackSeaBass') #Fleets
#Note - nothing is eating black sea bass in this model - will need to fix this


#Reduce catch by 30%
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'BlackSeaBass', fleet := fleet - fleet * 0.3]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

# Adding 0.2% predation from Sharks, Odontocetes, LittleSkate, OtherPelagics, Barndoor, OtherSkates, WinterSkate


GB.params.adj$diet[Group == 'BlackSeaBass', Sharks := 0.002]
GB.params.adj$diet[Group == 'BlackSeaBass', Odontocetes := 0.002]

GB.params.adj$diet[Group == 'BlackSeaBass', OtherPelagics := 0.002]
GB.params.adj$diet[Group == 'BlackSeaBass', Barndoor := 0.002]
GB.params.adj$diet[Group == 'BlackSeaBass', OtherSkates := 0.002]


# find somewhere to take that from in each diet
GB.params.adj$diet[Group == 'Haddock', Sharks := Sharks - 0.002]
GB.params.adj$diet[Group == 'Pollock', Odontocetes := Odontocetes - 0.002]

GB.params.adj$diet[Group == 'RedHake', OtherPelagics := OtherPelagics - 0.002]
GB.params.adj$diet[Group == 'OtherDemersals', Barndoor := Barndoor - 0.002]
GB.params.adj$diet[Group == 'OtherDemersals', OtherSkates := OtherSkates - 0.002]


check.mort(GB.new, 'BlackSeaBass')

#Bump biomass
GB.params.adj$model[Group == 'BlackSeaBass', Biomass := Biomass * 4.5]

# Step 13 - SilverHake ------------------
check.mort(GB.new, 'SilverHake')
# increase B
GB.params.adj$model[Group == 'SilverHake', Biomass := Biomass * 2]


# Step 13 - AtlMackerel again ------------------
check.mort(GB.new, 'AtlMackerel')
# already adjusted diets in step 04
# Bumping B
GB.params.adj$model[Group == 'AtlMackerel', Biomass := Biomass * 2]


# Step 17 - RedHake -----------------
check.mort(GB.new, 'RedHake')
# Wide variety of mortality with low values. Bumping B
GB.params.adj$model[Group == 'RedHake', Biomass := Biomass * 2]

# Step 18 - EE < 1.5 -----------------

# OceanQuahog, Haddock, Microzooplankton, LgCopepods, SmCopepods, Loligo
# AmPlaice, Scup, AtlHerring, Detritus, WinterFlounder, YTFlounder

# OceanQuahog
GB.params.adj$model[Group == 'OceanQuahog', PB := 0.02]

# Haddock
check.mort(GB.new, 'Haddock')
# increase B
GB.params.adj$model[Group == 'Haddock', Biomass := Biomass * 1.5]

check.mort(GB.new, 'Microzooplankton')
check.mort(GB.new, 'SmCopepods')
check.mort(GB.new, 'LgCopepods')

#GelZooplankton major predator of Copepods and Detritus, reducing B
GB.params.adj$model[Group == 'GelZooplankton', Biomass := Biomass * 0.5]

check.mort(GB.new, 'Microzooplankton')
# Increasing B. Pedigree from EMAX suggests this is the parameter with least cofidence
GB.params.adj$model[Group == 'Microzooplankton', Biomass := Biomass * 2]

# Top down for Bacteria and Copepods
GB.params.adj$model[Group == 'Bacteria', Biomass := NA]
GB.params.adj$model[Group == 'Bacteria', EE := 0.95]
GB.params.adj$model[Group == 'SmCopepods', Biomass := NA]
GB.params.adj$model[Group == 'SmCopepods', EE := 0.95]
GB.params.adj$model[Group == 'LgCopepods', Biomass := NA]
GB.params.adj$model[Group == 'LgCopepods', EE := 0.95]

#AmPlaice
check.mort(GB.new, 'AmPlaice')
# Mostly fisheries. Don't want to change. Increase B
GB.params.adj$model[Group == 'AmPlaice', Biomass := Biomass * 1.5]

check.mort(GB.new, 'Scup')
# increase B
GB.params.adj$model[Group == 'Scup', Biomass := Biomass * 1.5]

# Detritus
#Increase unassim to 0.4 for zooplankton
GB.params.adj$model[Group %in% c('Microzooplankton', 'LgCopepods','SmCopepods'),
                    Unassim := 0.4]

#Increase unassim to 0.3 for other detritavores
GB.params.adj$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos',
                                 'AtlScallop', 'OtherShrimps'),
                    Unassim := 0.3]

# Loligo
check.mort(GB.new, 'Loligo')
# Cannibalism main issue. Moving 3% of diet from loligo to krill
GB.params.adj$diet[Group == 'Krill', Loligo := Loligo + 0.06]
GB.params.adj$diet[Group == 'Loligo', Loligo := Loligo - 0.06]

#AtlHerring
check.mort(GB.new, 'AtlHerring')
# Bumping B
GB.params.adj$model[Group == 'AtlHerring', Biomass := Biomass * 1.3]

#WinterFlounder
check.mort(GB.new, 'WinterFlounder')
# Bumping B
GB.params.adj$model[Group == 'WinterFlounder', Biomass := Biomass * 1.1]

#YTFlounder
check.mort(GB.new, 'YTFlounder')
# Bumping B
GB.params.adj$model[Group == 'YTFlounder', Biomass := Biomass * 1.1]

#Step 19- BA input --------------------
#Input BA values from BA.input
load(here('data', 'BA.input.rda'))

BA.Group <- c('AmLobster','Cod','Goosefish','RedHake','YTFlounder')
#Assign values to GB.params.adj$model$BioAcc from BA.input for groups in BA.Group
for(igroup in 1:length(BA.Group)){
  group.name <- BA.Group[igroup]
  GB.params.adj$model[Group == group.name, BioAcc := BA.input[RPATH == group.name, BA]]
}

## Cod BA change -------------------------
# Manually adjust Cod BA to help with Rsim fits
# Original value = -0.052171320

GB.params.adj$model[Group == 'Cod', BioAcc := -0.34]


#Step 20 - Shark Diet -----------------
# Shifting Shark Diet away from detritus to match GOM and MAB models

GB.params.adj$diet[Group == 'Detritus', Sharks := 0.001] #removed 0.05
GB.params.adj$diet[Group == 'Pinnipeds', Sharks := 0.025]
GB.params.adj$diet[Group == 'Odontocetes', Sharks := Sharks + 0.025]


# Check EEs and params -------------------------------------
GB.new <- rpath(GB.params.adj, eco.name = 'Georges Bank')
check.rpath.params(GB.params.adj)
check.ee(GB.new)

#save file plots
#current <- 'Bacteria'
slopePlot(GB.new)#, group = current)
ggplot2::ggsave(here('outputs', 'GB_biomass_slope.png'), width = 10, height = 4)
slopePlot(GB.new, type = 'PB')#, group = current)
ggplot2::ggsave(here('outputs', 'GB_PB_slope.png'), width = 10, height = 4)
slopePlot(GB.new, type = 'QB')#, group = current)
ggplot2::ggsave(here('outputs', 'GB_QB_slope.png'), width = 10, height = 4)
slopePlot(GB.new, type = 'Lifespan')#, group = current)
ggplot2::ggsave(here('outputs', 'GB_lifespan_slope.png'), width = 10, height = 4)

GB.prebal <- prebal(GB.new, spclass.GB)
#Turn into an object to plot
series <- c('Predator Prey Ratio', 'Energy Flows - Fish', 'Energy Flows - Invert',
            'Energy Flows - Diet')
prebal.toplot <- c()
for(i in 5:8){
  ratios <- GB.prebal[[i]]
  ratios[, Series := series[i - 4]]
  prebal.toplot <- rbindlist(list(prebal.toplot, ratios))
}

prebal.plot <- ggplot(data = prebal.toplot,
                      aes(x= Ratio, y = Value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Series, scales = 'free')

plot(prebal.plot)
ggplot2::ggsave(here('outputs', 'GB_prebal.png'), width = 10, height = 4)

# Comparison of pre and post balancing landings

library(tidyverse)
unbalanced.landings <- GB.params$model |> 
                        select(Group, fleets) |>
                        pivot_longer(cols = -Group, names_to = "fleet", values_to = "landings") |>
                       group_by(Group) |>
                       mutate(unbalanced_landings = sum(landings)) |> 
                      select(Group, unbalanced_landings) |>
                      unique()

balanced.landings <- GB.params.adj$model |> 
                        select(Group, fleets) |>
                        pivot_longer(cols = -Group, names_to = "fleet", values_to = "landings") |>
                       group_by(Group) |>
                       mutate(balanced_landings = sum(landings)) |> 
                      select(Group, balanced_landings) |>
                      unique()

landings.comparison <- merge(unbalanced.landings, balanced.landings, by = 'Group', all = T)
# Filter for cases where unbalanced and balanced landings are different
landings.comparison <- landings.comparison |> 
                        filter(unbalanced_landings != balanced_landings)
# Convert to mt by multiplying by area in km2
landings.comparison <- landings.comparison |> 
                        mutate(unbalanced_landings_mt = unbalanced_landings * 57307.7) |>
                        mutate(balanced_landings_mt = balanced_landings * 57307.7) |> 
                        mutate(landings_change_mt = balanced_landings_mt - unbalanced_landings_mt)

usethis::use_data(landings.comparison, overwrite = T)

# Save balanced params and model
GB.params.bal <- copy(GB.params.adj)
GB.bal <- copy(GB.new)

usethis::use_data(GB.params.bal, overwrite = T)
usethis::use_data(GB.bal, overwrite = T)

