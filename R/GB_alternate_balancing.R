# Alternate balancing script for GBrpath
# This script uses the same steps to balance the model but 
# leaves out the step multiplying all biomasses by 4
# This will allow for more direct comparison with the GOMrpath and MABrpath models

#Steps to balancing the Georges Bank model
library(data.table); library(here); library(Rpath)

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
#Biomass Slope -0.7946378
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

#Step 1 - Pelagics and aggregate groups----
#Several of the main offenders are pelagic or aggregate groups
#Set EE to 0.8 for agg groups due to lack of data
#Multiple pelagics (Mack/Herring) by 10

aggEE <- c('OtherPelagics', 'SmPelagics', 'Megabenthos', 'OtherCephalopods',
           'Mesopelagics', 'OtherSkates')
GB.params.adj$model[Group %in% aggEE, Biomass := NA]
GB.params.adj$model[Group %in% aggEE, EE := 0.8]

GB.params.adj$model[Group %in% c('AtlHerring', 'AtlMackerel'), 
                    Biomass := Biomass * 10]




#Step 2 - Deal with OceanPout, AmPlaice, and WitchFlounder----
#OceanPout
#Increase Biomass - Not well sampled
GB.params.adj$model[Group == 'OceanPout', Biomass := Biomass * 3]

#Increase production
#Ocean pout live 12-14 years
pbcalc(12) #0.208
pbcalc(14) #0.179
#Split the difference
GB.params.adj$model[Group == 'OceanPout', PB := 0.193]

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
pbcalc(20) #0.125
GB.params.adj$model[Group == 'AmPlaice', PB := 0.125]

#Predation
check.mort(GB.new, 'AmPlaice')
#Main pred - SpinyDogfish
#Large F from LargeMesh as well
#May revisit but not making any changes for now

#Witch Flounder
#Increase biomass - flatfish poorly sampled by gear
GB.params.adj$model[Group == 'WitchFlounder', Biomass := Biomass * 4]

#Live about 20 years
GB.params.adj$model[Group == 'WitchFlounder', PB := pbcalc(20)]

#Predation
check.mort(GB.new, 'WitchFlounder')
#Main pred - Macrobenthos
#Large F from LargeMesh and SmallMesh
#Droping macrobenthos biomass
GB.params.adj$model[Group == 'Macrobenthos', Biomass := Biomass / 3]
#Drop 30% of large and small mesh catch - Shelf break species
mesh.fleet <- c('SmallMesh', 'SmallMesh.disc', 'LargeMesh', 'LargeMesh.disc')
for(imesh in 1:length(mesh.fleet)){
  setnames(GB.params.adj$model, mesh.fleet[imesh], 'fleet')
  GB.params.adj$model[Group == 'WitchFlounder', fleet := fleet - 0.3 * fleet]
  setnames(GB.params.adj$model, 'fleet', mesh.fleet[imesh])
}


#Step 3 - Reduce F on Southern Dems, Redfish, and Barndoors----
fleets <- GB.params.adj$model[Type == 3, Group]
fleets <- c(fleets, paste0(fleets, '.disc'))

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
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'Barndoor', fleet := fleet / 2]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}


#Step 4 - Atlantic Mackerel----
check.mort(GB.new, 'AtlMackerel')

#Biomass and production seem ok
#Biggest Predators are spiny dogfish, silver hake, winter skate, cod
#Cutting DC in half and moving to import - assume they are eating mackerel
#off the bank
DC.mack <- data.table::melt(GB.params.adj$diet[Group == 'AtlMackerel', ],
                            id.var = 'Group')
DC.mack <- DC.mack[!is.na(value), ]
DC.mack[, new.value := value / 3]

mack.pred <- as.character(DC.mack[, variable])

for(ipred in 1:length(mack.pred)){
  new.value <- DC.mack[variable == mack.pred[ipred], new.value]
  setnames(GB.params.adj$diet, mack.pred[ipred], 'pred')
  GB.params.adj$diet[Group == 'AtlMackerel', pred := new.value]
  GB.params.adj$diet[Group == 'Import', pred := sum(c(pred, 2 * new.value), na.rm = T)]
  setnames(GB.params.adj$diet, 'pred', mack.pred[ipred])
}

#Cut Spinydogfish and winterskate biomasses in half
GB.params.adj$model[Group %in% c('SpinyDogfish', 'WinterSkate'),
                    Biomass := Biomass / 2]

#Also fixed PB and QB for these predators in Step 8

#Reduce catch in half
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'AtlMackerel', fleet := fleet / 2]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

check.mort(GB.new, 'AtlHerring')
#Top predator now pollock
#Check Pollack
lscalc(GB.params.adj$model[Group == 'Pollock', PB]) #62.5!
GB.params.adj$model[Group == 'Pollock', PB := pbcalc(25)]
#New production pushes the PQ ratio very low ... need to decrease Q
GB.params.adj$model[Group == 'Pollock', QB := QB / 5]


#Step 5 - More EE balance----
aggEE <- c('OtherShrimps', 'SmFlatfishes')
GB.params.adj$model[Group %in% aggEE, Biomass := NA]
GB.params.adj$model[Group %in% aggEE, EE := 0.8]

#Step 6 - Silver Hake----
check.mort(GB.new, 'SilverHake')

#Cannibalism is the biggest issue
#PQ ratio is 0.02 should be between 0.1 and 0.3
#Decrease QB
GB.params.adj$model[Group == 'SilverHake', QB := QB / 2]
lscalc(GB.params.adj$model[Group == 'SilverHake', PB]) #30
#Max age ~ 12
pbcalc(12)
GB.params.adj$model[Group == 'SilverHake', PB := pbcalc(12)]

#Some room to decrease QB to closer to 0.3
GB.params.adj$model[Group == 'SilverHake', QB := QB / 2]

#Step 7 - OtherDemersals----
#DC looks too much like piscivores
#Changed DC to look like SouthernDemersals which are more omnivorous
check.mort(GB.new, 'OtherDemersals')

#Dogfish, Skates, and Cod are the major culprits -- again
#Step 8 - Dealing with SpinyDogfish, WinterSkates, and Cod----
#longevity estimates taken from FishBase
#Cod - too productive
lscalc(GB.params.adj$model[Group == 'Cod', PB]) #2.86!
GB.params.adj$model[Group == 'Cod', PB := pbcalc(25)]

#New production pushes the PQ ratio very low ... need to decrease Q
GB.params.adj$model[Group == 'Cod', QB := QB / 3]

#WinterSkate
lscalc(GB.params.adj$model[Group == 'WinterSkate', PB]) #52
GB.params.adj$model[Group == 'WinterSkate', PB := pbcalc(21)]

#Spinydogfish
lscalc(GB.params.adj$model[Group == 'SpinyDogfish', PB]) #7.8
GB.params.adj$model[Group == 'SpinyDogfish', PB := pbcalc(31)]

#New production pushes the PQ ratio very low ... need to decrease Q
GB.params.adj$model[Group == 'SpinyDogfish', QB := QB / 4]

#Step9 - Goosefish----
lscalc(GB.params.adj$model[Group == 'Goosefish', PB]) #75
GB.params.adj$model[Group == 'Goosefish', PB := pbcalc(30)]

#New production pushes the PQ ratio very low ... need to decrease Q
GB.params.adj$model[Group == 'Goosefish', QB := QB / 3]

check.mort(GB.new, 'Goosefish')
#Biggest mortality is from scallop fleet

#Step 10 - Under 10s - Group #1 BSB, Butter, WinterFlounder----
#Black Sea Bass
lscalc(GB.params.adj$model[Group == 'BlackSeaBass', PB]) #75
GB.params.adj$model[Group == 'BlackSeaBass', PB := pbcalc(20)]

check.mort(GB.new, 'BlackSeaBass') #Fleets
#Note - nothing is eating black sea bass in this model - will need to fix this

#Reduce catch by 30%
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'BlackSeaBass', fleet := fleet - fleet * 0.3]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

#Double biomass
GB.params.adj$model[Group == 'BlackSeaBass', Biomass := Biomass * 2]

#Butterfish/Winter Flounder
check.mort(GB.new, 'Butterfish') #LowerTroph/CharsmaticMF
check.mort(GB.new, 'WinterFlounder') #LargeMesh, macrobenth/toothwhale

#Reduce consumption by Higher trophic level species - PQ ratios much lower than
#0.1
high.TL <- c('Seals', 'BalWhale', 'ToothWhale', 'HMS')
GB.params.adj$model[Group %in% high.TL, QB := QB / 3]
#Further reduce seals and whales
high.TL <- high.TL[which(high.TL != 'HMS')]
GB.params.adj$model[Group %in% high.TL, QB := QB / 3]

#Look at GelZoop and OtherPel
#GelZoop highly productive with high QB
GB.params.adj$model[Group == 'GelZooplankton', PB := PB / 2]
GB.params.adj$model[Group == 'GelZooplankton', QB := 100]
#Still very high mort - reducing DC and moving to Micronekton
GB.params.adj$diet[Group == 'Butterfish', GelZooplankton := 0.001]
GB.params.adj$diet[Group == 'Micronekton', GelZooplankton := 0.0137]

#Fix other pelagics
GB.params.adj$model[Group == 'OtherPelagics', PB := PB / 2]
GB.params.adj$model[Group == 'OtherPelagics', QB := QB / 3]

#WinterFlounder productivity needs to increase
lscalc(GB.params.adj$model[Group == 'WinterFlounder', PB]) #35
GB.params.adj$model[Group == 'WinterFlounder', PB := pbcalc(14)]

#Step 11 - White Hake, Other Skates, and other PQ issues----
#Need to fix some other predators in the system
lscalc(GB.params.adj$model[Group == 'WhiteHake', PB]) #57.5
GB.params.adj$model[Group == 'WhiteHake', PB := pbcalc(23)]
GB.params.adj$model[Group == 'WhiteHake', QB := QB / 5]

lscalc(GB.params.adj$model[Group == 'OtherSkates', PB]) #38
GB.params.adj$model[Group == 'OtherSkates', PB := pbcalc(25)]
GB.params.adj$model[Group == 'OtherSkates', QB := QB / 2]

lscalc(GB.params.adj$model[Group == 'RedHake', PB]) #5.56
GB.params.adj$model[Group == 'RedHake', QB := QB * 1.75]

lscalc(GB.params.adj$model[Group == 'Fourspot', PB]) #5.5
GB.params.adj$model[Group == 'Fourspot', QB := QB * 1.75]

lscalc(GB.params.adj$model[Group == 'Scup', PB]) #5.5
GB.params.adj$model[Group == 'Scup', QB := QB * 1.75]

lscalc(GB.params.adj$model[Group == 'Barndoor', PB]) #5.5
GB.params.adj$model[Group == 'Barndoor', PB := pbcalc(30)]
#Unfortunately lowered the PB of Barndoor and are now way out of balance

lscalc(GB.params.adj$model[Group == 'Redfish', PB]) #5.5
GB.params.adj$model[Group == 'Redfish', PB := pbcalc(40)]
GB.params.adj$model[Group == 'Redfish', QB := QB / 4]
#Same for redfish ... boo

check.mort(GB.new, 'Barndoor') #All fisheries related
#Increasing Biomass as not well sampled especially in the 80s
GB.params.adj$model[Group == 'Barndoor', Biomass := Biomass * 2]
#Reduce catch by 30%
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'Barndoor', fleet := fleet - fleet * 0.3]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}

check.mort(GB.new, 'Redfish') #Cod and fisheries
GB.params.adj$model[Group == 'Cod', QB := QB / 2]
#Also doing Spinydogs
GB.params.adj$model[Group == 'SpinyDogfish', QB := QB / 2]
#Reduce catch by 60%
for(ifleet in 1:length(fleets)){
  setnames(GB.params.adj$model, fleets[ifleet], 'fleet')
  GB.params.adj$model[Group == 'Redfish', fleet := fleet - fleet * 0.6]
  setnames(GB.params.adj$model, 'fleet', fleets[ifleet])
}
#Still off so doubling biomass
GB.params.adj$model[Group == 'Redfish', Biomass := Biomass * 2]

#Step 12 - Fix Discards in diet----
check.mort(GB.new, 'Discards') #Macrobenthos

# EE is ratio of DetOut to DetIN - need to lower out unless jacking up landings
# This makes sense as discards should be a low portion of diets
discard.diet <- c('AmLobster', 'Megabenthos', 'OtherShrimps', 'SouthernDemersals',
                  'OtherFlatfish', 'Macrobenthos', 'OtherDemersals')
for(isp in 1:length(discard.diet)){
  setnames(GB.params.adj$diet, discard.diet[isp], 'switch')
  todet <- GB.params.adj$diet[Group == 'Discards', switch]
  GB.params.adj$diet[Group == 'Detritus', switch := switch + todet]
  GB.params.adj$diet[Group == 'Discards', switch := NA]
  setnames(GB.params.adj$diet, 'switch', discard.diet[isp])
}

#Step 13 - Revisit Mackerel, SilverHake, OtherDemersals----
#OtherDemersals
lscalc(GB.params.adj$model[Group == 'OtherDemersals', PB]) #4.8
check.mort(GB.new, 'OtherDemersals') #WinterSkate, SpinyDogs, Little skate?

#Decrease QB by 20%
GB.params.adj$model[Group == 'WinterSkate', QB := QB / 2]

#Top down - 0.9
GB.params.adj$model[Group == 'OtherDemersals', Biomass := NA]
GB.params.adj$model[Group == 'OtherDemersals', EE := 0.9]

#Mackerel
check.mort(GB.new, 'AtlMackerel')
check.mort(GB.new, 'SilverHake')
#SilverHake, Pollock, and Bluefish common problem to both

lscalc(GB.params.adj$model[Group == 'Pollock', PB]) #25
GB.params.adj$model[Group == 'Pollock', QB := QB / 2]

lscalc(GB.params.adj$model[Group == 'Bluefish', PB]) #4.66
GB.params.adj$model[Group == 'Bluefish', PB := pbcalc(9)]
GB.params.adj$model[Group == 'Bluefish', QB := QB / 3]

#SilverHake
GB.params.adj$model[Group == 'SilverHake', QB := QB * .75]
#Adjust DC
GB.params.adj$diet[Group == 'AtlMackerel', SilverHake := 0.01] #removed .0114
GB.params.adj$diet[Group == 'SmPelagics', SilverHake := SilverHake + 0.0057]
GB.params.adj$diet[Group == 'RiverHerring', SilverHake := 0.0057]

GB.params.adj$diet[Group == 'SilverHake', SilverHake := SilverHake / 2]
GB.params.adj$diet[Group == 'Krill', SilverHake := SilverHake + 0.07786667]

#Look at Haddock
lscalc(GB.params.adj$model[Group == 'Haddock', PB]) #3.57
GB.params.adj$model[Group == 'Haddock', PB := pbcalc(20)]
GB.params.adj$model[Group == 'Haddock', QB := QB / 5]

#Step 14 - Lobster----
lscalc(GB.params.adj$model[Group == 'AmLobster', PB]) #1.08
GB.params.adj$model[Group == 'AmLobster', PB := pbcalc(15)]
GB.params.adj$model[Group == 'AmLobster', QB := QB / 12]

check.mort(GB.new, 'AmLobster') #Macrobenthos, Megabenthos

#Moving DC into Megabenthos
GB.params.adj$diet[Group == 'AmLobster', Macrobenthos := NA]
GB.params.adj$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 0.0018]

GB.params.adj$diet[Group == 'AmLobster', Megabenthos := 0.001] #removed 0.0436
GB.params.adj$diet[Group == 'Megabenthos', Megabenthos := Megabenthos + 0.0437]

GB.params.adj$diet[Group == 'AmLobster', OtherDemersals := 0.01] #removed 0.1218
GB.params.adj$diet[Group == 'Megabenthos',
                   OtherDemersals := OtherDemersals + 0.122]

#Step 15 - Last above 3 - Haddock ----
check.mort(GB.new, 'Haddock') #Macrobenthos?
#Moving DC
GB.params.adj$diet[Group == 'Haddock', Macrobenthos := NA] #removed 2e-4
GB.params.adj$diet[Group == 'RedHake', Macrobenthos := NA] #removed 1e-4
GB.params.adj$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 3e-4]

#Step 16 - Fix Offshore Hake diet ----
#For some reason 88% of this diet is silver hake
GB.params.adj$diet[Group == 'SilverHake', OffHake := 0.4401] #removed 0.44
GB.params.adj$diet[Group == 'Mesopelagics', OffHake := 0.22]
GB.params.adj$diet[Group == 'Krill', OffHake := OffHake + 0.11]
GB.params.adj$diet[Group == 'Micronekton', OffHake := OffHake + 0.1]
GB.params.adj$diet[Group == 'OffHake', OffHake := 0.01]

#Step 17 - Summer Flounder ----
lscalc(GB.params.adj$model[Group == 'SummerFlounder', PB]) #2.2
GB.params.adj$model[Group == 'SummerFlounder', PB := pbcalc(9)]
GB.params.adj$model[Group == 'SummerFlounder', QB := QB / 3]

#Step 18 - Balance all EEs by bumping biomass ----
GB.params.adj$model[Group == 'SilverHake', Biomass := Biomass * 4]
GB.params.adj$model[Group == 'AtlHerring', Biomass := Biomass * 4]
GB.params.adj$model[Group == 'AtlMackerel', Biomass := Biomass * 3.5]
GB.params.adj$model[Group == 'Redfish', Biomass := Biomass * 2.75]
GB.params.adj$model[Group == 'Haddock', Biomass := Biomass * 2.75]
GB.params.adj$model[Group == 'AmLobster', Biomass := Biomass * 3.25]
GB.params.adj$model[Group == 'Butterfish', Biomass := Biomass * 2.5]
GB.params.adj$model[Group == 'Windowpane', Biomass := Biomass * 2.5]
GB.params.adj$model[Group == 'OceanPout', Biomass := Biomass * 2.25]
GB.params.adj$model[Group == 'WinterFlounder', Biomass := Biomass * 2]
GB.params.adj$model[Group == 'WitchFlounder', Biomass := Biomass * 1.75]
GB.params.adj$model[Group == 'AmPlaice', Biomass := Biomass * 1.75]
GB.params.adj$model[Group == 'Goosefish', Biomass := Biomass * 1.75]
GB.params.adj$model[Group == 'Microzooplankton', Biomass := Biomass * 2]
GB.params.adj$model[Group == 'Bacteria', Biomass := Biomass * 1.5]
GB.params.adj$model[Group == 'RedHake', Biomass := Biomass * 1.5]
GB.params.adj$model[Group == 'Mesozooplankton', Biomass := Biomass * 1.25]



#Step 19 - Detritus----
#Increase unassim to 0.4 for zooplankton
GB.params.adj$model[Group %in% c('Microzooplankton', 'LgCopepods','SmCopepods'),
                    Unassim := 0.4]

#Increase unassim to 0.3 for other detritavores
GB.params.adj$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos',
                                 'AtlScallop', 'AtlCopepods', 'OceanQuahog', 'OtherShrimps'),
                    Unassim := 0.3]

#Decrease consumption
GB.params.adj$model[Group == 'Bacteria', QB := QB * 0.75]
GB.params.adj$model[Group == 'Microzooplankton', QB := QB * 0.75]
GB.params.adj$model[Group == 'LgCopepods', QB := QB * 0.75]
GB.params.adj$model[Group == 'SmCopepods', QB := QB * 0.75]

#Step 20 - Finishing touches -------
#bump up PBs
GB.params.adj$model[Group == 'SilverHake', PB := PB + 0.15 * PB]
GB.params.adj$model[Group == 'Cod', PB := PB + 0.1 * PB]
GB.params.adj$model[Group == 'AmLobster', PB := PB + 0.05 * PB]
GB.params.adj$model[Group == 'Barndoor', PB := PB + 0.05 * PB]
GB.params.adj$model[Group == 'Haddock', PB := PB + 0.01 * PB]

# 21 groups still off with the removal of the 4x biomass
# bumping biomasses to have a straw man to work with
# Step 21 - Bump Biomasses ----
over.ones <- c('WitchFlounder',
     'BlackSeaBass', 
   'SummerFlounder', 
         'AmPlaice', 
               'Cod', 
         'Barndoor', 
        'Goosefish', 
           'Redfish', 
   'WinterFlounder', 
             'Scup', 
          'Haddock', 
       'AtlMackerel', 
         'WhiteHake' ,
        'AmLobster' ,
        'LgCopepods' ,
        'Windowpane' ,
        'SmCopepods' ,
           'RedHake' ,
        'SilverHake' ,
        'Butterfish' ,
         'OceanPout' )

GB.params.adj$model[Group %in% over.ones, Biomass := Biomass * 4]

#Assign Pedigrees------
#0 - perfect
#1 - No clue
#realistic 0.1 - 0.8
fleets <- c('ScallopDredge', 'ClamDredge', 'OtherDredge', 'FixedGear', 'Pelagic',
            'Trap', 'SmallMesh', 'LargeMesh', 'HMSFleet') #, 'OtherFisheries')

#Biomass -
GB.params$pedigree[, B := 0.2]
GB.params$pedigree[Group %in% fleets, B := 0]
GB.params$pedigree[Group %in% c('Redfish', 'AmPlaice', 'SouthernDemersals', 'Pollock',
                                'OceanPout', 'OffHake', 'WitchFlounder', 'Mesopelagics',
                                'Goosefish', 'BlackSeaBass', 'Sharks'), B := 0.4]
GB.params$pedigree[Group %in% c('Seabirds', 'BalWhale', 'ToothWhale', 'HMS',
                                'Macrobenthos', 'Krill', 'Micronekton', 'GelZooplankton',
                                'LgCopepods','SmCopepods', 'Microzooplankton', 'Phytoplankton'),
                   B := 0.5]
GB.params$pedigree[Group %in% c('Bacteria', 'Detritus','Discards', 'Seals'), B := 0.8]
GB.params$pedigree[Group %in% c('SmPelagics', 'OtherCephalopods', 'OtherShrimp',
                                'OtherPelagics', 'OtherFlatfish', 'SmFlatfishes'),
                   B := 0.5]

#PB -
GB.params$pedigree[, PB := 0.2]
GB.params$pedigree[Group %in% c(fleets, 'Discards', 'Detritus'), PB := 0]
GB.params$pedigree[Group %in% c('Phytoplankton', 'Bacteria', 'Microzooplankton',
                                'LgCopepods','SmCopepods', 'GelZooplankton', 'Micronekton',
                                'Macrobenthos', 'OtherShrimps', 'Mesopelagics',
                                'SmPelagics', 'OtherPelagics', 'Illex', 'SouthernDemersals',
                                'OtherDemersals', 'Sharks', 'HMS', 'Seals', 'BalWhale',
                                'ToothWhale', 'Seabirds'), PB := 0.5]
GB.params$pedigree[Group %in% c('Krill', 'OceanQuahog', 'SurfClam', 'AtlScallop', 'AmLobster','Megabenthos',
                                'Illex', 'Loligo', 'OtherCephalopods', 'Barndoor',
                                'Fourspot', 'OffHake',  'RedHake', 'Redfish', 'Scup',
                                'SmoothDogfish'), PB := 0.6]
GB.params$pedigree[Group %in% c('Goosefish',  'SilverHake',  'WhiteHake',  'Pollock',
                                'OceanPout', 'BlackSeaBass', 'AmPlaice', 'Windowpane',
                                'WinterFlounder', 'WitchFlounder', 'WinterSkate',
                                'LittleSkate'), PB := 0.7]
GB.params$pedigree[Group %in% c('OtherFlatfish', 'SmFlatfishes'), PB := 0.8]

#QB -
GB.params$pedigree[, QB := 0.2]
GB.params$pedigree[Group %in% c(fleets, 'Discards', 'Detritus'), QB := 0]
GB.params$pedigree[Group %in% c('Phytoplankton', 'Bacteria', 'Microzooplankton',
                                'LgCopepods','SmCopepods', 'GelZooplankton', 'Micronekton',
                                'Macrobenthos', 'OtherShrimps', 'Mesopelagics',
                                'SmPelagics', 'OtherPelagics', 'Illex', 'SouthernDemersals',
                                'OtherDemersals', 'Sharks', 'HMS', 'Seals', 'BalWhale',
                                'ToothWhale', 'Seabirds'), QB := 0.5]
GB.params$pedigree[Group %in% c('Krill', 'OceanQuahog', 'SurfClam', 'AtlScallop', 'AmLobster','Megabenthos',
                                'Illex', 'Loligo', 'OtherCephalopods', 'Barndoor',
                                'Fourspot', 'OffHake',  'RedHake', 'Redfish', 'Scup',
                                'SmoothDogfish', 'Goosefish',  'OceanPout',
                                'BlackSeaBass', 'AmPlaice', 'Windowpane',
                                'WinterFlounder', 'WitchFlounder', 'WinterSkate',
                                'LittleSkate'), QB := 0.6]
GB.params$pedigree[Group %in% c('SilverHake',  'WhiteHake',  'Pollock'), QB := 0.7]
GB.params$pedigree[Group %in% c('OtherFlatfish', 'SmFlatfishes'), QB := 0.8]

#Diet
GB.params$pedigree[, Diet := 0.2]
GB.params$pedigree[Group %in% c(fleets, 'Discards', 'Detritus'), Diet := 0]
GB.params$pedigree[Group %in% c('Phytoplankton', 'Bacteria', 'Microzooplankton',
                                'LgCopepods','SmCopepods', 'GelZooplankton', 'Micronekton',
                                'Macrobenthos', 'OtherShrimps', 'Mesopelagics',
                                'SmPelagics', 'OtherPelagics', 'Illex', 'SouthernDemersals',
                                'OtherDemersals', 'Sharks', 'HMS', 'Seals', 'BalWhale',
                                'ToothWhale', 'Seabirds'), Diet := 0.5]
GB.params$pedigree[Group %in% c('Krill', 'OceanQuahog', 'SurfClam', 'AtlScallop', 'AmLobster','Megabenthos',
                                'Illex', 'Loligo', 'OtherCephalopods'), Diet := 0.6]
GB.params$pedigree[Group %in% c('OtherFlatfish', 'SmFlatfishes'), Diet := 0.8]

#Fleets
for(igear in 1:length(fleets)){
  setnames(GB.params$pedigree, fleets[igear], 'gear')
  GB.params$pedigree[, gear := 0.2]
  GB.params$pedigree[Group %in% fleets, gear := 0]
  GB.params$pedigree[Group %in% c('Redfish', 'AmPlaice', 'SouthernDemersals', 'Pollock',
                                  'OceanPout', 'OffHake', 'WitchFlounder', 'Mesopelagics',
                                  'Goosefish', 'BlackSeaBass', 'Sharks'), gear := 0.4]
  GB.params$pedigree[Group %in% c('Seabirds', 'BalWhale', 'ToothWhale', 'HMS',
                                  'Macrobenthos', 'Krill', 'Micronekton', 'GelZooplankton',
                                  'LgCopepods','SmCopepods', 'Microzooplankton', 'Phytoplankton'),
                     gear := 0.5][]
  setnames(GB.params$pedigree, 'gear', fleets[igear])
}





#Check results----
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



alternate.GB.params.bal <- copy(GB.params.adj)
alternate.GB.bal <- copy(GB.new)

usethis::use_data(alternate.GB.params.bal, overwrite = T)
usethis::use_data(alternate.GB.bal, overwrite = T)



# 
# 
# #Save balanced parameter set
# save(GB.params, file = file.path(data.dir, 'GB_balanced_params.RData'))











