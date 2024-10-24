# Data pedigree assignments
# Author: Max Grezlik

# 05/16/2024


#0.2 if estimate is directly from data without modification
#0.4 if estimate is from data but data are variable or sparse
#0.5 if estimate is from stock assessment/regional analysis
#0.6 if estimate is from FishBase or other source
#0.8 if estimate was dramatically adjusted in balancing 

#Edited 5/15/24 to match pedigree justifications for GOM model
# Sarah Weisberg laid out the justification as:
# 1. See Whitehouse & Aydin (2020) for pedigree assignment scheme
# 2. Start with parameters changed in balancing - all biological parameters 
#    changed 0.8x or more get pedigree of 0.8, otherwise change = pedigree; 
#    if diet was changed at all, set to 0.8
# 3. Diet parameters of single species derived from food habit database set 
#    to 0.4 (because this was aggregated across time due to sparse sampling)
# 4. Diets derived from EMAX use data pedigree value from EMAX - but minimum 
#    of 0.4 due to change in time period
# 5. Biomass estimates derived from EMAX take into account biomass changes 
#    made during EMAX balancing (ratio of estimate reported in 
#    documentation : estimate in final model)
# 6. P/B or Q/B estimates from NWACS = 0.5, from Sean’s GB model = 0.6, 
#    from multiple sources = 0.4


# Load packages
library(data.table); library(dplyr); library(here)

# Load GB.params and GB.params.adj
load(here('data', "GB.params.rda"))
load(here('data', "alternate.GB.params.bal.rda"))

# Compare biological parameters from GB.params and GB.params.adj
pedigree.cols <- colnames(alternate.GB.params.bal$pedigree)

pedigree.cols <-pedigree.cols[!pedigree.cols == "Diet"]

init.params <- GB.params$model |> 
  select(all_of(pedigree.cols))

balanced.params <- alternate.GB.params.bal$model |> 
  select(all_of(pedigree.cols))

# Create new table comparing relative change in values
# start with an empty table with columns named the values in pedigree.cols
bio.params.change <- data.table(matrix(NA, nrow = nrow(init.params), ncol = length(pedigree.cols)))
setnames(bio.params.change, pedigree.cols)
bio.params.change$Group <- init.params$Group


pedigree.params <-pedigree.cols[!pedigree.cols == "Group"]

for (param in pedigree.params) {
  bio.params.change[[param]] <- (balanced.params[[param]] / init.params[[param]])-1
}

# Compare diet from GB.params and GB.params.adj
init.diet <- GB.params$diet 
balanced.diet <- alternate.GB.params.bal$diet

# Create new table comparing relative change in values
# start with an empty table with columns named the values in pedigree.cols
diet.change <- data.table(matrix(NA, nrow = nrow(init.diet), ncol = ncol(init.diet)))
setnames(diet.change, colnames(init.diet))
diet.change$Group <- init.diet$Group

diet.params <- colnames(init.diet)
diet.params <- diet.params[!diet.params == "Group"]

for (param in diet.params) {
  diet.change[[param]] <- (balanced.diet[[param]] / init.diet[[param]])-1
}

# Load in starting pedigree
# - pedigree values informed by initial data source
# - changes in this script change pedigree values for inputs where 
#   initial value was modified during the balancing process

pedigree <- as.data.table(read.csv(here("data/GB_data_pedigree.csv")))
# remove unneeded columns
pedigree <- pedigree[, -c(2:6)]

# Remove pedigree for discards
pedigree <- pedigree[1:60]


# Groups where B was estimated by Rpath get 0.8
pedigree[Group %in% c('Bacteria','SmCopepods','LgCopepods','OtherDemersals'), Biomass := 0.8]

# Parameters changed more than 0.8x get 0.8
# start with B
Bchange <- bio.params.change |> 
            filter(Biomass > 0.8 | Biomass < -0.8) |> 
            select(Group)

pedigree[Group %in% Bchange$Group, Biomass := 0.8]

# PB
PBchange <- bio.params.change |> 
            filter(PB > 0.8 | PB < -0.8) |> 
            select(Group)
pedigree[Group %in% PBchange$Group, PB := 0.8]

# QB
QBchange <- bio.params.change |> 
            filter(QB > 0.8 | QB < -0.8) |> 
            select(Group)
pedigree[Group %in% QBchange$Group, QB := 0.8]

# Diet
# look for max and min change
long.diet.change <- diet.change |> 
                 pivot_longer(cols = -Group, names_to = "Rpred", values_to = "Change") |> 
                 select(Group, Change) |> 
                 na.omit() |> 
                 group_by(Group) |>
                 summarise(max = max(Change), min = min(Change)) |> 
                 filter(max > 0.8 | min < -0.8)

# set diet to 0.8 for groups in long.diet.change
pedigree[Group %in% long.diet.change$Group, Diet := 0.8]


 
# Input values in pedigree of alternate.GB.params.bal --------------
#Remove pedigree for fleets & discards
alternate.GB.params.bal$pedigree<-alternate.GB.params.bal$pedigree[1:60]

# Biomass
alternate.GB.params.bal$pedigree[, Biomass := pedigree$Biomass]

# PB
alternate.GB.params.bal$pedigree[, PB := pedigree$PB]

# QB
alternate.GB.params.bal$pedigree[, QB := pedigree$QB]

# Diet
alternate.GB.params.bal$pedigree[, Diet := pedigree$Diet]

# Fleets

# look at landings changed in balancing
load(here('data', "landings.comparison.rda"))

landings.comparison <- landings.comparison |> 
  mutate(Change = (balanced_landings / unbalanced_landings) - 1) |> 
  filter(Change > 0.8 | Change < -0.8)

# set landings to 0.8 for groups in landings.comparison
pedigree[Group %in% landings.comparison$Group, Fleets := 0.8]

alternate.GB.params.bal$pedigree[, 'Scallop Dredge' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'Clam Dredge' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'Other Dredge' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'Fixed Gear' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'Pelagic' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'Trap' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'SM Mesh' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'LG Mesh' := pedigree$Fleets]
alternate.GB.params.bal$pedigree[, 'HMS Fleet' := pedigree$Fleets]

# Save the updated pedigree
usethis::use_data(alternate.GB.params.bal, overwrite = T)

alternate.GB.bal <- rpath(alternate.GB.params.bal, eco.name = 'Georges Bank')
usethis::use_data(alternate.GB.bal, overwrite = T)
