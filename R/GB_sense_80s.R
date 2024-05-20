#Ecosense runs for GB 80s model
#Author: Sarah J . Weisberg

# Fri May 17 11:15:28 2024 ------------------------------

#Prep-------------------------------------------------------------
library(here); library(data.table); library(Rpath)
#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

# Load model and parameter files
load(here("data/alternate.GB.params.bal.rda"))
load(here("data/alternate.GB.bal.rda"))

#Rename model/parameter files
GB<-alternate.GB.bal
GB.params<-alternate.GB.params.bal

#adjust Bacteria diet to match MAB
GB.params$diet[Group == 'Phytoplankton',Bacteria :=0.15]
GB.params$diet[Group == 'Detritus',Bacteria :=0.85]

GB<-rpath(GB.params,eco.name = 'Georges Bank')
#Set up model with group names and types
groups<-as.vector(GB$Group)

#Count number of each group type
nliving <- nrow(GB.params$model[Type <  2, ])
ndead   <- nrow(GB.params$model[Type == 2, ])
#Identify index of pp
pp<-which(groups=="Phytoplankton")

#Fix PB/QB pedigree values so that Respiration cannot <0
#Exclude pp and detritus, which do not technically respire in Rpath model
Unassim<-GB$Unassim[1:(nliving+ndead)]
QB<-GB$QB[1:(nliving+ndead)]
PB<-GB$PB[1:(nliving+ndead)]
#Identify which groups violate this rule
fixers<-which((1-Unassim)*QB*(1-GB.params$pedigree[,QB]) < (1+GB.params$pedigree[, PB])*PB)
#exclude Phytoplankton & Detritus
fixers <- fixers[!fixers %in% c(pp,(nliving+ndead))]

#adjust QB and PB values where needed
for (i in 1:length(fixers)){
  to_fix<-fixers[i]
  Resp_edge<-PB[to_fix]/((1-Unassim[to_fix])*QB[to_fix])
  QB_ped<-GB.params$pedigree[to_fix,QB]
  PB_ped<-GB.params$pedigree[to_fix,PB]
  limit<-(1-QB_ped)/(1+PB_ped)
  while(Resp_edge>limit){
    if(QB_ped >= PB_ped){
      QB_ped <- QB_ped - 0.1
    }
    else{
      PB_ped <- PB_ped - 0.1
    }
    limit<-(1-QB_ped)/(1+PB_ped)
  }
  GB.params$pedigree[to_fix,PB := PB_ped]
  GB.params$pedigree[to_fix,QB := QB_ped]
}

#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(GB, GB.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 50000
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(19)
for (irun in 1:NUM_RUNS){
  GBsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GBsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(GBsense, GB.params)	# Replace the base params with Ecosense params  
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GBsense$params <- parlist[[irun]]
  GBtest <- rsim.run(GBsense, method = "RK4", years = all_years)
  failList <- which((is.na(GBtest$end_state$Biomass) | GBtest$end_state$Biomass/orig.biomass > 1000 | GBtest$end_state$Biomass/orig.biomass < 1/1000))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",GBtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
GB_sense_bound_bac <- parlist[KEPT]

GB_sense_unbound_bac <-parlist
#save
save(GB,file = "data/GB_bac.RData")
save(GB.params,file = "data/GB_params_bac.RData")
save(GB_sense_bound_bac, file = "data/GB_sense_bound_bac.RData")
save(GB_sense_unbound_bac, file = "data/GB_sense_unbound_bac.RData")
