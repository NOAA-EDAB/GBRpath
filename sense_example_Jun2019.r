# Sample code for ecosense

source("rsim_sense_master_Jun2019.r")
setwd("\\\\AKC0SS-N086/REFM_Users/andy.whitehouse/My Documents/Andy/REEM/EwE/Rclean")
rm(list=ls())

# Rpath data files
  # Ebase <- "sensemodels/EBS_base_Aug2016_AGG.csv"  # Base biomass, production, fishing, etc.
  # Ediet <- "sensemodels/EBS_diet_Aug2016_AGG.csv"  # Diet matrix
  # Eped  <- "sensemodels/EBS_ped_Aug2016_AGG.csv"   # Data pedigree = quality of input data
  Ebase <- "data/EBS_base_Aug2016_AGG.csv"
  Ediet <- "data/EBS_diet_Aug2016_AGG.csv"
  Eped  <- "data/EBS_ped_Aug2016_AGG.csv"
  

# Setup Base Ecopath and Base Rsim scenario
#unbal <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced
unbal <- read.rpath.params(Ebase, Ediet, Eped)
bal   <- rpath(unbal) # balanced

all_years <- 2000:2099

scene <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 5000
parlist<-as.list(rep(NA,NUM_RUNS))
kept<-rep(NA,NUM_RUNS)

for (i in 1:5000){
  EBSsense <- scene 
  # INSERT SENSE ROUTINE BELOW
  parlist[[i]]<- scene$params 		# Base ecosim params
  parlist[[i]]<- rsim.sense.orig(scene, bal, unbal)	# Replace the base params with Ecosense params  
  EBSsense$start_state$BB <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50			# Set Burn Years to 50
  EBSsense$params <- parlist[[i]]
  EBStest <- rsim.run(EBSsense, method="AB", years=2000:2099)
  failList <- which(is.na(EBStest$end_state$BB))
  {if (length(failList)>0)
  {cat(i,": fail in year ",EBStest$crash_year,": ",failList,"\n"); kept[i]<-F; flush.console()}
    else 
    {cat(i,": success!\n"); kept[i]<-T;  flush.console()}}
  parlist[[i]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
# 4.46 %


################################################################################
################################################################################
################################################################################

setwd("C:/src/EBS_ecosim-master")

Ebase <- "models/EBS_ACLIM_72_BIO_base.csv"  # Base biomass, production, fishing, etc.
Ediet <- "models/EBS_ACLIM_72_BIO_diet.csv"  # Diet matrix
Eped  <- "models/EBS_ACLIM_72_BIO_ped_2.csv"   # Data pedigree = quality of input data
Estz  <- "models/EBS_ACLIM_72_BIO_stanzas.csv"   # Stanzas
Estg  <- "models/EBS_ACLIM_72_BIO_stanza_groups.csv" # Stanza groups

# Setup Base Ecopath and Base Rsim scenario
unbal <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced
bal <- rpath(unbal) # balanced

all_years <- 2000:2099

scene <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 10000
parlist<-as.list(rep(NA,NUM_RUNS))
kept<-rep(NA,NUM_RUNS)

for (i in 1:10000){
  EBSsense <- scene 
  # INSERT SENSE ROUTINE BELOW
  parlist[[i]]<- scene$params 		# Base ecosim params
  parlist[[i]]<- rsim.sense.orig(scene, bal, unbal)	# Replace the base params with Ecosense params  
  EBSsense$start_state$BB <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50			# Set Burn Years to 50
  EBSsense$params <- parlist[[i]]
  EBStest <- rsim.run(EBSsense, method="AB", years=2000:2099)
  failList <- which(is.na(EBStest$end_state$BB))
  {if (length(failList)>0)
  {cat(i,": fail in year ",EBStest$crash_year,": ",failList,"\n"); kept[i]<-F; flush.console()}
    else 
    {cat(i,": success!\n"); kept[i]<-T;  flush.console()}}
  parlist[[i]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept

# kept 17 out of 1000


################################################################################
################################################################################
################################################################################

# old ECS AGG
setwd("\\\\AKC0SS-N086/REFM_Users/andy.whitehouse/My Documents/Andy/REEM/EwE/Rclean")
rm(list=ls())

# Rpath data files
Ebase <- "data/ECS_eis_base_July2015_AGG.csv"
Ediet <- "data/ECS_eis_diet_Jun2015_AGG.csv"
Eped  <- "data/ECS_eis_ped_Jun2015_AGG.csv"


# Setup Base Ecopath and Base Rsim scenario
#unbal <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced
unbal <- read.rpath.params(Ebase, Ediet, Eped)
bal   <- rpath(unbal) # balanced

all_years <- 2000:2099

scene <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 5000
parlist<-as.list(rep(NA,NUM_RUNS))
kept<-rep(NA,NUM_RUNS)

for (i in 1:5000){
  EBSsense <- scene 
  # INSERT SENSE ROUTINE BELOW
  parlist[[i]]<- scene$params 		# Base ecosim params
  parlist[[i]]<- rsim.sense.orig(scene, bal, unbal)	# Replace the base params with Ecosense params  
  EBSsense$start_state$BB <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50			# Set Burn Years to 50
  EBSsense$params <- parlist[[i]]
  EBStest <- rsim.run(EBSsense, method="AB", years=2000:2099)
  failList <- which(is.na(EBStest$end_state$BB))
  {if (length(failList)>0)
  {cat(i,": fail in year ",EBStest$crash_year,": ",failList,"\n"); kept[i]<-F; flush.console()}
    else 
    {cat(i,": success!\n"); kept[i]<-T;  flush.console()}}
  parlist[[i]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept

# 3.36%


################################################################################
################################################################################
################################################################################

# old GOA AGG
setwd("\\\\AKC0SS-N086/REFM_Users/andy.whitehouse/My Documents/Andy/REEM/EwE/Rclean")
rm(list=ls())

# Rpath data files
Gbase <- "data/GOA_base_Aug2016_AGG.csv"
Gdiet <- "data/GOA_diet_Aug2016_AGG.csv"
Gped  <- "data/GOA_ped_Aug2016_AGG.csv"


# Setup Base Ecopath and Base Rsim scenario
#unbal <- rpath.stanzas(read.rpath.params(Ebase, Ediet, Eped, Estg, Estz)) # unbalanced
unbal <- read.rpath.params(Gbase, Gdiet, Gped)
bal   <- rpath(unbal) # balanced

all_years <- 2000:2099

scene <- rsim.scenario(bal, unbal, years = all_years) # Ecosim params

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 5000
parlist<-as.list(rep(NA,NUM_RUNS))
kept<-rep(NA,NUM_RUNS)

for (i in 1:5000){
  EBSsense <- scene 
  # INSERT SENSE ROUTINE BELOW
  parlist[[i]]<- scene$params 		# Base ecosim params
  parlist[[i]]<- rsim.sense.orig(scene, bal, unbal)	# Replace the base params with Ecosense params  
  EBSsense$start_state$BB <- parlist[[i]]$B_BaseRef
  parlist[[i]]$BURN_YEARS <- 50			# Set Burn Years to 50
  EBSsense$params <- parlist[[i]]
  EBStest <- rsim.run(EBSsense, method="AB", years=2000:2099)
  failList <- which(is.na(EBStest$end_state$BB))
  {if (length(failList)>0)
  {cat(i,": fail in year ",EBStest$crash_year,": ",failList,"\n"); kept[i]<-F; flush.console()}
    else 
    {cat(i,": success!\n"); kept[i]<-T;  flush.console()}}
  parlist[[i]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept

# 2.68 %