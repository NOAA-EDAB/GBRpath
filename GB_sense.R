#Georges Bank Rpath Ecosense
#SML

#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/GBRpath"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/GBRpath"
}

data.dir <- file.path(main.dir, 'data')

#Required packages--------------------------------------------------------------
library(data.table); library(Rpath)

#-------------------------------------------------------------------------------
#User created functions
source(file.path(main.dir, 'rsim_sense_master_Jun2019.r'))
       
#-------------------------------------------------------------------------------
#Load and balance model
load(file.path(data.dir, 'GB_balanced_params.RData'))

#Test dynamic run
# GB <- rpath(GB.params)
# GB.scene <- rsim.scenario(GB, GB.params, years = 2014:2113)
# GB.scene$params$NoIntegrate[2:6] <- 0
# GB.testrun <- rsim.run(GB.scene, method = 'AB', years = 2014:2113)
# rsim.plot(GB.testrun, GB.params$model[Type < 3, Group])
#Assign Pedigrees
#0 - perfect
#1 - No clue
#realistic 0.1 - 0.8
#Test
fleets <- c('DredgeScallop', 'DredgeClam', 'Gillnet', 'Longline', 'PotTrap', 
            'OtterTrawlSm', 'OtterTrawlLg', 'Midwater', 'OtherFisheries')

GB.params$pedigree[, B := 0]
GB.params$pedigree[Group %in% fleets, B := 0]
GB.params$pedigree[, PB := 0]
GB.params$pedigree[, QB := 0]
GB.params$pedigree[, Diet := 0]
for(igear in 1:length(fleets)){
  setnames(GB.params$pedigree, fleets[igear], 'gear')
  GB.params$pedigree[, gear := 0]
  setnames(GB.params$pedigree, 'gear', fleets[igear])
}

GB <- rpath(GB.params, 'Georges Bank')

#Set up sense runs
all_years <- 2014:2063
scene <- rsim.scenario(GB, GB.params, years = all_years)

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 50
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(123)
for (irun in 1:NUM_RUNS){
  GBsense <- copy(scene) 
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GBsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense.orig(GBsense, GB, GB.params)	# Replace the base params with Ecosense params  
  GBsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GBsense$params <- parlist[[irun]]
  GBtest <- rsim.run(GBsense, method = "RK4", years = all_years)
  failList <- which(is.na(GBtest$end_state$Biomass))
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
