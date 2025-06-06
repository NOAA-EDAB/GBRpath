################################################################################
#
# *Ecosense function for the Master branch of Rpath*
#
# To merge branches we need to rewrite the ecosense parameter generation
# function. This new function is loosely based on the rsim.params function,
# where the Ecopath outputs are converted into rates for use with Ecosim.
#
# Similar to Rsim.scenario, the object returned by this Ecosense function will
# have the same attributes as the rsim object and can be supplied to the
# rsim.run function.
#
################################################################################



#'Ecosense module of Rpath
#'
#'
#'@family Rpath functions
#'
#'@param Rpath.scenario R object that is generated by the rsim.scenario
#' function and can be supplied to the rsim.run function.
#'@param Rpath R object containing a balanced Rpath model.
#'@param Rpath.params R object containing the Rpath parameters.  This is generated
#'  either by the create.rpath.params or read.rpath.params functions.
#'
#'
#'@return Returns an Rsim.scenario object that can be supplied to the rsim.run function.



# rsim.scenario has already produced the 5 core list objects that need to be
# supplied to rsim.run(): params, start_state, forcing, fishing, stanzas.
# In rsim.sense() we are only concerned with changing those 'params' which have
# uncertainty incorporated in them as part of the ecosense routine, or are
# otherwise derivative from the uncertain parameters (e.g., parameters that are
# intermediate steps in the calculation of sim parameters, larger for-loops
# which may include one (or more) parameter(s) that incorporates uncertainty
# from one of the base parameters).


rsim.sense.orig <- function(Rpath.scenario, Rpath, Rpath.params, mscramble = 2,
                        mhandle = 1000, preyswitch = 1, scrambleselfwt = 1,
                        handleselfwt = 1, steps_yr = 12, steps_m = 1)   {
  
  sense.params <- Rpath.scenario$params

  nliving <- Rpath$NUM_LIVING
  ndead   <- Rpath$NUM_DEAD
  
  # Set-up pedigree vectors, including zeroes for gear groups.
  BBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,2])))
  PBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,3])))
  QBVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,4])))
  DCVAR	<- c(as.numeric(unlist(Rpath.params$pedigree[,5])))
  

  # Biomass
  ranBB <- Rpath$Biomass * (1 + BBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))
  sense.params$B_BaseRef <- c(1.0, ranBB)
  # PB
  ranPB <- Rpath$PB * (1 + PBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))
  # QB
  ranQB <- Rpath$QB * (1 + QBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0))

  # Mzero
  sense.params$MzeroMort <- c(0.0, ranPB * (1.0 - Rpath$EE)  *
                          (1 + PBVAR * runif(Rpath$NUM_GROUPS,-1.0,1.0)))
    # The zero at the start of the M0 vector is for "outside"
    # same for the other vectors

  #Active respiration is proportion of CONSUMPTION that goes to "heat"
  #Passive respiration/ VonB adjustment is left out here
  sense.params$ActiveRespFrac <-  c(0.0, ifelse(ranQB > 0, 
                                          1.0 - (ranPB / ranQB) - Rpath$GS, 
                                          0.0))

  #####	rQBPrimProd is a added here because we need a non-zero QB for
  #####	primary production groups. QB's of zero produce NaN's in ecosim
  #####	and just bring the whole thing to a screaching hault. rQBPrimProd
  #####	sets the QB of PP groups equal to their PB.
  rQBPrimProd	<- ifelse(Rpath$type==1,ranPB,ranQB)
  sense.params$FtimeQBOpt <-   c(1.0, rQBPrimProd)
  #sense.params$FtimeQBOpt <-   c(1.0, ranQB)
  sense.params$PBopt      <-   c(1.0, ranPB)           

  #No Integrate
  sense.params$NoIntegrate <- ifelse(sense.params$MzeroMort * sense.params$B_BaseRef > 
                                 2 * steps_yr * steps_m, 0, sense.params$spnum)
  
  #primary production links
  primTo   <- ifelse(Rpath$type > 0 & Rpath$type <= 1, 
                     1:length(ranPB),
                     0)
  primFrom <- rep(0, length(Rpath$PB))
  primQ    <- ranPB * ranBB 

  # Change production to consusmption for mixotrophs
  mixotrophs <- which(Rpath$type > 0 & Rpath$type < 1)
  primQ[mixotrophs] <- primQ[mixotrophs] / Rpath$GE[mixotrophs] * 
          Rpath$type[mixotrophs]

  #Predator/prey links
  preyfrom  <- row(Rpath$DC)
  preyto    <- col(Rpath$DC)	
  predpreyQ <- Rpath$DC[1:(nliving + ndead + 1), ] * 
    t(matrix(rQBPrimProd[1:Rpath$NUM_LIVING] * ranBB[1:Rpath$NUM_LIVING],
            nliving, nliving + ndead + 1))
  
  #combined
  sense.params$PreyFrom <- c(primFrom[primTo > 0], preyfrom [predpreyQ > 0])
  # Changed import prey number to 0
  sense.params$PreyFrom[which(sense.params$PreyFrom == nrow(Rpath$DC))] <- 0
  sense.params$PreyTo   <- c(primTo  [primTo > 0], preyto   [predpreyQ > 0])
  
  ##### This is where we add uncertainty to diet  #####
  # Diet comp vector
  DCvector <- c(rep(0.0, sum(Rpath$type==1)), Rpath$DC[Rpath$DC>0])
  # Diet comp pedigree
  DCped <- as.numeric(unlist(Rpath.params$pedigree[,5]))
  DCpedigree <- DCped[sense.params$PreyTo]
  ## Random diet comp
  EPSILON <- 1*10^-8
  betascale <- 1.0
  DCbeta <- betascale * DCpedigree * DCpedigree
  alpha <- DCvector/DCbeta
  DClinks <- rgamma(length(DCvector), shape=alpha, rate=DCbeta)
  #For testing - SML
  #DClinks <- DCvector
  DClinks2 <- ifelse(DClinks < EPSILON, 2 * EPSILON, DClinks)
  # DClinks2 prevents random diet comps from becoming too low, effectively
  # equal to zero. Zeros in DClinks will produce NaN's in sense.params$QQ, and
  # others, ultimately preventing ecosim.
  DCtot <- tapply(DClinks2, sense.params$PreyTo, "sum")    
  # Normalized diet comp
  DCnorm <- ifelse(Rpath$type[sense.params$PreyTo]==1, 1.0, DClinks2/DCtot[sense.params$PreyTo])
  # The "if" part of DCnorm is so the DC of phytoplankton (type==1) won't equal zero
  DCQB <- rQBPrimProd[sense.params$PreyTo]
  DCBB <- ranBB[sense.params$PreyTo]  
  sense.params$QQ <- DCnorm * DCQB * DCBB             	
   
  numpredprey <- length(sense.params$QQ)

  # Sarah used the following formula to vary vulnerability in Gaichas et al. (2012)
  # That paper states that "vulnerability" (also known as X*predprey) has an
  # effective range from 1.01 to 91 in EwE.
  sense.params$VV	<-	1 + exp(9 * (runif(length(sense.params$QQ))-0.5))
  sense.params$DD	<-	1000 + exp(0 * (runif(length(sense.params$QQ))-0.5))

  # Test - SML
  #sense.params$VV <- sense.params$VV[which(sense.params$VV > 0)]
  #sense.params$DD <- sense.params$DD[which(sense.params$DD > 0)]
  
  # Scramble combined prey pools
  Btmp <- sense.params$B_BaseRef
  py   <- sense.params$PreyFrom + 1.0
  pd   <- sense.params$PreyTo + 1.0
  VV   <- sense.params$VV * sense.params$QQ / Btmp[py]
  AA   <- (2.0 * sense.params$QQ * VV) / (VV * Btmp[pd] * Btmp[py] - sense.params$QQ * Btmp[pd])
  sense.params$PredPredWeight <- AA * Btmp[pd] 
  sense.params$PreyPreyWeight <- AA * Btmp[py] 
  
  sense.params$PredTotWeight <- rep(0, length(sense.params$B_BaseRef))
  sense.params$PreyTotWeight <- rep(0, length(sense.params$B_BaseRef))
  
  for(links in 1:numpredprey){
    sense.params$PredTotWeight[py[links]] <- sense.params$PredTotWeight[py[links]] + sense.params$PredPredWeight[links]
    sense.params$PreyTotWeight[pd[links]] <- sense.params$PreyTotWeight[pd[links]] + sense.params$PreyPreyWeight[links]    
  }  

  sense.params$PredPredWeight <- sense.params$PredPredWeight/sense.params$PredTotWeight[py]
  sense.params$PreyPreyWeight <- sense.params$PreyPreyWeight/sense.params$PreyTotWeight[pd]

  sense.params$PreyFrom       <- c(0, sense.params$PreyFrom)
  sense.params$PreyTo         <- c(0, sense.params$PreyTo)
  sense.params$QQ             <- c(0, sense.params$QQ)
  sense.params$DD             <- c(0, sense.params$DD)
  sense.params$VV             <- c(0, sense.params$VV) 
  sense.params$PredPredWeight <- c(0, sense.params$PredPredWeight)
  sense.params$PreyPreyWeight <- c(0, sense.params$PreyPreyWeight)


  #catchlinks
  fishfrom    <- row(as.matrix(Rpath$Catch))
  fishthrough <- col(as.matrix(Rpath$Catch)) + (nliving + ndead)
  fishcatch   <- Rpath$Catch
  fishto      <- fishfrom * 0
  
  if(sum(fishcatch) > 0){
    sense.params$FishFrom    <- fishfrom   [fishcatch > 0]
    sense.params$FishThrough <- fishthrough[fishcatch > 0]
    sense.params$FishQ       <- fishcatch  [fishcatch > 0] / sense.params$B_BaseRef[sense.params$FishFrom + 1]  
    sense.params$FishTo      <- fishto     [fishcatch > 0]
  }

  #discard links
  for(d in 1:Rpath$NUM_DEAD){
    detfate <- Rpath$DetFate[(nliving + ndead + 1):Rpath$NUM_GROUPS, d]
    detmat  <- t(matrix(detfate, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))
    
    fishfrom    <-  row(as.matrix(Rpath$Discards))                      
    fishthrough <-  col(as.matrix(Rpath$Discards)) + (nliving + ndead)
    fishto      <-  t(matrix(nliving + d, Rpath$NUM_GEAR, Rpath$NUM_GROUPS))
    fishcatch   <-  Rpath$Discards * detmat
    if(sum(fishcatch) > 0){
      sense.params$FishFrom    <- c(sense.params$FishFrom,    fishfrom   [fishcatch > 0])
      sense.params$FishThrough <- c(sense.params$FishThrough, fishthrough[fishcatch > 0])
      ffrom <- fishfrom[fishcatch > 0]
      sense.params$FishQ       <- c(sense.params$FishQ,  fishcatch[fishcatch > 0] / sense.params$B_BaseRef[ffrom + 1])  
      sense.params$FishTo      <- c(sense.params$FishTo, fishto   [fishcatch > 0])
    }
  } 

  sense.params$FishFrom        <- c(0, sense.params$FishFrom)
  sense.params$FishThrough     <- c(0, sense.params$FishThrough)
  sense.params$FishQ           <- c(0, sense.params$FishQ)  
  sense.params$FishTo          <- c(0, sense.params$FishTo)   

 
  class(sense.params) <- 'Rsim.params'
  return(sense.params)   
  
}




################################################################################ 



################################################################################ 



################################################################################ 



################################################################################ 
