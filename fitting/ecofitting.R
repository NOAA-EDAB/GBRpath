#Copy-pasted from Sarah's GOM-Rpath Repo
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/ecofitting.R
# 06/23/2024 ------------------------------

################################################################################
#'@useDynLib Rpath
#'@export
read.fitting.biomass <- function(SCENE, filename){
  
  # Base variables
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))
  
  cdat  <-read.csv(filename)
  c0dat <-cdat[!is.na(cdat$Value)& cdat$Year %in% years ,]
  ccdat <- c0dat[c0dat$Value>0 & c0dat$Stdev>0 ,]
  
  #type <- as.character(rep("absolute",length(ccdat$YEAR)))
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Value),
                 as.numeric(ccdat$Value) * as.numeric(ccdat$Scale))   
  sd   <- ifelse(as.numeric(ccdat$Scale)<0, as.numeric(ccdat$Stdev),
                 as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale))   
  wt   <- rep(1,length(obs))
  
  SIM$fitting$Biomass <- cbind(ccdat,obs,sd,wt)
  
  return(SIM)
}

################################################################################
#'@export
read.fitting.catch <- function(SCENE, filename){
  SIM   <- SCENE
  years <- as.numeric(row.names(SCENE$fishing$ForcedFRate))
  # Columns needed
  #  Group	Year	Value	SD	Scale   
  cdat  <- read.csv(filename)
  ccdat <- cdat[!is.na(cdat$Value) & cdat$Year %in% years,] 
  ccdat$Year  <- as.character(ccdat$Year)
  ccdat$Group <- as.character(ccdat$Group) 
  obs  <- as.numeric(ccdat$Value) * as.numeric(ccdat$Scale)   
  sd   <- as.numeric(ccdat$Stdev) * as.numeric(ccdat$Scale)  
  wt   <- rep(1,length(obs))
  SIM$fitting$Catch <- cbind(ccdat,obs,sd,wt)
  #sdat  <- aggregate(as.numeric(ccdat$Value)*as.numeric(ccdat$Scale),list(ccdat$Year,ccdat$Group),"sum")
  #sd    <- 0.1*sdat$x
  #colnames(SIM$fitting$CATCH) <- c("year","species","obs","sd","wt")
  
  # Apply fit fishing to matrix
  #SIM$fishing$ForcedEffort[] <- 0
  #SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
  #                        length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
fitcatch.to.forcecatch <- function(SCENE){
  SIM <- SCENE
  #SIM$fishing$ForcedEffort[] <- 0
  SIM$fishing$ForcedCatch[matrix(c(SIM$fitting$Catch$Year, SIM$fitting$Catch$Group),
                                 length(SIM$fitting$Catch$Year),2)] <- SIM$fitting$Catch$obs
  return(SIM)
}

################################################################################
#'@export
rsim.plot.catch <- function(scene, run, species){
  qdat <- scene$fitting$Catch[scene$fitting$Catch$Group==species,]
  mn   <- qdat$obs
  up   <- mn + 1.96*qdat$sd
  dn   <- mn - 1.96*qdat$sd 
  tot <- 0 #sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Catch)),run$annual_Catch[,species],type="l",
       ylim=c(0,max(up,run$annual_Catch[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"catch"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}

################################################################################
#'@export
rsim.plot.biomass <- function(scene, run, species){
  bio.obj <- rsim.fit.obj(scene,run)$Biomass 
  qdat <- bio.obj[bio.obj$Group==species,]
  #survey_q <- 1
  mn   <- qdat$obs_scaled #* qdat$survey_q   #/survey_q
  up   <- mn + 1.96*qdat$sd * qdat$survey_q #/survey_q
  dn   <- mn - 1.96*qdat$sd * qdat$survey_q #/survey_q 
  tot  <- sum(qdat$fit)
  plot(as.numeric(rownames(run$annual_Biomass)),run$annual_Biomass[,species],type="l",
       ylim=c(0,max(up,run$annual_Biomass[,species])),xlab=tot,ylab="")
  mtext(side=2, line=2.2, paste(species,"biomass"), font=2, cex=1.0)
  points(as.numeric(qdat$Year),mn)
  segments(as.numeric(qdat$Year),y0=up,y1=dn)
}
#################################################################################
#'@export
rsim.fit.obj <- function(SIM,RES,verbose=TRUE){
  FLOGTWOPI <- 0.5*log(2*pi) #0.918938533204672
  epsilon <- 1e-36
  
  OBJ <- list()
  OBJ$tot <- 0
  
  # BIOMASS to NON-RESCALED "Actual" biomass estimate
  est <- RES$annual_Biomass[matrix(c(as.character(SIM$fitting$Biomass$Year),as.character(SIM$fitting$Biomass$Group)),
                                   ncol=2)] + epsilon
  obs <- SIM$fitting$Biomass$obs + epsilon
  sd  <- SIM$fitting$Biomass$sd  + epsilon
  wt  <- SIM$fitting$Biomass$wt 
  # We need to get variance-weighted survey means by species, for
  # calculating mean values needed for setting best-fit q
  
  # Formula for weighted average q: 
  # q = exp(sum(w * log(obs/est))/sum(w)) where w is wt/sd  
  logdiff       <- log(obs/est)
  sdlog         <- sqrt(log(1.0+sd*sd/(obs*obs))) # sigma^2 of lognormal dist 
  wt_sd_inverse <- wt/sdlog# sd
  wt_logdiffsum <- tapply(logdiff*wt_sd_inverse, as.character(SIM$fitting$Biomass$Group),sum)
  wt_sum        <- tapply(wt_sd_inverse,         as.character(SIM$fitting$Biomass$Group),sum)
  q_est         <- exp(wt_logdiffsum/wt_sum) # need ifelse here for 0 weights?
  survey_q      <- ifelse(SIM$fitting$Biomass$Type=="absolute", 1.0,
                          q_est[as.character(SIM$fitting$Biomass$Group)])
  ## Jan 2023 incorrect code
  #inv_var <- 1.0/(sd*sd)
  #obs_sum <- tapply(obs*inv_var*wt, as.character(SIM$fitting$Biomass$Group),sum)
  #inv_sum <- tapply(inv_var*wt,     as.character(SIM$fitting$Biomass$Group),sum)
  #obs_mean <- obs_sum/inv_sum
  #est_mean <- tapply(est,as.character(SIM$fitting$Biomass$Group),mean)
  #survey_q <- ifelse(SIM$fitting$Biomass$Type=="absolute", 1.0,
  #            #(obs_mean/est_mean)[as.character(SIM$fitting$Biomass$Group)])
  #            (est_mean/obs_mean)[as.character(SIM$fitting$Biomass$Group)])
  #obs_scaled <-obs*survey_q 
  #sdlog  <- sqrt(log(1.0+sd*sd*survey_q*survey_q/(obs_scaled*obs_scaled)))
  sdiff  <- log((obs/survey_q)/est)/sdlog
  fit    <- wt * (FLOGTWOPI + log(sdlog) + 0.5*sdiff*sdiff)
  
  if (verbose){
    obs_scaled  <- obs/survey_q
    OBJ$Biomass <- cbind(SIM$fitting$Biomass,est,survey_q,obs_scaled,sdiff,fit)
  } else {
    OBJ$tot <- OBJ$tot + sum(fit)
  }
  
  # Catch compared (assumes all catch is clean, absolute values)
  est <- RES$annual_Catch[matrix(c(as.character(SIM$fitting$Catch$Year),as.character(SIM$fitting$Catch$Group)),
                                 ncol=2)] + epsilon
  obs <- SIM$fitting$Catch$obs + epsilon
  sd  <- SIM$fitting$Catch$sd  + epsilon
  sdlog  <- sqrt(log(1.0+sd*sd/(obs*obs)))
  sdiff  <- log(obs/est)/sdlog
  fit    <- SIM$fitting$Catch$wt * (log(sdlog) + FLOGTWOPI + 0.5*sdiff*sdiff)
  if (verbose){
    OBJ$Catch <- cbind(SIM$fitting$Catch,est,sdiff,fit)
  } else {
    OBJ$tot <- OBJ$tot + sum(fit)
  }
  
  # # RATION
  # obs <- SIM$fitting$ration$obs + epsilon
  # sd  <- SIM$fitting$ration$sd  + epsilon
  # inv_var <- (1.0/sd)*(1.0/sd)
  # obs_sum <- tapply(obs*inv_var,as.character(SIM$fitting$ration$Group),sum)
  # inv_sum <- tapply(inv_var,as.character(SIM$fitting$ration$Group),sum)
  # obs_mean <- obs_sum/inv_sum
  # est <- RES$annual_QB[matrix(c(as.character(SIM$fitting$ration$Year),as.character(SIM$fitting$ration$Group)),
  #                             ncol=2)] + epsilon
  # est_mean <- tapply(est,as.character(SIM$fitting$ration$Group),mean)
  # survey_q <- (obs_mean/est_mean)[as.character(SIM$fitting$ration$Group)]
  # est_scaled <-est*survey_q 
  # sdlog  <- sqrt(log(1.0+sd*sd/(obs*obs)))
  # sdiff  <- (log(obs)-log(est_scaled))/sdlog
  # fit    <- SIM$fitting$ration$wt * (log(sdlog) + FLOGTWOPI + 0.5*sdiff*sdiff)
  # OBJ$ration <- cbind(GOA_SIM$fitting$ration,est,survey_q,est_scaled,sdiff,fit)
  # 
  # # Diet proportions estimation
  # linklook   <- matrix(c(as.character(SIM$fitting$diets$Year),as.character(SIM$fitting$diets$simlink)),ncol=2)
  # totlook    <- matrix(c(as.character(SIM$fitting$diets$Year),as.character(SIM$fitting$diets$pred)),ncol=2) 
  # dietTot    <- tapply(RES$annual_Qlink[linklook],list(SIM$fitting$diets$Year,SIM$fitting$diets$pred),sum)
  # dietProp   <- RES$annual_Qlink[linklook]/dietTot[totlook]
  # logest     <- log(dietProp)
  # #NEGATIVE log likelihood now
  # fit        <- -SIM$fitting$diets$wt * (SIM$fitting$diets$log_diff + SIM$fitting$diets$alphaM1*logest)  
  # OBJ$diet   <- cbind(SIM$fitting$diets,dietProp,logest,fit)
  
  # Final summation and return
  if(verbose){
    OBJ$tot <- sum(OBJ$Biomass$fit, OBJ$Catch$fit)# , OBJ$ration$fit, OBJ$diet$fit)
    return(OBJ)
  }
  else{
    return(OBJ$tot)
  }
}
#################################################################################
#'@export
rsim.fit.table <- function(SIM,RES){
  fitobj  <- rsim.fit.obj(SIM,RES,verbose=T)
  Btmp <- tapply(fitobj$Biomass$fit,fitobj$Biomass$Group,sum)
  Ctmp <- tapply(fitobj$Catch$fit,fitobj$Catch$Group,sum)
  out <- rep(NA,length(SIM$params$spname)); names(out)<- SIM$params$spname
  Biomass <- out; Biomass[names(Btmp)] <- Btmp
  Catch <- out;   Catch[names(Ctmp)] <- Ctmp
  return(data.frame(Biomass,Catch))
}
#################################################################################
#'@export
rsim.fit.obj.species <- function(SIM,RES,species=NULL){
  OBJ <- list()
  fitobj <- rsim.fit.obj(SIM,RES,verbose=T)
  OBJ$Biomass <- fitobj$Biomass[fitobj$Biomass$Group%in%species,] 
  OBJ$Catch   <- fitobj$Catch[fitobj$Catch$Group%in%species,]
  return(OBJ)
}

#################################################################################
#Internal Only
rsim.fit.apply <- function(values, species, vartype, scene.params){
  mzerodiff <- values[vartype=="mzero"]
  mzero.sp  <- species[vartype=="mzero"]
  
  predvuls <- values[vartype=="predvul"]
  names(predvuls) <- species[vartype=="predvul"]   
  preddiff <- as.numeric(predvuls[scene.params$spname[scene.params$PreyTo+1]])
  preddiff[is.na(preddiff)] <- 0
  
  preyvuls <- values[vartype=="preyvul"]
  names(preyvuls) <- species[vartype=="preyvul"]   
  preydiff <- as.numeric(preyvuls[scene.params$spname[scene.params$PreyFrom+1]])
  preydiff[is.na(preydiff)] <- 0
  
  scene.params$MzeroMort[mzero.sp] <- scene.params$MzeroMort[mzero.sp] + mzerodiff
  scene.params$VV <- (1 + exp(log(scene.params$VV-1) + preddiff + preydiff))
  
  return(scene.params)
}
#################################################################################
#'@export
rsim.fit.run <- function(values, species, vartype, scene, run_method, verbose=F, ...){
  scene$params <- rsim.fit.apply(values, species, vartype, scene$params)
  run.out <- rsim.run(scene, method=run_method, ...)
  if(!verbose){ return(rsim.fit.obj(scene, run.out, FALSE))}
  else{         return(run.out)}
}
#################################################################################
#'@export
rsim.fit.update <- function(values, species, vartype, scene){
  scene$params <- rsim.fit.apply(values, species, vartype, scene$params) 
  return(scene)
}

#################################################################################
test<-function(){
  
  #Group	Year	Value	SD	Scale
  
  # DATA from CATCH time series (Angie provided)   
  
  
  # APPLY FISHING TO FITTING
  SIM$fishing$EFFORT[]<-0
  colnames(SIM$fishing$CATCH)<-SIM$params$spname[1:(SIM$params$NUM_BIO+1)]
  #rownames(SIM$fishing$CATCH)<-c(years,end_year+1)
  SIM$fishing$CATCH[matrix(c(as.character(SIM$fitting$CATCH$year),as.character(SIM$fitting$CATCH$species)),
                           length(SIM$fitting$CATCH$year),2)] <- SIM$fitting$CATCH$obs 
  
  # diet composition  
  dfiles <- c("data/HMC_GOA_pollockdiet.csv","data/HMC_GOA_coddiet.csv","data/HMC_GOA_atfdiet.csv","data/HMC_GOA_halibutdiet.csv")
  dcdat <- read_diet_alphas(SIM,dfiles)
  SIM$fitting$diets <- dcdat[dcdat$year %in% years,]
  # total ration index
  qdat <- NULL
  for (f in dfiles){
    ddat <- read.csv(f)
    qdat <- rbind(qdat,data.frame(ddat$year,ddat$pred,ddat$cperwMean,ddat$cperwSD,rep(1,length(ddat[,1]))))
  }
  colnames(qdat)<-c("year","species","obs","sd","wt")
  SIM$fitting$ration<-qdat[qdat$year %in% years,]
  
  
}