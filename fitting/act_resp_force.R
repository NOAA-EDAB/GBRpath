#Incorporating temp-dependent P and Q

#Data files: "data/bot_temp_GB.csv" - Bottom temperature of GOM from ecodata
#             "data/Fitting_Inputs_Bioen.csv" - Bioenergetic parameters from literature reviews

# Data from https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/bot_temp_GB.csv
# Data processing code from https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/get_bottom_temp.R

#first need a temperature time series
#code from ecodata
library(ecodata)
# Process ocean temperature anomaly data

#load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)
library(data.table)
library(survdat)
library(ggplot2)
library(Rpath)

# call in data -----------------------------------------------------------------------------------------
bottom_temp_GB_csv<-read.csv(here("data/bot_temp_GB.csv")) 
temp<- bottom_temp_GB_csv |> dplyr::mutate(Time = as.Date(format(lubridate::date_decimal(Time))),
                                             Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",#Rename variables
                                                                                      "Tsfc_ref",
                                                                                      "Tbot_anom",
                                                                                      "Tbot_ref"),
                                                                        to = c("sst_anomaly",
                                                                               "sst",
                                                                               "bt_anomaly",
                                                                               "bt")))


#isolate mean bottom temp --------------------------------------------------------------
mean_bt<-as.numeric(temp  |>  
                      filter(Var == "bt")  |>  
                      distinct(Value))

#use mean + anomaly to get time series ---------------------------------------------------
bottom_temp <- temp  |>  
                  filter(Var == "bt_anomaly")  |>  
                  dplyr::group_by(Time = lubridate::year(Time),Var)  |> 
                  dplyr::summarise(Value = mean(Value))  |> 
                  dplyr::summarise(Value = Value+mean_bt)

ggplot(bottom_temp,aes(x=Time,y=Value))+
  geom_line()

#let's start easy - respiration curves
resp<-function(temp_c){
  temp_k<-temp_c+273
  tau<-exp(25.55-0.63/(8.62*10^-5*temp_k))
  return(tau)
}

#make this relative to start period of model
#using bottom temperature
start_temp<-bottom_temp %>% filter(Time<=1985 & Time >=1980)
start_temp<-mean(start_temp$Value)
rel_resp<-function(temp_c){
  rel_tau<-resp(temp_c = temp_c)/resp(start_temp)
  return(rel_tau)
}

#consumption function
rc<-function(Tmax,Topt,Q10,Temp){
  x<-((log(Q10)*(Tmax-Topt))^2/400)*(1+(1+(40/(log(Q10)*(Tmax-Topt+2)))^0.5)^2)
  rc<-((Tmax-Temp)/(Tmax-Topt))^x*exp(x*(1-((Tmax-Temp)/(Tmax-Topt))))
  return(rc)
}

#Pull Tmax_survey from survey data
#Load survey data
load(here('data/NEFSC_BTS_2021_all_seasons.RData'))
#load species codes
load(here('data-raw/Species_codes.Rdata'))

#pull out fish groups chosen for variable bioenergetics ---------------------------------------------------

#remove duplicate NESPP3 codes
GB.groups<-distinct(spp,RPATH,.keep_all=TRUE)

cons_groups<- GB.groups  |> 
                        dplyr::filter(RPATH %in% c('OceanPout',
                                                       'RiverHerring',
                                                       'AtlHerring','YTFlounder','Cod',
                                                       'Haddock','AmPlaice','AtlMackerel','AtlHalibut',
                                                       'SummerFlounder', 
                                                       #'Cusk',
                                                       'RedHake','Fourspot',
                                                       'SmoothDogfish','Pollock','Goosefish','SilverHake',
                                                       'WhiteHake','SpinyDogfish','Redfish',
                                                       'Windowpane','WinterFlounder',
                                                       'WitchFlounder','BlackSeaBass','Butterfish'))


survdat<-survey$survdat
survdat<-left_join(survdat,spp, by = "SVSPP", relationship = "many-to-many")

#Load balanced model
load(here("data/GB.bal.rda"))
load(here("data/GB.params.bal.rda"))

GB <- GB.bal 
GB.params <- GB.params.bal

#Run Rsim to get initial resp_fraction
GB.sim<- rsim.scenario(GB, GB.params, years = 1985:2019)

#write a loop to pull max observed temp
bt_max_survey<-c()
for (i in 1: length(cons_groups$RPATH)){
  group<-cons_groups$RPATH[i]
  test<-survdat %>% dplyr::filter(RPATH == group)
  bt_max_survey[i]<-max(na.omit(test$BOTTEMP))
}
bt_max_survey <- as.data.frame(cbind(cons_groups,bt_max_survey))

#load data from literature review
bioen<-read.csv(here("data/Fitting_Inputs_Bioen.csv"))
#make empty data frame
cons<-data.frame(matrix(ncol=4,nrow = length((cons_groups$RPATH))))
colnames(cons)<-c("RPATH","Tmax","Topt","Q10")

for (i in 1: length(cons_groups$RPATH)){
  group<-cons_groups$RPATH[i]
  test<-bioen %>% dplyr::filter(Group == group)
  Tmax<- test %>% dplyr::select(Tmax_lit,Tmax_BTS,Tmax_Aquamaps)
  Tmax<-max(Tmax,na.rm = T) #pick out max of Tmax (from literature, aquamaps, survey)
  Topt<-na.omit(test$Topt)
  Topt_growth <- na.omit(test$Topt_growth)
  if(length(Topt) == 1){
    Topt<- Topt
  } else if (length(Topt) > 1){
    Topt<-mean(Topt)
  } else if (length(Topt_growth) >0){
    Topt <- (mean(Topt_growth)+Tmax)/2
  } else {
    Topt<-0.9*Tmax
  }
  Q10<-na.omit(test$Q10)
  Q10<-ifelse(length(Q10)>0,mean(Q10),2.3)
  cons[i,]<-c(group,Tmax,Topt,Q10)
}

cons$Tmax<-as.numeric(cons$Tmax)
cons$Topt<-as.numeric(cons$Topt)
cons$Q10<-as.numeric(cons$Q10)

#need index from starting model to get QB, B
#calculate starting rc
cons <- cons %>% mutate(index = which(cons_groups$RPATH %in% GB.groups$RPATH),
                        start_rc = rc(Tmax = Tmax,Topt = Topt,Q10=Q10,Temp=start_temp))

#change lobster-specific values
#cons <- cons %>% mutate(bt_opt = replace(bt_opt, RPATH == "AmLobster", 20)) %>%
# mutate(q10 = replace(q10, RPATH == "AmLobster", 1.8))#lobster specific values

cons <- cons %>% mutate(qb = GB$QB[cons$index],
                        biomass = GB$Biomass[cons$index],
                        act_start = GB.sim$params$ActiveRespFrac[cons$index+1], #because of outside
                        cons_start = qb*biomass,
                        resp_start = cons_start*act_start)

#params over time
#first few years are missing from temp data
bottom_temp<-bottom_temp %>% filter(Time >= 1991 & Time <=2019)
cons_time<-c()
for (i in 1:length(cons$RPATH)) {
  group<-cons[i,]
  cons_time_group<-bottom_temp %>%  mutate(rc = rc(Tmax = group$Tmax,Topt = group$Topt,Q10=group$Q10,Temp=bottom_temp$Value), 
                                           rel_rc = rc/group$start_rc,
                                           cons = rel_rc*group$qb*group$biomass,
                                           rel_resp=rel_resp(temp_c = Value),
                                           resp=rel_resp*group$resp_start,
                                           reL_act_resp=resp/(group$act_start*cons),
                                           RPATH = group$RPATH,
                                           index = group$index)
  cons_time<-rbind(cons_time,cons_time_group)
}

#plug into forcing
for (i in 1:length(cons$RPATH)){
  group<-cons_groups$RPATH[i]
  cons_time_group<- cons_time %>% filter (RPATH == group)
  index<-as.numeric(unique(cons_time_group$index)) + 1 #add 1 because of Outside
  force_act_resp <- c(rep(1,6*12),rep(cons_time_group$reL_act_resp,each=12))
  #adjust forcing
  scene0$forcing$ForcedActresp[,index]<-force_act_resp
}

GB.sim$params$NoIntegrate[57:58]<-0

#run
GB.run <- rsim.run(GB.sim, method = 'AB', years = fit.years)


# #plot
rsim.plot(GB.run)

# all combined
fit_values   <- c(rep(0,length(test_sp)),rep(0,length(test_sp)),rep(0,length(test_sp))) 
fit_values   <- c(rep(0.2,length(test_sp)),rep(0.2,length(test_sp)),rep(0.2,length(test_sp)))
fit_species  <- c(test_sp,test_sp,test_sp)
fit_vartype  <- c(rep("mzero",length(test_sp)),
                  rep("predvul",length(test_sp)),
                  rep("preyvul",length(test_sp)))

#Initial fit
fit.initial  <- rsim.fit.run(fit_values, fit_species, fit_vartype, scene0, verbose=T,
                             run_method='AB', years=fit.years)
for (i in 1:length(test_sp)){
  rsim.plot.biomass(scene0, fit.initial, test_sp[i])
  rsim.plot.catch(scene0, fit.initial, test_sp[i])
}
