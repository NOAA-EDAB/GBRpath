#Title: GOM Rpath Discards Estimates

# Purpose: To estimate discards (t/km^2) for functional groups in GB RPATH model.
#       Based on data from NOAA observer program since 1989. 

# Note: Data availability is mixed. To deal with this, I first calculate which group/gear combinations
#       have discards < 10^-5 t/km^2, and round these down to 0. For group/gear 
#       combinations > 10^-5 t/km^2, I average discard:kept (DK) ratios over the first
#       five years of available data and apply this ratio to landings biomass to estimate
#       discards. For groups where reported DK is highly variable (sd>mean), I average
#       over first 10 years of available data rather than first 5 years.

# DataFile:'observer_data.RData'

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Fri Aug  2 17:32:58 2024 ------------------------------

#Note: need to come back and think more about megabenthos

library(here);library(tidyr);library(data.table);library(dplyr)

#Load observer data
load(here("data-raw/observer_data.RData"))
#load species codes
load(here("data-raw/Species_codes.RData"))

#Change "HMS" to "HMSFleet" to avoid confusion
ob.all[FLEET =="HMS",FLEET:="HMSFleet"]

load(here('data', 'land.input.rda'))

#change case of fleet column
land.input <- land.input %>% rename(FLEET = Fleet)

#Load landings data
#source("R/landings_conversion.R")

#Filter observer data for GOM only
#remove EPU column
ob_gb<-filter(ob.all, EPU == "GB")
ob_gb<-ob_gb %>% dplyr::select(-EPU)

#Merge filtered observer data with species codes
ob_gb$NESPP3<-as.numeric(ob_gb$NESPP3)
ob_gb<-left_join(ob_gb,spp,by="NESPP3")

#Remove NAs
ob_gb<-na.exclude(ob_gb)

#Remove NESPP3 column
#ob_gom<-ob_gom[,-3]

#Because of the many:1 relationship of NESPP3:RPATH, will have some redundancy
#Try to average over this
#Could come back and try to weight the multispecies groups by biomass but that is 
# probably overkill
ob_gb<-ob_gb %>% 
  group_by(YEAR,FLEET,RPATH) %>% 
  summarise(DK = mean(DK))


#Find DK ratios for first available year for every gear/group combination
firsts<-ob_gb %>% 
  group_by(FLEET,RPATH) %>% 
  mutate(first_YEAR = min(YEAR)) %>%
  ungroup() %>%
  filter(YEAR==first_YEAR)

#remove duplicate year column
#Rename DK column
firsts<-firsts[,-1]
names(firsts)[3]<-"first_DK"
firsts<-unique(firsts)

#Merge with mean landings
#Exclude NAs
firsts<-merge(firsts,land.input,by=c("RPATH","FLEET"))

#Multiply landings by DK
firsts$discards<-firsts$Landings*firsts$first_DK

#Exclude anything less than 10^-5
firsts$discards<-as.numeric(firsts$discards)
firsts<-subset(firsts, discards > 1e-05)

#Merge top discards with all DK ratios
top_discards<-merge(firsts,ob_gb,by=c("RPATH","FLEET"))

#Average DK for first five years for each of the gear/group combos
temp<-c()
for (i in 1:length(firsts$discards)){
  temp<-subset(top_discards,firsts$RPATH[i] == top_discards$RPATH & firsts$FLEET[i] == top_discards$FLEET)
  temp<-subset(temp,YEAR<=first_YEAR+4)
  firsts$five_means[i]<-mean(temp$DK)
  firsts$cv[i]<-mean(temp$DK)/sd(temp$DK)
}

#Separate into low and high variability (DK)
#Cut-off is mean:sd (=cv) >1
low_var<-subset(firsts,cv<=1 | is.na(cv)==T)
low_var<-low_var %>%
  select(RPATH,FLEET,five_means)
names(low_var)[3]<-'DK'

high_var<-subset(firsts,cv>1)

#For high variability group, average DK over first 10 years
temp<-c()
for (i in 1:length(high_var$discards)){
  temp<-subset(top_discards,high_var$RPATH[i] == top_discards$RPATH & high_var$FLEET[i] == top_discards$FLEET)
  temp<-subset(temp,YEAR<=first_YEAR+9)
  high_var$ten_mean[i]<-mean(temp$DK)
}

#Subset high variability
high_var<-high_var %>%
  select(RPATH,FLEET,ten_mean)
names(high_var)[3]<-'DK'

#Put it all together for model discard values
discards<-firsts %>%
  select(RPATH,FLEET,Landings)

discards<-left_join(discards,low_var,by=c("RPATH","FLEET"))
discards<-left_join(discards,high_var,by=c("RPATH","FLEET"))
discards[is.na(discards)]<-0

discards$DK<-discards$DK.x+discards$DK.y
discards<-discards %>% dplyr::select(-DK.x,-DK.y)
discards$discards<-discards$Landings*discards$DK
#discards<-discards[-c(3,4)]

# #Separate each fleet into its own data frame
# fixed.d<-filter(discards,FLEET == "Fixed Gear")
# lg_mesh.d<-filter(discards,FLEET == "LG Mesh")
# other.d<-filter(discards,FLEET == "Other")
# sm_mesh.d<-filter(discards,FLEET == "SM Mesh")
# #scallop.d<-filter(discards,FLEET == "Scallop Dredge")
# trap.d<-filter(discards,FLEET == "Trap")
# hms.d<-filter(discards,FLEET == "HMS Fleet")
# pelagic.d<-filter(discards,FLEET == "Pelagic")
# other_dredge.d<-filter(discards,FLEET == "Other Dredge")
# 
# #Combine with all groups
# #Fill in zeros for groups not commercially exploited
# #Repeat for each gear type
# #Start with fixed gear
# fixed.d<-left_join(GOM.groups,fixed.d,by="RPATH")
# fixed.d$FLEET<-"Fixed Gear"
# fixed.d[is.na(fixed.d)]<-0
# 
# #Large mesh
# lg_mesh.d<-left_join(GOM.groups,lg_mesh.d,by="RPATH")
# lg_mesh.d$FLEET<-"LG Mesh"
# lg_mesh.d[is.na(lg_mesh.d)]<-0
# 
# #Other
# other.d<-left_join(GOM.groups,other.d,by="RPATH")
# other.d$FLEET<-"Other"
# other.d[is.na(other.d)]<-0
# 
# #Small mesh
# sm_mesh.d<-left_join(GOM.groups,sm_mesh.d,by="RPATH")
# sm_mesh.d$FLEET<-"SM Mesh"
# sm_mesh.d[is.na(sm_mesh.d)]<-0
# 
# #Scallop
# #Ignoring because I've removed this fishery from the model (because there is no catch)
# #scallop.d<-left_join(GOM.groups,scallop.d,by="RPATH")
# #scallop.d$FLEET<-"Scallop"
# #scallop.d[is.na(scallop.d)]<-0
# 
# #Trap
# trap.d<-left_join(GOM.groups,trap.d,by="RPATH")
# trap.d$FLEET<-"Trap"
# trap.d[is.na(trap.d)]<-0
# 
# #HMS
# hms.d<-left_join(GOM.groups,hms.d,by="RPATH")
# hms.d$FLEET<-"HMS Fleet"
# hms.d[is.na(hms.d)]<-0
# 
# #Pelagic
# pelagic.d<-left_join(GOM.groups,pelagic.d,by="RPATH")
# pelagic.d$FLEET<-"Pelagic"
# pelagic.d[is.na(pelagic.d)]<-0
# 
# #Other dredge
# other_dredge.d<-left_join(GOM.groups,other_dredge.d,by="RPATH")
# other_dredge.d$FLEET<-"Other Dredge"
# other_dredge.d[is.na(other_dredge.d)]<-0
# 
# rm(high_var,low_var,temp,top_discards,ob.all,ob_gom,firsts)



