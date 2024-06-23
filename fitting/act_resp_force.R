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
cons_groups<- spp  |> 
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
