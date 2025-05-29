# Title: ChlA Biomass Time Series
# Purpose: This script generates a time series of phytoplankton biomass to be
#           used for fitting the 1980-85 Gulf of Maine Rpath model to data.
#           OCCCI satellite data available from 1998 on.
# Data file: "OCCCI-V6.0-L3B4-SUBAREAS_EXTRACT.CSV" courtesy of Kim Hyde

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/phyto_time_ecodata.R

# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)
library(here)
library(RCurl)

# input files 
curlSetOpt(timeout=200)
ppd_csv <- read.csv("https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/19980101_20221231-OCCCI_GLOBCOLOUR-PPD-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv?raw=TRUE")
chl_csv <- read.csv("https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/19980101_20221231-OCCCI_GLOBCOLOUR-CHLOR_A-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv?raw=TRUE")

# transformation ----

ppd<- ppd_csv  |> 
        dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA)  |> 
        dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                      EPU =SUBAREA, Value = VALUE)

# filter for just GB
ppd_gb <- ppd |> dplyr::filter(EPU == "GB")

# Make numeric column for year, get rid of weekly estimates
ppd_gb$Year<- as.numeric(str_remove(ppd_gb$Time,"A_"))
ppd_gb <- na.omit(ppd_gb)

#Make sure values are numeric
ppd_gb$Value<-as.numeric(ppd_gb$Value)

#Turn Var colum into useful categories - anomaly vs. median
ppd_gb$Var<-str_remove(ppd_gb$Var,"ANNUAL_PPD_")

#Plot both median and anomaly
# ggplot(data = ppd_gb,aes(x=Year, y=Value))+
#   geom_line()+
#   stat_smooth(method = "lm")+
#   facet_wrap(~Var, scales = "free")

#Plot just anomalies
# ppd_gb %>% filter(Var == "RATIO_ANOMALY") %>%
#   ggplot(aes(x=Year, y=Value))+
#   geom_point()


chl <- chl_csv  |> 
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA)  |> 
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU = SUBAREA, Value = VALUE)

#chl_pp <- rbind(ppd,chl)%>%
# tibble::as_tibble() %>%
#  dplyr::select(Time, Var, Value, EPU, Units)

#Filter for just GoM
chl_gb<-chl |> 
          filter(EPU == "GB")

#Make numeric column for year, get rid of weekly estimates
chl_gb$Year<-as.numeric(str_remove(chl_gb$Time,"A_"))
chl_gb<-na.omit(chl_gb)

#Make sure values are numeric
chl_gb$Value<-as.numeric(chl_gb$Value)

#Turn Var colum into useful categories - anomaly vs. median
chl_gb$Var<-str_remove(chl_gb$Var,"ANNUAL_CHLOR_A_")

#Plot both median and anomaly
ggplot(data = chl_gb,aes(x=Year, y=Value))+
  geom_line()+
  facet_wrap(~Var, scales = "free")

#Plot just anomalies
# chl_gb %>% filter(Var == "RATIO_ANOMALY") %>%
#   ggplot(aes(x=Year, y=Value))+
#   geom_point()

# lm<-lm(Value~Year, data = chl_gb, subset = (Var == "RATIO_ANOMALY"))
# summary(lm)

#set baselines
#prior to 2001, which is years included in EMAX estimates
#start with ppd
ppd_EMAX<-ppd_gb |> 
  group_by(Var) |> 
  filter(Year <2001) |> 
  summarise(mean=mean(Value))

ppd_gb <- left_join(ppd_gb,ppd_EMAX, by="Var")

ppd_gb$anom<-(ppd_gb$Value-ppd_gb$mean)/ppd_gb$mean

#plot just after 2000
# ppd_gb  |> 
#   filter(Year >2000, Var == "MEDIAN") |> 
# ggplot(aes(x=Year, y=anom))+
#   geom_line()+
#   #geom_line(aes(y=zoo::rollmean(anom,3,na.pad = T)))+
#   #facet_wrap(~Var)+
#   labs(y="Production Anomaly (gC/m^2/day)")+
#   stat_smooth(method = "lm",col="red")

#repeat with chl
chl_EMAX<-chl_gb |> 
  group_by(Var) |> 
  filter(Year <2001)  |> 
  summarise(mean=mean(Value))

chl_gb <- left_join(chl_gb, chl_EMAX, by="Var")
#chl_gb$anom <- (chl_gb$Value-chl_gb$mean)/chl_gb$mean

#rescale to starting biomass value
pp_start<-1.977300e+01
pp_force<-chl_gb  |> 
  filter(Year>=2001,Var == "MEDIAN")  |>  
  mutate(force_b = Value/mean * pp_start)  |> 
  dplyr::select(Year,force_b)

#add previous years
# pre<-cbind(seq(1985,2000),rep(pp_start,16))
# colnames(pre)<-c("Year","force_b")
# pp_force<-rbind(pre,pp_force)

#plot just after 2003
# chl_gb |> 
#   filter(Year >2000, Var == "MEDIAN") |> 
#   ggplot(aes(x=Year, y=anom))+
#   geom_line()+
#   #geom_line(aes(y=zoo::rollmean(anom,3,na.pad = T)))+
#  #facet_wrap(~Var, scales = "free")+
#   labs(y="ChlA Anomaly (mg/m^3)")+
#   stat_smooth(method = "lm",col="red")
