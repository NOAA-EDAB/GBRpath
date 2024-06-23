# Title: ChlA Biomass Time Series
# Purpose: This script generates a time series of phytoplankton biomass to be
#           used for fitting the 1980-85 Gulf of Maine Rpath model to data.
#           OCCCI satellite data available from 1998 on.
# Data file: "OCCCI-V6.0-L3B4-SUBAREAS_EXTRACT.CSV" courtesy of Kim Hyde

# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/Phyto_time.R

# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)
library(here)

# input files 
chl_csv <- read.csv(here("data/OCCCI-V6.0-L3B4-SUBAREAS_EXTRACT.CSV"))

#isolate years, geometric means
chl<-chl_csv %>% dplyr::select(PERIOD, GMEAN) %>%
  dplyr::rename(Year = PERIOD, Value = GMEAN)

#remove total mean
chl<-chl %>% filter(Year != "ANNUAL_1998_2022")


#Make numeric column for year, get rid of weekly estimates
chl$Year<-as.numeric(str_remove(chl$Year,"A_"))

#Plot
ggplot(data = chl,aes(x=Year, y=Value))+
  geom_line()

#set baselines
#prior to 2001, which is years included in EMAX estimates
#start with ppd
chl_EMAX<-chl %>% filter(Year < 2001) %>% summarise(mean=mean(Value))

chl$EMAX_mean<-rep(chl_EMAX$mean, length(chl$Year))

chl<-chl %>% mutate (anom = (Value-EMAX_mean)/EMAX_mean)

#rescale to starting biomass value
pp_start<-22.126
pp_force<-chl %>% 
  mutate(force_b = Value/EMAX_mean * pp_start) %>% dplyr::select(Year,force_b)

#plot
ggplot(data = pp_force,aes(x=Year, y=force_b))+
  geom_line()