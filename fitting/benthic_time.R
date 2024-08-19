# Benthic time series
# Author: M.T.Grezlik
# mgrezlik@umassd.edu
# Following Sarah Weisberg's approach for the GOM
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/benthic_time.R

# Load packages and data --------------------------------

#load packages
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)

#Load balanced model
load(here("data/alternate.GB.bal.rda"))
load(here("data/alternate.GB.params.bal.rda"))

#macrobenthos 
macro <- readRDS(url('https://github.com/NOAA-EDAB/Rpathdata/blob/dd034d1573f79ce011c01054bdc017e241e7857e/data-raw/fallmacrobenthosindex.rds?raw=true'))


#megabenthos
mega <-readRDS(url("https://github.com/NOAA-EDAB/Rpathdata/blob/main/data-raw/fallmegabenthosindex.rds?raw=true"))

# Data processing --------------------------------

#filter for GB only, remove unneeded column, relabel biomass, SE for simplicity
macro_gb <- macro |>  
  filter(EPU == "GB") |> 
  select(-Units)  |> 
  mutate(Var = ifelse(Var == "Fall Macrobenthos Biomass Index Estimate","biomass","SE"))
mega_gb <- mega  |>  
  filter(EPU == "GB") |> 
  select(-Units) |> 
  mutate(Var = ifelse(Var == "Fall Megabenthos Biomass Index Estimate","biomass","SE"))

#calculate 1980-85 mean
macro_start <- macro_gb |>  
  filter(Time < 1986) |> 
  group_by(Var) |> 
  summarise(mean_start = mean(Value))
mega_start <- mega_gb |>  
  filter(Time < 1986) |>  
  group_by(Var) |> 
  summarise(mean_start = mean(Value))

#merge with time series, calculate anomalies
macro_gb <- left_join(macro_gb,macro_start,by="Var")
macro_gb <- macro_gb |>  mutate(anom = (Value-mean_start)/mean_start)
mega_gb <- left_join(mega_gb,mega_start,by="Var")
mega_gb <- mega_gb |>  mutate(anom = (Value-mean_start)/mean_start)

#generate forcing time series
macro<-alternate.GB.params.bal$model[Group == "Macrobenthos",Biomass]
# mega biomass set by model with EE = 0.8
# putting in manually for now
mega<-2.393409e+00

macro_time<-macro_gb |>  
  filter(Var == "biomass" & Time > 1984) |>  
  select(Time,anom) |> 
  mutate(biomass = macro*(anom+1))

mega_time<-mega_gb |>  
  filter(Var == "biomass" & Time > 1984)  |>  
  select(Time,anom)  |> 
  mutate(biomass = mega*(anom+1))

# #visualize
# mega_gb |>  filter(Var == "biomass" & Time > 1984)  |>
#   ggplot(aes(x=Time,y=anom))+
#   geom_line()+
#   geom_point()+
#   geom_smooth(method = "loess")
