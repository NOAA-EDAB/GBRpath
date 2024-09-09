# Title: Copepod Biomass Time Series
# Purpose: This script generates a time series of large and small copepod biomass to be
#           used for fitting the 1980-85 Gulf of Maine Rpath model to data.
#         
# Data files: (1) data/ZooData_GOM.csv, (2) data/EcoMon_Copepod size.csv from Harvey Walsh
#            (1) Ecomon data filtered for region, taxa of interest
#            (2) Parameters for converting abundance to biomass for taxa of interest
#
# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/copepods_time.R

#load packages
library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(dplyr) 
library(tidyverse)
library(zoo)
library(broom)
library(here)


world <- ne_countries(scale = "medium", returnclass = "sf")

#load data
plankton <- read.csv(here("data", "EcoMon_Plankton_Data_v3_8.csv"))
#convert data and create year, month, day columns: 
plankton$date <- as.Date(plankton$date, format =  "%d-%b-%y")
plankton$month <- month(plankton$date)
plankton$year <- year(plankton$date)
plankton$day <- day(plankton$date)

#filter for GB only
#combine with the strata dataset 
#load strata
strata <- read_sf(here("data", "EcomonStrata_v4b.shp"))
#set coordinate system
st_crs(strata) = 4326 #this is WGS 1984

#make ecomon spatial with the same defined coordinate system
plankton_sp <- st_as_sf(x = plankton, 
                        coords = c("lon", "lat"),
                        crs = 4326)

#extract stratum values to points
#get a weird error - need to follow these steps: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
plankton_extract <- st_intersection(plankton_sp, strata)
plankton_extract <- as.data.frame(plankton_extract)
plankton <- plankton_extract

#filter for GB only
plankton_extract <- plankton_extract  |>  filter(Region %in% c("GB"))
plankton <- plankton_extract

#create season column based on 2 month period
plankton$season <- ifelse(plankton$month == 1 | plankton$month == 2, "JanFeb", 
                          ifelse(plankton$month == 3 | plankton$month == 4, "MarApril",
                            ifelse(plankton$month == 5 | plankton$month == 6, 
                                   "MayJun",ifelse(plankton$month == 7 | plankton$month == 8, 
                                    "JulAug",ifelse(plankton$month == 9 | plankton$month == 10, 
                                     "SepOct",ifelse(plankton$month == 11 | plankton$month == 12, 
                                      "NovDec", NA))))))

#filter for copepods of interest
#tidy
copes_y<-plankton  |>  dplyr::select(ctyp_10m2,calfin_10m2,mlucens_10m2)
copes_x<-as.data.frame(cbind(plankton$year, plankton$season))
colnames(copes_x)<-c("Year","Season")
copes_y[is.na(copes_y)] <- 0 #NAs should be true 0s
copes <- cbind(copes_x, copes_y)

#pull out species names
copes_ynames <- colnames(copes_y)

#make long dataframe
copes_long <- copes  |>  pivot_longer(all_of(copes_ynames), names_to = "spp")

#rename spp
copes_long<- copes_long |> 
  mutate(spp_long=if_else(spp=="ctyp_10m2","CENTROPAGES_TYPICUS",
                          if_else(spp=="calfin_10m2","CALANUS_FINMARCHICUS", 
                                  if_else(spp=="mlucens_10m2","METRIDIA_LUCENS",NA))))

#filter for fit.years
copes_long <- copes_long |> 
                filter(Year %in% c(1980:2019))

#look at temporal coverage
cruises<-copes_long  |>  dplyr::select(Year,Season)  |>  distinct()

#average abundance per cruise
copes_cruise<-copes_long  |>  group_by(Year,Season,spp_long)  |>  dplyr::summarise(abd = mean(value))
#annual average (across cruises)
copes_annual<-copes_cruise |> group_by(Year,spp_long)  |>  dplyr::summarise(abd=mean(abd))
#average across fit.years
copes_avg<-copes_annual |> group_by(spp_long)  |>  dplyr::summarise(abd=mean(abd))

#convert to biomass
sizes<-read.csv(here("data/EcoMon_Copepod size.csv"))
sizes<-sizes[-1,]
sizes<-sizes  |>  dplyr::select(c("X","Literature"))
colnames(sizes)<-c("spp_long","length")
sizes$length<-as.numeric(sizes$length)
#use l-w conversion from EMAX (wet weight)
sizes$weight<-0.0881*sizes$length^2.8514

#join back with abd density estimates
#join back with abd estimates
copes_annual<-left_join(copes_annual,sizes,by="spp_long")
#multiply ind * mg/ind
copes_annual$biomass<-copes_annual$abd*copes_annual$weight

#classify as sm/lg
copes_annual$class<-ifelse(copes_annual$spp_long == "CALANUS_FINMARCHICUS", "lg","sm")
ts<-copes_annual |> 
        group_by(class,Year)  |> 
          dplyr::summarise(biomass = sum(biomass))

# #scale to starting values (from initial balanced model)
sm_start<-1.492115e+01
lg_start<- 7.073332e+00
cope_start<-as.data.frame(cbind(c("lg","sm"),c(lg_start,sm_start)))
colnames(cope_start)<-c("class","biomass_start")
cope_start$biomass_start<-as.numeric(cope_start$biomass_start)

initial_biomass<- ts  |> 
                    filter(Year<=1986)  |> 
                        group_by(class)  |> 
                          dplyr::summarise(avg_80_85 = mean(biomass))

ts<-left_join(ts,initial_biomass,by="class")
ts<-left_join(ts,cope_start,by="class")
ts<- ts |> 
        filter(Year>1986 & Year<2020)  |> 
          mutate(force_b = biomass/avg_80_85*biomass_start)

#calculate anomalies
ts<-ts  |>  
      group_by(class)  |>  
          mutate(mean = mean(force_b), anomaly = (force_b - mean)/mean)

#plot
ts$Year<-as.numeric(ts$Year)
P<-ts %>%
  ggplot(aes(x = Year, y = anomaly)) +
  geom_line(aes(color = class)) 
P

lg<-ts  |>  filter(class == "lg")  |>  ungroup()
sm<-ts  |>  filter(class == "sm") |>  ungroup()

# #compare results with top 4 taxa of sm grouping -- used in SOE reporting
# sm_new <- copes_annual %>% filter(spp_long %in% c("CENTROPAGES_HAMATUS","CENTROPAGES_TYPICUS","TEMORA_LONGICORNIS","PSEUDOCALANUS_SPP"))
# sm_new<-sm_new %>% group_by(Year) %>% dplyr::summarise(biomass = sum(biomass))
# sm_new_initial <- sm_new %>%
#   filter(Year<=1986) %>% dplyr::summarise(avg_80_85 = mean(biomass))
# sm_new <- sm_new %>% mutate(avg_80_85 = sm_new_initial$avg_80_85)
# sm_new<- sm_new %>% filter(Year>1986 & Year<2020) %>% mutate(force_b = biomass/avg_80_85*sm_start)%>%
#   mutate(mean = mean(force_b), anomaly = (force_b - mean)/mean) %>% mutate(class="sm_new")
# sm_new$Year <- as.numeric(sm_new$Year)
# 
# ggplot()+
#   geom_line(data=sm,aes(x = Year, y = anomaly),color="blue")+
#   geom_line(data=sm_new,aes(x = Year, y = anomaly),color="magenta")
