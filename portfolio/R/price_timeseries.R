# Import and clean data in preparation for Portfolio Theory Analysis
# Public landings database used for prices until comlandr data is available
# M.T. Grezlik
# 08.21.24
# Objective: generate a time series of price by Rpath group for initial portfolio analysis

# LOAD PACKAGES ----
require(readxl)
require(tidyverse)
require(priceR) #for standardizing price with inflation
require(RColorBrewer)
require(viridis)
require(eeptools) #decomma function
require(openxlsx)
require(grDevices)
require(extrafont)
require(boot)
require(kernlab)
require(reshape)
require(matrixcalc)
require(corrplot)
require(here)


# public landings data ----
## call in data -------------
raw <- read_csv("portfolio/data_raw/FOSS_landings_NE_2021_Year_Species_State.csv")

raw <- raw |> 
            rename_with(~gsub(" ", ".", .x), everything())

## Data manipulation ----
## 1. Add Fishery Management Plan vector (including factor: "Other")
## 2. Create vector NMFS.Namesv2 where the skates are all grouped together as SKATE
## 3. Dollars standardized to 2021
## 4. Pounds converted to metric tons

raw<-raw |> 
  mutate(Pounds=decomma(Pounds),
         Metric.Tons=decomma(Metric.Tons),
         Dollars=decomma(Dollars),
         Metric.Tons=Pounds/2204.62,
         Terminal.Yr.Dollars = adjust_for_inflation(Dollars, from_date = Year,
                                                    country = "US", to_date = 2021),
         Management.Group = case_when(grepl("Gadus morhua", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Melanogrammus aeglefinus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Limanda ferruginea", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Pollachius virens", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Hippoglossoides platessoides", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Glyptocephalus cynoglossus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Urophycis tenuis", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Scophthalmus aquosus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Pseudopleuronectes americanus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Sebastes fasciatus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Hippoglossus hippoglossus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Anarhichas lupus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Zoarces", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Macrozoarces americanus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Merluccius bilinearis", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("SKATE", NMFS.Name, ignore.case = TRUE) ~"Northeast Skate Complex (NE)",
                                      grepl("Urophycis chuss", Scientific.Name, ignore.case = TRUE) ~"Small-Mesh Multispecies",
                                      grepl("Merluccius albidus", Scientific.Name, ignore.case = TRUE) ~"Small-Mesh Multispecies",
                                      grepl("Placopecten magellanicus", Scientific.Name, ignore.case = TRUE) ~"Atlantic Sea Scallop (NE)",
                                      grepl("Lophius americanus", Scientific.Name, ignore.case = TRUE) ~"Monkfish (NE/MA)",
                                      grepl("Chaceon quinquedens", Scientific.Name, ignore.case = TRUE) ~"Red Crab",
                                      grepl("Clupea harengus", Scientific.Name, ignore.case = TRUE) ~"Atlantic Herring (NE)",
                                      grepl("Squalus acanthias", Scientific.Name, ignore.case = TRUE) ~"Spiny Dogfish (NE/MA)",
                                      grepl("Salmo salar", Scientific.Name, ignore.case = TRUE) ~"Atlantic Salmon")) |> 
  mutate(Management.Group=replace_na(Management.Group, "Other")) |> 
  mutate(NMFS.Namev2=recode(NMFS.Name, "SKATE, ROSETTE"='SKATE',
                            "SKATE, BARNDOOR"='SKATE',
                            "SKATE, CLEARNOSE"='SKATE',
                            "SKATE, THORNY"='SKATE',
                            "SKATE, SMOOTH"='SKATE',
                            "SKATE, WINTER"='SKATE',
                            "SKATE, LITTLE"='SKATE',
                            "SKATES, RAJIDAE (FAMILY) **"="SKATE",
                            "LITTLE/WINTER SKATE MIX **"="SKATE"))  |> 
  dplyr::rename(Taxonkey=Tsn) |> 
  mutate_if(is.character,as.factor) #covert all character strings to factors

str(raw)
table(raw$Management.Group)

## 5. Filter to public data only
## 6. Remove seaweeds
## 7. Aggregate dataframe to ignore states (if wanted)

YrSps <- raw %>%
  filter(Confidentiality=="Public") %>%
  filter(!grepl("SEAWEED", NMFS.Name)) %>%
  dplyr::select(-c("State", "Source")) %>%
  aggregate(cbind(Terminal.Yr.Dollars, Dollars, Metric.Tons, Pounds) ~ ., sum)

YrSps_plus<- YrSps |> mutate(MgGp=as.numeric(Management.Group))  |> 
  mutate(Scaled.Value=Terminal.Yr.Dollars/max(Terminal.Yr.Dollars))

# Set scaling factor ----
range(YrSps_plus$Scaled.Value)
SCALING_F<-max(YrSps_plus$Terminal.Yr.Dollars) #Save your scaling factor to easily re-scale values back to original
SCALING_F

#set correct order of magnitude for revenue -----
OM<-1.0e8 


LB_SUM<-aggregate(cbind(Metric.Tons, Terminal.Yr.Dollars)~Year, YrSps_plus, sum) |> 
  dplyr::rename(SumCT=Metric.Tons,
                SumVAL=Terminal.Yr.Dollars)

data<-merge(YrSps_plus, LB_SUM, by=c("Year"))  |> 
  mutate(Price=Terminal.Yr.Dollars/Metric.Tons,
         MgGp=as.numeric(Management.Group))  |> 
  mutate(IYear=Year, #Naming for the function
         Catch=Metric.Tons,
         Value=Scaled.Value)

# Add Rpath names --------------------
load(here('data-raw', 'Species_codes.RData'))
spp <- spp |> dplyr::select(SCINAME,RPATH) |> dplyr::rename(Scientific.Name = SCINAME) |> dplyr::distinct()

data <- data |> dplyr::mutate(Scientific.Name = toupper(Scientific.Name)) |> dplyr::distinct()

data <- left_join(data,spp, by='Scientific.Name', relationship = "many-to-many")

# remove rows with NA for Rpath
data <- data |> filter(!is.na(RPATH))

# filter for Groundfish plus portfolio species
data <- data |> filter(RPATH %in% c('Cod', 'Haddock', 'YTFlounder', 'Pollock', 
                                    'AmPlaice', 'WitchFlounder', 'WhiteHake', 
                                    'Windowpane', 'WinterFlounder', 'Redfish', 
                                    'OceanPout', 'WinterSkate', 'LittleSkate', 
                                    'Barndoor', 'OtherSkates', 
                                    'Goosefish', 'SpinyDogfish')) |> 
                filter(Year >= 1985) |> 
                filter(Year <= 2019)


# replace landings and revenue with landings from Rsim and new landings*price

# source the latest Rsim fit
source(here('fitting/fitting.R')) # This takes a few minutes as its optimizing the Rsim model

# pull catch by species
rsim_catch <- scene0[["fitting"]][["Catch"]] |> 
                dplyr::select(Group, Year, Value) |> 
                dplyr::rename(RPATH = Group) |> 
                dplyr::rename(mt_km2 = Value) |> 
                dplyr::filter(RPATH %in% data$RPATH) |> 
                dplyr::mutate(Metric.Tons = mt_km2 * 37500) |> 
                dplyr::select(-mt_km2)
rsim_catch$Year <- as.numeric(rsim_catch$Year)

# remove data$Metric.Tons and replace it with rsim_catch$Metric.Tons
data <- data |> dplyr::select(-Metric.Tons) |> 
                left_join(rsim_catch, by=c('Year', 'RPATH'))

#repeat scaling with new values -----


LB_SUM<-aggregate(cbind(Metric.Tons, Terminal.Yr.Dollars)~Year, data, sum) |> 
  dplyr::rename(SumCT=Metric.Tons,
                SumVAL=Terminal.Yr.Dollars)

data <- data |> dplyr::select(-c(SumCT,SumVAL))

data<-merge(data, LB_SUM, by=c("Year"))  |> 
  mutate(Price=Terminal.Yr.Dollars/Metric.Tons,
         MgGp=as.numeric(Management.Group))  |> 
  mutate(IYear=Year, #Naming for the function
         Catch=Metric.Tons,
         Value=Scaled.Value)

# Barndoor, LittleSkate, WinterSkate, Windowpane have incomplete timeseries, filter out for now
data <- data |> filter(!RPATH %in% c('Barndoor', 'LittleSkate', 'WinterSkate','Windowpane'))

# plot price by year, facet wrap by species
data |> ggplot(aes(x=Year, y=Price, color=RPATH)) + 
  geom_line() + 
  facet_wrap(~RPATH, scales='free_y')
