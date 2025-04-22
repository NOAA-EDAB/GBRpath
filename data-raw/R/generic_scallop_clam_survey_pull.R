#Generic survey data pull
library(data.table); library(survdat); library(dbutils); library(here)

#Pull Data----
channel <- dbutils::connect_to_database(server="sole",uid="slucey")

clams <- survdat::get_survdat_clam_data(channel)

save(clams, file = here('data', 'survdatClams.RData'))

scallops <- survdat::get_survdat_scallop_data(channel, getWeightLength = T)

save(scallops, file = here::here('data', 'survdatScallops.RData'))

