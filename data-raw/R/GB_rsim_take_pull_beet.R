channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

# Define all EPUs
gom <- data.table::data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
gb <- data.table::data.table(
  AREA = c(521:526, 551, 552, 561, 562),
  EPU = 'GB'
)
mab <- data.table::data.table(
  AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
  EPU = 'MAB'
)
ss <- data.table::data.table(AREA = c(463:467, 511), EPU = 'SS')

epuAreas <- data.table::rbindlist(list(gom, gb, mab, ss))
epuAreas[, NESPP3 := 1]
epuAreas[, MeanProp := 1]

# Pull landings
landings <- comlandr::get_comland_data(channel, 
                                       filterByYear = 1981:2022 ,
                                       userAreas = epuAreas,
                                       aggGear = T,
                                       aggArea = T)
# Estimate discards
discards <- comlandr::get_comdisc_data(channel, 
                                       landings, 
                                       aggArea = T,
                                       aggGear = T)

# filter for GB
landings$comland |> 
  dplyr::filter(EPU == "GB")
discards$comdisc |> 
  dplyr::filter(EPU == "GB")

