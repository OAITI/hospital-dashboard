# Hospital Compare data was last updated on Mar 21, 2019.
# Source: https://data.medicare.gov/data/hospital-compare

library(tidyverse)
library(usmap)
hospitals <- read_csv("data/Hospital General Information.csv")

hospital_by_typer <- hospitals %>%
    group_by(State, `Hospital Type`, `Hospital overall rating`) %>% 
    summarise(Count = n()) %>%
    #adding state pop
    left_join(statepop, by = c("State" = "abbr"))

#write_csv(hospital_by_typer, "hospital_by_state.csv")

library(highcharter)

# state maps - fl
# https://code.highcharts.com/mapdata/countries/us/us-fl-all.js

mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/us/us-all.js"))

hospital_by_state  <- hospital_by_typer %>%
    group_by(State, pop_2015) %>%
    summarise(Count = sum(Count)) %>%
    filter(!is.na(pop_2015)) %>%
    mutate(HPP = (Count*100000)/pop_2015)

hcmap("https://code.highcharts.com/mapdata/countries/us/us-all.js", data = hospital_by_state, value = "HPP",
      name = "Hospitals per 100k", joinBy = c("hc-a2", "State"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "grey", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)) 

mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/us/us-all-all.js"))
#county pop
cpop <- usmap::countypop %>%
    right_join(select(mapdata, fips, name), by = c("fips")) %>%
    filter(!is.na(fips))

# zip to fip mapping from https://data.world/niccolley/us-zipcode-to-county-state
zip_fips <- read_csv("data/ZIP-COUNTY-FIPS_2018-03.csv") %>%
    mutate(ZIPchar = as.character(ZIP), ziplength = nchar(ZIPchar),
           ZIP = ifelse(ziplength == 3, paste0("00",ZIPchar), 
                        ifelse(ziplength == 4, paste0("0",ZIPchar),
                               ZIPchar)))
test=zip_fips %>%
 group_by(ZIP) %>%
 summarise(count = length(unique(STCOUNTYFP))) %>%
 arrange(desc(count))
testt=filter(hospitals, `ZIP Code` %in% t$ZIP[t$count !=1])
# many zipcodes in multiple fips so we take the first fip/county so that its not double counted for the state
# (make 1-1 mapping for county to zip, even though it isn't)
zip_fips11 <- zip_fips %>%
    group_by(ZIP) %>%
    slice(1)
# adding fips to hospitals data so we can combine pop with it
hospitals_county <- hospitals %>%
    select(`Provider ID`, `ZIP Code`, State) %>%
    left_join(select(zip_fips11, ZIP, fips = STCOUNTYFP), by = c(`ZIP Code` = "ZIP")) %>%
    left_join(select(cpop, fips, pop_2015), by = "fips")
agg_HPP <- hospitals_county %>%
    group_by(fips, pop_2015, State) %>%
    summarise(Count = n()) %>%
    filter(!is.na(pop_2015)) %>%
    mutate(HPP = (Count*10000)/pop_2015)
    
hcmap("https://code.highcharts.com/mapdata/countries/us/us-all-all.js", data = cpop, value = "pop_2015",
      name = "Population(2015)", joinBy = c("fips"),
      dataLabels = list(enabled = FALSE, format = '{point.name}', color = "black"),
      borderColor = "black", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0))
hcmap("https://code.highcharts.com/mapdata/countries/us/us-all-all.js", data = agg_HPP, value = "HPP",
      name = "Hospitals per 10k", joinBy = c("fips"),
      dataLabels = list(enabled = FALSE, format = '{point.name}', color = "black"),
      borderColor = "black", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2))

hospital_by_state$drilldown <- hospital_by_state$State
agg_HPP$id <- agg_HPP$State

county_data <- agg_HPP %>%
  group_by(id) %>%
  nest() %>%
  list_parse()
# county_data <- list(
#   id = agg_HPP$State,
#   data = list_parse(agg_HPP)
# )

hcmap("https://code.highcharts.com/mapdata/countries/us/us-all.js", data = hospital_by_state, value = "HPP",
      name = "Hospitals per 100k", joinBy = c("hc-a2", "State"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "grey", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = county_data,
    data = mapdata,
    map = "https://code.highcharts.com/mapdata/countries/us/us-all-all.js",
    joinBy = c("fips")
  )

# To verify
library(jsonlite)
x=toJSON(ss$x$hc_opts, pretty = TRUE, auto_unbox = TRUE)

################################
# Number of Beds -- NOTUSING this as data is probably wrong
library(rvest)
#url <- "https://www.ahd.com/state_statistics.html"
# hosp_stats <- read_html(url) %>%
#      html_nodes(".report table") %>%
#      html_table() 
# stat_table <- hosp_stats %>%
#      .[[1]]
# write_csv(stat_table, "hosp_stats.csv")
stat_table <- read_csv("data/hosp_stats.csv") %>%
    separate(State, sep = " - ", into = c("State", "Name")) %>%
    left_join(statepop, by = c("State" = "abbr")) %>%
    mutate(HPP = (NumberHospitals*100000)/pop_2015, BPP = (StaffedBeds*100000)/pop_2015, 
           BPH = StaffedBeds/NumberHospitals)

hcmap("https://code.highcharts.com/mapdata/countries/us/us-all.js", data = stat_table, value = "HPP",
      name = "Hospitals per 100k", joinBy = c("hc-a2", "State"),
      dataLabels = list(enabled = TRUE, format = '{point.name}', color = "black"),
      borderColor = "grey", borderWidth = 0.1,
      tooltip = list(valueDecimals = 1))
hcmap("https://code.highcharts.com/mapdata/countries/us/us-all.js", data = stat_table, value = "BPP",
      name = "Staffed Beds per 100k", joinBy = c("hc-a2", "State"),
      dataLabels = list(enabled = TRUE, format = '{point.name}', color = "black"),
      borderColor = "grey", borderWidth = 0.1,
      tooltip = list(valueDecimals = 1))
hospitalbeds <- stat_table %>%
    select(State, BPH) %>%
    left_join(hospital_by_state) %>%
    mutate(BPP = (Count*BPH*1000)/pop_2015)
hcmap("https://code.highcharts.com/mapdata/countries/us/us-all.js", data = hospitalbeds, value = "BPP",
      name = "Hospital Beds per 1000", joinBy = c("hc-a2", "State"),
      dataLabels = list(enabled = TRUE, format = '{point.name}', color = "black"),
      borderColor = "grey", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0))


