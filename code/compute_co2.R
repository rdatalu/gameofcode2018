# This function returns a data frame with info on co2
# emissions. The data was taken from: https://www.theguardian.com/environment/datablog/2009/sep/02/carbon-emissions-per-transport-type
# It comes in the form of an google sheet that was downloaded into /data as a csv.

library(rio)
library(here)
library(tidyverse)


compute_co2 = function(trans_mode, distance){

   # Attention: distance must be in km

    emissions_stats = rio::import(paste0(here::here(),
                                         "/data/emissions_stats.csv"))

    emissions_stats = emissions_stats %>%
        filter(!(mode %in% c("FLY", "M/CYCLE", "TUBE", "RAIL", "TAXI"))) %>%
        select(-median_emissions) %>%
        mutate(mode = case_when(mode == "BUS" ~ "driving-hgv",
                                mode == "CAR" ~ "driving-car",
                                mode == "CYCLE" ~ "cycling-regular",
                                mode == "WALK" ~ "foot_walking")) %>%
        mutate(green = ifelse(green == "", "fuel", green))

    emissions_stats %>%
        filter(mode == trans_mode) %>%
        mutate(total_emissions = mean_emissions * distance)

}
