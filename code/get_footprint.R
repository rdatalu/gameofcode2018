# source: https://www.theguardian.com/environment/datablog/2009/sep/02/carbon-emissions-per-transport-type

# Data: Camden council/Travelfootprint.org/ Clear Zone Partnership
# co2 in grams per passenger km
# Returns the cleaned data for the co2 footprint


library(rio)
library(here)
library(tidyverse)

emissions = import(paste0(here(), "/data/emissions_transport.csv"))

emissions = emissions %>%
    janitor::clean_names() %>%
    mutate(hybrid = str_detect(tolower(detail), "hybrid")) %>%
    mutate(hybrid = ifelse(hybrid, "hybrid", NA_character_)) %>%
    mutate(electric = str_detect(tolower(detail), "electric")) %>%
    mutate(electric = ifelse(electric, "electric", NA_character_)) %>%
    mutate(green = coalesce(hybrid, electric)) %>%
    select(mode, detail, green, everything())


emissions_stats = emissions %>%
    group_by(mode, green) %>%
    summarise(mean_emissions = mean(co2_gpkm),
              median_emissions = median(co2_gpkm))


rio::export(emissions_stats, paste0(here::here(), "/data/emissions_stats.csv"))
