# To avoid having to waste time with paths
# install.packages("here")
library(here)
library(tidyverse)
library(rvest)
library(janitor)

# Source: opendata STATBEL

# http://statbel.fgov.be/sites/default/files/files/documents/Bouwen%20%26%20wonen/2.1%20Vastgoedprijzen/FR_immo_statbel_ann%C3%A9e_170919_125304.xls


# Horrible excel sheet


raw_data = rio::import_list(paste0(here(), "/data/FR_immo_statbel_année_170919_125304.xls"))

liste_communes_prov_lux = read_html("https://en.wikipedia.org/wiki/Luxembourg_(Belgium)")

# Get liste of communes in Province de Luxembourg
communes = liste_communes_prov_lux %>%
  html_table(fill = TRUE) %>%
  .[[3]] %>%
  pull(Municipality) %>%
  tolower %>%
  str_replace_all("é", "e") %>%
  str_replace_all("û", "u") %>%
  str_replace_all("â", "a")

# Start cleaning data, only keep year that interests us and the communes in Province de Luxembourg
raw_data = raw_data$`Communes wallonnes` %>%
  clean_names() %>%
  filter(x_2 == 2016) %>%
  mutate(x_1 = tolower(x_1)) %>%
  filter(x_1 %in% communes) %>%
  select(-1, -3, -4, -5, -6, -(11:16), -(21:26), -(31:46))


columns = c("communes", "maison_nb_trans", "maison_prix_total", "maison_superficie_totale", "maison_prix_moyen",
            "villa_nb_trans", "villa_prix_total", "villa_superficie_totale", "villa_prix_moyen",
            "appart_nb_trans", "appart_prix_total", "appart_superficie_totale", "appart_prix_moyen")

colnames(raw_data) <- columns

housing_prices_be = raw_data %>%
  mutate_at(-1, as.numeric)

# Save data
rio::export(housing_prices_be, paste0(here(), "/data/housing_prices_be.csv"))

# second method

prix_be = read_html("https://www.immoweb.be/fr/a-louer/article/prix-moyens-de-la-location-en-2010.htm?mycurrent_section=rent&artid=4087&page=1#7") %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>% .[[5]]

prix_prov_lux_index = str_which(prix_be$X1, "Luxembourg")[2]

prix_prov_lux = prix_be[prix_prov_lux_index:(nrow(prix_be)-5), 1:2]

colnames(prix_prov_lux) <- c("commune", "prix_loyer_moyen")

rio::export(prix_prov_lux, paste0(here(), "/data/loyers_be.csv"))
