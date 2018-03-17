# To avoid having to waste time with paths
# install.packages("here")
library(here)
library(tidyverse)
library(rvest)

# Data from the Observatoire de l'habitat

raw_table =  "http://observatoire.liser.lu/prixenregistres.cfm?pageKw=pe_appart_commune" %>%
  read_html %>%
  html_table %>%
  .[[2]] # The interesting table is in second position


columns = c("commune", "existing_app_m2", "existing_app_range", "new_app_m2", "new_app_range")

colnames(raw_table) = columns

# Clean data

housing_prices_lux =
  as_tibble(raw_table) %>%
  slice(-(1:2)) %>%
  separate(existing_app_range, c("existing_app_min", "existing_app_max"), sep = "-") %>%
  separate(new_app_range, c("new_app_min", "new_app_max"), sep = "-") %>%
  map_df(function(x)(str_replace_all(x, "\\.|\\*|\\s|_", ""))) %>%
  mutate_at(vars(matches("app")), as.numeric)

# Save data

rio::export(housing_prices_lux, paste0(here(), "/data/housing_prices_lux.csv"))

# Second method


url = "http://observatoire.liser.lu/basedeprix_fiche.cfm?comid={comid}&trid=2"

comid = seq(1,130)

urls = glue::glue(url)


get_mean_rents = function(url){

  raw_data = read_html(url) %>%
      html_table() %>%
      .[[2]]

  mean_rent = raw_data %>%
      filter(X1 == "Loyer moyen") %>%
      slice(1) %>%
      select(X2) %>%
      pull() %>%
      str_extract_all("\\d", simplify = TRUE) %>%
      paste(collapse = "")


  commune = raw_data %>%
      slice(1) %>%
      select(X2) %>%
      pull()

    tibble("commune" = commune,
           "mean_rent" = mean_rent)

}


rents_lux = map_df(urls, get_mean_rents)

rio::export(rents_lux, paste0(here::here(), "/data/loyers_lux.csv"))
