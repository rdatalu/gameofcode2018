# To avoid having to waste time with paths
# install.packages("here")
library(here)
library(tidyverse)
library(rvest)


dep = c("moselle-57/", "bas-rhin-67/", "meurthe-et-moselle-54/", "meuse-55/", "ardennes-08/", "vosges-88/", "marne-51/")

main_cities = map(dep, function(x){
        table = read_html(paste0("https://www.meilleursagents.com/prix-immobilier/",x)) %>%
                html_nodes(xpath='//*[@id="villes"]/div[2]/div[1]/table') %>% #panel__content--with-only-table container--row
                html_table() %>% .[[1]] }) %>%
        reduce(rbind)



loyer_fr = main_cities %>%
    janitor::clean_names() %>%
    select(ville, prix = prix_m2_moyenappartement) %>%
    mutate(prix = str_remove_all(prix, " ")) %>%
    mutate(prix = str_remove_all(prix, "€"))

rio::export(loyer_fr, paste0(here::here(), "/data/loyers_fr.csv"))
