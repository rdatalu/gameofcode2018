library(here)
library(tidyverse)
library(rvest)
library(janitor)

url = "https://www.immowelt.de/immobilienpreise/{stadt}/mietspiegel"

stadt = readRDS(paste0(here(), "/data/city_de.rds"))

stadt = stadt %>%
    tolower %>%
    str_replace_all(" ", "-") %>%
    str_remove_all("\\(|\\)") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ß", "ss") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ä", "ae")


urls = glue::glue(url)

urls[1]

test = read_html(urls[1])
