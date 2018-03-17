# This function extracts the cities from the Rheinland-Pfalz state
# This was supposed to help us get data on rents in this state
# but finding information for rents in Germany turned out to be very difficult

library(rvest)


cities_de = read_html("https://de.wikipedia.org/wiki/Liste_der_St%C3%A4dte_und_Gemeinden_in_Rheinland-Pfalz") %>%
  html_nodes("table")%>%
  html_table()

city_de = cities_de[1:4]%>% map(str_split,"\n ") %>% map(unlist) %>% unlist

saveRDS(city_de,paste0(here::here(), "/data/city_de.rds"))
  html_table()
