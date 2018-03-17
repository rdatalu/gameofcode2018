# This function get the data in real time from chargy's api
# Unfortunatly, on the day of the competition, the api is down

library(httr)
library(tidyverse)
library(jsonlite)
result <- GET("https://my.chargy.lu/b2bev-external-services/resources/kml",
              add_headers(Authorization="486ac6e4-93b8-4369-9c6a-28f7c4e1a81f"))
#jsonlite::fromJSON(content(result,type="text"))

require(XML)
data <-xmlParse(result)
xml_data <- xmlToList(data)[[1]]


new_data = xml_data %>% map(`[`, c("name",#"description","visibility",
                                   "address")) %>%
        compact()  %>%  do.call("rbind",.) %>% as.data.frame(.,row.names = FALSE)
colnames(new_data) = c("name", #"description","visibility",
                       "address")
new_data = new_data %>% filter_all((any_vars(.!="NULL"&.!="NA")))
nrow(new_data)
point = xml_data %>% map(`[`, c("Point")) %>%
        unlist()  %>%  strsplit(.,",")%>%  do.call("rbind",.) %>% as.data.frame(.,row.names = FALSE) %>%
        na.omit()
colnames(point) = c("longitude", "latitude")
nrow(point)
chargy = cbind.data.frame(new_data, point)
