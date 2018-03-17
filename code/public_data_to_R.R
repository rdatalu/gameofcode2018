library(tidyverse)
library(rvest)
library(janitor)

setwd("/Users/user/Desktop/GitHub/data_public_lu")


park_relais = read_csv2("2016.06.01_Parking_relais_existants.csv")


pop_zip = read_csv("pop_zipcode.csv")


#More explanations here: http://www.blog.rdata.lu/post/2017-08-21-scraping-data-from-statec-s-public-tables/
densi_commune <- read_html("http://www.statistiques.public.lu/stat/TableViewer/tableViewHTML.aspx?ReportId=12862&IF_Language=eng&MainTheme=2&FldrName=1")
densi_commune = densi_commune %>%
        html_nodes(".b2020-datatable") %>% .[[1]] %>% html_table(fill = TRUE) 

colnames(densi_commune) = c("Commune",substr(data_raw[2,2:7],1,4),data_raw[2,-(1:7)])
densi_commune = densi_commune[-c(1,2),] 


pop_commune <- read_html("http://www.statistiques.public.lu/stat/TableViewer/tableViewHTML.aspx?ReportId=12861&IF_Language=eng&MainTheme=2&FldrName=1")
pop_commune = pop_commune %>%
        html_nodes(".b2020-datatable") %>% .[[1]] %>% html_table(fill = TRUE) 
colnames(pop_commune) = c("Commune",pop_commune[2,-1])
pop_commune = pop_commune[-c(1,2),] 

pop_commune  <- map_df(pop_commune , gsub, pattern=',', replacement='') %>%  
        mutate_at(-1, as.numeric) 


#Nettoyer bus_stop
bus_stop = readLines("http://travelplanner.mobiliteit.lu/hafas/query.exe/dot?performLocating=2&tpl=stop2csv&look_maxdist=150000&look_x=6112550&look_y=49610700&stationProxy=yes" ) %>% as.data.frame()
bus_stop = 
bus_stop = read_delim("http://travelplanner.mobiliteit.lu/hafas/query.exe/dot?performLocating=2&tpl=stop2csv&look_maxdist=150000&look_x=6112550&look_y=49610700&stationProxy=yes","@",col_names = FALSE ) 
colnames(bus_stop)=c("id","O","lat","lon","U","L","B","p")
bus_stop = bus_stop %>%
        map_df(~gsub("^.*=","",.))



#Loyer France

dep = c("moselle-57/", "bas-rhin-67/", "meurthe-et-moselle-54/", "meuse-55/", "ardennes-08/", "vosges-88/", "marne-51/")
main_cities = map(dep, function(x){
        table = read_html(paste0("https://www.meilleursagents.com/prix-immobilier/",x)) %>%
                html_nodes(xpath='//*[@id="villes"]/div[2]/div[1]/table') %>% #panel__content--with-only-table container--row
                html_table() %>% .[[1]] }) %>%
        reduce(rbind)

#Bike

bike = jsonlite::fromJSON("https://api.tfl.lu/v1/BikePoint") %>% .[[2]]
bike_prop = bike$properties %>%
        select(-photo, -last_update, -dock_status) %>%
        mutate(city = ifelse(city=="<NA>","Luxembourg",city),
               name = tolower(name),
               color = ifelse(open=="OPEN","blue","red"))
bike_coord = bike$geometry %>%
        mutate(coordinates = gsub("c|\\(|\\)","", coordinates)) %>%
        separate(coordinates, c("lat", "lon"), sep = ",") %>%
        mutate_at(-1,as.numeric) %>%
        select(lat, lon)

bikes = cbind.data.frame(bike_prop, bike_coord)

