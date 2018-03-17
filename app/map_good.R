#Chargement des packages
if(!require('shinymaterial')) install.packages('shinymaterial')
library(shinymaterial)
if(!require('shiny')) install.packages('shiny')
library(shiny)
if(!require('here')) install.packages('here')
library(here)
if(!require('leaflet')) install.packages('leaflet')
library('leaflet')
if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if(!require('jsonlite')) install.packages('jsonlite')
library(jsonlite)
if(!require('curl')) install.packages('curl')
library(curl)
if(!require('gtfsr')) install.packages('gtfsr')
library(gtfsr)
if(!require('DT')) install.packages('DT')
library('DT') 
if(!require('rio')) install.packages('rio')
library(rio)
if(!require('data.table')) install.packages('data.table')
library('data.table') 


#API key
ors_key = "58d904a497c67e00015b45fc2d33e7bee1c341669aa0d34b654c72e0"

#chargement des fichiers logements
loyer_lu = read_csv(paste0(here::here(),"/data/loyers_lux.csv")) %>%
  mutate(pays="Luxembourg") %>% rename(monthly_rent_price = mean_rent)
loyer_be = read_csv(paste0(here::here(),"/data/loyers_be.csv")) %>%
  mutate(pays="Belgique") %>% rename(monthly_rent_price = prix_loyer_moyen)
loyer_fr = read_csv(paste0(here::here(),"/data/loyers_fr.csv")) %>%
  .[,-2] %>%
  mutate(pays="France")

loyer = rbind(loyer_lu, loyer_be, loyer_fr) %>%
  rename(`Monthly Rent Price` = monthly_rent_price)


#chargement des fichiers gtfs
gtfs_lux = readRDS(paste0(here::here(), "/data/gtfs_lux.rds"))

#chargement lignes
road_lines = readRDS(paste0(here::here(), "/data/road_lines.rds"))
#chargement Polution
emission = read_csv(paste0(here::here(), "/data/emissions_stats.csv"))

#Chargement des arrets de bus
busStop = readRDS(paste0(here::here(),"/data/bus_lines_stops_collapsed.rds")) %>%
  mutate(title_attrib = paste0(stop_name,"<br/>",lines))

#Chargement des données carloh
carloh = readRDS(paste0(here::here(), "/data/carloh.rds"))

#Chargement du parking relais
p_relais = readRDS(paste0(here::here(), "/data/p_relais.rds"))


#chargement warning meteo
# Get weather alerts
get_weather_alert = function(){
  weather_alert = read.csv("http://meteolux.lu/Opendata/data_alerts.csv",
                           sep = ";", skip = 2)
  
  vigilance = tibble("code" = as.character(weather_alert$COLOR),
                     "message" = weather_alert$VALUE)
  
  vigilance = vigilance %>%
    mutate(message = ifelse(is.na(message), "RAS", message))
  
  return(vigilance)
}

warnings =  suppressWarnings(possibly(get_weather_alert, otherwise = NULL)())
#Icone de Bus
busIcon  <- awesomeIcons(
  icon = 'bus',
  iconColor = 'beige',
  library = 'fa',
  markerColor = "lightred"
)


#Icone carloh
carlohIcon  <- awesomeIcons(
  icon = 'car',
  iconColor = 'beige',
  library = 'fa',
  markerColor = "cadetblue"
)

#Icon de parking relais
p_relaisIcon  <- awesomeIcons(
  icon = 'product-hunt',
  iconColor = 'beige',
  library = 'fa',
  markerColor = "darkblue"
)




#############################

##### FUNCTIONS

#############################

#distance a vol d'oiseau
bird_dist = function(x1,x2,y1,y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
}

#Calcul le niveau d'emission co2
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
  
  emissions_stats = emissions_stats %>%
    filter(mode == trans_mode) %>%
    mutate(total_emissions = mean_emissions * distance)
  res = HTML(paste(emissions_stats$green, "<br/>", round(emissions_stats$total_emissions), "CO² grams/km/person", "<br/> (",
                   round(emissions_stats$mean_emissions), "CO² grams/km/person",")", "<br/>"))
  return(res)
  
}
#compute_co2("driving-hgv", 2332)

#Renvoie l'adresse le chemin, la latitude et longitude avec API nominatim
info_url = function(url){
  info = jsonlite::fromJSON(url)
  if(length(info)==0){
    address = "Please try to write the correct address"
    road = NULL
    coord = NULL
    lon = NULL
    lat = NULL
  } else{
    address = info$display_name
    road = info$polygonpoints[[1]]
    colnames(road)=c("lon","lat")
    coord = paste0(info$lon,",",info$lat)
    lon = as.numeric(info$lon)
    lat = as.numeric(info$lat)
  }
  return(list(address = address, road = road, coord = coord, lat = lat, lon = lon))
}
#Renvoie a partir d'une URL, le chemin, la distance et la duree mais a partir d'une autre api
info_road_url = function(url_road){
  loc = RJSONIO::fromJSON(url_road)
  val = unlist(loc$features[[1]]$geometry$coordinates)
  road = matrix(as.numeric(val),ncol=2, byrow=TRUE) %>%
    as.data.frame()
  colnames(road)=c("lon","lat")
  
  route_step = unlist(loc$features[[1]]$properties$segments[[1]])
  duration = sum(as.numeric(route_step[grepl("steps.duration",names(route_step))]))
  distance = sum(as.numeric(route_step[grepl("steps.distance",names(route_step))]))
  return(list(road=road, distance=distance, duration=duration))
}



#Renvoie pour un arret de depart et arrivé renvoie les lignes qui y passent sans correspondance
get_bus_travel_direct = function(gtfs_lux, start_name, end_name){
  #start_name='Bettembourg, Gare'
  #end_name='Vianden, Bréck'
  start_name = str_remove(start_name, ",")
  end_name = str_remove(end_name, ",")
  
  start_stop_id = gtfs_lux$stops_df %>%
    filter(
      str_detect(
        str_remove(
          tolower(stop_name), ","
        ),
        str_remove(
          tolower(start_name), ","
        )
      )
    ) %>%
    pull(stop_id)
  
  if(length(start_stop_id) > 1){
    start_stop_id = start_stop_id[1]
  }
  
  
  end_stop_id = gtfs_lux$stops_df %>%
    filter(
      str_detect(
        str_remove(
          tolower(stop_name), ","
        ),
        str_remove(
          tolower(end_name), ","
        )
      )
    ) %>%
    pull(stop_id)
  
  
  if(length(end_stop_id) > 1){
    end_stop_id = end_stop_id[1]
  }
  
  
  trip_id_start = gtfs_lux$stop_times_df %>%
    filter(stop_id == start_stop_id)
  
  trip_id_end = gtfs_lux$stop_times_df %>%
    filter(stop_id == end_stop_id)
  
  
  trips = semi_join(trip_id_start,
                    trip_id_end, by = "trip_id")
  
  #add from Kevin
  if(nrow(trips)==0){
    trip_id_start = trip_id_start %>%
      left_join(unique(busStop[,c("stop_id", "line", "lines")]))
    
    trip_id_end = trip_id_end %>%
      left_join(unique(busStop[,c("stop_id", "line", "lines")]))
    
    line_start = unique(trip_id_start$line)
    line_end = unique(trip_id_end$line)
    
    #commun
    commun_line = line_start[which(line_start %in% line_end)]
    
    
    if(length(commun_line)==0){
      link_to_start = unique(unlist(str_split(busStop$lines[line_start %in% busStop$lines],",")))
      
      commun_line = link_to_start[which(line_end %in% link_to_start)][1]
    }
    
    lines=commun_line
  }else{
    routes_direct = gtfs_lux$trips_df %>%
      filter(trip_id %in% trips$trip_id) %>%
      pull(route_id)
    
    lines = gtfs_lux$routes_df %>%
      filter(route_id %in% routes_direct) %>%
      pull(route_short_name)
    
  }
  
  
  
  return(lines)
}

#Transforme le format de secondes a minutes
toMin = function(x){
  x = as.numeric(x)
  if(x>60){
    min = floor(x/60)
    sec = x- (60*min)
    if(sec==0){
      res = paste0(min, " min")
    }else{
      sec = substr(x-(60*min),1,2)
      sec = ifelse(nchar(sec)==1,paste0(0,sec),sec)
      res = paste0(min,".",sec, " min")
    }
  }else{
    res = paste0(x, " sec")
  }
  
  return(res)
}




#######################

### SERVER

#######################


server <- function(input, output, session) {
  
  #Variable reactives
  isFirstTime <- reactiveValues(x = TRUE)
  values_infoTable <- reactiveValues(icon=NULL, time=NULL, distance = NULL, co2=NULL)
  
  
  ## API de veloh
  bike_df <- reactive({
    material_spinner_show(session, output_id = "wholeApp")
    Sys.sleep(2)
    
    
    
    
    
    
    bike = jsonlite::fromJSON("https://api.tfl.lu/v1/BikePoint") %>% .[[2]]
    bike_prop = bike$properties %>%
      select(-photo, -last_update, -dock_status) %>%
      mutate(city = ifelse(grepl("veloh", id),"Luxembourg",city),
             name = tolower(name),
             address = tolower(address),
             color = ifelse(open==TRUE,"blue","red"),
             title_attrib = ifelse(city!="Luxembourg",paste0("Name: ",name,"<br/>Adress: ",address,
                                                             "<br/>City: ",city,"<br/>Available bikes: ",
                                                             available_bikes,"<br/>Available ebikes: ",
                                                             available_ebikes,"<br/>Advailable docks: ",
                                                             available_docks), paste0("Name: ",name,"<br/>Adress: ",address,
                                                                                      "<br/>City: ",city,"<br/>Available bikes: ",
                                                                                      available_bikes ,"<br/>Advailable docks: ",
                                                                                      available_docks
                                                             )
             ))
    
    bike_prop$title_attrib <- lapply(1:nrow(bike),
                                     function(x) {
                                       htmltools::HTML(
                                         sprintf(
                                           bike_prop$title_attrib[x]
                                         )
                                       )
                                     })
    
    bike_coord = bike$geometry %>%
      mutate(coordinates = gsub("c|\\(|\\)","", coordinates)) %>%
      separate(coordinates, c("lon", "lat"), sep = ",") %>%
      mutate_at(-1,as.numeric) %>%
      select(lat, lon)
    
    cbind.data.frame(bike_prop, bike_coord)
    #
    # url <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=Luxembourg&apiKey=491f3117fe7107c0115276912b7ff9ba9c7730ba")
    #
    # df_api = jsonlite::fromJSON(url)
    # position.lng = df_api$position$lng
    # position.lat = df_api$position$lat
    # df_api = cbind(df_api %>% select(-position), position.lng, position.lat) %>%
    #   mutate( name = trimws(gsub("[0-9-]"," ", name)),
    #           #address = paste0("<a href='https://www.google.com/maps/search/?api=1&query=",position.lat,",",position.lng,"' target='_blank'>",address,"</a>"),
    #           address = paste0("<a href='https://www.openstreetmap.org/?mlat=",position.lat,"&mlon=",position.lng,"#map=15/",position.lat,"/",position.lng,"' target='_blank'>",address,"</a>"),
    #           title_attrib = paste0("Name: ",name,"<br>Adress: ",address,"<br>Available: ",available_bikes,"/",bike_stands),
    #           color= ifelse(status != "OPEN", "red","blue"),
    #           available_bike_stands = ifelse(color=="red",0,available_bikes))%>%
    #   rename(lat=position.lat, lon=position.lng)
  })
  
  
  
  #Donnee pour montrer la carte
  output$map <- renderLeaflet({
    
    #Appelle la bike
    bike = bike_df()
    
    bikeIcon  <- awesomeIcons(
      icon = 'bicycle',
      iconColor = 'beige',
      library = 'fa',
      markerColor = bike$color)
    
    if(!is.null(input$long) & !is.null(input$lat)){
      material_spinner_hide(session, output_id = "wholeApp")
      leaflet( ) %>%
        addAwesomeMarkers(lng=bike$lon, lat=bike$lat, popup = bike$title_attrib, icon = bikeIcon) %>%
        addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon) %>%
        addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon) %>%
        setView(lng = input$long, lat = input$lat, zoom = 14) %>%
        addTiles()
    }else{
      material_spinner_hide(session, output_id = "wholeApp")
      leaflet( ) %>%
        addAwesomeMarkers(lng=bike$lon, lat=bike$lat, popup = bike$title_attrib, icon = bikeIcon) %>%
        addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon) %>%
        addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon) %>%
        setView(lng = 6.11371, lat = 49.60256, zoom = 14) %>%
        addTiles()
    }
    
  })
  
  
  #Donne pour montrer la carte
  output$infoTable <- renderUI({
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$h4(paste0("Route info: ",input$nstation),
                       id='title_tb'),
               tags$thead(
                 tags$tr(
                   tags$th("Transport. Means"),
                   tags$th("Travel Time"),
                   tags$th("Travel Distance"),
                   tags$th("Pollutant Emissions"),
                   tags$th("Weather Alerts")
                 )),
               tags$tbody(
                 tags$tr(
                   tags$td(HTML(values_infoTable$icon)),
                   tags$td(values_infoTable$time),
                   tags$td(values_infoTable$distance),
                   tags$td(values_infoTable$co2),
                   tags$td(values_infoTable$meteo)
                 )
               )
    )
  })
  
  output$tab_housing <- DT::renderDataTable({
    DT::datatable(
      loyer, extensions = 'Buttons', 
      filter="top", selection="multiple", escape=FALSE, options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })
  
  #Actualise le trajet (carte+ tableau)
  observeEvent(input$runButton, {
    isFirstTime$x = FALSE
    values_infoTable$icon=NULL
    values_infoTable$time=NULL
    values_infoTable$distance = NULL
    values_infoTable$co2=NULL
    values_infoTable$meteo = NULL
    road = NULL
    from_address = NULL
    from_coord = NULL
    to_address = NULL
    to_coord = NULL
    mid_lon = NULL
    mid_lat = NULL
    
    
    bike = bike_df()
    #bike = df_api
    
    
    if(!(is.null(input$from) | input$from=='""')){
      
      from_nospace = gsub(" ", "%20", input$from)
      url_from = paste0("https://nominatim.openstreetmap.org/search/", from_nospace,"?format=json&polygon=1&addressdetails=1")
      fromUrl = info_url(url_from )
    }else{
      fromUrl = list(lat=NULL, lon=NULL, address=NULL, coord=NULL)
      
    }
    
    if(!(is.null(input$to) | input$to=='""')){
      to_nospace = gsub(" ", "%20", input$to)
      url_to = paste0("https://nominatim.openstreetmap.org/search/", to_nospace,"?format=json&polygon=1&addressdetails=1")
      toUrl = info_url(url_to )
      
    }else{
      toUrl = list(lat=NULL, lon=NULL, address=NULL, coord=NULL)
      
    }
    
    if( !is.null(toUrl$lon) & !is.null(fromUrl$lon) & !is.null(toUrl$lat) & !is.null(fromUrl$lat) ){
      
      fromUrl$coord[1] =gsub(",","%2C",fromUrl$coord[1])
      toUrl$coord[1]=gsub(",","%2C",toUrl$coord[1])
      
      #fromUrl=list(coord="49.626879%2C6.104087", lon=6.104087, lat=49.626879)
      #toUrl=list(coord="49.621976%2C6.151566", lon=6.151566, lat=49.621976)
      
      subset_bike = bike %>%
        mutate(dist_from = bird_dist(fromUrl$lon[1],bike$lon,fromUrl$lat[1],bike$lat),
               dist_to =  bird_dist(toUrl$lon[1],bike$lon,toUrl$lat[1],bike$lat)
        )
      
      min_from_disbike = subset_bike %>%
        group_by(name) %>%
        slice(1) %>%
        arrange(dist_from) %>%
        .[1:5,]
      
      
      min_to_disbike = subset_bike %>%
        group_by(name) %>%
        slice(1) %>%
        arrange(dist_to) %>%
        .[1:5,]
      
      subset_bike = rbind.data.frame(min_from_disbike, min_to_disbike) %>%
        group_by(name) %>%
        filter(row_number(name) == 1 & dist_to<0.06 & dist_from<0.06)
      
      subset_bike = subset_bike %>%
        .[1:max(10,nrow(subset_bike)),]
      
      subset_Bus = busStop %>%
        ungroup() %>%
        mutate(dist_from = bird_dist(fromUrl$lon[1],busStop$stop_lon,fromUrl$lat[1],busStop$stop_lat),
               dist_to =  bird_dist(toUrl$lon[1],busStop$stop_lon,toUrl$lat[1],busStop$stop_lat))
      
      
      min_from_disBus = subset_Bus %>%
        group_by(stop_id) %>%
        slice(1) %>%
        arrange(dist_from) %>%
        .[1:5,]
      
      
      min_to_disBus = subset_Bus %>%
        group_by(stop_id) %>%
        slice(1) %>%
        arrange(dist_to) %>%
        .[1:5,]
      
      subset_Bus = rbind.data.frame(min_from_disBus, min_to_disBus) %>%
        group_by(stop_id) %>%
        filter(row_number(stop_id) == 1 #& dist_to<0.06 & dist_from<0.06
        )
      
      subset_Bus = subset_Bus %>%
        .[1:max(10,nrow(subset_Bus)),]
      
      
      #pour veloh
      if(input$type=="veloh"){
        
        # subset_bike = bike %>%
        #   mutate(dist_from = bird_dist(fromUrl$lon[1],bike$lon,fromUrl$lat[1],bike$lat),
        #          dist_to =  bird_dist(toUrl$lon[1],bike$lon,toUrl$lat[1],bike$lat)
        #   )
        #
        # min_from_disbike = subset_bike %>%
        #   group_by(name) %>%
        #   slice(1) %>%
        #   arrange(dist_from) %>%
        #   .[1:5,]
        #
        #
        # min_to_disbike = subset_bike %>%
        #   group_by(name) %>%
        #   slice(1) %>%
        #   arrange(dist_to) %>%
        #   .[1:5,]
        #
        # subset_bike = rbind.data.frame(min_from_disbike, min_to_disbike) %>%
        #   group_by(name) %>%
        #   filter(row_number(name) == 1 & dist_to<0.06 & dist_from<0.06)
        #
        # subset_bike = subset_bike %>%
        #   .[1:max(10,nrow(subset_bike)),]
        
        
        
        fromUrl_bike = paste0(min_from_disbike$lon[1],"%2C",min_from_disbike$lat[1])
        toUrl_bike = paste0(min_to_disbike$lon[1],"%2C",min_to_disbike$lat[1])
        from_road_walk = paste0(fromUrl$lon[1],"%2C",fromUrl$lat[1])
        to_road_walk = paste0(toUrl$lon[1],"%2C",toUrl$lat[1])
        url_road_walkfrom = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",from_road_walk,"%7C",fromUrl_bike,"&profile=foot-walking&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        url_road_bike = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",fromUrl_bike,"%7C",toUrl_bike,"&profile=cycling-regular&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        url_road_walkto = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",toUrl_bike,"%7C",to_road_walk,"&profile=foot-walking&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        
        
        path1 = info_road_url(url_road_walkfrom)
        path2 = info_road_url(url_road_bike)
        path3 = info_road_url(url_road_walkto)
        
        road = rbind(path1$road, path2$road,path3$road)
        
        duration= path1$duration+path2$duration+path3$duration
        
        distance= path1$distance+path2$distance+path3$distance
        
        values_infoTable$distance = distance
        values_infoTable$co2=0
        
      } else if(input$type=="driving-hgv"){ #pour veloh
        
        # subset_Bus = busStop %>%
        #   ungroup() %>%
        #   mutate(dist_from = bird_dist(fromUrl$lon[1],busStop$stop_lon,fromUrl$lat[1],busStop$stop_lat),
        #          dist_to =  bird_dist(toUrl$lon[1],busStop$stop_lon,toUrl$lat[1],busStop$stop_lat))
        #
        #
        # min_from_disBus = subset_Bus %>%
        #   group_by(stop_id) %>%
        #   slice(1) %>%
        #   arrange(dist_from) %>%
        #   .[1:5,]
        #
        #
        # min_to_disBus = subset_Bus %>%
        #   group_by(stop_id) %>%
        #   slice(1) %>%
        #   arrange(dist_to) %>%
        #   .[1:5,]
        #
        # subset_Bus = rbind.data.frame(min_from_disBus, min_to_disBus) %>%
        #   group_by(stop_id) %>%
        #   filter(row_number(stop_id) == 1 #& dist_to<0.06 & dist_from<0.06
        #   )
        #
        # subset_Bus = subset_Bus %>%
        #   .[1:max(10,nrow(subset_Bus)),]
        
        
        
        fromUrl_bus = paste0(min_from_disBus$stop_lon[1],"%2C",min_from_disBus$stop_lat[1])
        toUrl_bus = paste0(min_to_disBus$stop_lon[1],"%2C",min_to_disBus$stop_lat[1])
        
        select_line = get_bus_travel_direct(gtfs_lux, min_from_disBus$stop_name[1] , min_to_disBus$stop_name[1])[1]
        #select_line = 262
        select_path = road_lines[[2]][which(road_lines[[1]]==select_line) ] %>% as.data.frame()
        # select_path =  select_path %>%
        #   mutate(n = row_number()))
        select_path$n = 1:nrow(select_path)
        
        start_n=NULL
        i=0
        l_start=0
        while(l_start==0){
          int= 5-i
          start_n = select_path[round(select_path$lon,int)==round(min_from_disBus$stop_lon[1],int) & round(select_path$lat,int)==round(min_from_disBus$stop_lat[1],int),c("lon","lat")]
          i=i+1
          l_start=nrow(start_n)
          
        }
        start_n = as.numeric(rownames(start_n)[1])
        end_n=NULL
        j=0
        l_end=0
        while(l_end==0){
          int= 5-j
          end_n = select_path[round(select_path$lon,int)==round(min_to_disBus$stop_lon[1],int) & round(select_path$lat,int)==round(min_to_disBus$stop_lat[1],int),c("lon","lat")]
          j=j+1
          l_end=nrow(end_n)
          
        }
        end_n = as.numeric(rownames(end_n)[1])
        
        if(start_n==end_n | length(end_n)==0|length(start_n)==0){
          url_road = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",fromUrl_bus,"%7C",toUrl_bus,"&profile=foot-walking&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
          path2 = info_road_url(url_road)
          select_path = path2$road
          duration2= path2$duration
          distance2= path2$distance
          
          
        }else if(input$type=="cycling-regular"){
          
          
          url_road_bike = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",fromUrl_bike,"%7C",toUrl_bike,"&profile=cycling-regular&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
          
          
          
        }else{
          
          select_path = select_path %>% .[start_n:end_n,] %>%
            mutate(coord=paste0(lon,',',lat)) %>%
            mutate(list_coord = paste0(coord,collapse = ";"))
          
          url_road = paste0("http://router.project-osrm.org/route/v1/driving/",select_path$list_coord[1],"?overview=false&alternatives=false&steps=true")
          loc = RJSONIO::fromJSON(url_road)
          unlist_loc = unlist(loc$routes)
          distance2 = as.numeric(unlist_loc[names(unlist_loc)=="distance"])
          duration2 = as.numeric(unlist_loc[names(unlist_loc)=="duration"])
          
        }
        from_road_walk = paste0(fromUrl$lon[1],"%2C",fromUrl$lat[1])
        to_road_walk = paste0(toUrl$lon[1],"%2C",toUrl$lat[1])
        
        url_road_walkfrom = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",from_road_walk,"%7C",fromUrl_bus,"&profile=foot-walking&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        url_road_walkto = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",toUrl_bus,"%7C",to_road_walk,"&profile=foot-walking&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        
        
        path1 = info_road_url(url_road_walkfrom)
        
        path3 = info_road_url(url_road_walkto)
        
        road = rbind(path1$road, select_path,path3$road)
        
        duration= path1$duration+duration2+path3$duration
        
        distance= path1$distance+distance2+path3$distance
        
        # values_infoTable$co2 = 0
        values_infoTable$distance = round(distance, 3)
        values_infoTable$co2 = compute_co2(input$type, distance/1000)
        
        
        
      }else{
        
        
        url_road = paste0("https://api.openrouteservice.org/directions?api_key=",ors_key,"&coordinates=",fromUrl$coord[1],"%7C",toUrl$coord[1],"&profile=",input$type,"&preference=",input$pref,"&format=geojson&units=m&language=en&geometry=true&geometry_format=encodedpolyline&geometry_simplify=&instructions=true&instructions_format=text&roundabout_exits=&attributes=&maneuvers=&radiuses=&bearings=&continue_straight=&elevation=&extra_info=steepness&optimized=true&options=%7B%7D&id=")
        
        loc = RJSONIO::fromJSON(url_road)
        val = unlist(loc$features[[1]]$geometry$coordinates)
        road = matrix(as.numeric(val),ncol=2, byrow=TRUE) %>%
          as.data.frame()
        colnames(road)=c("lon","lat")
        
        route_step = unlist(loc$features[[1]]$properties$segments[[1]])
        duration = sum(as.numeric(route_step[grepl("steps.duration",names(route_step))]))
        distance = sum(as.numeric(route_step[grepl("steps.distance",names(route_step))]))
        
        # values_infoTable$co2 = 0
        values_infoTable$distance = round(distance, 3)
        values_infoTable$co2 = compute_co2(input$type, distance/1000)
        
      }
      
      fa=case_when(input$type== "foot-walking" ~ "blind",
                   input$type== "cycling-regular" ~ "bicycle",
                   input$type== "veloh" ~ "bicycle",
                   input$type== "driving-hgv" ~ "bus",
                   input$type== "driving-car" ~ "car"
      )
      
      values_infoTable$icon=paste0('<i class="fa fa-',fa,' "></i>')
      values_infoTable$time= toMin(duration)
      values_infoTable$distance = paste(round(distance/1000, 3)," km")
      
      
      
    }
    
    # path = list(road = road, from_address = fromUrl$address, from_coord = fromUrl$coord,
    #             to_address = toUrl$address, toUrl$coord, mid_lon = (fromUrl$lon[1]+toUrl$lon[1])/2, mid_lat = (fromUrl$lat[1] + toUrl$lat[1])/2)
    
    warnings_color = pull(warnings, code)
    
    warnings_message = pull(warnings, message)
    html_meteo = glue::glue('<span style=\"width:1.1em; height:1.1em; background-color:{warnings_color}; display:inline-block;\"></span><span> {warnings_message}</span>')
    values_infoTable$meteo = HTML(html_meteo)
    itineris = data.frame(lon=c(fromUrl$lon[1],toUrl$lon[1]), lat=c(fromUrl$lat[1], toUrl$lat[1]),color=c("green","red"))
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'white',
      library = 'ion',
      markerColor = itineris$color,
      fontFamily = "serif"
    )
    
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()
    
    bikeIcon  <- awesomeIcons(
      icon = 'bicycle',
      iconColor = 'beige',
      library = 'fa',
      markerColor = bike$color)
    
    
    if(input$show_bike){
      proxy <- proxy %>%
        addAwesomeMarkers(lng=subset_bike$lon, lat=subset_bike$lat, popup = subset_bike$title_attrib, icon = bikeIcon)
    }
    
    if(input$show_bus){
      proxy <- proxy %>%
        addAwesomeMarkers(lng= subset_Bus$stop_lon, lat= subset_Bus$stop_lat, popup = subset_Bus$title_attrib, icon = busIcon)
    }
    
    if(input$show_p_relais){
      proxy <- proxy %>%
        addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon)
    }
    
    if(input$show_carloh){
      proxy <- proxy %>%
        addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon)
      
    }
    
    if(is.null(road) & is.null(input$long)){
      proxy %>%
        # marker_velo %>%
        # marker_stopBus %>%
        # marker_p_relais %>%
        # marker_carloh %>%
        # addAwesomeMarkers(lng= busStop$stop_lon, lat= busStop$stop_lat, popup = busStop$title_attrib, icon = busIcon) %>%
        # addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon) %>%
        # addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon) %>%
        # addAwesomeMarkers(lng=bike$lon, lat=bike$lat, popup = bike$title_attrib, icon = bikeIcon) %>%
        setView(lng = 6.11371, lat = 49.60256, zoom = 14)
    } else if(is.null(road) & !is.null(input$long)){
      proxy %>%
        # clearShapes() %>%
        # clearMarkers() %>%
        # marker_velo %>%
        # marker_stopBus %>%
        # marker_p_relais %>%
        # marker_carloh %>%
        #addAwesomeMarkers(lng= busStop$stop_lon, lat= busStop$stop_lat, popup = busStop$title_attrib, icon = busIcon) %>%
        #addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon) %>%
        #addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon) %>%
        #addAwesomeMarkers(lng=bike$lon, lat=bike$lat, popup = bike$title_attrib, icon = bikeIcon) %>%
        setView(lng = as.numeric(input$long), lat = as.numeric(input$lat), zoom = 14)
    } else{
      proxy %>%
        # clearShapes() %>%
        # clearMarkers() %>%
        addAwesomeMarkers(itineris$lon, itineris$lat,
                          icon=icons) %>%
        # marker_velo %>%
        # marker_stopBus %>%
        # marker_p_relais %>%
        # marker_carloh %>%
        #addAwesomeMarkers(lng=carloh$lon, lat=carloh$lat, popup = carloh$title_attrib, icon = carlohIcon) %>%
        #addAwesomeMarkers(lng=p_relais$Longitude, lat=p_relais$Latitude, popup = p_relais$title_attrib, icon = p_relaisIcon) %>%
        #addAwesomeMarkers(lng=bike$lon, lat=bike$lat, popup = bike$title_attrib, icon = bikeIcon) %>%
        addPolylines(data = road, lng = ~lon, lat = ~lat) %>%
        fitBounds(lng1 = max(road$lon),lat1 = max(road$lat),
                  lng2 = min(road$lon),lat2 = min(road$lat))
      
      
    }
    
  })
  
  
  
}




ui <- material_page(
  title = "",
  nav_bar_fixed = TRUE,
  include_fonts = T,
  nav_bar_color = "teal lighten-1",
  tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);

              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }

              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              '),
  # Define tabs
  
  material_tabs(
    tabs = c(
      "Transport" = "first_tab",
      "Housing" = "second_tab"
    )
  ),
  
  
  # material_side_nav(
  #   fixed = TRUE,
  #   # Place side-nav tabs within side-nav
  #   material_side_nav_tabs(
  #     side_nav_tabs = c(
  #       "Transport" = "first_tab",
  #       "Housing" = "second_tab"
  #     ),
  #     icons = c("cast", "insert_chart")
  #   )
  # ),
  # Define tab content
  material_tab_content(
    # material_side_nav_tab_content(
    tab_id = "first_tab",
    # side_nav_tab_id = "first_tab",
    #background_color = "blue lighten-4",
    # tags$h1("First Tab Content"),
    tags$div(id = 'wholeApp', style = "opacity:0",
             material_row(
               material_column(
                 width = 4,
                 offset = 1,
                 
                 #material_card(
                 #        depth = 3,
                 material_text_box(input_id = "from", label = "depart :"),
                 #        material_text_box(input_id = "to", label = "arrivée :"),
                 #material_button(
                 #        input_id = "example_button",
                 #        label = "BUTTON"
                 #)
                 actionButton( "runButton","Run")
               ),
               material_column(
                 width = 4,
                 offset = 1,
                 material_text_box(input_id = "to", label = "arrivée :"),
                 # material_radio_button(input_id = "transport", label="",choices = c(
                 #   "bus" = "c",
                 #   "velo" = "s",
                 #   "car" = "f"
                 # ))
                 
                 material_modal(
                   modal_id = "option",
                   button_text = "Option",
                   material_dropdown(
                     # material_radio_button(
                     input_id = "type",
                     label = "Transport Type",
                     choices = c(
                       "foot" = "foot-walking",
                       "veloh" = "veloh",
                       "personal bike" = "cycling-regular",
                       "bus" = "driving-hgv",
                       "car" = "driving-car"
                     ),
                     selected = "cycling-regular"
                   ),
                   material_row(
                     material_column(
                       width = 4,
                       offset = 1,
                       material_radio_button(
                         input_id = "pref",
                         label = "route preference",
                         choices = c(
                           "recommended" = "recommended",
                           "fastest" = "fastest",
                           "shortest" = "shortest"
                         )
                       )),
                     material_column(
                       width = 4,
                       offset = 1,
                       HTML('<div>
                            <label>show markers</label>'),
                       material_checkbox(
                         input_id = "show_bus",
                         label = "Bus stops",
                         initial_value = FALSE
                       ),
                       
                       material_checkbox(
                         input_id = "show_bike",
                         label = "sharing bike",
                         initial_value = TRUE
                       ),
                       
                       material_checkbox(
                         input_id = "show_p_relais",
                         label = "Parking relays",
                         initial_value = FALSE
                       ),
                       material_checkbox(
                         input_id = "show_carloh",
                         label = "Car sharing",
                         initial_value = TRUE
                       ),
                       HTML('</div>')
                     ),
                     material_row(
                       footer = tagList(
                         modalButton("Cancel"),
                         actionButton("ok", "OK")
                       )
                     )
                   )
                 )
               )
             ),
             material_row(
               material_column(
                 width = 10,
                 offset = 1,
                 material_card(
                   depth = 3,
                   leafletOutput("map")
                 )
               )
             ),
             material_row(
               material_column(
                 width=10,
                 offset = 1,
                 material_card(
                   depth=3,
                   # Create a Bootstrap-styled table
                   uiOutput("infoTable")
                 )
               )
             )
    )
  ),
  material_tab_content(
    # material_side_nav_tab_content(
    # side_nav_tab_id =  "second_tab",
    material_parallax(
      image_source =
        "https://pixabay.com/get/eb36b40828f2083ed1584d05fb0938c9bd22ffd41cb310439cf1c679a1/luxembourg-2357069_1280.jpg"
    ),
    tab_id = "second_tab",
    tags$span("This feature of our app is designed to help you choose where you live. We scrape data on monthly rents
of apartments in Belgian, French and Luxembourguish communes (for France, we have data on cities).
Choose which country(ies) you would prefer to live in, as well as the amount you are willing to pay for rent.
This feature is still in very early development. In the future we will add data on Germany,
as well as commuting times from the communes or cities you chose to your place of work."),
    
    
    material_row(
      material_column(
        width=12,
        offset = ,
        material_card(
          depth=3,
          DT::dataTableOutput('tab_housing')
          
        )
      )
    )
  )
  
)




shinyApp(ui = ui, server = server)
