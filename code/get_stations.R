# This function creates a data set with all the bus stops for all
# the lines and save it in /data

gtfs_lux = readRDS(paste0(here::here(), "/data/gtfs_lux.rds"))

get_stops = function(bus, gtfs_lux){

    bus_route_id = gtfs_lux$routes_df %>%
        filter(route_short_name == bus) %>%
        pull(route_id)


    bus_trips = gtfs_lux$trips_df %>%
        filter(route_id == bus_route_id) %>%
        pull(trip_id)

    bus_stop_id = gtfs_lux$stop_times_df %>%
        filter(trip_id %in% bus_trips) %>%
        pull(stop_id)

    result = gtfs_lux$stops_df %>%
        filter(stop_id %in% bus_stop_id)

    result$line = bus

    result = result %>%
        select(line, stop_id, stop_name, stop_lat, stop_lon)

    return(result)

}

all_lines = unique(gtfs_lux$routes_df$route_short_name)

bus_lines_stops = all_lines %>%
    map_df(get_stops, gtfs_lux = gtfs_lux)

rio::export(bus_lines_stops, paste0(here::here(), "/data/bus_lines_stops.csv"))

bus_lines_stops_collapsed = bus_lines_stops %>%
    group_by(stop_name) %>%
    mutate(lines = paste(line, collapse = ","))

saveRDS(bus_lines_stops_collapsed, paste0(here::here(), "/data/bus_lines_stops_collapsed.rds"))



#if (!require(devtools)) {
#  install.packages('devtools')
#}
#devtools::install_github('ropensci/gtfsr')

#library(gtfsr)
#library(tidyverse)
#
#gtfs_lux = import_gtfs("https://download.data.public.lu/resources/gtfs/20180228-140138/VV_GTFS_20180305_20180531.zip")

#saveRDS(gtfs_lux, paste0(here::here(), "/data/gtfs_lux.rds"))

#
#
##set_api_key()
#
##"8e2c3424-d09b-4ecf-ade1-ab76235dbfcd"
#
#
## First step: I want to go from home to work
#
## Arret Bei der Fiels until Jean Monnet
#
## get stop id
#(
#    stopid_bei_der_fiels = gtfs_lux$stops_df %>%
#  filter(grepl("Bei der Fiels", .$stop_name)) %>%
#  pull(stop_id)
#)
#
#(
#stopid_jean_monnet = gtfs_lux$stops_df %>%
#  filter(grepl("Monnet", .$stop_name)) %>%
#  pull(stop_id)
#)
#
#(
#trip_id_fiels = gtfs_lux$stop_times_df %>%
#  filter(stop_id == stopid_bei_der_fiels)
#)
#
#(
#trip_id_monnet = gtfs_lux$stop_times_df %>%
#  filter(stop_id == stopid_jean_monnet)
#)
#
#(
#trips = semi_join(trip_id_fiels, trip_id_monnet, by = "trip_id")
#)
#
#(
#routes_fiels_monnet = gtfs_lux$trips_df %>%
#  filter(trip_id %in% trips$trip_id) %>%
#  pull(route_id)
#)
#
#(
#lines = gtfs_lux$routes_df %>%
#  filter(route_id %in% routes_fiels_monnet) %>%
#  pull(route_short_name)
#)
#
##map_gtfs_stop(gtfs_lux, stop_id = "000200403005")
#
##map_gtfs(gtfs_lux, route_ids = c("1489"))
#
##huhu = routes_df_as_sf(gtfs_lux)
#
## AVL routes
#route_avl = gtfs_lux$routes_df %>%
#  filter(agency_id == 6)
#
#trips_avl = gtfs_lux$trips_df %>%
#  filter(route_id %in% route_avl$route_id)
#
#
#strassen_primeur = trips_avl %>%
#  filter(trip_headsign == "Strassen, Primeurs")
#
#strassen_stops = gtfs_lux$stop_times_df %>%
#  filter(trip_id %in% strassen_primeur$trip_id)
#
#
#strassen_stops %>% left_join(gtfs_lux$routes_df, by = "trip_id")


# All the stops from a line

# Example bus 21
