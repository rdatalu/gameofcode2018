library(gtfsr)
library(tidyverse)

# This gets you the bus lines from start_name to end_name
# WITHOUT transfers! Only direct lines
get_bus_travel_direct = function(gtfs_lux, start_name, end_name){

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
      #filter(grepl(tolower(start_name), tolower(.$stop_name))) %>%
      pull(stop_id)

    if(length(start_stop_id) > 1){
        stop("The starting bus stop name is not precise enough, enter the correct bus stop name")
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
      #filter(grepl(tolower(start_name), tolower(.$stop_name))) %>%
      pull(stop_id)

    #end_stop_id = gtfs_lux$stops_df %>%
     #   filter(str_detect(tolower(stop_name), tolower(end_name))) %>%
      #filter(grepl(tolower(end_name), tolower(.$stop_name))) %>%
      #pull(stop_id)

    if(length(end_stop_id) > 1){
        stop("The ending bus stop name is not precise enough, enter the correct bus stop name")
        }

#    start_stop_id = gtfs_lux$stops_df %>%
#        filter(stop_lat == start_coordinates$lat,
#               stop_lon == start_coordinates$lon) %>%
#        pull(stop_id)
#
#    end_stop_id = gtfs_lux$stops_df %>%
#        filter(stop_lat == end_coordinates$lat,
#               stop_lon == end_coordinates$lon) %>%
#        pull(stop_id)

    trip_id_start = gtfs_lux$stop_times_df %>%
        filter(stop_id == start_stop_id)

    trip_id_end = gtfs_lux$stop_times_df %>%
        filter(stop_id == end_stop_id)


    trips = semi_join(trip_id_start,
                      trip_id_end, by = "trip_id")

    routes_direct = gtfs_lux$trips_df %>%
        filter(trip_id %in% trips$trip_id) %>%
        pull(route_id)

    lines = gtfs_lux$routes_df %>%
        filter(route_id %in% routes_direct) %>%
        pull(route_short_name)

    return(lines)
}


# Bei der Fiels
#start_coordinates = list("lat" = 49.61730,
#                  "lon" = 6.11333)

# Jean Monnet
#end_coordinates = list("lat" = 49.62477,
#                "lon" = 6.14539)

#get_bus_travel_direct(gtfs_lux, "Roudebierg", "Europe")
#
#get_bus_travel_direct(gtfs_lux, "der fiels", "scato")
#
## Generates error
#get_bus_travel_direct(gtfs_lux, "Kirchberg", "Pescatore")
#
## Try again
#get_bus_travel_direct(gtfs_lux, "Kirchberg, Jean Monnet", "Pescatore")
#
## Works without , too
#get_bus_travel_direct(gtfs_lux, "Kirchberg Jean Monnet", "Pescatore")
#
#get_bus_travel_direct(gtfs_lux, "Jules Fischer", "Rotonde")
#
#get_bus_travel_direct(gtfs_lux, "Bei der Fiels", "Kroll")




#trip_id_fiels = gtfs_lux$stop_times_df %>%
#  filter(stop_id == stopid_bei_der_fiels)
#
#trip_id_monnet = gtfs_lux$stop_times_df %>%
#  filter(stop_id == stopid_jean_monnet)

## get stop id
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
