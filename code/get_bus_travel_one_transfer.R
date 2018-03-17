# Attempt to get bus trips from start_name to end_name with one transfer
# Not really working; not used for the project

get_bus_travel_one_transfer = function(gtfs_lux, start_name, end_name){

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

    trip_id_start = gtfs_lux$stop_times_df %>%
        filter(stop_id == start_stop_id)

    trip_id_end = gtfs_lux$stop_times_df %>%
        filter(stop_id == end_stop_id)


    trips_wrong = anti_join(trip_id_start,
                      trip_id_end, by = "trip_id")

    trips_too_many = full_join(trip_id_start,
                      trip_id_end, by = "trip_id")


    routes_direct_wrong = gtfs_lux$trips_df %>%
        filter(trip_id %in% trips_wrong$trip_id) %>%
        pull(route_id)


    routes_direct_too_many = gtfs_lux$trips_df %>%
        filter(trip_id %in% trips_too_many$trip_id) %>%
        pull(route_id)

    routes_transfer = setdiff(routes_direct_too_many, routes_direct_wrong)

    lines = gtfs_lux$routes_df %>%
        filter(route_id %in% routes_transfer) %>%
        pull(route_short_name)

    return(lines)

}


#get_bus_travel_one_transfer(gtfs_lux, "Bei der Fiels", "Froment")
#get_bus_travel_one_transfer(gtfs_lux, "Bei der Fiels", "Plantin")
#get_bus_travel_direct(gtfs_lux, "Bei der Fiels", "Froment")
