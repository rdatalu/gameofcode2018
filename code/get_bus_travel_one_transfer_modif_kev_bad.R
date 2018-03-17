get_bus_travel_one_transfer = function(gtfs_lux, start_name, end_name){
  #start_name = "Bei der Fiels"
  #end_name ="Froment"
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
    
    bus_lines_stop = read_csv(paste0(here::here(),"/data/bus_lines_stops.csv"))
    head( bus_lines_stop)
    stations_link = bus_lines_stop %>%
      filter(line %in% lines) %>%
      inner_join(unique(gtfs_lux$stop_times_df[,c("stop_id", "stop_sequence")]), by="stop_id")
    lines[1]
    
    trip1= unique(new_trip$trip_id[new_trip$stop_id %in% start_stop_id])
    trip2= unique(new_trip$trip_id[new_trip$stop_id %in% end_stop_id])
    
    good_trip = trip1[trip1 %in% trip2]
    trip2 = new_trip %>%
      filter(trip_id==trip1)
    from_num = trip2$stop_sequence[trip2$stop_id==start_stop_id]
    to_num = trip2$stop_sequence[trip2$stop_id==start_stop_id]
    
    
    
    trip1= new_trip %>%
      filter( c(start_stop_id,end_stop_id) %in% stop_id) %>%
      .[1,1]
    
    trip2 = gtfs_seq %>%
      filter(trip_id==as.character(good_trip))
    from_num = trip2$stop_sequence[trip2$stop_id==start_stop_id]
    to_num = trip2$stop_sequence[trip2$stop_id==end_stop_id]
    
    
      left_join(unique(bus_lines_stop[,c("stop_id","line")]),by="stop_id") %>%
      filter(line %in% lines[1]) %>%
      group_by(trip_id) 

      group_by()
    head(bus_lines_stop)
    
    trip = new_trip %>%
      filter(line %in% lines) %>%
      group_by(trip_id) %>%
      top_n(1)
    
    return(lines)

}


get_bus_travel_one_transfer(gtfs_lux, "Bei der Fiels", "Froment")
get_bus_travel_one_transfer(gtfs_lux, "Bei der Fiels", "Plantin")
get_bus_travel_direct(gtfs_lux, "Bei der Fiels", "Froment")


new_trip = gtfs_seq %>%
  left_join(unique(bus_lines_stop[,c("stop_id","stop_name", "stop_lat", "stop_lon")]),by="stop_id") %>%
  group_by(trip_id) %>%
  mutate(station_from = stop_name[which.min(stop_sequence)], 
         station_to = stop_name[which.max(stop_sequence)])
