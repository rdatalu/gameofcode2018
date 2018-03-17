# This function returns all the bus stops for the given bus
# Needs gtfs file that you can find in /data (or on open data portal)

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

  bus_stops = gtfs_lux$stops_df %>%
      filter(stop_id %in% bus_stop_id)

    bus_stops$line = bus

    bus_stops = bus_stops %>%
        select(line, everything())

  return(bus_stops)

}


#get_stops(18, gtfs_lux)





#if (!require(devtools)) {
#  install.packages('devtools')
#}
#devtools::install_github('ropensci/gtfsr')

#
#gtfs_lux = import_gtfs("https://download.data.public.lu/resources/gtfs/20180228-140138/VV_GTFS_20180305_20180531.zip")
#
#
##set_api_key()
#
##"8e2c3424-d09b-4ecf-ade1-ab76235dbfcd"
#
#
## First step: I
