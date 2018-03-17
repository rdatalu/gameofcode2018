# This function takes a list with gps coordinates of departure point and arrival point, as well as
# travel mode and returns the info (travel time, etc) using https://docs.api.tfl.lu/v1/en/RESTAPIs.html
# not used for the project in the end
get_journey = function(coordinates){

  from = coordinates$from
  to = coordinates$to

  api_call = glue::glue("https://api.tfl.lu/v1/Journey/{from}/to/{to}")

  result = jsonlite::read_json(api_call)

  return(result)

}

example_coordinates = list("from" = "49.59744,6.14077",
                           "to" = "49.542,6.19942")

journey = get_journey(example_coordinates)

home_to_work = list("from" = "49.6141,6.1185",
                    "to" = "49.6238,6.1497")

journey2 = get_journey(home_to_work)

home_to_cloche_dor = list("from" = "49.6141,6.1185",
                          "to" = "49.5809,6.1220")

journey3 = get_journey(home_to_cloche_dor)


                                        # need to try this: https://github.com/Kaweechelchen/mobiliteit/blob/master/app/mobiliteit/jsonControllerProvider.php


'http://travelplanner.mobiliteit.lu/'
. 'hafas/cdt/stboard.exe/en?L=vs_stb'
. '&start=yes'
. '&requestType=0'
. '&input=' . $stationId #need to add station id; perhaps from gtfs file?
. '&time=now'
. '&maxJourneys=' . $limit );
