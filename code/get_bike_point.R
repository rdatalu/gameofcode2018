# This unfinished function was supposed to give us the nearest
# bike station. Unfortunately, on the day of the hackathon,
# api.tfl.lu was down

get_bike_point = function(coordinates, radius = "300"){

  lon = coordinates$lon
  lat = coordinates$lat

  api_call = glue::glue("https://api.tfl.lu/v1/BikePoint/around/{lon}/{lat}/{radius}")

  result = jsonlite::read_json(api_call)

  return(result)
}

# What happens if radius too short? then increase with while loop
# return id
# check if bikes available

#test_coordinates = list("lon" = "6.133646",
#                        "lat" = "49.60067	")
#
#
#get_bike_point(test_coordinates)
#
#work_coordinates = list("lat" = "49.62379",
#                        "lon" = "6.14967")
#
#
#get_bike_point(work_coordinates)
