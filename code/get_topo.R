# Attempt to use the topography api but
# in the end we did not


library(rgdal)
library(leaflet)

test <- rgdal::readOGR("https://opendata.vdl.lu/odaweb/?cat=553de34b711dbdb8177af9f5", "OGRGeoJSON", require_geomType = "wkbLineString")


leaflet(test) %>%
    addTiles() %>%
    addPolylines()
