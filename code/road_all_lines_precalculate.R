

new_trip %>%
  filter("Bei der Fiels", "Froment")

list_stop = bus_lines_stop %>%
  mutate(coord=paste0(stop_lon,',',stop_lat)) %>%
  group_by(line) %>%
  mutate(list_coord = paste0(coord,collapse = ";")) %>%
  filter(row_number(line) == 1)
head(list_stop$list_coord)

test = map(list_stop$list_coord,
    function(x){
      url_road = paste0("http://router.project-osrm.org/route/v1/driving/",x,"?overview=false&alternatives=false&steps=true")
      loc = RJSONIO::fromJSON(url_road)
      unlist_loc = unlist(loc$routes)
      val = unlist_loc[grepl("intersections.location",names(unlist_loc))]
      road = matrix(as.numeric(val),ncol=2, byrow=TRUE) %>%
        as.data.frame()
      colnames(road)=c("lon","lat")
      list(road=road)
      return(road)
    })

road_lines= list(line= list_stop$line, road = test )

saveRDS(paste0(here::here(), "/data/road_lines.rds"))
