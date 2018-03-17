# Gets weather alerts
# this function has been copied into the the shiny code
# it stays here for posterity, but is could be erased

get_weather_alert = function(){
weather_alert = read.csv("http://meteolux.lu/Opendata/data_alerts.csv",
                          sep = ";", skip = 2)

vigilance = tibble("code" = weather_alert$COLOR,
                   "message" = weather_alert$VALUE)

vigilance = vigilance %>%
    mutate(message = ifelse(is.na(message), "RAS", message))

return(vigilance)
}
