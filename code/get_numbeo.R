# In the end, we did not use this
# Allows you to compare the cost of living
# in two cities. But there are not enough
# cities to make this really useful
# To avoid having to waste time with paths
# install.packages("here")
library(here)
library(tidyverse)
library(rvest)
library(janitor)
library(glue)



country1 = "France"
country2 = "Luxembourg"

city1 = "Ottange"
city2 = "Luxembourg"



get_rents = function(city_country1, city_country2){

  country1 = city_country1$country
  country2 = city_country2$country

  city1 = city_country1$city
  city2 = city_country2$city

  url = "https://www.numbeo.com/property-investment/compare_cities.jsp?country1={country1}&country2={country2}&city1={city1}&city2={city2}"

  raw_data = read_html(glue::glue(url)) %>%
    html_table(fill=TRUE)

  rent_prices = raw_data[[4]]

}

city_country1 = list(city = "Trier", country = "Germany")
city_country2 = list(city = "Luxembourg", country = "Luxembourg")


(test = get_rents(city_country1, city_country2))
