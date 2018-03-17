library(tidyverse)
library(rvest)
library(here)

here()

url = "https://moovitapp.com/index/en/public_transit-line-1-Luxembourg-3827-865731-507648-0"

stops = list(lines = c("1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9",
            "10",
            "11",
            "12",
            "13",
            "14",
            "15",
            "16",
            "17",
            "18",
            "19",
            "20",
            "21",
            "22",
            "23",
            "24",
            "25",
            "26",
            "27",
            "28",
            "29",
            "30",
            "31",
            "32",
            "CN1",
            "CN2",
            "CN3",
            "CN4",
            "CSB"),
            urls = c("https://moovitapp.com/index/en/public_transit-line-1-Luxembourg-3827-865731-507648-0",
                     "https://moovitapp.com/index/en/public_transit-line-2-Luxembourg-3827-865731-546580-0",
                     "https://moovitapp.com/index/en/public_transit-line-3-Luxembourg-3827-865731-604922-0",
                     "https://moovitapp.com/index/en/public_transit-line-4-Luxembourg-3827-865731-430216-0",
                     "https://moovitapp.com/index/en/public_transit-line-5-Luxembourg-3827-865731-430217-0",
                     "https://moovitapp.com/index/en/public_transit-line-6-Luxembourg-3827-865731-546581-0",
                     "https://moovitapp.com/index/en/public_transit-line-7-Luxembourg-3827-865731-430219-0",
                     "https://moovitapp.com/index/en/public_transit-line-8-Luxembourg-3827-865731-472803-0",
                     "https://moovitapp.com/index/en/public_transit-line-9-Luxembourg-3827-865731-453618-0",
                     "https://moovitapp.com/index/en/public_transit-line-10-Luxembourg-3827-865731-430222-0",
                     "https://moovitapp.com/index/en/public_transit-line-11-Luxembourg-3827-865731-450705-0",
                     "https://moovitapp.com/index/en/public_transit-line-12-Luxembourg-3827-865731-430224-0",
                     "https://moovitapp.com/index/en/public_transit-line-13-Luxembourg-3827-865731-472805-0",
                     "https://moovitapp.com/index/en/public_transit-line-14-Luxembourg-3827-865731-430226-0",
                     "https://moovitapp.com/index/en/public_transit-line-15-Luxembourg-3827-865731-430227-0",
                     "https://moovitapp.com/index/en/public_transit-line-16-Luxembourg-3827-865731-495949-0",
                     "https://moovitapp.com/index/en/public_transit-line-17-Luxembourg-3827-865731-457580-0",
                     "https://moovitapp.com/index/en/public_transit-line-18-Luxembourg-3827-865731-483970-0",
                     "https://moovitapp.com/index/en/public_transit-line-19-Luxembourg-3827-865731-707732-0",
                     "https://moovitapp.com/index/en/public_transit-line-20-Luxembourg-3827-865731-707733-0",
                     "https://moovitapp.com/index/en/public_transit-line-21-Luxembourg-3827-865731-483972-0",
                     "https://moovitapp.com/index/en/public_transit-line-22-Luxembourg-3827-865731-472807-0",
                     "https://moovitapp.com/index/en/public_transit-line-23-Luxembourg-3827-865731-472808-0",
                     "https://moovitapp.com/index/en/public_transit-line-24-Luxembourg-3827-865731-453622-0",
                     "https://moovitapp.com/index/en/public_transit-line-25-Luxembourg-3827-865731-430237-0",
                     "https://moovitapp.com/index/en/public_transit-line-26-Luxembourg-3827-865731-677804-0",
                     "https://moovitapp.com/index/en/public_transit-line-27-Luxembourg-3827-865731-483973-0",
                     "https://moovitapp.com/index/en/public_transit-line-28-Luxembourg-3827-865731-675869-0",
                     "https://moovitapp.com/index/en/public_transit-line-29-Luxembourg-3827-865731-628996-0",
                     "https://moovitapp.com/index/en/public_transit-line-30-Luxembourg-3827-865731-586870-0",
                     "https://moovitapp.com/index/en/public_transit-line-31-Luxembourg-3827-865731-707734-0",
                     "https://moovitapp.com/index/en/public_transit-line-31-Luxembourg-3827-865731-707734-0",
                     "https://moovitapp.com/index/en/public_transit-line-CN1-Luxembourg-3827-865731-450716-0",
                     "https://moovitapp.com/index/en/public_transit-line-CN2-Luxembourg-3827-865731-450717-0",
                     "https://moovitapp.com/index/en/public_transit-line-CN3-Luxembourg-3827-865731-450718-0",
                     "https://moovitapp.com/index/en/public_transit-line-CN4-Luxembourg-3827-865731-450719-0",
                     "https://moovitapp.com/index/en/public_transit-line-CSB-Luxembourg-3827-865731-430245-0")
            )

get_stations = function(line, url){

  station = url %>%
    read_html() %>%
    html_text() %>%
    str_split("\n", simplify = TRUE) %>%
    str_trim() %>%
    `[`(which(str_detect(., "Direction:")) : which(str_detect(., "Moovit is the world's"))) %>%
    str_match_all("[:upper:][:lower:]+,\\s.+") %>%
    unlist

  result = tibble("line" = line, "station" = station)

}


stations = map2_df(stops$lines,
                stops$urls,
                get_stations)

rio::export(stations, paste0(here(), "/bus_avl.csv"))
