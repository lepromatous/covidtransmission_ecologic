library(rnoaa)
library(Hmisc)
library(tidyverse)
library(rvest)

station_names <- isd_stations()
bad_stations <- c("72211800482", #Sarasota FL
                  "72334703809", #Dyer, TN
                  "99818099999", #Sanilac, MI
                  "99726099999", #Sanilac MI
                  "72522904858",
                  "72340703953",
                  "72028803711",
                  "74003024103", #Tooele, UT
                  "72575399999", #Tooele, UT
                  "91197721508", #Also in the mountains on Hawaii
                  "99999921514") #On top of a volcano at 11,000' in Hawaii


station_names %>%
  filter(station_name %nin% bad_stations,
         ctry %in% "US") %>%
  mutate(station_id = paste0(usaf, wban)) %>%
  filter(state %nin% c("", "PR", "VI")) -> station_names


url2020 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2020/"
url2021 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2021/"

url2020 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2020_links
weather2020_links <- weather2020_links[grep(".csv", weather2020_links)]
url2021 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2021_links
weather2021_links <- weather2021_links[grep(".csv", weather2021_links)]

links <- paste0(url2020, weather2020_links)
 
links[1:10]%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat

#test <- vroom::vroom(paste0(url2020, weather2020_links[1]))

test <- data.table::rbindlist(dat)

map <- tigris::counties(cb = T)
map %>%
  filter(STATEFP %nin%c(72, 60, 66, 69, 78 ))  -> map
  #tigris::shift_geometry()-> map  # move ak/HI

### get centroids of counties and create sf objecct
centroids <- sf::st_centroid(map$geometry)

centroids <- data.frame(t(sapply(centroids, function(x) unlist(x))))
centroids <- sf::st_as_sf(centroids, coords=c("X1", "X2"))

### set crs for centroids since decimal degrees use 4269
sf::st_crs(centroids) <- 4269
### fix projection
centroids<-sf::st_transform(centroids, crs =5070)
                            #crs='ESRI:102003')
map <- sf::st_transform(map, crs =5070)

station_names %>%
  filter(!is.na(lon),
         !is.na(lat)) -> station_names
station_names <- sf::st_as_sf(station_names, coords = c("lon", "lat")) 

map <- sf::st_transform(map, crs='ESRI:102003')
centroids <- sf::st_transform(centroids, crs='ESRI:102003')

ggplot()+
 geom_sf(data=map) + 
  geom_sf(data=centroids, size=1)



