library(rnoaa)
library(Hmisc)
library(tidyverse)
library(rvest)
library(tigris)
library(sf)


### get station names and drop some crappy stations
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


### pull map 
map <- tigris::counties(cb = T)
map %>%
  filter(STATEFP %nin%c(72, 60, 66, 69, 78 )) -> map

### get centroids of counties and create sf objecct
centroids <- sf::st_centroid(map$geometry)
centroids <- data.frame(t(sapply(centroids, function(x) unlist(x))))
centroids <- sf::st_as_sf(centroids, coords=c("X1", "X2"))

### set crs for centroids since decimal degrees use 4269
sf::st_crs(centroids) <- 4269
### fix projection
centroids<-tigris::shift_geometry(centroids)
map<-tigris::shift_geometry(map)


station_names %>%
  filter(!is.na(lon),
         !is.na(lat)) -> station_names
station_names <- sf::st_as_sf(station_names, coords = c("lon", "lat")) 

### set crs for centroids since decimal degrees use 4269
sf::st_crs(station_names) <- 4269
### fix projection
station_names<-tigris::shift_geometry(station_names)



### find nearest weather station to centroid
centroids %>% 
  #dplyr::group_by( id ) %>%
  dplyr::mutate( np = sf::st_nearest_feature( geometry, station_names )) -> test
### this just gives centroid geometry and weather station name - need county name for centroid       

### add back useful stuff          
test$station_id <- station_names$station_id[test$np]                
test$state <- station_names$station_name[test$np]                
test$state <- station_names$state[test$np]                


#st_geometry(test)<-NULL
### put nearest station in map
map$station_id <- test$station_id

### extract map
stations <- map
st_geometry(stations)<-NULL

#write.table(stations, "/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidtransmission_ecologic/stations_and_counties.csv", sep=",", row.names=F, na="")



### uniques
unique.stations <- unique(stations$station_id)

## clear
rm(list=setdiff(ls(), c("unique.stations")))
gc()

url2020 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2020/"
url2021 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2021/"

url2020 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2020_links
### drop non csv files
weather2020_links <- weather2020_links[grep(".csv", weather2020_links)]
## find just ids in our dataset
weather2020_links <- gsub(".csv", "", weather2020_links)
weather2020_links <- weather2020_links[weather2020_links%in%unique.stations]
weather2020_links <- paste0(weather2020_links, ".csv")

url2021 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2021_links
weather2021_links <- weather2021_links[grep(".csv", weather2021_links)]
## find just ids in our dataset
weather2021_links <- gsub(".csv", "", weather2021_links)
weather2021_links <- weather2021_links[weather2021_links%in%unique.stations]
weather2021_links <- paste0(weather2021_links, ".csv")

links2020 <- paste0(url2020, weather2020_links)
links2021 <- paste0(url2021, weather2021_links)

links2020%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat

#test <- vroom::vroom(paste0(url2020, weather2020_links[1]))

test <- data.table::rbindlist(dat)
test$station_id <- as.character(test$STATION)
test2 <- merge(test, stations, by="station_id", all.x=T)
