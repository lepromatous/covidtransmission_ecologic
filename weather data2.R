library(rnoaa)
library(Hmisc)
library(tidyverse)
library(rvest)
library(tigris)
library(sf)
library(zoo)


### get station names from rnoaa
station_names <- isd_stations()
### add moyr to filter out old stations that wont have data anymore
station_names$end<- anytime::anydate(station_names$end)
station_names$end_moyr <- zoo::as.yearmon(station_names$end)

### do the filtering and create station_id
### drop non US states
station_names %>%
  filter(ctry %in% "US",
         end_moyr == "Nov 2021") %>%
  mutate(station_id = paste0(usaf, wban)) %>%
  filter(state %nin% c("", "PR", "VI")) -> station_names


### pull map using tigris - filter out non us states
map <- tigris::counties(cb = T)
map %>%
  filter(STATEFP %nin%c(72, 60, 66, 69, 78 )) -> map

### get centroids of counties and create sf object and create df from it
centroids <- sf::st_centroid(map$geometry)
centroids <- data.frame(t(sapply(centroids, function(x) unlist(x))))
centroids <- sf::st_as_sf(centroids, coords=c("X1", "X2"))

### set crs for centroids since decimal degrees use 4269
sf::st_crs(centroids) <- 4269
### fix projection to albers
centroids<-tigris::shift_geometry(centroids)
map<-tigris::shift_geometry(map)

### remove station names that dont have lat/long and convert to sf
station_names %>%
  filter(!is.na(lon),
         !is.na(lat)) -> station_names
station_names <- sf::st_as_sf(station_names, coords = c("lon", "lat")) 

### set crs for centroids since decimal degrees use 4269
sf::st_crs(station_names) <- 4269
### fix projection to albers
station_names<-tigris::shift_geometry(station_names)

### find nearest weather station to centroid
centroids %>% 
  #dplyr::group_by( id ) %>%
  dplyr::mutate( np = sf::st_nearest_feature( geometry, station_names )) -> test
### this  gives centroid geometry and weather station name - need county name for centroid       

### add back useful stuff from station_names       
test$station_id <- station_names$station_id[test$np]                
test$state <- station_names$station_name[test$np]                
test$state <- station_names$state[test$np]                
### manual spot check seems to work fine

### put nearest station in map object
map$station_id <- test$station_id

### extract map and remove geometry
stations <- map
st_geometry(stations)<-NULL

#write.table(stations, "/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidtransmission_ecologic/stations_and_counties.csv", sep=",", row.names=F, na="")

### several stations are matched to multiple centroids
#sort(table(stations$station_id), decreasing=T)

### uniques only
unique.stations <- unique(stations$station_id)

## clear environment
rm(list=setdiff(ls(), c("unique.stations", "stations")))
gc()


### pull urls from ftp
url2020 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2020/"
url2021 <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2021/"


### scrape links using rvest
url2020 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2020_links
### drop non csv files
weather2020_links <- weather2020_links[grep(".csv", weather2020_links)]
## find just ids in our dataset
### remove .csv from link then match to uniques only - things we want to pull
### then add .csv back in for links
weather2020_links <- gsub(".csv", "", weather2020_links)
weather2020_links <- weather2020_links[weather2020_links%in%unique.stations]
weather2020_links <- paste0(weather2020_links, ".csv")

### same for 2021
url2021 %>% 
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") -> weather2021_links
weather2021_links <- weather2021_links[grep(".csv", weather2021_links)]
## find just ids in our dataset
weather2021_links <- gsub(".csv", "", weather2021_links)
weather2021_links <- weather2021_links[weather2021_links%in%unique.stations]
weather2021_links <- paste0(weather2021_links, ".csv")

### create full web link
links2020 <- paste0(url2020, weather2020_links)
links2021 <- paste0(url2021, weather2021_links)

### map links to vroom to read in all data. 
links2020[1:500]%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat1

### bind to one df
test1 <- data.table::rbindlist(dat1)
test1$station_id <- as.character(test1$STATION)
test1 <- as.data.frame(test1)

### part 2
links2020[501:1000]%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat2

### bind to one df chunk 2
test2 <- data.table::rbindlist(dat2)
test2$station_id <- as.character(test2$STATION)
test2 <- as.data.frame(test2)

### part 3
links2020[1001:1400]%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat3

### bind to one df chunk 2
test3 <- data.table::rbindlist(dat3)
test3$station_id <- as.character(test3$STATION)
test3 <- as.data.frame(test3)
test3 <- merge(test3, stations[unique(stations$station_id),], by="station_id", all=T)


### part 4
links2020[1401:1740]%>%
  purrr::map(
    ~vroom::vroom(.)
  ) -> dat4

### bind to one df chunk 2
test4 <- data.table::rbindlist(dat4)
test4$station_id <- as.character(test4$STATION)
test4 <- as.data.frame(test4)
test4 <- merge(test4, stations[unique(stations$station_id),], by="station_id", all=T)






df2020 <- plyr::rbind.fill(test1,test2, test3, test4)

df2020 <- merge(df2020, stations[unique(stations$station_id),], by="station_id", all=T)
write.table(df2020, "~/Desktop/weather2020.csv", sep=",", row.names=F, na="")
