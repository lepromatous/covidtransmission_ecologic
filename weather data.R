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
  mutate(station_id = paste0(usaf, wban)) -> station_names


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

links %>%
  purrr::map(
    ~fxn(timez = .)
  ) %>%
  bind_cols() -> huh
         