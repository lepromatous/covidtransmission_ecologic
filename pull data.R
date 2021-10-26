################################################################################
################################################################################
############## TRANSMISSION ####################################################
################################################################################
################################################################################
library(tidyverse)
library(tidycensus)
library(readxl)
library(httr)
library(vroom)
library(RSocrata)
library(janitor)
library(sf)
library(fst)
library(mvmeta)
library(gsheet)

################################################################################
################################################################################
### HOUSEHOLD SIZE BY COUNTY (US CENSUS)
#varz <- load_variables(2019, "acs5", cache = TRUE)
#varz$label[grep("household", varz$label)] ##78th one, Estimate!!Total:!!In households:
#varz$name[varz$label=='Estimate!!Total:!!In households:']

## ACS Tables: https://api.census.gov/data/2016/acs/acs5/subject/groups/S1101.html
## drop moe from house size
get_acs(
  geography = "county",
  year = 2019,
  variables = c(house_size = "S1101_C01_002E",
                pop = "B01003_001E")
) %>%
  select(-c(moe)) -> household_size

household_size %>%
  pivot_wider(names_from = variable,
              values_from = estimate) -> household_size


### check nchar of fips
#table(nchar(household_size$GEOID))
### rename
### if add vars above later, rename drops the E
household_size %>%
  rename(
    fips = "GEOID",
    county_pop = "B01003_001",
    mean_house_size = "S1101_C01_002"
  ) -> household_size

################################################################################
################################################################################
### read MDPI from census - 2017 data but thats the lastest avaiable
url1 <- "https://www2.census.gov/programs-surveys/demo/tables/income-poverty/county-level-mdi-rates-2017.xls"
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
adi <- read_excel(tf)
adi <- adi[complete.cases(adi),]
### check nchar of FIPS
#table(nchar(adi$county))
### pad nchar
adi$county <- stringr::str_pad(adi$county, side = "left", pad = "0", width = 5)
### rename
adi %>%
  rename(
    fips = "county"
  ) -> adi

################################################################################
################################################################################
### VOTING 2016 (HARVARD DATAVERSE) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
prez <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/countypres_2000-2020.csv")
### pad fips
prez$county_fips <- stringr::str_pad(prez$county_fips, pad = "0", side = "left", width = 5)
### rename fips and only keep actual total 
### compute percent vote
prez %>%
  rename(
    fips = "county_fips"
  ) %>%
  filter(mode == "TOTAL") %>%
  mutate(
    prop.votes = candidatevotes/totalvotes) -> prez
## select top prop by year/fips
prez %>%
  group_by(year, fips) %>%
  top_n(1, prop.votes) %>%
  ungroup()-> prez
### sub by year. 
#prez2000 <- subset(prez, prez$year==2000)
#prez2004 <- subset(prez, prez$year==2004)
#prez2008 <- subset(prez, prez$year==2008)
#prez2012 <- subset(prez, prez$year==2012)
prez2016 <- subset(prez, prez$year==2016)
#prez2020 <- subset(prez, prez$year==2020)
rm(prez)
gc()
################################################################################
################################################################################
### COUNTY CASE COUNTS (NYT): https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv
covid <- vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
#table(nchar(covid$fips))
covid %>%
  group_by(fips) %>%
  mutate(
    new.cases = cases - lag(cases),
    new.deaths = deaths - lag(deaths)
  ) %>%
  ungroup() -> covid

################################################################################
################################################################################
### VACCINE UPTAKE BY COUNTY (CDC): https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
### slow to load
# uptake <- read.socrata(
#   "https://data.cdc.gov/resource/8xkx-amqh.csv",
#   app_token = 'chCxsk4zel6QXbaemotF65C9L',
#   email     = "tim.wiemken@gmail.com",
#   password  = "ThisIsNotAGoodP@ssw0rd!!!")
# ## remove UNK county
# uptake <- subset(uptake, uptake$fips!="UNK")
#table(nchar(uptake$fips))
uptake <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/uptake.csv")

################################################################################
################################################################################
### VACCINE HESITANCY BY COUNTY (CDC) https://data.cdc.gov/Vaccinations/Social-Vulnerability-Index/ypqf-r5qs (download csv)
hesitant <- vroom("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv")
### rename fips and pad 0
hesitant %>%
  rename(
    fips = "FIPS Code"
  ) %>%
  mutate(
    fips = stringr::str_pad(fips, side="left", pad="0", width=5)
  ) -> hesitant


################################################################################
################################################################################
### MERGE
covid %>%
  left_join(household_size, by="fips") %>%
  left_join(adi, by="fips") %>%
  left_join(prez2016, by="fips") %>%
  left_join(hesitant, by="fips") -> df

### clean environment  
rm(list=setdiff(ls(), c("df", "uptake")))
gc()

### clean data to merge uptake
min(uptake$date) ## date not the same as in df
df <- subset(df, df$date>=min(uptake$date))

### merge uptake
df <- merge(df, uptake, by=c("date", "fips"))
rm(uptake)

gc()

## clean names and drop dup/unused columns
df %>%
  janitor::clean_names() -> df

df %>%
  select(-c(name, year, state_y, county_name, office, candidate,
            version, mode, county_name_2, state, state_code, geographical_point,
            county_boundary, state_boundary, recip_county, recip_state)) %>%
  rename(
    state = "state_x",
    state_abbr = "state_po",
    winner2016 = "party"
    )-> df

### what month was peak of cases by county?
df %>%
  group_by(
    fips
  ) %>%
  slice_max(
            n = 1, 
            order_by = c(new_cases, date)
              ) %>%
  ungroup() -> test


### quartiles of hesitant
### also rename them to low medium high
df$hesitant_quartile <- factor(Hmisc::cut2(df$estimated_hesitant, g=3), levels = levels(Hmisc::cut2(df$estimated_hesitant, g=3)), labels = c("Low", "Medium", "High"))

### also cut at 3rd quartile. 
summary(df.mod$estimated_hesitant) # 0.1615 use df.mod from models.R 
df$hesitant_3rd_quartile <- factor(ifelse(df$estimated_hesitant<0.1615,0,1), levels=c(0,1), labels = c("Low Hesitancy", "High Hesitancy"))


### cut up series complete at 3rd quartile
summary(df.mod$series_complete_pop_pct)[5] ## 50.3 ### need to use df.mod from models.R b/c cant contaminate with duplicates

df$vax_3rd_quartile <- factor(ifelse(df$series_complete_pop_pct<50.3,0,1), levels=c(0,1), labels = c("Low Coverage", "High Coverage"))


### labor stats
## https://www.bls.gov/lau/
download.file("https://www.bls.gov/web/metro/laucntycur14.zip", "C:/Users/Wiemkt/Downloads/laucntycur14.zip")
unzip(zipfile="C:/Users/Wiemkt/Downloads/laucntycur14.zip", files = "laucntycur14.xlsx", exdir=".")
labor <- readxl::read_excel("laucntycur14.xlsx", skip = 4)
names(labor) <- c("laus_code", "fips_state", "fips_county", "name", "time", "n_labor_force", "n_employed", "n_unemployed", "unemployment_rate")
labor <- labor[-1,]
labor$time[labor$time=="Aug-21 p"] <- "Aug-21"

## dump zip file
file.remove("C:/Users/Wiemkt/Downloads/laucntycur14.zip") 

### make fips code
labor$fips <- stringr::str_pad(paste0(labor$fips_state, labor$fips_county), pad="0", side="left", width=5)

### create function for map to pivot wider by fips and time
fxn <- function(timez){
  labor %>%
    filter(time == timez) %>%
    pivot_wider(names_from = time, values_from = c(n_labor_force, n_employed, n_unemployed, unemployment_rate)) %>%
    ungroup()-> test
  return(test)
}

t <- names(table(labor$time))

### map over function for teach unique time period
t %>%
  purrr::map(
     ~fxn(timez = .)
            ) %>%
  bind_cols() -> huh
 

## find dup columns to drop
drops <- c(
  names(huh)[grep("laus", names(huh))][-1], ## keep first laus
  names(huh)[grep("fips", names(huh))][-3],## keep actual bound fips code
  names(huh)[grep("name", names(huh))] ### name not useful
  )

### clean
huh %>%
  select(-drops) %>%
  rename(
    laus_code = "laus_code...1",
    fips = "fips...5"
    )-> labor


### merge
df <- merge(df, labor, by="fips")
df %>%
  janitor::clean_names() -> df

df %>%
  mutate(
    unemployment_difference = unemployment_rate_aug_21 - unemployment_rate_aug_20
  ) -> df



#### add education from:  https://www.ers.usda.gov/data-products/county-level-data-sets/ stored on google drive

education <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1PjvHfRgOBu-UvGTQ-_dOGwFi0dbSXaF3NJRFJkNKSOk/edit?usp=sharing")
education %>%
  janitor::clean_names() %>%
  rename(
    fips = "fips_code"
  ) %>%
  right_join(df, by = "fips") -> df


### clean
df %>%
  select(-c(state.x)) %>%
  rename(
    state = "state.y"
  ) -> df



#data.world.token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmxlcHJvbWF0b3VzIiwiaXNzIjoiYWdlbnQ6bGVwcm9tYXRvdXM6OmRkZWZkYTRhLTdhODItNGE3ZC04YzQ5LWEyMTNjNDNiNTk1MyIsImlhdCI6MTU4NTY2Mzc1MSwicm9sZSI6WyJ1c2VyX2FwaV9yZWFkIiwidXNlcl9hcGlfd3JpdGUiXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlLCJzYW1sIjp7fX0.rqu14ONNZr9zwa5ClcH-PizxTuhd4yPdc1PEqJAIaIPgcuiexwr8ad1HEvfO8NHS1TIhROpbOR1sggWYmIaoTQ"

#NOAA token 
#token <- "ldWqEmdVQXPMFotRJzBkNSyLuRJstnIJ"

### other data from kaggle:

#https://www.kaggle.com/johnjdavisiv/us-counties-covid19-weather-sociohealth-data

test <- vroom::vroom("C:/Users/Wiemkt/Downloads/US_counties_COVID19_health_weather_data.csv")


library(fst)
write.fst(df, "C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/uptake.fst")
