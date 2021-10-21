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


################################################################################
################################################################################
### HOUSEHOLD SIZE BY COUNTY (US CENSUS)
#varz <- load_variables(2019, "acs5", cache = TRUE)
#varz$label[grep("household", varz$label)] ##78th one, Estimate!!Total:!!In households:
#varz$name[varz$label=='Estimate!!Total:!!In households:']

## ACS Tables: https://api.census.gov/data/2016/acs/acs5/subject/groups/S1101.html
household_size <- get_acs(
  geography = "county",
  year = 2019,
  variables = c(house_size = "S1101_C01_002E")
)
### check nchar of fips
#table(nchar(household_size$GEOID))
### rename
household_size %>%
  rename(
    fips = "GEOID"
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
  select(-c(name, variable, year, state_y, county_name, office, candidate,
            version, mode, county_name_2, state, state_code, geographical_point,
            county_boundary, state_boundary, recip_county, recip_state)) %>%
  rename(
    mean_house_size = "estimate",
    error_house_size = "moe",
    state = "state_x",
    state_abbr = "state_po",
    winner2016 = "party"
    )-> df

library(fst)
write.fst(df, "C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/uptake.fst")
