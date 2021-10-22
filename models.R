################################################################################
################################################################################
############## 
############## Transmission analysis
############## Build data with pull data.R
############## 
################################################################################
################################################################################
################################################################################
################################################################################

### https://stackoverflow.com/questions/35372090/clustered-standard-error-for-zero-inflated-negative-binomial-model 
library(pscl)
library(sandwich)
library(lmtest)
library(MASS)
library(Hmisc)
library(tidyverse)

### read data
df <- read.fst("C:/Users/Wiemkt/OneDrive - Pfizer/Documents/Research/COVID Transmission/covidtransmission_ecologic/uptake.fst")

### select only most recent date by county
df %>%
  group_by(
    fips
  ) %>%
  arrange(
    date
    ) %>%
  slice(
    n()
  ) -> df.mod


### keep states only
df.mod %>%
  filter(
    state %nin% c("District of Columbia", "Puerto Rico", "Virgin Islands")
  ) -> df.mod

### Run model  
mod1 <- MASS::glm.nb(cases ~ mean_house_size + 
                       winner2016 + 
                       hesitant_3rd_quartile + 
                       relevel(factor(svi_category), ref="Very Low Vulnerability") + 
                       vax_3rd_quartile  + 
                       vax_3rd_quartile  * hesitant_3rd_quartile + 
                       # mean_house_size * relevel(factor(svi_category), ref="Very Low Vulnerability") +  ### not significant
                       offset(log(county_pop)), 
                     data=df.mod)
### look
summary(mod1)

### cluster SE by state
coeftest(mod1, vcov = vcovCL(mod1, cluster = df.mod$state))


table(df.mod$svi_category)
exp( 0.1360144)

