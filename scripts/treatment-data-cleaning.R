
###################
## Environment Setup
###################

setwd("~/Code/loud-study/scripts")

library(tidyverse)
library(sf)
library(tmap)

fac <- read.csv("../indicators_raw/facility-detail.csv", header = TRUE)
str(fac) #662
glimpse(fac)

# Original data file name:
# FindTreament_Facility_listing_2025_08_19_150020

###################
## SUPPORTIVE SERVICES
###################
## Data Review
###################

# Mental health treatment - will = 0 with substance use
#fac.mh <- fac %>%
#  filter(mh == 1) #294

# Housing services
fac.hs <- fac %>%
  filter(hs == 1) #350

# Mentoring/ peer support
fac.peer <- fac %>%
  filter(peer == 1) #412

# Case management service
fac.cm <- fac %>%
  filter(cm == 1) #527

# Telemedicine/ telehealth therapy
fac.tele <- fac %>%
  filter(tele == 1) #538

# Mental health services
fac.mhs <- fac %>%
  filter(mhs == 1) #270

# Integrated primary care services
fac.ipc <- fac %>%
  filter(ipc == 1) #162

# Transportation assistance
fac.ta <- fac %>%
  filter(ta == 1) #232

# Vocational rehabilitation services
fac.vrs <- fac %>%
  filter(vrs == 1) #55

# Supported employment
fac.semp<- fac %>%
  filter(semp == 1) #38

# Chronic disease/illness management
fac.cdm<- fac %>%
  filter(cdm == 1) #52

# Legal advocacy
fac.lad<- fac %>%
  filter(lad == 1) #23

# Combination: Any of the above, as part of SU category
fac.supportive<- fac %>%
  filter(type_facility == "SU" & 
           (hs == 1 |
              peer == 1 |
              cm == 1 | 
              tele == 1 |
              mhs == 1 |
              ipc == 1 |
              ta == 1 |
              vrs == 1 |
              semp == 1 |
              cdm == 1 | 
              lad == 1
              )) #366

fac.supportive <- select(fac.supportive, name1, type_facility, longitude, latitude)
fac.supportive.sf <- st_as_sf(fac.supportive, coords = c("longitude","latitude"), crs = 4326)
tm_shape(fac.supportive.sf) + tm_dots()


### Do it for the whole U.S.

fac.us <- read.csv("../indicators_raw/us-facility-detail.csv", header = TRUE)
str(fac.us) #13336

# Combination: Any of the above, as part of SU category
fac.supportive<- fac.us %>%
  filter(type_facility == "SU" & 
           (hs == 1 |
              peer == 1 |
              cm == 1 | 
              tele == 1 |
              mhs == 1 |
              #ipc == 1 |
              ta == 1 ))
              #vrs == 1 |
              #semp == 1 |
             # cdm == 1 | 
              #lad == 1 ))
            #13230

fac.supportive <- na.omit(select(fac.supportive, name1, type_facility, longitude, latitude))
head(fac.supportive) #11923
fac.supportive.sf <- st_as_sf(fac.supportive, coords = c("longitude","latitude"), crs = 4326)
tm_shape(fac.supportive.sf) + tm_dots()

write.csv(fac.supportive, "../indicators_raw/us-supportive-services.csv",row.names = FALSE)

###################
## Abstinence-Based Services
###################
## Data Review
###################

# 12-step facilitation
fac.twfa<- fac %>%
  filter(twfa == 1) #177

# Does not use MAT for opioid use disorders
fac.nmoa<- fac %>%
  filter(nmoa == 1) #1

# Opioids detoxification
fac.odtx<- fac %>%
  filter(odtx == 1) #58

# Detoxification
fac.dt <- fac %>%
  filter(dt == 1) #59

# Combination: Any of the above
fac.abstinence<- fac %>%
  filter(twfa == 1 |  nmoa == 1 | odtx == 1 | dt == 1 ) #202

fac.abstinence <- select(fac.abstinence, name1, type_facility, longitude, latitude)
fac.abstinence.sf <- st_as_sf(fac.abstinence, coords = c("longitude","latitude"), crs = 4326)
tm_shape(fac.abstinence.sf) + tm_dots()

# Combination: Most Strict
fac.abstinence.2<- fac %>%
  filter(twfa == 1 |  nmoa == 1 ) #178

fac.abstinence.2 <- na.omit(select(fac.abstinence.2, name1, type_facility, longitude, latitude))
#6064
fac.abstinence.2.sf <- st_as_sf(fac.abstinence.2, coords = c("longitude","latitude"), crs = 4326)
tm_shape(fac.abstinence.2.sf) + tm_dots()

write.csv(fac.abstinence.2, "../indicators_raw/us-abstinence-services.csv",row.names = FALSE)

test<- st_drop_geometry(fac.abstinence.2.sf)
head(fac.abstinence.2)
write.csv(fac.abstinence.2, "../indicators_raw/us-abstinence-services.csv",row.names = FALSE)



### Didn't use below


###################
## Residential Services
###################
## Data Review
###################

# Residential/24-hour residential
fac.res<- fac %>%
  filter(res == 1) #170

# Residential treatment center (RTC) for adults
fac.rtca<- fac %>%
  filter(rtca == 1) #41

# Residential detoxification
fac.rd<- fac %>%
  filter(rd == 1) #20

# Long-term residential
fac.rl<- fac %>%
  filter(rl == 1) #82

# Short-term residential
fac.rs<- fac %>%
  filter(rs == 1) #28

# Combination: Any of the above
fac.residential<- fac %>%
  filter(res == 1 | rtca == 1 | rd == 1 | rl == 1 | rs == 1 ) #170

fac.residential <- select(fac.residential, name1, type_facility, longitude, latitude)
fac.residential.sf <- st_as_sf(fac.residential, coords = c("longitude","latitude"), crs = 4326)
tm_shape(fac.residential.sf) + tm_dots()
