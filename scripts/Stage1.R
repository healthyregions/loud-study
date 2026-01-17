library(tidyverse)
setwd("~/Code/loud-study/scripts")

############
# Read in Data 
############

### Internet Access ###
internet <- read.csv("../indicators_raw/internet_tract23.csv")

internet.loud <- internet %>%
  select(HEROP_ID,CompHhldsP,BbndInternetP)

head(internet.loud) #84400

internet.loud <- select(internet.loud,HEROP_ID,CompHhldsP,BbndInternetP)
head(internet.loud)
  
internet.loud$IntInd <- (scale(internet.loud$CompHhldsP)+ scale(internet.loud$BbndInternetP))/2
head(internet.loud)

### Census Response Rate ###
CensusRate.county <- read.csv("../indicators_raw/RspRt_county_2023.csv")
head(CensusRate.county)

CensusRate.county$HEROP_County <- str_sub(CensusRate.county$HEROP_ID, 6,10)
head(CensusRate.county)

censusRate.loud <- select(CensusRate.county,HEROP_County,RspRt)
head(censusRate.loud)


### Social Capital ###

social.capital <- read.csv("../indicators_raw/Social_Capital_Measures_2023.csv")
head(social.capital)

socialcapital.loud <- social.capital %>%
  select(HEROP_ID,LibPerCap,RlgPerCap, LngTermP,SocCapInd) 

socialcapital.loud$LibPer10k <- socialcapital.loud$LibPerCap * 10000
socialcapital.loud$RlgPer10k <- socialcapital.loud$RlgPerCap * 10000
head(socialcapital.loud)


#### Limited English Proficiency

## OEPS 2023 data package -- too large to store in Git, reading locally
oeps <- read.csv("~/Code/tract.csv")
head(oeps)

english <- select(oeps,HEROP_ID,EngProf)
head(english)
summary(english)

english$EngProf <- english$EngProf * 100
head(english)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
english$LimEngProfSc <- english$EngProf * (-1)
head(english)

english.loud <- english
head(english)


############
# Merge stage 3 measures 
############

loud.stage1.1 <- left_join(english.loud, internet.loud, by="HEROP_ID")
loud.stage1.2 <- left_join(loud.stage1.1, socialcapital.loud, by="HEROP_ID")
head(loud.stage1.2)

loud.stage1.2$HEROP_County <- str_sub(loud.stage1.2$HEROP_ID, 6,10)
head(loud.stage1.2)

loud.stage1.3 <- left_join(loud.stage1.2, censusRate.loud, by="HEROP_County")

head(loud.stage1.3)

## Merge with Geographic Boundaries, Continent only

library(sf)
tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
head(tract.sf)

## Limit to US-continent only
loud.stage1.3.us <- merge(tract.sf,loud.stage1.3, by="HEROP_ID")
head(loud.stage1.3.us) #82628

summary(loud.stage1.3.us)

loud.stage1 <- loud.stage1.3.us

### Stage 1 Prep
loud.stage1$IntIndPPL <- percent_rank(loud.stage1$IntInd)
loud.stage1$CenRspRtPPL <- percent_rank(loud.stage1$RspRt)
loud.stage1$SocCapIndPPL <- percent_rank(loud.stage1$SocCapInd)
loud.stage1$LimEngProfPPL <- percent_rank(loud.stage1$LimEngProfSc)
head(loud.stage1)

# Equally Weighted
loud.stage1$Stage1 <- (loud.stage1$IntIndPPL + loud.stage1$CenRspRtPPL+
                            loud.stage1$SocCapIndPPL + loud.stage1$LimEngProfPPL )/4
hist(loud.stage1$Stage1)
head(loud.stage1)

# Weighted by Advisory

loud.stage1$Stage1W <- ((.613*loud.stage1$IntIndPPL) + 
                            (.380*loud.stage1$CenRspRtPPL) +
                            (.761*loud.stage1$SocCapIndPPL) + 
                            (.716*loud.stage1$LimEngProfPPL) )/4
hist(loud.stage1$Stage1W)
head(loud.stage1)

### Write Data

st_write(loud.stage1, "../data_final/loud.stage1.geojson")

#save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")
loud.stage1.df <- st_drop_geometry(loud.stage1)

write.csv(loud.stage1.df, "../data_final/loud_stage1.csv", row.names = FALSE)
