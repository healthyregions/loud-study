library(tidyverse)
setwd("~/Code/loud-study")

#commuting <- read.csv("../commuting_tract23.csv")
#insurance <- read.csv("../insurance_tract23.csv")

###############
## STAGE 1 
###############

### Internet Access ###
internet <- read.csv("../indicators_raw/internet_tract23.csv")

internet.loud <- internet %>%
  select(HEROP_ID,GEOID,CompHhldsP,BbndInternetP)

head(internet.loud)
dim(internet.loud) #84400

loud.stage1 <- internet.loud
head(loud.stage1)

loud.stage1$HEROP_County <- str_sub(loud.stage1$HEROP_ID, 6,10)
head(loud.stage1)

loud.stage1 <- select(loud.stage1,HEROP_ID,HEROP_County,CompHhldsP,BbndInternetP)
head(loud.stage1)
  
loud.stage1$IntInd <- (scale(loud.stage1$CompHhldsP)+ scale(loud.stage1$BbndInternetP))/2
head(loud.stage1)

### Census Response Rate ###
CensusRate.county <- read.csv("../indicators_raw/RspRt_county_2023.csv")
head(CensusRate.county)

CensusRate.county$HEROP_County <- str_sub(CensusRate.county$HEROP_ID, 6,10)
head(CensusRate.county)

CensusRate.county <- select(CensusRate.county,HEROP_County,RspRt)
head(CensusRate.county)

loud.stage1x <- left_join(loud.stage1,CensusRate.county, by="HEROP_County")
head(loud.stage1x)

### Social Capital ###

social.capital <- read.csv("../indicators_raw/Social_Capital_Measures_2023.csv")
head(social.capital)

social.capital.loud <- social.capital %>%
  select(HEROP_ID,LibPerCap,RlgPerCap, LngTermP,SocCapInd) 

head(social.capital.loud)

social.capital.loud$SocCapIndPL <- percent_rank(social.capital.loud$SocCapInd)
head(social.capital.loud)

loud.stage1y <- merge(loud.stage1x, social.capital.loud, by="HEROP_ID")
head(loud.stage1y)

hist(loud.stage1y$SocCapIndPL)
dim(loud.stage1y) # 83364


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
english$EngProf <- english$EngProf * (-1)
head(english)

loud.stage1.df <- merge(loud.stage1y, english, by="HEROP_ID")
head(loud.stage1.df)

### Stage 1 Prep
loud.stage1.df$IntIndPPL <- percent_rank(loud.stage1.df$IntInd)
loud.stage1.df$CenRspRtPL <- percent_rank(loud.stage1.df$RspRt)
loud.stage1.df$SocCapIndPL <- percent_rank(loud.stage1.df$SocCapInd)
loud.stage1.df$EngProfPL <- percent_rank(loud.stage1.df$EngProf)
head(loud.stage1.df)

# Equally Weighted
loud.stage1.df$Stage1 <- (loud.stage1.df$IntIndPPL + loud.stage1.df$CenRspRtPL+
                     loud.stage1.df$SocCapIndPL + loud.stage1.df$EngProfPL )/4
hist(loud.stage1.df$Stage1)
head(loud.stage1.df)

# Weighted by Advisory

loud.stage1.df$Stage1W <- ((.613*loud.stage1.df$IntIndPPL) + 
                            (.380*loud.stage1.df$CenRspRtPL) +
                            (.761*loud.stage1.df$SocCapIndPL) + 
                            (.716*loud.stage1.df$EngProfPL) )/4
hist(loud.stage1.df$Stage1W)
head(loud.stage1.df)

save(loud.stage1.df,  file = "../data_final/loud_stage1.RData")

write.csv(loud.stage1.df, "../data_final/loud_stage1.csv", row.names = FALSE)

library(sf)
tract.sf <- st_read("https://herop-geodata.s3.us-east-2.amazonaws.com/census/tract-2010-500k.geojson")
loud.stage1.sf <- left_join(tract.sf,loud.stage1.df, by="HEROP_ID")
st_write(loud.stage1.sf, "../data_final/loud.stage1.geojson")

###################
