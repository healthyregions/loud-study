library(tidyverse)
setwd("~/Code/loud-study/scripts")

############
# Read in Data 
############

### Historic presence of methadone  ###
pastMethdn <- read.csv("../indicators_raw/historic-methadone-timeseries.csv")
head(pastMethdn)
str(pastMethdn)

## Generate HEROP ID from GEOID integer that lost a digit
pastMethdn$GEOIDchar <- as.character(pastMethdn$GEOID)
view(pastMethdn)

pastMethdn$GEOIDchar2 <- ifelse(nchar(pastMethdn$GEOIDchar) == 10, paste0("0", pastMethdn$GEOID), pastMethdn$GEOID)
view(pastMethdn)

pastMethdn$HEROP_ID <- paste0('140US',pastMethdn$GEOIDchar2)
head(pastMethdn)

## Rename variable
pastMethdn <- pastMethdn %>%
  rename(pastMethd10 = minutes2010)
head(pastMethdn)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
pastMethdn$pastMethd10Sc <- pastMethdn$pastMethd10*(-1)

pastMethdn.2 <- pastMethdn %>%
  mutate(across(c(pastMethd10Sc), ~ replace_na(., -999)))
head(pastMethdn.2)

pastMetdn.loud <- select(pastMethdn.2,HEROP_ID,pastMethd10,pastMethd10Sc)
head(pastMetdn.loud)

summary(pastMetdn.loud)

############
### MOUD types nearby ###
MOUDType_wOTP <- read.csv("../indicators_raw/MOUDType_wOTP.csv")
head(MOUDType_wOTP)

hist(MOUDType_wOTP$MetTmDr2)

MOUDType_wOTP$MetType <- ifelse(MOUDType_wOTP$MetTmDr2 < 20, 1, 0)
MOUDType_wOTP$BupType <- ifelse(MOUDType_wOTP$BupCntDr2 < 20, 1, 0)
MOUDType_wOTP$NalType <- ifelse(MOUDType_wOTP$NaltTmDr2 < 20, 1, 0)

MOUDType_wOTP$MOUDType <- MOUDType_wOTP$MetType + MOUDType_wOTP$BupType + MOUDType_wOTP$NalType
summary(MOUDType_wOTP)

MOUDType_wOTP <- MOUDType_wOTP %>%
  mutate(across(c(MOUDType), ~ replace_na(., 0)))
head(MOUDType_wOTP)

summary(MOUDType_wOTP)
hist(MOUDType_wOTP$MOUDType)
dim(MOUDType_wOTP) #85187

MOUDType.loud <- select(MOUDType_wOTP, HEROP_ID,MOUDType)
head(MOUDType.loud)

############
### Availability of HRSOs/SSPs ###
ssp <- read.csv("../indicators_raw/ssp_2025.csv")
head(ssp)
ssp.df <- select(ssp,HEROP_ID,Minutes2)
head(ssp.df)

ssp.df1 <- ssp.df %>%
  rename(ssp2 = Minutes2)

head(ssp.df1)
dim(ssp.df1) # 85187

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
ssp.df1$ssp2Sc <- ssp.df1$ssp2 * (-1)
head(ssp.df1)

ssp.df2 <- ssp.df1 %>%
  mutate(across(c(ssp2Sc), ~ replace_na(., -999)))
head(ssp.df2)

ssp.loud <- ssp.df2

############
#### Spatial availability of Abstinence-based approach
abstinence <- read.csv("../indicators_raw/abstinence-tract-2020.csv")
head(abstinence)
abstinence.df <- select(abstinence,HEROP_ID,Minutes2)
head(abstinence.df)

abstinence.df1 <- abstinence.df %>%
  rename(abst2 = Minutes2)

head(abstinence.df1)
dim(abstinence.df1) # 85187

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
#abstinence.df1$abst2Sc <- abstinence.df1$abst2 * (-1)
#head(abstinence.df1)

#abstinence.df2 <- abstinence.df1 %>%
#  mutate(across(c(abst2Sc), ~ replace_na(., -999)))
#head(abstinence.df2)

abstinence.df2 <- abstinence.df1 %>%
  mutate(across(c(abst2), ~ replace_na(., -999)))
head(abstinence.df2)

summary(abstinence.df2)

abs.loud <- abstinence.df2

############
# Merge stage 2 measures 
############

loud.stage2.1 <- left_join(MOUDType.loud, abs.loud, by="HEROP_ID")
loud.stage2.2 <- left_join(loud.stage2.1, ssp.loud, by="HEROP_ID")
loud.stage2.3 <- left_join(loud.stage2.2, pastMetdn.loud, by="HEROP_ID")
head(loud.stage2.3)


loud.stage2.3 <- loud.stage2.3 %>%
  mutate(across(c(pastMethd10Sc), ~ replace_na(., -999)))
head(loud.stage2.3)

## Merge with Geographic Boundaries, Continent only

library(sf)
tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
head(tract.sf)

## Limit to US-continent only
loud.stage2.3.us <- merge(tract.sf,loud.stage2.3, by="HEROP_ID")
head(loud.stage2.3.us) #82628

summary(loud.stage2.3.us)

loud.stage2 <- loud.stage2.3.us

### Stage 1 Prep
loud.stage2$pastMethd10ScPPL <- percent_rank(loud.stage2$pastMethd10Sc)
loud.stage2$ssp2ScPPL <- percent_rank(loud.stage2$ssp2Sc)
loud.stage2$abstPPL <- percent_rank(loud.stage2$abst2)
loud.stage2$MOUDTypePPL <- percent_rank(loud.stage2$MOUDType)
head(loud.stage2)

# Equally Weighted

loud.stage2$Stage2 <- (loud.stage2$pastMethd10ScPPL+ loud.stage2$ssp2ScPPL+
                         loud.stage2$abstPPL + loud.stage2$MOUDTypePPL)/4
hist(loud.stage2$Stage2)
head(loud.stage2)
summary(loud.stage2$Stage2)

head(loud.stage2)

### Write Data

st_write(loud.stage2, "../data_final/loud.stage2.geojson")

#save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")
loud.stage2.df <- st_drop_geometry(loud.stage2)

write.csv(loud.stage2.df, "../data_final/loud_stage2.csv", row.names = FALSE)
