# M. Kolak - original script
# Last updated: 10/5/26 by Hilary

library(tidyverse)
setwd("~/Code/loud-study/scripts")

############
# Read in Data 
############

############
### Historic presence of methadone ###
############

###### Travel time ###################

## Read in csv file
pastMethdn <- read.csv("../indicators_raw/historic-methadone-timeseries.csv")

## Generate HEROP ID from GEOID integer that lost a digit
pastMethdn$GEOIDchar <- as.character(pastMethdn$GEOID)
pastMethdn$GEOIDchar2 <- ifelse(nchar(pastMethdn$GEOIDchar) == 10, paste0("0", pastMethdn$GEOID), pastMethdn$GEOID)
pastMethdn$HEROP_ID <- paste0('140US',pastMethdn$GEOIDchar2)

## Rename variable
pastMethdn <- pastMethdn %>%
  rename(pastMethd10 = minutes2010)

## Flip directionality as higher value == higher vulnerability
pastMethdn$pastMethd10Sc <- pastMethdn$pastMethd10*(-1)

## Replace NA values with -999
pastMethdn.2 <- pastMethdn %>%
  mutate(across(c(pastMethd10Sc), ~ replace_na(., -999)))

## Travel time version
view(pastMethdn.2)


###### Gravity model ###################

## Read in csv file
pastMethdn_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_Historic_met_RAAM_2SFCA_with_geom.csv")

## Rename variables
pastMethdn_g <- pastMethdn_g %>%
  rename(pastMethd10_g30 = HmetRm30,
         pastMethd10_g60 = HmetRm60)

## Flip directionality as higher value == higher vulnerability
pastMethdn_g$pastMethd10Sc_g30 <- pastMethdn_g$pastMethd10_g30*(-1)
pastMethdn_g$pastMethd10Sc_g60 <- pastMethdn_g$pastMethd10_g60*(-1)

## Replace NA values with -999
pastMethdn.2_g <- pastMethdn_g %>%
  mutate(across(c(pastMethd10Sc_g30), ~ replace_na(., -999)),
         across(c(pastMethd10Sc_g60), ~ replace_na(., -999)))

## Gravity model version
view(pastMethdn.2_g)


###### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
pastMetdn.loud <- select(pastMethdn.2,HEROP_ID,pastMethd10,pastMethd10Sc)
pastMetdn2.loud <- select(pastMethdn.2_g,HEROP_ID,pastMethd10_g30,pastMethd10Sc_g30, pastMethd10_g60,pastMethd10Sc_g60)

## Join
pastMetdn.loud <- pastMetdn.loud %>%
  left_join(pastMetdn2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
pastMetdn.loud <- pastMetdn.loud %>%
  mutate(across(c(pastMethd10Sc_g30), ~ replace_na(., -999)),
         across(c(pastMethd10Sc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(pastMetdn.loud)



############
### MOUD types nearby ###
##########

## Read in csv file
MOUDType_wOTP <- read.csv("../indicators_raw/MOUDType_wOTP.csv")

MOUDType_wOTP$MetType <- ifelse(MOUDType_wOTP$MetTmDr2 < 20, 1, 0)
MOUDType_wOTP$BupType <- ifelse(MOUDType_wOTP$BupCntDr2 < 20, 1, 0)
MOUDType_wOTP$NalType <- ifelse(MOUDType_wOTP$NaltTmDr2 < 20, 1, 0)

## Calculate variable
MOUDType_wOTP$MOUDType <- MOUDType_wOTP$MetType + MOUDType_wOTP$BupType + MOUDType_wOTP$NalType

## Replace NAs with zero
MOUDType_wOTP <- MOUDType_wOTP %>%
  mutate(across(c(MOUDType), ~ replace_na(., 0)))

## Select necessary columns
MOUDType.loud <- select(MOUDType_wOTP, HEROP_ID,MOUDType)

## Dataframe to use later
view(MOUDType.loud)


############
### Availability of HRSOs/SSPs - travel time###
##############

##### Travel time ###################

## Read in csv file
ssp <- read.csv("../indicators_raw/ssp_2025.csv")

## Rename variable
ssp <- ssp %>%
  rename(ssp2 = Minutes2)

## Flip directionality as higher value == higher vulnerability
ssp$sspSc <- ssp$ssp2 * (-1)

## Replace NAs with -999
ssp.2 <- ssp %>%
  mutate(across(c(sspSc), ~ replace_na(., -999)))

## Travel time version
view(ssp.2)



######### Gravity model ###################

## Read in csv file
ssp_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_SSP_RAAM_2SFCA_with_geom.csv")

## Rename variables
ssp_g <- ssp_g %>%
  rename(ssp_g30 = SspRm30,
         ssp_g60 = SspRm60)

## Flip directionality as higher value == higher vulnerability
ssp_g$sspSc_g30 <- ssp_g$ssp_g30*(-1)
ssp_g$sspSc_g60 <- ssp_g$ssp_g60*(-1)

## Replace NAs with -999
ssp_g <- ssp_g %>%
  mutate(across(c(sspSc_g30), ~ replace_na(., -999)),
         across(c(sspSc_g60), ~ replace_na(., -999)))


## Gravity model version
view(ssp_g)



####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
ssp.loud <- select(ssp.2, HEROP_ID, ssp2, sspSc)
ssp2.loud <- select(ssp_g, HEROP_ID, ssp_g30, sspSc_g30, ssp_g60, sspSc_g60)

## Join
ssp.loud <- ssp.loud %>%
  left_join(ssp2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
ssp.loud <- ssp.loud %>%
  mutate(across(c(sspSc_g30), ~ replace_na(., -999)),
         across(c(sspSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(ssp.loud)




############
#### Spatial availability of Abstinence-based approach
#########

###### Travel time ###################

## Read in csv file
abst <- read.csv("../indicators_raw/abstinence-tract-2020.csv")

## Rename variable
abst <- abst %>%
  rename(abst2 = Minutes2)

## Flip directionality as higher value == higher vulnerability 
abst$abst2Sc <- abst$abst2 * (-1)

## Replace NAs with -999
abst.2 <- abst %>%
  mutate(across(c(abst2Sc), ~ replace_na(., -999)))

## Travel time version
view(abst.2)



##### Gravity model ###################

## Read in csv file
abst_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_ABSTINENCE_RAAM_2SFCA_with_geom.csv")

## Rename variables
abst_g <- abst_g %>%
  rename(abst_g30 = AbsRm30,
         abst_g60 = AbsRm60)

## Flip directionality as higher value == higher vulnerability
abst_g$abstSc_g30 <- abst_g$abst_g30*(-1)
abst_g$abstSc_g60 <- abst_g$abst_g60*(-1)


## Replace NAs with -999
abst_g <- abst_g %>%
  mutate(across(c(abstSc_g30), ~ replace_na(., -999)),
         across(c(abstSc_g60), ~ replace_na(., -999)))

## Gravity model version
view(abst_g)



####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
abst.loud <- select(abst.2, HEROP_ID, abst2, abst2Sc)
abst2.loud <- select(abst_g, HEROP_ID, abst_g30, abstSc_g30, abst_g60, abstSc_g60)

## Join
abst.loud <- abst.loud %>%
  left_join(abst2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
abst.loud <- abst.loud %>%
  mutate(across(c(abstSc_g30), ~ replace_na(., -999)),
         across(c(abstSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(abst.loud)



############
# Merge stage 2 measures 
############

loud.stage2.1 <- left_join(MOUDType.loud, abst.loud, by="HEROP_ID")
loud.stage2.2 <- left_join(loud.stage2.1, ssp.loud, by="HEROP_ID")
loud.stage2.3 <- left_join(loud.stage2.2, pastMetdn.loud, by="HEROP_ID")
head(loud.stage2.3)

## Ensure -999 for travel variables
loud.stage2.3 <- loud.stage2.3 %>%
  mutate(across(c(pastMethd10Sc), ~ replace_na(., -999)),
         across(c(pastMethd10Sc_g30), ~ replace_na(., -999)),
         across(c(pastMethd10Sc_g60), ~ replace_na(., -999)),
         across(c(sspSc), ~ replace_na(., -999)),
         across(c(sspSc_g30), ~ replace_na(., -999)),
         across(c(sspSc_g60), ~ replace_na(., -999)),
         across(c(abst2Sc), ~ replace_na(., -999)),
         across(c(abstSc_g30), ~ replace_na(., -999)),
         across(c(abstSc_g60), ~ replace_na(., -999)))


## Merge with Geographic Boundaries, Continent only
library(sf)
tract.sf <- st_read("../indicators_raw/loud-cleaned.geojson") %>% 
  select("HEROP_ID")


## Limit to US-continent only
loud.stage2.3.us <- merge(tract.sf,loud.stage2.3, by="HEROP_ID")
#head(loud.stage2.3.us) #82628

#summary(loud.stage2.3.us)

loud.stage2 <- loud.stage2.3.us

### Stage 2 Prep
loud.stage2$pastMethd10ScPPL <- percent_rank(loud.stage2$pastMethd10Sc)
loud.stage2$ssp2ScPPL <- percent_rank(loud.stage2$sspSc)
loud.stage2$abstScPPL <- percent_rank(loud.stage2$abst2Sc)
loud.stage2$MOUDTypePPL <- percent_rank(loud.stage2$MOUDType)

loud.stage2$pastMethd10Sc_g30PPL <- percent_rank(loud.stage2$pastMethd10Sc_g30)
loud.stage2$ssp2Sc_g30PPL <- percent_rank(loud.stage2$sspSc_g30)
loud.stage2$abstSc_g30PPL <- percent_rank(loud.stage2$abstSc_g30)

loud.stage2$pastMethd10Sc_g60PPL <- percent_rank(loud.stage2$pastMethd10Sc_g60)
loud.stage2$ssp2Sc_g60PPL <- percent_rank(loud.stage2$sspSc_g60)
loud.stage2$abstSc_g60PPL <- percent_rank(loud.stage2$abstSc_g60)



# Equally Weighted, travel time
loud.stage2$Stage2 <- (loud.stage2$pastMethd10ScPPL+ loud.stage2$ssp2ScPPL+
                         loud.stage2$abstScPPL + loud.stage2$MOUDTypePPL)/4

# Equally Weighted, gravity 30 m
loud.stage2$Stage2_G30 <- (loud.stage2$pastMethd10Sc_g30PPL+ loud.stage2$ssp2Sc_g30PPL+
                         loud.stage2$abstSc_g30PPL + loud.stage2$MOUDTypePPL)/4

# Equally Weighted, gravity 60 m
loud.stage2$Stage2_G60 <- (loud.stage2$pastMethd10Sc_g60PPL+ loud.stage2$ssp2Sc_g60PPL+
                             loud.stage2$abstSc_g60PPL + loud.stage2$MOUDTypePPL)/4

# Unequal weighting, travel time
loud.stage2$Stage2W <- ((.611*loud.stage2$pastMethd10ScPPL) + 
                          (.769*loud.stage2$ssp2ScPPL) +
                          (.679*loud.stage2$abstScPPL) + 
                          (.759*loud.stage2$MOUDTypePPL) )/ (.611 + .769 + .679 + .759)

# Unequal weighting, gravity 30 m
loud.stage2$Stage2W_G30 <- ((.611*loud.stage2$pastMethd10Sc_g30PPL) + 
                          (.769*loud.stage2$ssp2Sc_g30PPL) +
                          (.679*loud.stage2$abstSc_g30PPL) + 
                          (.759*loud.stage2$MOUDTypePPL) )/ (.611 + .769 + .679 + .759)

# Unequal weighting, gravity 60 m
loud.stage2$Stage2W_G30 <- ((.611*loud.stage2$pastMethd10Sc_g60PPL) + 
                              (.769*loud.stage2$ssp2Sc_g60PPL) +
                              (.679*loud.stage2$abstSc_g60PPL) + 
                              (.759*loud.stage2$MOUDTypePPL) )/ (.611 + .769 + .679 + .759)


### Write Data

st_write(loud.stage2, "../data_final_09-16-26/loud.stage2.geojson")

loud.stage2.df <- st_drop_geometry(loud.stage2)

write.csv(loud.stage2.df, "../data_final_09-16-26/loud_stage2.csv", row.names = FALSE)
