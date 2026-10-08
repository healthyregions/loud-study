# M. Kolak - original script
# Last updated: 10/8/26 by Hilary

setwd("~/Code/loud-study/scripts")

library(tidyverse)

############
# Read in Data 
############

############
## Transportation behaviors
############

## Read in csv file
commuting <- read.csv("../indicators_raw/commuting_tract23.csv")

## Flip directionality as higher value == higher vulnerability 
commuting$NoVehHHldSc <- commuting$NoVehHHld*(-1)
commuting$CommTransitSc <- commuting$CommTransit*(-1)

## Select necessary columns
commuting.loud <- select(commuting, HEROP_ID, NoVehHHld,CommTransit,NoVehHHldSc,CommTransitSc)

## Dataframe to use later
view(commuting.loud)


############
## Disability
############

## Read in csv file
##disability <- read.csv("~/Code/tract.csv")
disability <- read.csv("https://github.com/healthyregions/oeps/raw/refs/heads/main/backend/oeps/data/tables/tract-2023.csv")

## Flip directionality as higher value == higher vulnerability
disability$DisbPSc <- disability$DisbP * (-1)

## Select necessary columns
disability.loud <- select(disability,HEROP_ID,DisbP,DisbPSc)

## Dataframe to use later
view(disability.loud)


############
## MOUD Spatial Availability
############

##### Travel time ###################

## Read in csv files
methdne <- read.csv("../indicators_raw/Methadone-tract-2020.csv")
bup <- read.csv("../indicators_raw/Buprenorphine-tract-2020.csv")
nalt <- read.csv("../indicators_raw/Naltrexone-tract-2020.csv")

## Merge them together
moud.1 <- merge(methdne,bup, by= "HEROP_ID")
moud.2 <- merge(moud.1,nalt, by= "HEROP_ID")
moud <- select(moud.2, HEROP_ID,MetTmDr2, BupTmDr2,NaltTmDr2)

## Flip directionality as higher value == higher vulnerability
moud$MetTmDr2Sc <- moud$MetTmDr2 * (-1)
moud$BupTmDr2Sc <- moud$BupTmDr2 * (-1)
moud$NaltTmDr2Sc <- moud$NaltTmDr2 * (-1)

## Replace NAs with -999
moud <- moud %>%
  mutate(across(c(MetTmDr2Sc), ~ replace_na(., -999)),
         across(c(BupTmDr2Sc), ~ replace_na(., -999)),
         across(c(NaltTmDr2Sc), ~ replace_na(., -999)))

## Travel time version
view(moud)


######### Gravity model ###################

## Read in csv file
moud_g <- read.csv("../indicators_raw/tract.csv")

## Rename variables
moud_g <- moud_g %>%
  rename(bup_g30 = BupRm30,
         bup_g60 = BupRm60,
         met_g30 = MetRm30,
         met_g60 = MetRm60,
         nal_g30 = NaltRm30,
         nal_g60 = NaltRm60)


## Flip directionality as higher value == higher vulnerability
moud_g$bupSc_g30 <- moud_g$bup_g30*(-1)
moud_g$bupSc_g60 <- moud_g$bup_g60*(-1)
moud_g$metSc_g30 <- moud_g$met_g30*(-1)
moud_g$metSc_g60 <- moud_g$met_g60*(-1)
moud_g$nalSc_g30 <- moud_g$nal_g30*(-1)
moud_g$nalSc_g60 <- moud_g$nal_g60*(-1)

## Replace NAs with -999
moud_g <- moud_g %>%
  mutate(across(c(bupSc_g30), ~ replace_na(., -999)),
         across(c(bupSc_g60), ~ replace_na(., -999)),
         across(c(metSc_g30), ~ replace_na(., -999)),
         across(c(metSc_g60), ~ replace_na(., -999)),
         across(c(nalSc_g30), ~ replace_na(., -999)),
         across(c(nalSc_g60), ~ replace_na(., -999)))


## Gravity model version
view(moud_g)


####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
moud.loud <- select(moud, HEROP_ID, MetTmDr2, MetTmDr2Sc,  BupTmDr2, BupTmDr2Sc,  NaltTmDr2, NaltTmDr2Sc)
moud2.loud <- select(moud_g, HEROP_ID, met_g30, metSc_g30, met_g60, metSc_g60, bup_g30, bupSc_g30, bup_g60, bupSc_g60, nal_g30, nalSc_g30, nal_g60, nalSc_g60)

## Join
moud.loud <- moud.loud %>%
  left_join(moud2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
moud.loud <- moud.loud %>%
  mutate(across(c(metSc_g30), ~ replace_na(., -999)),
         across(c(metSc_g60), ~ replace_na(., -999)),
         across(c(bupSc_g30), ~ replace_na(., -999)),
         across(c(bupSc_g60), ~ replace_na(., -999)),
         across(c(nalSc_g30), ~ replace_na(., -999)),
         across(c(nalSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(moud.loud)


############
## Pharmacy Availability
############

##### Travel time ###################

## Read in csv file
pharmacy <- read.csv("../indicators_raw/Pharmacy-2025.csv")

## Flip directionality as higher value == higher vulnerability
pharmacy$PharmTmDr2Sc <- pharmacy$PharmTmDr2 * (-1)

## Replace NAs with -999
pharmacy.loud <- pharmacy %>%
  mutate(across(c(PharmTmDr2Sc), ~ replace_na(., -999)))

## Travel time version
view(pharmacy.loud)


######### Gravity model ###################

## Read in csv file
pharmacy_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_PHAR_RAAM_2SFCA_with_geom.csv")

## Rename variables
pharmacy_g <- pharmacy_g %>%
  rename(pharm_g30 = RAAM_30,
         pharm_g60 = RAAM_60)

## Flip directionality as higher value == higher vulnerability
pharmacy_g$pharmSc_g30 <- pharmacy_g$pharm_g30*(-1)
pharmacy_g$pharmSc_g60 <- pharmacy_g$pharm_g60*(-1)

## Replace NAs with -999
pharmacy_g <- pharmacy_g %>%
  mutate(across(c(pharmSc_g30), ~ replace_na(., -999)),
         across(c(pharmSc_g60), ~ replace_na(., -999)))

## Gravity model version
view(pharmacy_g)


####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
pharmacy.loud <- select(pharmacy.loud, HEROP_ID, PharmTmDr2, PharmTmDr2Sc)
pharmacy2.loud <- select(pharmacy_g, HEROP_ID, pharm_g30, pharmSc_g30, pharm_g60, pharmSc_g60)

## Join
pharmacy.loud <- pharmacy.loud %>%
  left_join(pharmacy2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
pharmacy.loud <- pharmacy.loud %>%
  mutate(across(c(pharmSc_g30), ~ replace_na(., -999)),
         across(c(pharmSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(pharmacy.loud)


############
## FQHC
############

##### Travel time ###################

## Read in csv file
fqhc <- read.csv("../indicators_raw/fqhc-tract-2025.csv")

## Flip directionality as higher value == higher vulnerability
fqhc$FqhcTmDr2Sc <- fqhc$FqhcTmDr2 * (-1)

## Replace NAs with -999
fqhc.loud <- fqhc %>%
  mutate(across(c(FqhcTmDr2Sc), ~ replace_na(., -999)))

## Travel time version
view(fqhc.loud)


######### Gravity model ###################

## Read in csv file
fqhc_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_FQHC_RAAM_2SFCA_with_geom.csv")

## Rename variables
fqhc_g <- fqhc_g %>%
  rename(fqhc_g30 = RAAM_30,
         fqhc_g60 = RAAM_60)

## Flip directionality as higher value == higher vulnerability
fqhc_g$fqhcSc_g30 <- fqhc_g$fqhc_g30*(-1)
fqhc_g$fqhcSc_g60 <- fqhc_g$fqhc_g60*(-1)

## Replace NAs with -999
fqhc_g <- fqhc_g %>%
  mutate(across(c(fqhcSc_g30), ~ replace_na(., -999)),
         across(c(fqhcSc_g60), ~ replace_na(., -999)))

## Gravity model version
view(fqhc_g)


####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
fqhc.loud <- select(fqhc.loud, HEROP_ID, FqhcTmDr2, FqhcTmDr2Sc)
fqhc2.loud <- select(fqhc_g, HEROP_ID, fqhc_g30, fqhcSc_g30, fqhc_g60, fqhcSc_g60)

## Join
fqhc.loud <- fqhc.loud %>%
  left_join(fqhc2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
fqhc.loud <- fqhc.loud %>%
  mutate(across(c(fqhcSc_g30), ~ replace_na(., -999)),
         across(c(fqhcSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(fqhc.loud)



############
## OUD Overdose Rates
############

## Read in csv file
od.mort <- read.csv("../indicators_raw/ODMortRtAv.csv")

## Get county
od.mort$HEROP_County <- str_sub(od.mort$HEROP_ID, 6,10)

## Select necessary columns
od.mort.loud <- select(od.mort,HEROP_County,OdMortRtAv)

## Flip directionality as higher value == higher vulnerability 
od.mort.loud$OdMortRtAvSc <- od.mort.loud$OdMortRtAv * (-1)

## Dataframe to use later
view(od.mort.loud)



############
## State bup policies
############

## Read in csv file
bupPol.1 <- read.csv("../indicators_raw/histRstMMT_state23_updatedBupPol.csv")

## Select necessary columns
bupPol <- select(bupPol.1,HEROP_ID,BupPolRst)

## Get state
bupPol$HEROP_State <- str_sub(bupPol$HEROP_ID, 6,7)

## Flip directionality as higher value == higher vulnerability 
bupPol$BupPolRstSc <- bupPol$BupPolRst * (-1)

## Select necessary columns
bupPol.loud <- select(bupPol,HEROP_State,BupPolRst,BupPolRstSc)

## Dataframe to use later
view(bupPol.loud)



############
# Merge stage 3 measures 
############

loud.stage3.1 <- merge(disability.loud, fqhc.loud, by="HEROP_ID")
loud.stage3.2 <- merge(loud.stage3.1, commuting.loud, by="HEROP_ID")
loud.stage3.3 <- merge(loud.stage3.2, moud.loud, by="HEROP_ID")
loud.stage3.4 <- merge(loud.stage3.3, pharmacy.loud, by="HEROP_ID")


loud.stage3.4$HEROP_State <- str_sub(loud.stage3.4$HEROP_ID, 6,7)
loud.stage3.4$HEROP_County <- str_sub(loud.stage3.4$HEROP_ID, 6,10)

loud.stage3.5 <- merge(loud.stage3.4, bupPol.loud, by="HEROP_State")
loud.stage3.final <- merge(loud.stage3.5, od.mort.loud, by="HEROP_County")

view(loud.stage3.final)


## Merge with Geographic Boundaries, Continent only
library(sf)
##tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
tract.sf <- st_read("../indicators_raw/loud-cleaned.geojson") %>% 
  select("HEROP_ID")


## Limit to US-continent only
loud.stage3 <- merge(tract.sf,loud.stage3.final, by="HEROP_ID")


## Replace null driving times with -999 (=worse access vulnerable)
## Only updating the "scaled" measure to preserve original data
# loud.stage3 <- loud.stage3.us %>%
#   mutate(across(c(FqhcTmDr2Sc, MetTmDr2Sc,
#                   BupTmDr2Sc,NaltTmDr2Sc,PharmTmDr2Sc), 
#                 ~ replace_na(., -999)))
# 
# summary(loud.stage3)

############
# Calcs #
############

loud.stage3$DisbPScPPL <- percent_rank(loud.stage3$DisbPSc)
loud.stage3$FqhcTmDr2ScPPL <- percent_rank(loud.stage3$FqhcTmDr2Sc)
loud.stage3$NoVehHHldScPPL <- percent_rank(loud.stage3$NoVehHHldSc)
loud.stage3$CommTransitScPPL <- percent_rank(loud.stage3$CommTransitSc)
loud.stage3$MetTmDr2ScPPL <- percent_rank(loud.stage3$MetTmDr2Sc)
loud.stage3$BupTmDr2ScPPL <- percent_rank(loud.stage3$BupTmDr2Sc)
loud.stage3$NaltTmDr2ScPPL <- percent_rank(loud.stage3$NaltTmDr2Sc)
loud.stage3$PharmTmDr2ScPPL <- percent_rank(loud.stage3$PharmTmDr2Sc)
loud.stage3$OdMortRtAvScPPL <- percent_rank(loud.stage3$OdMortRtAvSc)
loud.stage3$BupPolRstScPPL <- percent_rank(loud.stage3$BupPolRstSc)


loud.stage3$FqhcSc_g30PPL <- percent_rank(loud.stage3$fqhcSc_g30)
loud.stage3$PharmSc_g30PPL <- percent_rank(loud.stage3$pharmSc_g30)
loud.stage3$MetSc_g30PPL <- percent_rank(loud.stage3$metSc_g30)
loud.stage3$BupSc_g30PPL <- percent_rank(loud.stage3$bupSc_g30)
loud.stage3$NalSc_g30PPL <- percent_rank(loud.stage3$nalSc_g30)

loud.stage3$FqhcSc_g60PPL <- percent_rank(loud.stage3$fqhcSc_g60)
loud.stage3$PharmSc_g60PPL <- percent_rank(loud.stage3$pharmSc_g60)
loud.stage3$MetSc_g60PPL <- percent_rank(loud.stage3$metSc_g60)
loud.stage3$BupSc_g60PPL <- percent_rank(loud.stage3$bupSc_g60)
loud.stage3$NalSc_g60PPL <- percent_rank(loud.stage3$nalSc_g60)




# Equally Weighted, travel time
loud.stage3$Stage3 <- (loud.stage3$DisbPScPPL + loud.stage3$FqhcTmDr2ScPPL +
                             loud.stage3$NoVehHHldScPPL + loud.stage3$CommTransitScPPL + loud.stage3$MetTmDr2ScPPL + 
                             loud.stage3$BupTmDr2ScPPL + loud.stage3$NaltTmDr2ScPPL +
                             loud.stage3$PharmTmDr2ScPPL + loud.stage3$OdMortRtAvScPPL + 
                             loud.stage3$BupPolRstScPPL 
                           )/10

# Equally Weighted, gravity 30 m
loud.stage3$Stage3_G30 <- (loud.stage3$DisbPScPPL + loud.stage3$FqhcSc_g30PPL +
                         loud.stage3$NoVehHHldScPPL + loud.stage3$CommTransitScPPL + loud.stage3$MetSc_g30PPL + 
                         loud.stage3$BupSc_g30PPL + loud.stage3$NalSc_g30PPL +
                         loud.stage3$PharmSc_g30PPL + loud.stage3$OdMortRtAvScPPL + 
                         loud.stage3$BupPolRstScPPL
                         )/10


# Equally Weighted, gravity 60 m
loud.stage3$Stage3_G60 <- (loud.stage3$DisbPScPPL + loud.stage3$FqhcSc_g60PPL +
                             loud.stage3$NoVehHHldScPPL + loud.stage3$CommTransitScPPL + loud.stage3$MetSc_g60PPL + 
                             loud.stage3$BupSc_g60PPL + loud.stage3$NalSc_g60PPL +
                             loud.stage3$PharmSc_g60PPL + loud.stage3$OdMortRtAvScPPL + 
                             loud.stage3$BupPolRstScPPL
                           )/10


# Unequal weighting, travel time
loud.stage3$Stage3W <- ((0.881 * loud.stage3$NoVehHHldScPPL) + 
                          (0.881 * loud.stage3$CommTransitScPPL) +
                          (0.551 * loud.stage3$DisbPScPPL) +
                          (0.796 * loud.stage3$MetTmDr2ScPPL) +
                          (0.796 * loud.stage3$BupTmDr2ScPPL) +
                          (0.796 * loud.stage3$NaltTmDr2ScPPL) +
                          (0.717 * loud.stage3$PharmTmDr2ScPPL) +
                          (0.570 * loud.stage3$FqhcTmDr2ScPPL) +
                          (0.823 * loud.stage3$OdMortRtAvScPPL) +
                          (0.704 * loud.stage3$BupPolRstScPPL))/ (.881 + .881 + .551 + .796 + .796 + .796 + .717 + .57 + .823 + .704)


# Unequal weighting, gravity 30 m
loud.stage3$Stage3W_G30 <- ((0.881 * loud.stage3$NoVehHHldScPPL) + 
                          (0.881 * loud.stage3$CommTransitScPPL) +
                          (0.551 * loud.stage3$DisbPScPPL) +
                          (0.796 * loud.stage3$MetSc_g30PPL) +
                          (0.796 * loud.stage3$BupSc_g30PPL) +
                          (0.796 * loud.stage3$NalSc_g30PPL) +
                          (0.717 * loud.stage3$PharmSc_g30PPL) +
                          (0.570 * loud.stage3$FqhcSc_g30PPL) +
                          (0.823 * loud.stage3$OdMortRtAvScPPL) +
                          (0.704 * loud.stage3$BupPolRstScPPL))/ (.881 + .881 + .551 + .796 + .796 + .796 + .717 + .57 + .823 + .704)


# Unequal weighting, gravity 60 m
loud.stage3$Stage3W_G60 <- ((0.881 * loud.stage3$NoVehHHldScPPL) + 
                              (0.881 * loud.stage3$CommTransitScPPL) +
                              (0.551 * loud.stage3$DisbPScPPL) +
                              (0.796 * loud.stage3$MetSc_g60PPL) +
                              (0.796 * loud.stage3$BupSc_g60PPL) +
                              (0.796 * loud.stage3$NalSc_g60PPL) +
                              (0.717 * loud.stage3$PharmSc_g60PPL) +
                              (0.570 * loud.stage3$FqhcSc_g60PPL) +
                              (0.823 * loud.stage3$OdMortRtAvScPPL) +
                              (0.704 * loud.stage3$BupPolRstScPPL))/ (.881 + .881 + .551 + .796 + .796 + .796 + .717 + .57 + .823 + .704)



### Write Data

st_write(loud.stage3, "../data_final_09-16-26/loud.stage3.geojson")

loud.stage3.df <- st_drop_geometry(loud.stage3)

write.csv(loud.stage3.df, "../data_final_09-16-26/loud_stage3.csv", row.names = FALSE)
