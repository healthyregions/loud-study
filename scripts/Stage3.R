setwd("~/Code/loud-study/scripts")

library(tidyverse)

############
# Read in Data 
############

## Transportation behaviors
commuting <- read.csv("../indicators_raw/commuting_tract23.csv")
head(commuting)
summary(commuting) 

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
commuting$NoVehHHldSc <- commuting$NoVehHHld*(-1)
commuting$CommTransitSc <- commuting$CommTransit*(-1)
commuting$CommWalkingSc <- commuting$CommWalking*(-1)
head(commuting)

commuting.loud <- select(commuting, HEROP_ID, NoVehHHld,CommTransit,CommWalking,NoVehHHldSc,CommTransitSc,CommWalkingSc)
head(commuting.loud)

## Disability
oeps <- read.csv("~/Code/tract.csv")
head(oeps)

disability<- select(oeps,HEROP_ID,DisbP)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
disability$DisbPSc <- disability$DisbP * (-1)
head(disability)

disability.loud <- disability
head(disability.loud)

## MOUD Spatial Availability
methdne <- read.csv("../indicators_raw/Methadone-tract-2020.csv")
bup <- read.csv("../indicators_raw/Buprenorphine-tract-2020.csv")
nalt <- read.csv("../indicators_raw/Naltrexone-tract-2020.csv")
head(methdne)
moud.1 <- merge(methdne,bup, by= "HEROP_ID")
moud.2 <- merge(moud.1,nalt, by= "HEROP_ID")
head(moud.2)
moud <- select(moud.2, HEROP_ID,MetTmDr2, BupTmDr2,NaltTmDr2)
head(moud)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
moud$MetTmDr2Sc <- moud$MetTmDr2 * (-1)
moud$BupTmDr2Sc <- moud$BupTmDr2 * (-1)
moud$NaltTmDr2Sc <- moud$NaltTmDr2 * (-1)
head(moud)

moud.loud <- moud
head(moud.loud)

## Pharmacy Availability
pharmacy <- read.csv("~/Downloads/pharmacy-tract-2025.csv")
# this is incorrect -- only 3234 variables

pharmacy <- read.csv("../indicators_raw/Pharmacy-2025.csv")
head(pharmacy)
summary(pharmacy) 

pharmacy.loud <- select(pharmacy, HEROP_ID,PharmTmDr2)
pharmacy.loud$PharmTmDr2Sc <- pharmacy.loud$PharmTmDr2 * (-1)
head(pharmacy.loud)

## FQHC
fqhc <- read.csv("../indicators_raw/fqhc-tract-2025.csv")
head(fqhc)

fqhc.loud <- select(fqhc, HEROP_ID, FqhcTmDr2 )

fqhc.loud$FqhcTmDr2Sc <- fqhc.loud$FqhcTmDr2 * (-1)
head(fqhc.loud)

## OUD Overdose Rates
od.mort <- read.csv("../indicators_raw/ODMortRtAv.csv")
head(od.mort) 
# County ID = HEROP_ID: 050US01001 

od.mort$HEROP_County <- str_sub(od.mort$HEROP_ID, 6,10)
head(od.mort)

od.mort.loud <- select(od.mort,HEROP_County,OdMortRtAv)
head(od.mort.loud)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
od.mort.loud$OdMortRtAvSc <- od.mort.loud$OdMortRtAv * (-1)
head(od.mort.loud)

## State bup policies
bupPol.1 <- read.csv("../indicators_raw/histRstMMT_state23_updatedBupPol.csv")
head(bupPol.1)
bupPol <- select(bupPol.1,HEROP_ID,BupPolRst)
head(bupPol)

bupPol$HEROP_State <- str_sub(bupPol$HEROP_ID, 6,7)
head(bupPol)

bupPol$BupPolRstSc <- bupPol$BupPolRst * (-1)
head(bupPol)

bupPol.loud <- select(bupPol,HEROP_State,BupPolRst,BupPolRstSc)
head(bupPol.loud)

############
# Merge stage 3 measures 
############

loud.stage3.1 <- merge(disability.loud, fqhc.loud, by="HEROP_ID")
loud.stage3.2 <- merge(loud.stage3.1, commuting.loud, by="HEROP_ID")
loud.stage3.3 <- merge(loud.stage3.2, moud.loud, by="HEROP_ID")
loud.stage3.4 <- merge(loud.stage3.3, pharmacy.loud, by="HEROP_ID")

head(loud.stage3.4)

loud.stage3.4$HEROP_State <- str_sub(loud.stage3.4$HEROP_ID, 6,7)
loud.stage3.4$HEROP_County <- str_sub(loud.stage3.4$HEROP_ID, 6,10)
head(loud.stage3.4)

loud.stage3.5 <- merge(loud.stage3.4, bupPol.loud, by="HEROP_State")
loud.stage3.final <- merge(loud.stage3.5, od.mort.loud, by="HEROP_County")

head(loud.stage3.final)

## Merge with Geographic Boundaries, Continent only

library(sf)
tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
head(tract.sf)

## Limit to US-continent only
loud.stage3.us <- merge(tract.sf,loud.stage3.final, by="HEROP_ID")
head(loud.stage3.us) #82613

summary(loud.stage3.us)

## Replace null driving times with -999 (=worse access vulnerable)
## Only updating the "scaled" measure to preserve original data
loud.stage3 <- loud.stage3.us %>%
  mutate(across(c(FqhcTmDr2Sc, MetTmDr2Sc,
                  BupTmDr2Sc,NaltTmDr2Sc,PharmTmDr2Sc), 
                ~ replace_na(., -999)))

summary(loud.stage3)

############
# Calcs #
############

loud.stage3$DisbPScPPL <- percent_rank(loud.stage3$DisbPSc)
loud.stage3$FqhcTmDr2ScPPL <- percent_rank(loud.stage3$FqhcTmDr2Sc)
loud.stage3$NoVehHHldScPPL <- percent_rank(loud.stage3$NoVehHHldSc)
loud.stage3$MetTmDr2ScPPL <- percent_rank(loud.stage3$MetTmDr2Sc)
loud.stage3$BupTmDr2ScPPL <- percent_rank(loud.stage3$BupTmDr2Sc)
loud.stage3$NaltTmDr2ScPPL <- percent_rank(loud.stage3$NaltTmDr2Sc)
loud.stage3$PharmTmDr2ScPPL <- percent_rank(loud.stage3$PharmTmDr2Sc)
loud.stage3$OdMortRtAvScPPL <- percent_rank(loud.stage3$OdMortRtAvSc)
loud.stage3$BupPolRstScPPL <- percent_rank(loud.stage3$BupPolRstSc)

head(loud.stage3)


# Equally Weighted
loud.stage3$Stage3 <- (loud.stage3$DisbPScPPL + loud.stage3$FqhcTmDr2ScPPL +
                             loud.stage3$NoVehHHldScPPL + loud.stage3$MetTmDr2ScPPL + 
                             loud.stage3$BupTmDr2ScPPL + loud.stage3$NaltTmDr2ScPPL +
                             loud.stage3$PharmTmDr2ScPPL + loud.stage3$OdMortRtAvScPPL + 
                             loud.stage3$BupPolRstScPPL 
                           )/9
hist(loud.stage3$Stage3)
head(loud.stage3)

st_write(loud.stage3, "../data_final/loud.stage3.geojson")

#save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")
loud.stage3.df <- st_drop_geometry(loud.stage3)

write.csv(loud.stage3.df, "../data_final/loud_stage3.csv", row.names = FALSE)
