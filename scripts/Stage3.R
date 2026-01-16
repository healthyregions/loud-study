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

## Pharmacy Availabilty
pharmacy <- read.csv("~/Downloads/pharmacy-tract-2025.csv")
# this is incorrect -- only 3234 variables

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
# Calcs #
############

## Transportation behaviors
head(commuting)

commuting$NoVehHHldPPL <- percent_rank(commuting$NoVehHHldSc)
commuting$CommTransitPPL <- percent_rank(commuting$CommTransitSc)
commuting$CommWalkingPPL <- percent_rank(commuting$CommWalkingSc)

head(commuting.loud)

## Disability



## MOUD Spatial Availability


## Pharmacy Availabiltiy


## FQHC


## OUD Overdose Rates


## State bup policies

