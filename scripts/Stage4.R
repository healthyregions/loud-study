# M. Kolak - original script
# Last updated: 10/8/26 by Hilary


library(tidyverse)
setwd("~/Code/oeps2/data_to_merge/loud")

####################################################################
# STAGE 4
####################################################################

############
## Medicaid expansion
############

## Read in csv file
medicaid <- read.csv("../indicators_raw/Medicaid_Policy_Proportion_state_level_2023.csv")

## Select necessary columns
medicaid.df <- select(medicaid,HEROP_ID,MedPolProp)

## Get state
medicaid.df$HEROP_State <- str_sub(medicaid.df$HEROP_ID, 6,7)

## Select necessary columns
medicaid.loud <- select(medicaid.df,HEROP_State,MedPolProp)

## Dataframe to use later
view(medicaid.loud)


############
# Insurance Access
############

## Read in csv file
ins <- read.csv("../indicators_raw/insurance_tract23.csv")

## Select necessary columns
ins.loud <- select(ins,HEROP_ID,PrivateInsP)

## Dataframe to use later
view(ins.loud)


############
# Poverty
############

## Read in csv file
#oeps <- read.csv("~/Code/tract.csv")
oeps <- read.csv("https://github.com/healthyregions/oeps/raw/refs/heads/main/backend/oeps/data/tables/tract-2023.csv") ## I'm assuming this is the same thing as tract.csv

## Select necessary columns
pov.loud <- select(oeps,HEROP_ID,PovP)

## Change directionality
pov.loud$PovPSc <- pov.loud$PovP * (-1)

## Dataframe to use later
view(pov.loud)


#############
## Merging
#############

library(sf)
#tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
tract.sf <- st_read("../indicators_raw/loud-cleaned.geojson") %>% 
  select("HEROP_ID")


## Limit to US-continent only
loud.stage4.1 <- left_join(tract.sf,pov.loud, by="HEROP_ID")
loud.stage4.2 <- left_join(loud.stage4.1, ins.loud, by="HEROP_ID")
loud.stage4.2$HEROP_State <- str_sub(loud.stage4.2$HEROP_ID, 6,7)
loud.stage4 <- left_join(loud.stage4.2, medicaid.loud, by="HEROP_State")


### Stage 4 Prep
loud.stage4$PovPPL <- percent_rank(loud.stage4.3$PovPSc)
loud.stage4$PrivateInsPPL <- percent_rank(loud.stage4.3$PrivateInsP)
loud.stage4$MedPolPropPPL <- percent_rank(loud.stage4.3$MedPolProp)


# Equally Weighted
loud.stage4$Stage4 <- (loud.stage4$PovPPL + loud.stage4$PrivateInsPPL+
                             loud.stage4$MedPolPropPPL)/3


# Weighted by Advisory
loud.stage4$Stage4W <- ((.684*loud.stage4$PovPPL) + 
                          (.829*loud.stage4$PrivateInsPPL) +
                          (.744*loud.stage4$MedPolPropPPL))/ (.684 + .829 + .744)



### Write Data

st_write(loud.stage4, "../data_final_09-16-26/loud.stage4.geojson")

loud.stage4 <- st_drop_geometry(loud.stage4)

write.csv(loud.stage4, "../data_final_09-16-26/loud_stage4.csv", row.names = FALSE)
