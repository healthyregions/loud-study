# M. Kolak - original script
# Last updated: 10/8/26 by Hilary


library(tidyverse)
setwd("~/Code/oeps2/data_to_merge/loud")

####################################################################
# STAGE 5
####################################################################

############
# Typology of laws restricting access to methadone
############

## Read in csv file
histRstMMT <- read.csv("../indicators_raw/histRstMMT_state23.csv")

## Select necessary columns
histRstMMT.df <- select(histRstMMT,HEROP_ID,HistRstMMTOrd)

## Get state
histRstMMT.df$HEROP_State <- str_sub(histRstMMT.df$HEROP_ID, 6,7)

## Select necessary columns
histRstMMT.loud <- select(histRstMMT.df,HEROP_State,HistRstMMTOrd)

## Dataframe to use later
view(histRstMMT.loud)



############
# Availability of supportive services
############

##### Travel time ###################

## Read in csv file
supportive <- read.csv("../indicators_raw/supportives-tract-2020.csv")

## Select necessary columns
supportive.df <- select(supportive,HEROP_ID,minutes)

## Rename variables
supportive.df1 <- supportive.df %>%
  rename(supportive = minutes)

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
supportive.df1$supportiveSc <- supportive.df1$supportive * (-1)

## Replace NAs with -999
supportive.loud <- supportive.df1 %>%
  mutate(across(c(supportiveSc), ~ replace_na(., -999)))

## Travel time version
view(supportive.loud)


######### Gravity model ###################

## Read in csv file
supportive_g <- read.csv("../indicators_raw/GEOGRAPHIES_US_SUPPORTIVE_RAAM_2SFCA_with_geom.csv")

## Rename variables
supportive_g <- supportive_g %>%
  rename(sup_g30 = SuppRm30,
         sup_g60 = SuppRm60)

## Flip directionality as higher value == higher vulnerability
supportive_g$supSc_g30 <- supportive_g$sup_g30*(-1)
supportive_g$supSc_g60 <- supportive_g$sup_g60*(-1)

## Replace NAs with -999
supportive_g <- supportive_g %>%
  mutate(across(c(supSc_g30), ~ replace_na(., -999)),
         across(c(supSc_g60), ~ replace_na(., -999)))

## Gravity model version
view(supportive_g)


####### Combine travel time and gravity into one dataframe ###############

## Select necessary columns
supportive.loud <- select(supportive.loud, HEROP_ID, supportive, supportiveSc)
supportive2.loud <- select(supportive_g, HEROP_ID, sup_g30, supSc_g30, sup_g60, supSc_g60)

## Join
supportive.loud <- supportive.loud %>%
  left_join(supportive2.loud, by = "HEROP_ID")

## Ensure all scaled variable NA values are replaced with -999
supportive.loud <- supportive.loud %>%
  mutate(across(c(supSc_g30), ~ replace_na(., -999)),
         across(c(supSc_g60), ~ replace_na(., -999)))

## Dataframe to use later
view(supportive.loud)


############
# Merge stage 5 measures 
############

library(sf)
#tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
tract.sf <- st_read("../indicators_raw/loud-cleaned.geojson") %>% 
  select("HEROP_ID")

## Limit to US-continent only
loud.stage5.us1 <- merge(tract.sf,supportive.loud, by="HEROP_ID")
loud.stage5.us1$HEROP_State <- str_sub(loud.stage5.us1$HEROP_ID, 6,7)

loud.stage5 <- merge(loud.stage5.us1, histRstMMT.loud, by="HEROP_State")


### Stage 5 Prep
loud.stage5$supportiveScPPL <- percent_rank(loud.stage5$supportiveSc)
loud.stage5$HistRstMMTPPL <- percent_rank(loud.stage5$HistRstMMTOrd)

loud.stage5$supportiveSc_g30PPL <- percent_rank(loud.stage5$supSc_g30)
loud.stage5$supportiveSc_g60PPL <- percent_rank(loud.stage5$supSc_g60)

# Equally Weighted, travel time
loud.stage5$Stage5 <- (loud.stage5$supportiveScPPL + loud.stage5$HistRstMMTPPL)/2

# Equally Weighted, gravity 30 m
loud.stage5$Stage5_G30 <- (loud.stage5$supportiveSc_g30PPL + loud.stage5$HistRstMMTPPL)/2

# Equally Weighted, gravity 60 m
loud.stage5$Stage5_G60 <- (loud.stage5$supportiveSc_g60PPL + loud.stage5$HistRstMMTPPL)/2

# Weighted by Advisory, travel time
loud.stage4$Stage5W <- ((.699*loud.stage5$supportiveScPPL) + 
                          (.740*loud.stage5$HistRstMMTPPL) / (.699 + .74))


# Weighted by Advisory, gravity 30 m
loud.stage4$Stage5W_G30 <- ((.699*loud.stage5$supSc_g30) + 
                          (.740*loud.stage5$HistRstMMTPPL) / (.699 + .74))

# Weighted by Advisory, gravity 60 m
loud.stage4$Stage5W_G60 <- ((.699*loud.stage5$supSc_g60) + 
                          (.740*loud.stage5$HistRstMMTPPL) / (.699 + .74))


### Write Data

st_write(loud.stage5, "../data_final_09-16-26/loud.stage5.geojson")

loud.stage5.df <- st_drop_geometry(loud.stage5)

write.csv(loud.stage5.df, "../data_final_09-16-26/loud_stage5.csv", row.names = FALSE)

