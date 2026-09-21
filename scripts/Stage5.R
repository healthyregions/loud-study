# M. Kolak - original script
# Last updated: 9/21/26 by Hilary
## Weighting is updated
## need to add in gravity model metrics

library(tidyverse)
setwd("~/Code/oeps2/data_to_merge/loud")

####################################################################
# STAGE 5
####################################################################
# Typology of laws restricting access to methadone
# Availability of supportive services 

# Typology of laws restricting access to methadone
histRstMMT <- read.csv("../indicators_raw/histRstMMT_state23.csv")
head(histRstMMT)
histRstMMT.df <- select(histRstMMT,HEROP_ID,HistRstMMTOrd)
head(histRstMMT.df)

histRstMMT.df$HEROP_State <- str_sub(histRstMMT.df$HEROP_ID, 6,7)
head(histRstMMT.df)

histRstMMT.df1 <- select(histRstMMT.df,HEROP_State,HistRstMMTOrd)
dim(histRstMMT.df1)
head(histRstMMT.df1)

# Availability of supportive services 
supportive <- read.csv("../indicators_raw/supportives-tract-2020.csv")
head(supportive)
supportive.df <- select(supportive,HEROP_ID,minutes)
head(supportive.df)

supportive.df1 <- supportive.df %>%
  rename(supportive = minutes)

head(supportive.df1)
dim(supportive.df1) # 85187

## Flip directionality as higher value == higher vulnerability (lower English proficiency)
supportive.df1$supportiveSc <- supportive.df1$supportive * (-1)
head(supportive.df1)

supportive.df2 <- supportive.df1 %>%
  mutate(across(c(supportiveSc), ~ replace_na(., -999)))
head(supportive.df2)

summary(supportive.df2)


library(sf)
tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
head(tract.sf)

## Limit to US-continent only
loud.stage5.us1 <- merge(tract.sf,supportive.df2, by="HEROP_ID")
head(loud.stage5.us1) #82628

## 
loud.stage5.us1$HEROP_State <- str_sub(loud.stage5.us1$HEROP_ID, 6,7)
head(loud.stage5.us1)

loud.stage5.us2 <- merge(loud.stage5.us1, histRstMMT.df1, by="HEROP_State")
head(loud.stage5.us2) #82628


### Stage 5 Prep
loud.stage5.us2$supportiveScPPL <- percent_rank(loud.stage5.us2$supportiveSc)
loud.stage5.us2$HistRstMMTPPL <- percent_rank(loud.stage5.us2$HistRstMMTOrd)
head(loud.stage5.us2)

# Equally Weighted
loud.stage5.us2$Stage5 <- (loud.stage5.us2$supportiveScPPL + loud.stage5.us2$HistRstMMTPPL)/2
hist(loud.stage5.us2$Stage5)
head(loud.stage5.us2)


# Weighted by Advisory
loud.stage4$Stage5W <- ((.699*loud.stage5.us2$supportiveScPPL) + 
                          (.740*loud.stage5.us2$HistRstMMTPPL) / (.699 + .74)

hist(loud.stage5$Stage5W)
head(loud.stage5)

### Write Data

st_write(loud.stage5.us2, "../data_final_09-16-26/loud.stage5.geojson")

loud.stage5.us2.df <- st_drop_geometry(loud.stage5.us2)

write.csv(loud.stage5.us2.df, "../data_final_09-16-26/loud_stage5.csv", row.names = FALSE)

