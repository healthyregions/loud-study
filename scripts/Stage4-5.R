library(tidyverse)
setwd("~/Code/oeps2/data_to_merge/loud")

####################################################################
# STAGE 4
####################################################################
# Medicaid Expansion
# Individual-level poverty at pop level %
# Insurance Access

# Medicaid
medicaid <- read.csv("../indicators_raw/Medicaid_Policy_Proportion_state_level_2023.csv")
head(medicaid)
medicaid.df <- select(medicaid,HEROP_ID,MedPolProp)
head(medicaid.df)

medicaid.df$HEROP_State <- str_sub(medicaid.df$HEROP_ID, 6,7)
head(medicaid.df)

medicaid.df1 <- select(medicaid.df,HEROP_State,MedPolProp)

# Insurance Access
ins <- read.csv("../indicators_raw/insurance_tract23.csv")
head(ins)
priv.ins <- select(ins,HEROP_ID,PrivateInsP)
head(priv.ins)

# Poverty
# OEPS 2023 data package -- too large to store in Git, reading locally
oeps <- read.csv("~/Code/tract.csv")
head(oeps)

pov <- select(oeps,HEROP_ID,PovP)
head(pov)
summary(pov)

# Change directionality
pov$PovPSc <- pov$PovP * (-1)

## Merging
loud.stage4.df <- merge(pov, priv.ins, by="HEROP_ID")
head(loud.stage4.df)

## 
loud.stage4.df$HEROP_State <- str_sub(loud.stage4.df$HEROP_ID, 6,7)
head(loud.stage4.df)

loud.stage4.df2 <- merge(loud.stage4.df, medicaid.df1, by="HEROP_State")
head(loud.stage4.df2)

loud.stage4.df3 <- select(loud.stage4.df2,HEROP_ID,PovP,PovPSc,PrivateInsP,MedPolProp)

### Stage 4 Prep
loud.stage4.df3$PovPPL <- percent_rank(loud.stage4.df3$PovPSc)
loud.stage4.df3$PrivateInsPPL <- percent_rank(loud.stage4.df3$PrivateInsP)
loud.stage4.df3$MedPolPropPL <- percent_rank(loud.stage4.df3$MedPolProp)
head(loud.stage4.df3)

# Equally Weighted
loud.stage4.df3$Stage4 <- (loud.stage4.df3$PovPPL + loud.stage4.df3$PrivateInsPPL+
                            loud.stage4.df3$MedPolPropPL)/3
hist(loud.stage4.df3$Stage4)
head(loud.stage4.df3)

save(loud.stage4.df3,  file = "../data_final/loud_stage4.RData")

write.csv(loud.stage4.df3, "../data_final/loud_stage4.csv", row.names = FALSE)

library(sf)
tract.sf <- st_read("https://herop-geodata.s3.us-east-2.amazonaws.com/census/tract-2010-500k.geojson")
loud.stage4.sf <- left_join(tract.sf,loud.stage4.df3, by="HEROP_ID")
st_write(loud.stage4.sf, "../data_final/loud.stage4.geojson")


####################################################################
# STAGE 5
####################################################################
# Typology of laws restricting access to methadone
# Availability of supportive services 

# Typology of laws restricting access to methadone

histRstMMT <- read.csv("../indicators_raw/histRstMMT_state23.csv")
head(histRstMMT)
histRstMMT.df <- select(histRstMMT,HEROP_ID,HistRstMMT)
head(histRstMMT.df)

histRstMMT.df$HEROP_State <- str_sub(histRstMMT.df$HEROP_ID, 6,7)
head(histRstMMT.df)

histRstMMT.df1 <- select(histRstMMT.df,HEROP_State,HistRstMMT)
dim(histRstMMT.df1)

# Availability of supportive services 
supportive <- read.csv("../indicators_raw/supportives-tract-2020.csv")
head(supportive)
supportive.df <- select(supportive,HEROP_ID,minutes)
head(supportive.df)

supportive.df1 <- supportive.df %>%
  rename(supportive = minutes)

head(supportive.df1)
dim(supportive.df1) # 85187


## Merging
loud.stage5.df <- merge(loud.stage4.df3, supportive.df1, by="HEROP_ID")
head(loud.stage5.df)
dim(loud.stage5.df) #83228

## 
loud.stage5.df$HEROP_State <- str_sub(loud.stage5.df$HEROP_ID, 6,7)
head(loud.stage5.df)

loud.stage5.df2 <- merge(loud.stage5.df, histRstMMT.df1, by="HEROP_State")
head(loud.stage5.df2)

## rescale
loud.stage5.df2$supportiveSc <- loud.stage5.df2$supportive * (-1)

### Stage 5 Prep
loud.stage5.df2$supportivePL <- percent_rank(loud.stage5.df2$supportive)
loud.stage5.df2$HistRstMMTPPL <- percent_rank(loud.stage5.df2$HistRstMMT)
head(loud.stage5.df2)

# Equally Weighted
loud.stage5.df2$Stage5 <- (loud.stage5.df2$supportivePL + loud.stage5.df2$HistRstMMTPPL)/3
hist(loud.stage5.df2$Stage5)
head(loud.stage5.df2)

save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")

write.csv(loud.stage5.df2, "../data_final/loud_stage4-5.csv", row.names = FALSE)

library(sf)
tract.sf <- st_read("https://herop-geodata.s3.us-east-2.amazonaws.com/census/tract-2022-500k.geojson")
head(tract.sf)
tract.sf1 <- select(tract.sf,HEROP_ID,GEOID)
loud.stage5.df2 <- left_join(tract.sf1,loud.stage5.df2, by="HEROP_ID")
st_write(loud.stage5.df2, "../data_final/loud.stage4-5.geojson")
