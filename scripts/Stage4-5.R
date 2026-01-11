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

