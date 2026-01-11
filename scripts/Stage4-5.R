library(tidyverse)
setwd("~/Code/oeps2/data_to_merge/loud")

# STAGE 4
# Medicaid Expansion
# Individual-level poverty at pop level %
# Insurance Access

# Medicaid
medicaid <- read.csv("../indicators_raw/Medicaid_Policy_Proportion_state_level_2023.csv")
head(medicaid)
medicaid.df <- select(medicaid,HEROP_ID,MedPolProp)
head(medicaid.df)

# Insurance Access
ins <- read.csv("../indicators_raw/insurance_tract23.csv")
head(ins)
priv.ins <- select(ins,HEROP_ID,PrivateInsP)
head(priv.ins)

# Poverty
# OEPS 2023 data package -- too large to store in Git, reading locally
oeps <- read.csv("~/Code/tract.csv")
head(oeps)

pov <- select(oeps,HEROP_ID,PovP,)
head(pov)
summary(pov)





# STAGE 5
# Typology of laws restricting access to methadone
# Availability of supportive services 

