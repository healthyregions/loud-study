# M. Kolak - original script
# Last updated: 10/8/26 by Hilary

library(tidyverse)
library(sf)
#tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
#head(tract.sf)
tract.sf <- st_read("../indicators_raw/loud-cleaned.geojson") %>% 
  select("HEROP_ID")



stage1 <- st_read("../data_final_09-16-26/loud.stage1.geojson")
stage2 <- st_read("../data_final_09-16-26/loud.stage2.geojson")
stage3 <- st_read("../data_final_09-16-26/loud.stage3.geojson")
stage4 <- st_read("../data_final_09-16-26/loud.stage4.geojson")
stage5 <- st_read("../data_final_09-16-26/loud.stage5.geojson")

stage1.df <- st_drop_geometry(stage1)
stage2.df <- st_drop_geometry(stage2)
stage3.df <- st_drop_geometry(stage3)
stage4.df <- st_drop_geometry(stage4)
stage5.df <- st_drop_geometry(stage5)

## Limit to US-continent only, master version
loud1 <- left_join(tract.sf,stage1.df, by="HEROP_ID")
loud2 <- left_join(loud1,stage2.df, by="HEROP_ID")
loud3 <- left_join(loud2,stage3.df, by="HEROP_ID")
loud4 <- left_join(loud3,stage4.df, by="HEROP_ID")
loud5 <- left_join(loud4,stage5.df, by="HEROP_ID")

glimpse(loud5) 

loud5$Access <- loud5$Stage1 + loud5$Stage2 + loud5$Stage3 + loud5$Stage4 + loud5$Stage5

library(scales)
#loud5$AccessSc <- rescale(loud5$Access, to = c(0, 100))
loud5$AccessSc <- rescale(loud5$Access, to = c(0, 1))


loud5$Access_G30 <- loud5$Stage1 + loud5$Stage2_G30 + loud5$Stage3_G30 + loud5$Stage4 + loud5$Stage5_G30
loud5$AccessSc_G30 <- rescale(loud5$Access_G30, to = c(0, 1))

loud5$Access_G60 <- loud5$Stage1 + loud5$Stage2_G60 + loud5$Stage3_G60 + loud5$Stage4 + loud5$Stage5_G60
loud5$AccessSc_G60 <- rescale(loud5$Access_G60, to = c(0, 1))

loud5$AccessW <- loud5$Stage1W + loud5$Stage2W + loud5$Stage3W + loud5$Stage4W + loud5$Stage5W
loud5$AccessScW <- rescale(loud5$AccessW, to = c(0, 1))

loud5$AccessW_G30 <- loud5$Stage1W + loud5$Stage2W_G30 + loud5$Stage3W_G30 + loud5$Stage4W + loud5$Stage5W_G30
loud5$AccessScW_G30 <- rescale(loud5$AccessW_G30, to = c(0, 1))

loud5$AccessW_G60 <- loud5$Stage1W + loud5$Stage2W_G60 + loud5$Stage3W_G60 + loud5$Stage4W + loud5$Stage5W_G60
loud5$AccessScW_G60 <- rescale(loud5$AccessW_G60, to = c(0, 1))

### Write Data

st_write(loud5, "../data_final_09-16-26/loud-access-score.geojson")

#save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")
loud <- st_drop_geometry(loud5)

write.csv(loud, "../data_final_09-16-26/loud_access_score.csv", row.names = FALSE)

