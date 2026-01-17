library(sf)
tract.sf <- st_read("../indicators_raw/tract-continental.geojson")
head(tract.sf)


stage1 <- st_read("../data_final/loud.stage1.geojson")
stage2 <- st_read("../data_final/loud.stage2.geojson")
stage3 <- st_read("../data_final/loud.stage3.geojson")
stage4 <- st_read("../data_final/loud.stage4.geojson")
stage5 <- st_read("../data_final/loud.stage5.geojson")

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

glimpse(loud5) #83507

loud5$AccessScore <- loud5$Stage1 + loud5$Stage2 + loud5$Stage3 + loud5$Stage4 + loud5$Stage5

library(scales)
loud5$AccessScoreSc <- rescale(loud5$AccessScore, to = c(0, 100))



### Write Data

st_write(loud5, "../data_final/loud-access-score.geojson")

#save(loud.stage5.df2,  file = "../data_final/loud_stage4-5.RData")
loud <- st_drop_geometry(loud5)

write.csv(loud, "../data_final/loud_access_score.csv", row.names = FALSE)

