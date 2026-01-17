setwd("~/Code/loud-study/scripts")


library(sf)
loud <- st_read("../data_final/loud-access-score.geojson")

xwalk <- read.csv("../indicators_raw/TRACT_ZIP_122023.csv")
head(xwalk)

str(xwalk)


#### Fix GeoID

## Generate HEROP ID from GEOID integer that lost a digit
xwalk$GEOIDchar <- as.character(xwalk$TRACT)
view(xwalk)

xwalk$GEOIDchar2 <- ifelse(nchar(xwalk$GEOIDchar) == 10, paste0("0", xwalk$TRACT), xwalk$TRACT)
view(xwalk)

xwalk$HEROP_ID <- paste0('140US',xwalk$GEOIDchar2)
head(xwalk)


### Merge Crosswalk to Tract Data

loud.zip1 <- merge(loud,xwalk, by="HEROP_ID")
head(loud.zip1)

loud.zip2 <- select(loud.zip1,ZIP,TRACT,TOT_RATIO,HEROP_ID,AccessScoreSc,
                    Stage1,Stage1Cat,Stage2,Stage2Cat,Stage3,Stage3Cat,
                    Stage4,Stage4Cat,Stage5,Stage5Cat)
head(loud.zip2)

loud.zip2$AccessScoreScZip <- loud.zip2$AccessScoreSc * loud.zip2$TOT_RATIO
loud.zip2$Stage1Zip <- loud.zip2$Stage1 * loud.zip2$TOT_RATIO
loud.zip2$Stage1CatZip <- loud.zip2$Stage1Cat * loud.zip2$TOT_RATIO
loud.zip2$Stage2Zip <- loud.zip2$Stage2 * loud.zip2$TOT_RATIO
loud.zip2$Stage2CatZip <- loud.zip2$Stage2Cat * loud.zip2$TOT_RATIO
loud.zip2$Stage3Zip <- loud.zip2$Stage3 * loud.zip2$TOT_RATIO
loud.zip2$Stage3CatZip <- loud.zip2$Stage3Cat * loud.zip2$TOT_RATIO
loud.zip2$Stage4Zip <- loud.zip2$Stage4 * loud.zip2$TOT_RATIO
loud.zip2$Stage4CatZip <- loud.zip2$Stage4Cat * loud.zip2$TOT_RATIO
loud.zip2$Stage5Zip <- loud.zip2$Stage5 * loud.zip2$TOT_RATIO
loud.zip2$Stage5CatZip <- loud.zip2$Stage5Cat * loud.zip2$TOT_RATIO

loud.tractwzip.df <- st_drop_geometry(loud.zip2)
write.csv(loud.tractwzip.df, "../data_final/loud.tractwzip.df.csv", row.names = FALSE)

loud.zip3 <- loud.zip2 %>%
  group_by(ZIP) %>% 
  summarize(
    AccessScoreScZip = mean(AccessScoreScZip, na.rm = TRUE),
    Stage1Zip = mean(Stage1Zip, na.rm = TRUE),
    Stage1CatZip = mean(Stage1CatZip, na.rm = TRUE),
    Stage2Zip = mean(Stage2Zip, na.rm = TRUE),
    Stage2CatZip = mean(Stage2CatZip, na.rm = TRUE),
    Stage3Zip = mean(Stage3Zip, na.rm = TRUE),
    Stage3CatZip = mean(Stage3CatZip, na.rm = TRUE),
    Stage4Zip = mean(Stage4Zip, na.rm = TRUE),
    Stage4CatZip = mean(Stage4CatZip, na.rm = TRUE),
    Stage5Zip = mean(Stage5Zip, na.rm = TRUE),
    Stage5CatZip = mean(Stage5CatZip, na.rm = TRUE))

head(loud.zip3)

## Generate HEROP ID from ZIP integer that lost digits
loud.zip3$ZIPchar <- as.character(loud.zip3$ZIP)
view(loud.zip3)

loud.zip3$GEOIDchar2 <-ifelse(nchar(loud.zip3$ZIPchar) == 3, paste0("00", loud.zip3$ZIP), 
                                ifelse(nchar(loud.zip3$ZIPchar) == 4, paste0("0", loud.zip3$ZIP), 
                                loud.zip3$ZIP))
view(loud.zip3)


st_write(loud.zip3, "../data_final/loud.zip.geojson")


loud.zip.df <- st_drop_geometry(loud.zip3)
write.csv(loud.zip.df, "../data_final/loud.zip.csv", row.names = FALSE)


