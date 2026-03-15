setwd("~/Code/oeps2/scripts")

test <- read.csv("~/Downloads/TimeSeries (1).csv")
head(test) 
str(test)

dim(test)
summary(test$minutes2010)

test$GEOIDint <- as.numeric(test$GEOID)
head(test)

histmet <- select(test,GEOIDint,minutes2010 )
head(histmet)

head(loud.stage1.sf)
# 140US01051031000

loud.stage1.sf$GEOIDint <- as.numeric(str_sub(loud.stage1.sf$HEROP_ID, 6,20))
head(loud.stage1.sf)

loud.stagex <- left_join(loud.stage1.sf,histmet, by="GEOIDint" )
head(loud.stagex)
st_write(loud.stagex, "loud.stagex.geojson")
