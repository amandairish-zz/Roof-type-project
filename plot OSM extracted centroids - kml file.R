# plot osm points on google earth
# since osm BWA validation points are not
# working well with training data
# 5/5/19

library(ggmap)
library(maptools)
library(rgdal)
library(tidyverse)

setwd("/Users/amandairish/Desktop/Malaria project/Botswana")
bw.ext <- read.csv("/Users/amandairish/Desktop/Malaria project/Botswana/botswana buildings/BW_Sentinel_032519_new.csv")

# get rid of observations that aren't metal/tile/thatch
bw.ext <- bw.ext %>%
  filter(Type == "metal" | Type == "thatch" | Type == "tile") %>%
  rename(LULC = Type)

bw.ext$LULC <- factor(bw.ext$LULC) # get rid of unused factor levels
levels(bw.ext$LULC)

# rename levels to match SL predictions
levels(bw.ext$LULC)[levels(bw.ext$LULC)=="metal"] <- "Metal"
levels(bw.ext$LULC)[levels(bw.ext$LULC)=="tile"] <- "Tile"
levels(bw.ext$LULC)[levels(bw.ext$LULC)=="thatch"] <- "Thatch"


coordinates(bw.ext)<-c("Longitude", "Latitude")          #Build a SpatialPointsData Frame
proj4string(bw.ext)<-CRS("+proj=longlat +datum=WGS84")


##The one line of code below writes our SpatialPointsDataFrame to a KML File
writeOGR(bw.ext, dsn="bwa.kml", layer= "LULC", driver="KML")

