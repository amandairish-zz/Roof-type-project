# Botswana 2016-2017 raster of bands & NDVI

library(raster)
library(velox)
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)

setwd("/Users/amandairish/Desktop/Malaria project/Botswana/Botswana rasters 2016-2017")

# Import data
band2 <- raster("Band2.tif")
band3 <- raster("Band3.tif")
band4 <- raster("Band4.tif")
band8 <- raster("Band8.tif")
ndvi <- raster("NDVI.tif")
ndwi <- raster("NDWI.tif")
nl <- raster("NL.tif")


# Plot rasters
plot(band2)
plot(band3)
plot(band4)
plot(band8)
plot(ndvi)
plot(ndwi)
plot(nl)


# Combine raster4 & NDVI (raster3) into a new raster stack
raster_comb <- stack(band2, band3, band4, band8, ndvi, ndwi, nl)
raster_comb
plot(raster_comb)

# plot rasters in rgb
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(raster_comb, r=3, g=2, b=1,
        stretch = "lin",
        axes = TRUE,
        main = "Botswana 2016-17, Sentinel-2 RGB bands")


# Save raster stack of all relevant layers
writeRaster(raster_comb, "BWA_2016-17_raster.grd", bandorder='BIL')


# Create data matrix
bwa_data <- rasterToPoints(raster_comb)
head(bwa_data)
colSums(is.na(bwa_data))

# rename x & y to long/lat
bwa_data <- as.data.frame(bwa_data)
bwa_data <- bwa_data %>% 
  rename(
    Longitude = y,
    Latitude = x
  )

# Merge raster data with HOT OSM data for training and test sets
hot_osm <- read.csv("/Users/amandairish/Desktop/Malaria project/Botswana/botswana buildings/BWA_OSM_roof.csv")
head(hot_osm)
hot_osm <- hot_osm %>% 
  rename(
    Longitude = lon,
    Latitude = lat
  )


hot_osm_bands <- right_join(bwa_data, hot_osm, by=c("Latitude", "Longitude"))
head(hot_osm_bands)
colSums(is.na(hot_osm_bands))



# Save dataset for use with predictions
#write_csv(gambia.data, "/Users/amandairish/Desktop/Malaria project/Gambia/2003 data/gambia_2003_all.csv")
#write_csv(gambia.data.2015, "/Users/amandairish/Desktop/Malaria project/Gambia/2015 data/gambia_2015_all.csv")

