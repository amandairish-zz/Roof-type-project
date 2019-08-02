# Last updated 6/10/19 to include exporting polygons & centroids to KML files
# for mapping in Google Earth to see how well structures match up and if centroids
# were calculated correctly

library(osmdata)
library(leaflet)
library(geojsonio)
library(rgdal)
library(rgeos)
library(tidyverse)
library(sf)

# Define bounding box and data layer
q0 <- opq(bbox = c(23.489, -18.601, 25.679, -17.781)) 
q1 <- add_osm_feature(q0, key = 'building')
x <- osmdata_sp(q1)


leaflet() %>% addTiles() %>% addPolygons(data=x$osm_polygons, color="red")

to_save <- x$osm_polygons

for(i in 1:length(to_save)){
  slot(to_save@polygons[[i]], "ID") <- as.character(slot(to_save@polygons[[i]], "ID") )
}
geojson_write(to_save, file="/Users/amandairish/Desktop/Malaria project/Botswana/botswana buildings/B_buildings_test.geojson")


b <- rgdal::readOGR("/Users/amandairish/Desktop/Malaria project/Botswana/botswana buildings/B_buildings_test.geojson")
summary(b@data$building.roof)
head(b)


# Create separate spdf for each roof type
b_metal_poly <- b[which(b@data$building.roof=="metal"), ]
b_thatch_poly <- b[which(b@data$building.roof=="thatch"), ]
b_tile_poly <- b[which(b@data$building.roof=="tile"), ]

# create KML file for mapping polygons
writeOGR(b, dsn="bwa_polygons.kml", layer= "b@polygons", driver="KML")
writeOGR(b_metal_poly, dsn="bwa_polygons_metal.kml", layer= "b_metal_poly@polygons", driver="KML")
writeOGR(b_thatch_poly, dsn="bwa_polygons_thatch.kml", layer= "b_thatch_poly@polygons", driver="KML")
writeOGR(b_tile_poly, dsn="bwa_polygons_tile.kml", layer= "b_tile_poly@polygons", driver="KML")

# Convert data from SPDF to dataframe

# First extract center point of polygon for lat/long
poly_centers <- SpatialPointsDataFrame(gCentroid(b, byid=TRUE), 
                                       b@data, match.ID=FALSE)
poly_centers_dat <- as_tibble(poly_centers)
head(poly_centers_dat)
names(poly_centers_dat)[60:61] <- c("Longitude", "Latitude") # specify which is lat & long

# rename to prep for left join with bw_ext from Botswana h2o ML file
#poly_centers_dat <- poly_centers_dat %>%
#  rename(
#    OSM_ID = osm_id
#  )

#bw_ext$OSM_ID <- as.numeric(levels(bw_ext$OSM_ID))[bw_ext$OSM_ID]
#poly_centers_dat$OSM_ID <- as.numeric(levels(poly_centers_dat$OSM_ID))[poly_centers_dat$OSM_ID]

# Note: merging on OSM_ID doesn't work because OSM IDs got messed up in extraction of sentinel data

# Merge with bw_ext from Botswana h2o ML file in order to get building type
# so can subset based on that
bw_ext_2 <- left_join(bw_ext, poly_centers_dat, by="Latitude")

# save merged file for use with Botswana h2o ML file
write.csv(bw_ext_2, "bwa_hot_osm_sentinel_merged_6-10.csv") # wd already set to /Malaria project/Botswana


###############

# create roof type-specific dataframes for use in creating KML files
b_metal <- poly_centers[which(poly_centers@data$building.roof=="metal"), ]

b_thatch <- poly_centers[which(poly_centers@data$building.roof=="thatch"), ]

b_tile <- poly_centers[which(poly_centers@data$building.roof=="tile"), ]

# create KML file for mapping centroids
writeOGR(poly_centers, dsn="bwa_centroids.kml", layer= "poly_centers@coords", driver="KML")
writeOGR(b_metal, dsn="bwa_centroids_metal.kml", layer="b_metal@coords", driver = "KML")
writeOGR(b_thatch, dsn="bwa_centroids_thatch.kml", layer="b_thatch@coords", driver = "KML")
writeOGR(b_tile, dsn="bwa_centroids_tile.kml", layer="b_tile@coords", driver = "KML")

# create df for getting sentinel data
roof_polygons_df <- dplyr::select(poly_centers_dat, c("osm_id", "Longitude", "Latitude", "osm_id", "building.roof", "building"))

# Export as csv
write.csv(roof_polygons_df, "/Users/amandairish/Desktop/BWA_OSM_roof.csv")

# Data exploration
# See what type of buildings are in each roof category
summary(b_metal_poly$building)
summary(b_thatch_poly$building)
summary(b_tile_poly$building)


