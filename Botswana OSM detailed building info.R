library(rgdal)
if (!require(geojsonio)) {
  install.packages("geojsonio")
  library(geojsonio)
}
library(sp)
library(maps)
library(ggmap)
library(maptools)
library(tidyverse)
library(rgeos)
library(tmaptools)

setwd("/Users/amandairish/Desktop/Malaria project/Botswana/")
#clean_bot <- readOGR("hotosm_bwa_buildings_polygons_shp")

read_osm("/Users/amandairish/Downloads/map.osm")



#json.data <- geojsonio::geojson_read("20180502_nmp_buildings_merged_field_data.geojson",
#                          what = "sp") # doesn't work

# Explore data file since can't read in with package

ogrListLayers("20180502_nmp_buildings_merged_field_data.geojson")
#ogrInfo("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON") # doesn't work
ogrInfo("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON", require_geomType="wkbPoint")
ogrInfo("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON", require_geomType="wkbPolygon")


# Read in points and polygons separately
points <- readOGR("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON", require_geomType="wkbPoint")
  # NOTE: keeping only 7454 wkbPoint of 14004 features
  # Warning message:
  #   In readOGR("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON",  :
  #   Dropping null geometries: 120, 2127, 2453, 5082, 8937, 10709, 12170, 13069

polygons <- readOGR("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON", require_geomType="wkbPolygon")
  # NOTE: keeping only 6550 wkbPolygon of 14004 features
  # Warning message:
  #   In readOGR("20180502_nmp_buildings_merged_field_data.geojson", "OGRGeoJSON",  :
  #   Dropping null geometries: 120, 2127, 2453, 5082, 8937, 10709, 12170, 13069


# Plot data points
plot(points, col = "red")
plot(polygons, add = TRUE)

# Make dfs to look @ & work w/ data more easily
points.df <- as_data_frame(points)
names(points.df)[40:41] <- c("lon", "lat") # specify which is lat & long

poly.centers <- SpatialPointsDataFrame(gCentroid(polygons, byid=TRUE), 
                                      polygons@data, match.ID=FALSE)
polygons.df <- as_data_frame(poly.centers)
head(polygons.df)
names(polygons.df)[40:41] <- c("lon", "lat") # specify which is lat & long


# limit dfs to just roof type and lat/long to see if they match
roof.points.df <- dplyr::select(points.df, c("building.roof", "lon", "lat"))
roof.polygons.df <- dplyr::select(polygons.df, c("building.roof", "lon", "lat"))


# map points within Botswana
library(leaflet)

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = polygons,
              weight = 2,
              color = "red",
              fillOpacity=0.7) %>%
  addCircleMarkers(data = points,
                   color = "blue",
                   radius = 1,
                   fillOpacity = 0.4) %>%
  addCircleMarkers(data = poly.centers,
                   color = "green",
                   radius = 1,
                   fillOpacity = 0.4)
  
# No real overlap b/w points and polygons, but not always clear what exactly the
# points are tagging. Often seems to be small outbuildings and that type of thing,
# and sometimes points will be at the perimeter of a structure that is also tagged
# with a polygon. For now, consider just using polygons/polygon centroid data
# for validating ML predictions.

roof.poly.tally <- roof.polygons.df %>% count(building.roof)
  