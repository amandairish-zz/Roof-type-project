# Maps for predicted & actual roof types - Swaziland
# Last updated 12/14/17 by Amanda Irish

library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(mapview)
library(RColorBrewer)

# Set wd
setwd("/Users/amandairish/Desktop/Malaria project/Swaziland")

# read in data
df <- read.csv("Swazi sentinel CVSL data known and pred.csv")
df_v <- read.csv("Validation sentinel data with roof type pred.csv")
df_lg <- read.csv("Swazi Sentinel data with roof type pred.csv")

head(df_lg)

# Let's make a SpatialPointsDataFrame object (useful for other operations); assumes first object is x and second is y for lat/long
df_SPDF <- SpatialPointsDataFrame(coords = df[,c("Longitude", "Latitude")],
                                           data = df[,c("LULC", "pred_roof_type", "pred_T", "pred_M", "pred_Th")], 
                                           proj4string = CRS("+init=epsg:4326")) # WGS 1984 using lat/long. Optional but good to specify

# Summary of object
df_SPDF

# SPDFs partition data elements, e.g. the coordinates are stored separately from the data
df_SPDF@coords
df_SPDF@data

# You can use the plot or spplot function to get quick plots
plot(df_SPDF)
spplot(df_SPDF, zcol = "pred_roof_type")


#### Spatial data types (e.g. shapefiles, geojson, raster) - what to use if someone gives you a shapefile
S_Adm_1 <- raster::getData("GADM", country="SWZ", level=1)

# let's take a look at that object
S_Adm_1
plot(S_Adm_1)

# plot both country and data points
plot(S_Adm_1)
points(df$Longitude, df$Latitude, 
       ylab = "Latitude", xlab="Longitude", 
       col=color_class[df$pred_roof_type],
       pch=16)

# Create color palette for markers & legend

#color_pal <- colorFactor(brewer.pal(3, "Set1"), domain = df_SPDF$pred_roof_type)
colors <- c("#76FF03", "#FFFF00", "#D500F9") #bright green, bright yellow, bright purple (not official color names)
color_pal <- colorFactor(colors, domain = df_SPDF$pred_roof_type)

# to create the above, found colors @ https://material.io/guidelines/style/color.html#color-color-palette
# picked high-contrast, neon-y colors for this palette
# #76FF03 similar to #B2FF59 just more saturated


# Map of predicted roof types - CVSL dataset on satellite background
pred_map <- leaflet(df_SPDF) %>%
  #setView(22.17, -19.367, zoom = 15) %>%
  
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  
  addPolygons(data=S_Adm_1, weight = 1.5, fillOpacity=0) %>%
  
  addCircleMarkers(color = color_pal(df_SPDF$pred_roof_type),
                   opacity = 0.8, fill = 1, fillOpacity = 0.8,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Predicted roof type",
            opacity = 0.8)

pred_map


# Map of actual roof types - CVSL dataset

known_map <- leaflet(df_SPDF) %>%
  #setView(22.17, -19.367, zoom = 15) %>%  #Gumare
  
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  
  #addPolygons(data=S_Adm_1, weight = 1.5, fillOpacity=0) %>%
  
  addCircleMarkers(color = color_pal(df_SPDF$LULC),
                   opacity = 0.8, fill = 1, fillOpacity = 0.8,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~LULC,
            title = "Actual roof type",
            opacity = 0.8)

known_map
saveWidget(known_map, file = "cvsl_known_map.html")

# Map of predicted roof types - validation dataset
df_v_SPDF <- SpatialPointsDataFrame(coords = df_v[,c("Longitude", "Latitude")],
                                  data = df_v[,c("LULC", "pred_roof_type", "pred_T", "pred_M", "pred_Th")], 
                                  proj4string = CRS("+init=epsg:4326")) # WGS 1984 using lat/long. Optional but good to specify



v_pred_map <- leaflet(df_v_SPDF) %>%
  #setView(25.83, -24.3335919, zoom = 10) %>% #Gaborone
  #setView(22.17, -19.367, zoom = 15) %>%
  
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  
  addPolygons(data=S_Adm_1, weight = 1.5, fillOpacity=0) %>%
  
  addCircleMarkers(color = color_pal(df_v_SPDF$pred_roof_type),
                   opacity = 1, fill = 1, fillOpacity = 1,
                   weight = 5, radius = 4) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Predicted roof type in validation dataset",
            opacity = 1)

v_pred_map


# Map of actual roof types - validation dataset

v_known_map <- leaflet(df_v_SPDF) %>%
  #setView(25.83, -24.3335919, zoom = 10) %>%
  #setView(22.17, -19.367, zoom = 15) %>% 
  
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  
  addPolygons(data=S_Adm_1, weight = 1.5, fillOpacity=0) %>%
  
  addCircleMarkers(color = color_pal(df_v_SPDF$LULC),
                   opacity = 1, fill = 1, fillOpacity = 1,
                   weight = 5, radius = 4) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~LULC,
            title = "Actual roof type in validation dataset",
            opacity = 1)

v_known_map


# Map of large dataset

lg_SPDF <- SpatialPointsDataFrame(coords = df_lg[,c("Longitude", "Latitude")],
                                  data = df_lg[,c("pred_roof_type", "pred_T", "pred_M", "pred_Th")], 
                                  proj4string = CRS("+init=epsg:4326")) # WGS 1984 using lat/long. Optional but good to specify

lg_SPDF

lg_map <- leaflet(lg_SPDF) %>%
  #setView(22.17, -19.367, zoom = 15) %>%
  
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  
  #addPolygons(data=B_Adm_1, weight = 1.5, fillOpacity=0) %>%
  
  addCircleMarkers(color = color_pal(df_SPDF$pred_roof_type),
                   opacity = 1, fill = 1, fillOpacity = 1,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Roof type predictions in a large dataset",
            opacity = 1)

lg_map

# save maps
mapshot(pred_map, file = "pred_map_zoom.png")
mapshot(known_map, file = "known_map_zoom.png")
mapshot(lg_map, file = "full_data_map_zoom.png")
mapshot(v_pred_map, file = "v_pred_map_zoom.png")
mapshot(v_known_map, file = "v_known_map_zoom.png")


# plot large dataset over a satellite image - Mbabane

lg_map_sat_Mb <- leaflet(lg_SPDF) %>%
  setView(31.1394618, -26.324121, zoom = 17) %>%
  
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  addCircleMarkers(color = color_pal(lg_SPDF$pred_roof_type),
                   opacity = 0.3, fill = 1, fillOpacity = 0.3,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Roof type predictions in a large dataset",
            opacity = 0.6)
lg_map_sat_Mb

mapshot(lg_map_sat_Mb, file = "full_data_sat_map_zoom_Mb.png")


# plot large dataset over a satellite image - Matsapha

lg_map_sat_Mat <- leaflet(lg_SPDF) %>%
  setView(31.3232474, -26.4851937, zoom = 17) %>%
  
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  addCircleMarkers(color = color_pal(lg_SPDF$pred_roof_type),
                   opacity = 0.3, fill = 1, fillOpacity = 0.3,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Roof type predictions in a large dataset",
            opacity = 0.6)

lg_map_sat_Mat
mapshot(lg_map_sat_Mat, file = "full_data_sat_map_zoom_Mat.png")



# plot large dataset over a satellite image - rural

lg_map_sat_rural <- leaflet(lg_SPDF) %>%
  setView(31.8170886, -26.5723887, zoom = 17) %>%
  
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  addCircleMarkers(color = color_pal(lg_SPDF$pred_roof_type),
                   opacity = 0.3, fill = 1, fillOpacity = 0.3,
                   weight = 5, radius = 2) %>%
  
  addScaleBar("bottomright") %>%
  
  addLegend("bottomleft", pal = color_pal, values = ~pred_roof_type,
            title = "Roof type predictions in a large dataset",
            opacity = 0.6)

lg_map_sat_rural
mapshot(lg_map_sat_rural, file = "full_data_sat_map_zoom_rural.png")



library(htmlwidgets)
saveWidget(lg_map_sat_Mb, file = "lg_map_sat_Mb.html")
