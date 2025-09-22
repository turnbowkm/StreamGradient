library(ggplot2)
library(plotly)
library(lidR)
library(raster)   
library(leaflet)  
library(RColorBrewer) 
library(sf)
library(terra)
library(dplyr)
#input raster
ras <- terra::rast("C:/Users/kevin.turnbow/GIS/lida.tif")
ras <- terra::project(ras, "EPSG:4326") 
ras <- focal(ras, w = 5, fun = "min")



#input stream
line_feature <- st_read("C:/Users/kevin.turnbow/GIS/INTBStreams25_1.shp")
line_feature <- st_transform(line_feature, crs = 4326)%>%
  st_cast("LINESTRING")
line_feature <- terra::vect(line_feature)
line_length <- perim(line_feature)*0.000621371


raster_min <- min(values(ras), na.rm = TRUE)
raster_max <- max(values(ras), na.rm = TRUE)

pal <- colorNumeric(palette = "viridis",
                    domain = c(raster_min, raster_max),
                    na.color = "transparent") # Handle NA values

b<- centroids(line_feature, inside=TRUE)
c <- buffer(b, width = 1000)

new <- terra::intersect(line_feature, c)

m <- leaflet() %>%
  addTiles() %>% 
  #addPolylines(data =line_feature)%>%
  lines(new)%>%
  addPolygons(data = c)%>%
  points(b)%>%
  addRasterImage(ras,
                 colors = pal,
                 opacity = 0.8,
                 project = TRUE) %>% # 'project = TRUE' allows Leaflet to handle Web Mercator conversion
  # Add a legend for the raster data
  addLegend(pal = pal,
            values = c(raster_min, raster_max), # Use the min/max values for the legend range
            title = "Raster Value")

m


extracted_values_terra <- extract(ras, new, xy = TRUE)%>%
  na.omit()%>%
  mutate(ft = focal_min * 3.28084)


# Calculate approximate distance along the line for plotting
  distances <- c(0, cumsum(
    sqrt(diff(extracted_values_terra$x)^2 + diff(extracted_values_terra$y)^2)
  ))
  extracted_values_terra$distance_mi <- (distances / 1609.34)
  
  p1 <- plot_ly(data = extracted_values_terra, x = ~x, y = ~focal_min, type = 'scatter', mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('Distance: ', round(extracted_values_terra$distance_m, 2), 'm<br>Elevation: ', round(focal_min, 2), 'm')) %>%
    layout(title = "Interactive Stream Longitudinal Profile",
           xaxis = list(title = "Distance Along Stream (m)"),
           yaxis = list(title = "Elevation (m)"),
           hovermode = "closest")
  
  p1

summarytblmax <- extracted_values_terra %>%
  slice_max(ft)
summarytblmin <- extracted_values_terra %>%
  slice_min(ft)
summarytbl <- rbind(summarytblmin, summarytblmax)
  
