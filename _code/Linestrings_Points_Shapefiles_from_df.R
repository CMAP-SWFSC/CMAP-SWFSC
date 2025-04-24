# Convert dataframe position data to several formats:
# sf LINESTRING, sf POINTS, shapefiles, JSON

# load packages
library(dplyr)
library(geojsonio)
library(ggplot2)

# Assuming your dataframe is named 'df' and has 'long', 'lat', 'id' and 'group' columns.
# 'group' column is to indicate pairs. Each pair should have a unique group identifier.

# Sample dataframe
# df <- data.frame(
#    lat = c(34.05, 36.17, 40.71, 42.36),
#    long = c(-118.24, -115.14, -74.01, -71.06)
#)

# dynamically generate the number of pairs to plot, based on the lengths of latitude and longitude
# num_prs <- length(df$lat)/2
# group <- rep(seq(1, num_prs), each=2)
# df$id <- group

df <- read.csv("c:/carretta/survey/2024 MMTD Survey Planner/output/waypoints_final.csv")

# Plot lines, points, grouping ids
plot <- ggplot(df, aes(long, lat, group = transect)) +
    geom_line(color="black") +
    geom_point(aes(long, lat), size=0.1) + # Adds points on top of the lines for clarity
    geom_text(aes(label = id), size=3, color="blue") # label waypoint ids

plot

# Generate linestrings and points from data frame and export to shapefiles

linestring_sf <- df %>%
  group_by(id) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

 point_sf <- df %>%
  group_by(id) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POINT")
 
# write simple features to shapefiles. function delete_layer = TRUE allows overwriting

 st_write(linestring_sf, "c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_linestrings.shp", delete_layer = TRUE)

 st_write(point_sf, "c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_endpoints.shp", delete_layer = TRUE)
 
# read shapefiles and convert to JSON
 
offshore_transects <- st_read("c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_linestrings.shp")
offshore_endpoints <- st_read("c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_endpoints.shp")

#offshore_transects <- st_transform(offshore_transects, crs = 4326)

geojson_write(offshore_transects, file = "c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_linestrings.json")
geojson_write(offshore_endpoints, file = "c:/carretta/survey/2024 MMTD Survey Planner/output/offshore_offshore_endpoints.json")
 
