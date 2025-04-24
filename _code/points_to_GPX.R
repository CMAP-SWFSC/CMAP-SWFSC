# Points to GPX

 library(sf)

# Assuming you have a dataframe with latitude and longitude columns...

# Field name for waypoints must be 'name', anything else returns an error
# see https://gis.stackexchange.com/questions/40731/converting_shp-into-gpx-using-qgis

 df <- cbind.data.frame(name = c(1,2),
                        lat = c(34.05, 36.12),
                        long = c(-118.2, -115.2)
                        )
 
# Convert data frame to an 'sf' object
 
 gpx_data <- st_as_sf(df, coords = c("long", "lat"), crs =4326)
 
# write the sf object to a GPX file, checking to see if file of the same name already exists
 
 if (file.exists("example.gpx")==TRUE) { file.remove("example.gpx")}
 
 st_write(gpx_data, "output.gpx", driver="GPX")