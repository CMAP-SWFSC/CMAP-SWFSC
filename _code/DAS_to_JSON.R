# revised 8-23-2024 to include "P." events in linestrings. This solves problem
# of having some linestrings comprised solely of "R." and "E" events, which
# returns an error when a linestring contains <=2 points.

# read DAS data, parse on-effort position data, write lat, lon to data frame
#
# create simple feature (sf) linestring object for use in R plots
#
# export sf object to JSON, indexed by date, for plotting in mapping software such as CalTopo, Windy
#
# load packages

library(dplyr)
library(geojsonio)
library(ggplot2)
library(lubridate)
library(sf)
library(stringr)

rm(list=ls())

setwd("c:/carretta/Cruise_1652/data_DAS_edited")

input.files <- list.files(pattern="2024.das|2024.DAS")

combined_DAS <- character()

text <- lapply(input.files, function(i) {readLines(i)})

for (i in 1:length(text)) {
  
  content <- readLines(input.files[i])
  combined_DAS <- c(combined_DAS, content)
  
}

text <- combined_DAS

begin.effort <- which(substr(text, 4, 4)=="R")
end.effort <- which(substr(text, 4, 4)=="E")

all.effort <- text[substr(text, 4, 5)%in%c("*.","R.", "E ", "P.", "W.", "N.", "C.")]

# parse and identify date fields. Identify most-recent date in file for output name.

mm <- as.numeric(substr(all.effort, 13, 14))
dd <- as.numeric(substr(all.effort, 15, 16))
yyyy <- as.numeric(substr(all.effort, 17, 18)) + 2000

dates <- paste(yyyy, mm, dd, sep="-")

date.xtr <- date(dates)
julian.day <- as.integer(strftime(date.xtr, format = "%j"))
max.day <- which(julian.day==max(julian.day))
date.last <- as.character(date.xtr[max.day][[1]])

# Output file string:
output.str <- paste("Completed_Effort_Through", date.last, sep="_")

segment <- matrix(data=NA, length(all.effort))

segment.start <- 0

for (i in 1:length(all.effort)) {
  
  if (substr(all.effort[i], 4, 4)=="R") { segment[i] = segment.start + 1 } else { segment[i] = segment.start }
  
  segment.start <- segment[i]
  
}

lat.deg <- as.numeric(substr(all.effort, 21, 22))
lat.min <- as.numeric(substr(all.effort, 24, 28))/60

lon.deg <- as.numeric(substr(all.effort, 31, 33))
lon.min <- as.numeric(substr(all.effort, 35, 39))/60

lat <- lat.deg + lat.min
long <- -(lon.deg + lon.min) 

df <- cbind.data.frame(lat, long, segment)

df <- na.omit(df)

linestring_sf <- df |>
  group_by(segment) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

setwd("c:/carretta/Cruise_1652")

geojson_write(linestring_sf, file = paste(output.str, ".json", sep=""))

st_write(linestring_sf, paste(output.str, ".kml", sep=""), driver="KML", delete_layer=TRUE)

plot(linestring_sf, pch=8, cex=0.1)
