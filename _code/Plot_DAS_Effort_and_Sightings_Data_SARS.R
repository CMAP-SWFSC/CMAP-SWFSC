# Read DAS format data and plot effort and species sightings

rm(list=ls())
start.time=date()

# Plot Title
header = as.character("")
# Species code
#  species=c(" 016")  # ! species is a 4-character field in WinCruz, normally with a leading space
#  spcodes = c(paste(" 0", seq(1:99), sep=""))
effort = "ALL" # choose "ALL" or "ON" The latter plots only on-effort sightings

library(maps)
library(sp)
library(terra)
library(raster)

# parse DAS data to extract effort for plotting
# identify DAS input file(s)

setwd("C:/CARRETTA/MMTD RV DATA ARCHIVE/RV-Data")

# read input as a fixed-width format (fwf) file,  '#' symbols from input file are ignored via use of comment.char="" function

data = read.fwf("1991_2024_US_West_Coast.DAS", width=c(3,2,14,1,2,1,5,2,3,1,5,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1), comment.char="")
include.events = c("*.", "B.", "R.", "C.", "N.", "P.", "R.", "S.", "A.", "W.", "E ", "S ", "A ")
missing.pos = c(which(is.na(data$V5==T)), which(is.na(data$V7==T)), which(is.na(data$V9==T)), which(is.na(data$V11==T)))
data = data[-missing.pos,]
event.lines = which(data[,2]%in%include.events)
data = data[event.lines,]
latdeg = data[,5] 
latmin = data[,7] 
lat.effort = latdeg + latmin/60
londeg = data[,9]
lonmin = data[,11]
lon.effort = -(londeg + lonmin/60)

data$V2=as.character(data$V2)
data$V3=as.character(data$V3)
data$V4=as.character(data$V4)
data$V8=as.character(data$V8)
data$V21=as.character(data$V21)
data$V23=as.character(data$V23)
data$V25=as.character(data$V25)

# data mine all sightings species codes

sight.lines = which(data$V2%in%c("A.","A "))
sight.info = data[sight.lines,]
all.codes = names(table(c(sight.info$V21, sight.info$V23, sight.info$V25, sight.info$V27)))

#  read shapefiles (extension *.shp)

setwd("c:/Carretta/Mapping")
countries <- vect("world_countries.shp")
Canada <- subset(countries, countries$CNTRY_NAME=="Canada")
United.States <- subset(countries, countries$CNTRY_NAME=="United States")
Mexico <- subset(countries, countries$CNTRY_NAME=="Mexico")
states <- vect("US_States.shp")
ORCAWALE.bound = vect("US WC Ship Survey Boundary.shp")
EEZ.US = vect("EEZ_US_West_Coast.shp")

### data mine 2018 effort data only

data.2018 = data[which(substr(data$V3, 12, 13)=="18"),]
latdeg.2018 = data.2018[,5] 
latmin.2018 = data.2018[,7] 
lat.effort.2018 = latdeg.2018 + latmin.2018/60

londeg.2018 = data.2018[,9]
lonmin.2018 = data.2018[,11]
lon.effort.2018 = -(londeg.2018 + lonmin.2018/60)

for (s in 1:length(all.codes))  { species=all.codes[s]

jpeg(paste(species, "Plot.jpg"), width=3.5, height=4.3, units="in", res=1200, pointsize=8)

# plot size chosen for easy import into Word docs

plot(countries, border="black", col="gray", xlim=c(-133,-115), ylim=c(28,49), xlab="W Longitude", ylab="N Latitude", lwd=0.5)
lines(ORCAWALE.bound)
lines(EEZ.US, lty=2)
polys(states, border="black", col="gray", add=TRUE, lwd=0.5)
title(header)
text(-119.5, 37, labels="California", cex=0.6)
text(-121, 44, labels="Oregon", cex=0.6)
text(-121, 47, labels="Washington", cex=0.6)

## add data points to plot (y,x format)  
points(lon.effort, lat.effort, pch=18, cex=0.1, col="gray50")
## add separate 2018 effort lines for coastal surveys
points(lon.effort.2018, lat.effort.2018, pch=18, cex=0.1, col="black")
# add sightings to plot
on.effort = which(data$V2=="A.")
off.effort = which(data$V2=="A ")
all.sight = c(on.effort, off.effort)
if (effort=="ON") sightings = data[on.effort,]
if (effort=="ALL") sightings = data[all.sight,]
plot.sightings = c(which(sightings$V21%in%species), which(sightings$V23%in%species), which(sightings$V25%in%species))
species.data = sightings[plot.sightings,]
species.lat = species.data$V5 + species.data$V7/60
species.lon = -(species.data$V9 + species.data$V11/60)

points(species.lon, species.lat, pch=19, cex=0.4, col="blue")

# separate sightings points for 2018 data

sightings.2018 = species.data[which(substr(species.data$V3, 12, 13)=="18"),]
species.lat2 = sightings.2018$V5 + sightings.2018$V7/60
species.lon2 = -(sightings.2018$V9 + sightings.2018$V11/60)

points(species.lon2, species.lat2, pch=19, cex=0.4, col="red")

dev.off()                                                         }

# save humpback sightings to RData file

humpback.sightings.all <- sightings[sightings$V21==" 076",]
humpback.sightings.2018 <- which(substr(humpback.sightings.all$V3, 12, 13)=="18")
humpback.all.lat <- humpback.sightings.all$V5 + humpback.sightings.all$V7/60
humpback.all.lon <- -(humpback.sightings.all$V9 + humpback.sightings.all$V11/60)
humpback.all.pos <- cbind.data.frame(humpback.all.lat, humpback.all.lon)
names(humpback.all.pos) <- c("lat", "lon")
humpback.2018.pos <- cbind.data.frame(humpback.all.lat[humpback.sightings.2018], humpback.all.lon[humpback.sightings.2018])
names(humpback.2018.pos) <- c("lat","lon")

save.image("Humpback Plot Data.RData")


#### polygons can be added to map with function 'polygon'  ### polygon(eez[,1], eez[,2], col="red")

