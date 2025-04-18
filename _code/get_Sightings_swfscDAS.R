# read DAS file and output sighting information

remotes::install_github("swfsc/swfscDAS")
library(swfscDAS)

input.DAS <- "c:/carretta/MMTD RV DATA ARCHIVE/RV-Data/2024/Cruise_1652/DASALL2024.DAS"

cruise <- das_read(input.DAS)

cruise.proc <- das_process(cruise)


# the following will include all sighting related event codes;
# resights "s", sightings "S", subgroups "G", etc.

cruise.sight <- das_sight(cruise.proc)

# include only "S" events.  Resulting data frame may contain multiple lines
# per sighting to reflect multispecies sightings.

sightings <- cruise.sight[cruise.sight$Event=="S",]

write.csv(sightings, "c:/carretta/MMTD RV DATA ARCHIVE/RV-Data/2024/Cruise_1652/Cruise_1652_Sightings_swfscDAS.csv", row.names=FALSE)
