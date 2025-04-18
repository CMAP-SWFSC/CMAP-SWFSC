### Updated 12-05-2023
### build RF model from SI data to prorate unidentified whale entanglement cases to species.
### there is no equivalent model to prorate vessel strike cases

rm(list=ls())

library(rfPermute)
library(readxl)
set.seed(123)

setwd("c:/carretta/net_mort/Serious Injury")

# data <- read.csv("Anthropogenic_Mortality_Serious_Injury_Carretta.csv", stringsAsFactors = TRUE)
data = read_excel("Anthropogenic_Mortality_Serious_Injury_Carretta.xlsx")
large.whales <- data[grep("BLUE|BRYDE'S|FIN WHALE|HUMPBACK|GRAY|MINKE|SPERM|UNIDENTIFIED WHALE", data$Species, ignore.case=T),]

keep.source <- grep("GILLNET|HOOK AND LINE|MARINE DEBRIS|NET|POT|TRAP|UNIDENTIFIED FISHERY INTERACTION", large.whales$Interaction.Type, ignore.case=TRUE)

data.new <- large.whales[c(keep.source),]

days = paste(data.new$Year, data.new$Month, data.new$Day, sep="/")
days = as.integer(strftime(days, format = "%j"))

data.new$day.of.year <- days

keep.fields <- which(names(data.new)%in%c("Species", "day.of.year","Year","County.Locale","Stock.or.Area"))

model.df <- cbind.data.frame(data.new[,keep.fields])

covariates <- grep("day.of.year|Year|County.Locale", names(model.df), ignore.case=TRUE)

model.df$County.Locale <- as.factor(as.character(model.df$County.Locale))
model.df$Species <- as.factor(as.character(model.df$Species))

known.species <- model.df[model.df$Species!="UNIDENTIFIED WHALE",]
known.species$Species <- as.factor(as.character(known.species$Species))
unid.species <- model.df[model.df$Species=="UNIDENTIFIED WHALE",]
unid.species$Species <- as.factor(as.character(unid.species$Species))

rf.model <- rfPermute(known.species$Species ~., known.species[,covariates])
rf.model

# classPriors()
# prior: the expected percent of samples that would be correctly classified if the model was classifying samples at random. This is based on the relative difference in sample sizes of each class. If all classes have the same sample size, the priors will be all be 1 / k where k is the number of classes.
# class_p.value: the binomial probability that the number of correctly classified samples in each class would be observed given the size of that class under the assumption that prior is is the correct rate of correct classification.

classPriors(rf.model, sampsize=NULL)

# apply model to predict unid whale classes

predictions <- predict(rf.model, unid.species, type="prob")

predictions.df <- cbind.data.frame(unid.species, predictions)
predictions.df
write.csv(predictions.df, "Unid.Whale.Species.Predictions.csv")

# summarize most-recent 5-yr prediction totals by species

max.yr <- max(predictions.df$Year)
min.yr <- max.yr - 4
keep.yrs <- seq(min.yr, max.yr, 1)
df.5yr <- predictions.df[predictions.df$Year%in%keep.yrs,]

sum(df.5yr$`BLUE WHALE`)
sum(df.5yr$`FIN WHALE`)
sum(df.5yr$`GRAY WHALE`)
sum(df.5yr$`HUMPBACK WHALE`)
sum(df.5yr$`MINKE WHALE`)
sum(df.5yr$`PYGMY SPERM WHALE`)
sum(df.5yr$`SPERM WHALE`)

setwd("C:/Carretta/GitHub/HCM_SI")


