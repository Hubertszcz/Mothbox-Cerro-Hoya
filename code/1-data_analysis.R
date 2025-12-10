#load packages
library(vegan)


#load data
data <- read.csv("data_processed/data.csv")
metadata <- read.csv("data_processed/metadata.csv")




###########################################################################################################################
# quick look at the data
###########################################################################################################################

#number of unique taxonomic unites (species, morphospecies, and higher) in whole dataset 
length(unique(data$name))

#percentage of insect detections identified to species level


#percentage of order-level ID's Mothbox got correct


#percentage of order-level ID's Mothbox got correct by order



###########################################################################################################################
