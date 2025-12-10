#load packages
library(vroom)
library(lubridate)
library(dplyr)
library(readr)

#load data
data_raw <- vroom(list.files("data_raw/occurence_data", pattern = "\\.csv$", full.names = TRUE),id = "source_file")
metadata <- read.csv("data_raw/mothbox_metadata.csv")


#cleanup metadata
metadata <- subset(metadata, project == "Hoya")    ### Only data from the Cerro Hoya expedition
metadata <- metadata[c(21,2,7,8,9:14,17)]            ### Clean up some unnecessary columns

#clean data
#remove rows with no taxonID (errors)
data <- data_raw[complete.cases(data_raw[ , c('taxonID')]), ]

#make sure times are all correct
metadata$deployment_date <- parse_date_time(metadata$deployment_date, orders = c("Ybd", "dmy", "mdy", "ymd"))
metadata$collect_date <- parse_date_time(metadata$collect_date, orders = c("Ybd", "dmy", "mdy", "ymd"))

## for some reason R read the time as just seconds
data$eventTime <- seconds_to_period(data$eventTime)           
data$eventTime <- sprintf("%02d:%02d:%02d", hour(data$eventTime), minute(data$eventTime), second(data$eventTime))
data$eventDate <- parse_date_time(data$eventDate, orders = c("Ybd", "dmy", "mdy", "ymd"))

#add elevation column to metadata
metadata$elevation <- parse_number(metadata$site)

#deplotment_name == deployment
names(metadata)[names(metadata) == "deployment_name"] <- "deployment"


###########################################################################################################################
# tests to make sure everything is ok
###########################################################################################################################

#everything has a name
dataB <- subset(data, name == "")  #there should be no rows in dataB

#are we missing any deployments between data and metadata?
setequal(unique(data$deployment), unique(metadata$deployment))   #Check if deployments in are identical
#FALSE = they are not identical

setdiff(unique(data$deployment), unique(metadata$deployment))    #Values in data$deployment but NOT in metadata$deployment
setdiff(unique(metadata$deployment), unique(data$deployment))    #Values in metadata$deployment but NOT in data$deployment

#looks like I'm missing data for...

###########################################################################################################################
###########################################################################################################################

#export cleaned data
write.csv(data, "data_processed/data.csv", row.names = FALSE)
write.csv(metadata, "data_processed/metadata.csv", row.names = FALSE)


###########################################################################################################################
# analysis that requires raw data
###########################################################################################################################


#what percentage of detections were errors?
((nrow(data_raw) - nrow(data))/nrow(data_raw))*100

#what percentage did Mothbot think were errors?
errors <- data_raw[grepl("ERROR", data_raw$original_mothbox_identifciation), ]

(nrow(errors)/nrow(data_raw))*100

## so errors Mothbot-identified as insects are still a significant issue



