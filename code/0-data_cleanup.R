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
metadata <- metadata[c(21,2,7,8,9:14,17)]          ### Clean up some unnecessary columns

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

#deplotment_name == deployment
names(metadata)[names(metadata) == "deployment_name"] <- "deployment"

#add 'site' column to data
data <- data %>%
  left_join(metadata %>% select(deployment, site), by = "deployment")

#add elevation column to data
data$elevation <- parse_number(data$site)

#add 'night' column to data
data <- data %>%
  mutate(dt = ymd_hms(verbatimEventDate, quiet = TRUE),     # parse verbatimEventDate to POSIXct (ymd_hms)
    night = as.Date(if_else(
      hour(dt) < 5,          # times between 00:00 and 04:59 → previous date
      dt - days(1), dt)))    # times 05:00–23:59 → same date

#add 'site_night' column to data
data$site_night <- paste(data$site, data$night, sep = "_")

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

#looks like I'm missing data for the point at 202m elevation. In deployment notes: "DID NOT TAKE PHOTOS"


## In our deployment notes for elevation 1416: "Was entirely discharged (no lights on anywhere) at collection. The epoxy for the lens was cracked. Perhaps overheated? 

#it also got conspicuously almost no insects. I'm cutting it from the analysis

data <- data[data$elevation != "1416", ]
  
# Need to think about what to do with elevation 1204. Deployment notes: "Corrupted images"  

# Standardize taxonomic level for analysis:
# - Non-Coleoptera: set name to order (analyze at order level).
# - Coleoptera: keep finer taxonomy (do not change name).
# - No order (e.g. only Insecta): leave name unchanged.
data <- data %>%
  mutate(
    has_order = !is.na(order) & trimws(as.character(order)) != "",
    name = if_else(has_order & order != "Coleoptera", as.character(order), name)
  ) %>%
  select(-has_order)

###########################################################################################################################
###########################################################################################################################

#export cleaned data
write.csv(data, "data_processed/data.csv", row.names = FALSE)
write.csv(metadata, "data_processed/metadata.csv", row.names = FALSE)


###########################################################################################################################
# analysis that requires raw data
###########################################################################################################################

#percentage of detections that were errors
((nrow(data_raw) - nrow(data))/nrow(data_raw))*100

#percentage of detections that Mothbot thought were errors
errors <- data_raw[grepl("ERROR", data_raw$original_mothbox_identifciation), ]

(nrow(errors)/nrow(data_raw))*100

# 0.68% vs the 21.96% that were actually errors

## so errors Mothbot identified as insects are still a significant issue. 
# AKA errors are under-represented in raw computer vision outputs.