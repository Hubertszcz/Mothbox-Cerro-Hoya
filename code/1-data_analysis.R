#load packages
library(vegan)
library(dplyr)
library(tidyr)

#load data
data <- read.csv("data_processed/data.csv")
metadata <- read.csv("data_processed/metadata.csv")     #not currently using this


###########################################################################################################################
# quick look at the data
###########################################################################################################################

#number of unique taxonomic unites (species, morphospecies, and higher) in whole dataset 
length(unique(data$name))

#percentage of insect detections identified to species level


#percentage of order-level ID's Mothbox got correct


#percentage of order-level ID's Mothbox got correct by order


###########################################################################################################################
###########################################################################################################################
# activity and richness per sampling point
###########################################################################################################################
###########################################################################################################################


#calculating frequency of each taxon at each site
dataB <- data %>%
  group_by(site_night, name) %>%
  summarise(freq = n(), .groups = "drop")

#make point data wide for vegan package
data_wide <- dataB %>% spread(key=name, value=freq)

#replace NA values with 0:
data_wide[is.na(data_wide)] <- 0

#making dataframe of just counts:
data_counts <- data_wide[,-c(1)]

insect_activity <- rowSums(data_counts)           #Total activity (sum of unique observations/photo for all photos)
insect_richness <- specnumber(data_counts)        #morphospecies richness 
insect_shannon  <- diversity(data_counts)         #Shannon diversity index (treating detections as abundance)


#land_use <- c("teak", "teak", "reforestation", "mature")

#start dataframe for visualizations
hoya_data <- data_wide[,c(1)]

#add elevation to hoya_data
elev_table <- data %>% select(site_night, elevation) %>%
  distinct()     # ensures one row per site_night

hoya_data <- hoya_data %>%
  left_join(elev_table, by = "site_night")

#add simple metrics per site_night
hoya_data$insect_activity <- insect_activity
hoya_data$insect_richness <- insect_richness
hoya_data$insect_shannon  <- insect_shannon

#order by elevation
hoya_data <- arrange(hoya_data, elevation)

#average by elevation and calculate standard error 
hoya_summary <- hoya_data %>%
  group_by(elevation) %>%
  summarise(
    mean_insect_activity  = mean(insect_activity, na.rm = TRUE),
    se_insect_activity    = sd(insect_activity, na.rm = TRUE) / sqrt(n()),
    mean_insect_richness  = mean(insect_richness, na.rm = TRUE),
    se_insect_richness    = sd(insect_richness, na.rm = TRUE) / sqrt(n()),
    mean_insect_shannon  = mean(insect_shannon, na.rm = TRUE),
    se_insect_shannon    = sd(insect_shannon, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")


###########################################################################################################################
# ANOVA for elevation and detections/richness
###########################################################################################################################

activity_aov <- aov(insect_activity ~ elevation, data = hoya_data)
summary(activity_aov)

richness_aov <- aov(insect_richness ~ elevation, data = hoya_data)
summary(richness_aov)

shannon_aov <- aov(insect_shannon ~ elevation, data = hoya_data)
summary(shannon_aov)

# so that's good news...
###########################################################################################################################
###########################################################################################################################
# Time of night analyses
###########################################################################################################################
###########################################################################################################################

#Extract the hour and minute from the eventTime column
data$hour   <- as.integer(substr(data$eventTime, 1, 2))
data$minute <- as.integer(substr(data$eventTime, 4, 5))

#Filter based on the specified time windows in the mothbox programA 
#[in case there were errors] 
#look to see if number of rows in data changes
data <- data %>%
  filter((hour == 19 & minute >= 0 & minute < 60) |
           (hour == 21 & minute >= 0 & minute < 60) |
           (hour == 23 & minute >= 0 & minute < 60) |
           (hour == 2  & minute >= 0 & minute < 60) |
           (hour == 4  & minute >= 0 & minute < 60))

###########################################################################################################################
# cumulative richness per hour and per night
###########################################################################################################################

#making a column for cumulative richness per hour
data <- data %>%
  # combine date + time and parse to POSIXct
  mutate(
    event_datetime = ymd_hms(paste(eventDate, eventTime))
  ) %>%
  # one identifier per hour window
  mutate(session = floor_date(event_datetime, unit = "hour")) %>%
  group_by(session) %>%
  arrange(event_datetime, .by_group = TRUE) %>%
  mutate(
    cumulativeRichness_hour = cumsum(!duplicated(name) & !is.na(name))
  ) %>%
  ungroup()


#similar, but cumulative richness per night
data$cumulativeRichness_night <- with(
  data, ave(name, night, FUN = function(x) cumsum(!duplicated(x) & !is.na(x))))

#Convert 'minute' to numeric for proper plotting
data$minute <- as.numeric(data$minute)

#Add':00' to each hour value
data$hour <- paste(data$hour, "00", sep = ":")  

# Ensure 'hour' is treated as a factor
data$hour <- factor(data$hour)

#Determine max minute to pad the x-axis
max_minute <- max(data$minute)

###########################################################################################################################
# cumulative detections per hour and per night
###########################################################################################################################

######### cumulative detections per hour
data <- data %>%
  group_by(eventID) %>%                      #in current csv, 'eventID' refers to the sourceImage
  mutate(detections_per_photo = n()) %>%
  ungroup()


#make sure everything is numeric
data$cumulativeRichness_hour   <- as.numeric(data$cumulativeRichness_hour)
data$cumulativeRichness_night  <- as.numeric(data$cumulativeRichness_night)
data$detections_per_photo      <- as.numeric(data$detections_per_photo)


### adding 0-value rows to represent blank sheets at the start of each session

#find one template row per session that *doesn't* already have minute == 0
sessions_needing_zero <- data %>%
  group_by(session) %>%
  filter(!any(minute == 0)) %>%  # only sessions lacking a minute 0 row
  slice(1) %>%                   # take the first row as a template
  ungroup()

#create the "zero" rows
zeros <- sessions_needing_zero %>%
  mutate(
    minute                  = 0,
    cumulativeRichness_hour = 0,
    cumulativeRichness_night = 0,
    detections_per_photo    = 0
  )

#give NA values to each value that was specific to the template rows
cols_to_na <- c(2, 4, 6:14)
zeros[, cols_to_na] <- NA

#bind back and order so each session starts at minute 0
data <- bind_rows(data, zeros) %>%
  arrange(session, minute, eventTime)

#rename so no confusion in data_processed folder

dataA <- data


###########################################################################################################################
# export summarized data
###########################################################################################################################

write.csv(hoya_data, "data_processed/hoya_data.csv", row.names = FALSE)
write.csv(hoya_summary, "data_processed/hoya_summary.csv", row.names = FALSE)
write.csv(dataA, "data_processed/dataA.csv", row.names = FALSE)