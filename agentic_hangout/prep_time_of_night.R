# Time-of-night prep: derive hour, filter to programA windows, compute session effort.
# Reads data_processed/data.csv; writes session_effort.csv to agentic_hangout/output/time_of_night/.
# Run first; downstream scripts use session_effort.csv.

library(dplyr)

# Input and output
data <- read.csv("data_processed/data.csv")
dir_out <- "agentic_hangout/output/time_of_night"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Derive hour from eventTime (HH:MM:SS)
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))

# Restrict to programA time windows: 19, 21, 23, 2, 4
data <- data %>%
  filter(hour_int %in% c(19, 21, 23, 2, 4))

# Ordered factor for plotting (night sequence)
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Session = site_night x hour; n_photos per session
session_effort <- data %>%
  group_by(site_night, hour, hour_int) %>%
  summarise(
    n_photos = n_distinct(eventID),
    elevation = first(elevation),
    .groups = "drop"
  ) %>%
  select(site_night, hour, hour_int, elevation, n_photos)

write.csv(session_effort, file.path(dir_out, "session_effort.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "session_effort.csv"))
