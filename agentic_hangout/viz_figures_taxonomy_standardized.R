# Recreate the two main output figures using taxonomically standardized data.
# Does not modify any scripts in code/. Outputs go to agentic_hangout/output/.
#
# Figures: (1) Detections and elevation, (2) Richness and elevation
# Logic mirrors code/1-data_analysis.R (summary by site_night then elevation) and
# code/2-visualization.R (same ggplot code).

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Input: standardized data (use "name" column for taxonomy)
data <- read.csv("agentic_hangout/data_taxonomy_standardized.csv")

# Output directory (create if needed)
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# ---- Replicate analysis (same logic as code/1-data_analysis.R) ----
# Frequency of each taxon at each site_night
dataB <- data %>%
  group_by(site_night, name) %>%
  summarise(freq = n(), .groups = "drop")

# Wide format for vegan
data_wide <- dataB %>% pivot_wider(names_from = name, values_from = freq, values_fill = 0)
data_wide <- as.data.frame(data_wide)
data_wide[is.na(data_wide)] <- 0

# Counts only (exclude site_night)
data_counts <- data_wide[, -1, drop = FALSE]

insect_activity <- rowSums(data_counts)
insect_richness <- specnumber(data_counts)

# Elevation per site_night
elev_table <- data %>% select(site_night, elevation) %>% distinct()
hoya_data <- data_wide[, c(1), drop = FALSE]
hoya_data <- hoya_data %>% left_join(elev_table, by = "site_night")
hoya_data$insect_activity <- insect_activity
hoya_data$insect_richness <- insect_richness
hoya_data <- arrange(hoya_data, elevation)

# Summary by elevation (mean and SE)
hoya_summary <- hoya_data %>%
  group_by(elevation) %>%
  summarise(
    mean_insect_activity = mean(insect_activity, na.rm = TRUE),
    se_insect_activity   = sd(insect_activity, na.rm = TRUE) / sqrt(n()),
    mean_insect_richness = mean(insect_richness, na.rm = TRUE),
    se_insect_richness   = sd(insect_richness, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- Figure 1: Detections and elevation (same as code/2-visualization.R) ----
p1 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_activity, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_insect_activity - se_insect_activity,
        ymax = mean_insect_activity + se_insect_activity),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +
  labs(x = "Elevation (m)", y = "Total detections") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")

png(file.path(dir_out, "Detections and elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p1)
dev.off()
message("Written: ", file.path(dir_out, "Detections and elevation.png"))

# ---- Figure 2: Richness and elevation (same as code/2-visualization.R) ----
p2 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_richness, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_insect_richness - se_insect_richness,
        ymax = mean_insect_richness + se_insect_richness),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +
  labs(x = "Elevation (m)", y = "Mean richness") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")

png(file.path(dir_out, "Richness and elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p2)
dev.off()
message("Written: ", file.path(dir_out, "Richness and elevation.png"))
