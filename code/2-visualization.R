#load packages
library(ggplot2)
library(viridis)

#load data
dataA <- read.csv("data_processed/dataA.csv")
hoya_summary <- read.csv("data_processed/hoya_summary.csv")
hoya_data <- read.csv("data_processed/hoya_data.csv")

# =========================================================
# Figure 1: Detections and elevation
# =========================================================
p1 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_activity, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_insect_activity - se_insect_activity,
        ymax = mean_insect_activity + se_insect_activity),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Total detections") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")


png("output/Detections and elevation.png",
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p1)
dev.off()

# =========================================================
# Figure 2: Richness and elevation
# =========================================================

p1 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_richness, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_insect_richness- se_insect_richness,
        ymax = mean_insect_richness + se_insect_richness),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Mean richness") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")


png("output/Richness and elevation.png",
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p1)
dev.off()

# =========================================================
# Figure 3: Shannon diversity and elevation
# =========================================================

p1 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_shannon, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_insect_shannon- se_insect_shannon,
        ymax = mean_insect_shannon + se_insect_shannon),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Mean shannon") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")


png("output/shannon and elevation.png",
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p1)
dev.off()

