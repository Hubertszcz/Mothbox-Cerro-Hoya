# Q1: Activity (detections per photo) by time of night, per order / Coleoptera taxon / family.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# All outputs to agentic_hangout/output/time_of_night/.

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")
dir_out <- "agentic_hangout/output/time_of_night"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Derive hour and filter to 19, 21, 23, 2, 4
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Session-level detections by order
order_session <- data %>%
  group_by(site_night, hour, order) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos)

order_hour_summary <- order_session %>%
  group_by(order, hour) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    se_rate = sd(rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot: order x hour (detections per photo)
p_order <- ggplot(order_hour_summary, aes(x = hour, y = mean_rate, fill = order)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate),
                position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = "Time of night", y = "Detections per photo", fill = "Order") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "right")
png(file.path(dir_out, "activity_by_order_and_hour.png"), width = 12, height = 6, units = "in", res = 300, bg = "white")
print(p_order)
dev.off()
message("Written: ", file.path(dir_out, "activity_by_order_and_hour.png"))

# Coleoptera: by morphospecies (top 15 by total detections)
coleo <- data %>% filter(order == "Coleoptera")
taxon_totals <- coleo %>% count(name, name = "total") %>% slice_max(total, n = 15, with_ties = FALSE)
coleo_top <- coleo %>% filter(name %in% taxon_totals$name)

taxon_session <- coleo_top %>%
  group_by(site_night, hour, name) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos)

taxon_hour_summary <- taxon_session %>%
  group_by(name, hour) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

p_taxon <- ggplot(taxon_hour_summary, aes(x = hour, y = mean_rate, group = name, colour = name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "viridis") +
  labs(x = "Time of night", y = "Detections per photo", colour = "Taxon") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", legend.text = element_text(size = 8))
png(file.path(dir_out, "Coleoptera_activity_by_taxon_and_hour.png"), width = 12, height = 7, units = "in", res = 300, bg = "white")
print(p_taxon)
dev.off()
message("Written: ", file.path(dir_out, "Coleoptera_activity_by_taxon_and_hour.png"))

# Coleoptera: by family
coleo <- coleo %>% mutate(family = if_else(is.na(family) | trimws(as.character(family)) == "", "Unknown", as.character(family)))
family_session <- coleo %>%
  group_by(site_night, hour, family) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos)

family_hour_summary <- family_session %>%
  group_by(family, hour) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

p_family <- ggplot(family_hour_summary, aes(x = hour, y = mean_rate, fill = family)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate),
                position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = "Time of night", y = "Detections per photo", fill = "Family") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "right")
png(file.path(dir_out, "Coleoptera_activity_by_family_and_hour.png"), width = 12, height = 6, units = "in", res = 300, bg = "white")
print(p_family)
dev.off()
message("Written: ", file.path(dir_out, "Coleoptera_activity_by_family_and_hour.png"))
