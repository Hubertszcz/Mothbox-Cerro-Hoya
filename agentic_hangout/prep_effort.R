# Sampling effort per site_night: n_photos = n_distinct(eventID).
# Reads data_processed/data.csv (read-only), writes site_night_effort.csv,
# effort summary, and effort_vs_elevation.png to agentic_hangout/output/.
# Run this first; downstream agentic scripts read site_night_effort.csv.

library(dplyr)
library(ggplot2)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Effort = number of distinct photos (eventID) per site_night
effort_table <- data %>%
  group_by(site_night) %>%
  summarise(
    n_photos = n_distinct(eventID),
    elevation = first(elevation),
    .groups = "drop"
  )

# Write site_night-level effort table
write.csv(effort_table, file.path(dir_out, "site_night_effort.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "site_night_effort.csv"))

# Effort summary (overall and by elevation)
summary_overall <- data.frame(
  stat = c("min", "max", "mean", "median", "sd"),
  value = c(
    min(effort_table$n_photos),
    max(effort_table$n_photos),
    mean(effort_table$n_photos),
    median(effort_table$n_photos),
    sd(effort_table$n_photos)
  )
)
summary_by_elev <- effort_table %>%
  group_by(elevation) %>%
  summarise(
    n_site_nights = n(),
    n_photos_min = min(n_photos),
    n_photos_max = max(n_photos),
    n_photos_mean = round(mean(n_photos), 2),
    .groups = "drop"
  )

sink(file.path(dir_out, "effort_summary.txt"))
cat("Sampling effort (n_photos = n_distinct(eventID) per site_night)\n")
cat("========================================\n\n")
cat("Overall:\n")
print(summary_overall)
cat("\nBy elevation:\n")
print(as.data.frame(summary_by_elev))
sink()
message("Written: ", file.path(dir_out, "effort_summary.txt"))

# Effort vs elevation plot
effort_by_elev <- effort_table %>%
  group_by(elevation) %>%
  summarise(
    mean_n_photos = mean(n_photos, na.rm = TRUE),
    se_n_photos = sd(n_photos, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p <- ggplot(effort_by_elev, aes(x = factor(elevation), y = mean_n_photos, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_n_photos - se_n_photos, ymax = mean_n_photos + se_n_photos),
    width = 0.1, linewidth = 0.6
  ) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)) +
  labs(x = "Elevation (m)", y = "Mean number of photos per site_night") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    legend.position = "none"
  )

png(file.path(dir_out, "effort_vs_elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p)
dev.off()
message("Written: ", file.path(dir_out, "effort_vs_elevation.png"))
