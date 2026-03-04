# Q2: Hour x elevation interaction (detections per photo). Elevation as factor and as continuous.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# Two figures + ANOVA table. All outputs to agentic_hangout/output/time_of_night/.

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

# Session-level total detections (all insects) and rate
session_rate <- data %>%
  group_by(site_night, hour) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos, elevation), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos)

# Summary by hour and elevation (for plotting)
summary_factor <- session_rate %>%
  group_by(hour, elevation) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
  mutate(elevation_f = factor(elevation))

# Models
session_rate <- session_rate %>% mutate(elevation_f = factor(elevation))
aov_factor <- aov(rate ~ hour * elevation_f, data = session_rate)
lm_cont <- lm(rate ~ hour * elevation, data = session_rate)

# ANOVA table (factor model)
capture.output(
  cat("=== Elevation as FACTOR (aov: rate ~ hour * factor(elevation)) ===\n\n"),
  print(summary(aov_factor)),
  cat("\n=== Elevation as CONTINUOUS (lm: rate ~ hour * elevation) ===\n\n"),
  print(summary(lm_cont))
) -> anova_txt
writeLines(anova_txt, file.path(dir_out, "hour_elevation_ANOVA.txt"))
message("Written: ", file.path(dir_out, "hour_elevation_ANOVA.txt"))

# Optional: CSV of key terms
anova_tab <- summary(aov_factor)[[1]]
anova_tab$term <- rownames(anova_tab)
write.csv(anova_tab[, c("term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")],
          file.path(dir_out, "hour_elevation_ANOVA_factor.csv"), row.names = FALSE)
lm_tab <- as.data.frame(coef(summary(lm_cont)))
lm_tab$term <- rownames(lm_tab)
write.csv(lm_tab, file.path(dir_out, "hour_elevation_lm_continuous.csv"), row.names = FALSE)
message("Written: hour_elevation_ANOVA_factor.csv, hour_elevation_lm_continuous.csv")

# Figure 1: Elevation as factor (facet by elevation, hour on x, mean rate on y)
p_factor <- ggplot(summary_factor, aes(x = hour, y = mean_rate, group = 1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate), width = 0.15, linewidth = 0.5) +
  facet_wrap(~ elevation_f, scales = "free_y", ncol = 4) +
  labs(x = "Time of night", y = "Detections per photo") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(size = 12))
png(file.path(dir_out, "activity_by_hour_and_elevation_factor.png"), width = 14, height = 8, units = "in", res = 300, bg = "white")
print(p_factor)
dev.off()
message("Written: ", file.path(dir_out, "activity_by_hour_and_elevation_factor.png"))

# Figure 2: Elevation as continuous (x = hour, colour = elevation, viridis)
# Mean rate by hour and elevation; one line per elevation (colour = elevation)
summary_cont <- session_rate %>%
  group_by(hour, elevation) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop")
p_cont <- ggplot(summary_cont, aes(x = hour, y = mean_rate, group = elevation, colour = elevation)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_viridis_c(option = "viridis", name = "Elevation (m)") +
  labs(x = "Time of night", y = "Detections per photo") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
png(file.path(dir_out, "activity_by_hour_and_elevation_continuous.png"), width = 12, height = 7, units = "in", res = 300, bg = "white")
print(p_cont)
dev.off()
message("Written: ", file.path(dir_out, "activity_by_hour_and_elevation_continuous.png"))
