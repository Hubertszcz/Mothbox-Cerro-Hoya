# Variation partitioning: richness and abundance (rate) vs time of night.
# Mixed models with space (site) and time (site_night, hour) as random/fixed.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# Outputs: variance table CSV, variance-component bar chart, time-of-night panel plot.
# Run prep_time_of_night.R first.

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")
dir_out <- "agentic_hangout/output/time_of_night"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Derive hour and filter to programA windows
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Session-level richness (number of distinct orders per session) and detections
session_richness <- data %>%
  group_by(site_night, hour) %>%
  summarise(
    richness = n_distinct(order[!is.na(order) & trimws(as.character(order)) != ""]),
    detections = n(),
    .groups = "drop"
  ) %>%
  left_join(session_effort %>% select(site_night, hour, n_photos, elevation), by = c("site_night", "hour")) %>%
  mutate(
    rate = detections / n_photos,
    site = sub("_.*", "", site_night)
  )

# Drop sessions with no effort (should not occur)
session_richness <- session_richness %>% filter(n_photos > 0)

# Fit mixed models: hour fixed, site and site_night random (space and night-within-space)
m_rich <- lmer(richness ~ hour + (1 | site) + (1 | site_night), data = session_richness, REML = TRUE)
m_rate <- lmer(rate ~ hour + (1 | site) + (1 | site_night), data = session_richness, REML = TRUE)

# Extract variance components and compute percentages
get_var_partition <- function(model, name) {
  vc <- as.data.frame(VarCorr(model))
  comp <- vc$grp
  var_val <- vc$vcov
  total <- sum(var_val)
  pct <- 100 * var_val / total
  data.frame(
    response = name,
    component = comp,
    variance = var_val,
    pct = pct
  )
}

var_rich <- get_var_partition(m_rich, "richness")
var_rate <- get_var_partition(m_rate, "rate")
var_table <- rbind(var_rich, var_rate)
# Rename for display: site = Space, site_night = Night (within site), Residual = Residual
var_table <- var_table %>%
  mutate(component = case_when(
    component == "site" ~ "Space (site)",
    component == "site_night" ~ "Night (site_night)",
    component == "Residual" ~ "Residual",
    TRUE ~ component
  ))

write.csv(var_table, file.path(dir_out, "variance_components_time_of_night.csv"), row.names = FALSE)
message("Written: variance_components_time_of_night.csv")

# ---------- Visualization 1: Variance-component stacked bar chart ----------
var_plot <- var_table %>%
  mutate(component = factor(component, levels = c("Space (site)", "Night (site_night)", "Residual")))
p_var <- ggplot(var_plot, aes(x = response, y = pct, fill = component)) +
  geom_col() +
  scale_fill_viridis_d(option = "viridis", name = "Variance component") +
  labs(x = "Response", y = "% variance", title = "Variation partitioning (richness and rate ~ hour + (1|site) + (1|site_night))") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
png(file.path(dir_out, "variance_components_bar.png"), width = 8, height = 5, units = "in", res = 300, bg = "white")
print(p_var)
dev.off()
message("Written: variance_components_bar.png")

# ---------- Visualization 2: Time of night panel by elevation (space) ----------
session_summary <- session_richness %>%
  group_by(elevation, hour) %>%
  summarise(
    mean_richness = mean(richness, na.rm = TRUE),
    se_richness = sd(richness, na.rm = TRUE) / sqrt(n()),
    mean_rate = mean(rate, na.rm = TRUE),
    se_rate = sd(rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(elevation = factor(elevation))

p_rich_panel <- ggplot(session_summary, aes(x = hour, y = mean_richness, colour = elevation, group = elevation)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_richness - se_richness, ymax = mean_richness + se_richness), width = 0.15, linewidth = 0.5) +
  scale_colour_viridis_d(option = "viridis", name = "Elevation (m)") +
  labs(x = "Time of night", y = "Mean richness (orders)", title = "Richness by time of night and elevation") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

p_rate_panel <- ggplot(session_summary, aes(x = hour, y = mean_rate, colour = elevation, group = elevation)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate), width = 0.15, linewidth = 0.5) +
  scale_colour_viridis_d(option = "viridis", name = "Elevation (m)") +
  labs(x = "Time of night", y = "Mean detections per photo", title = "Activity by time of night and elevation") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

png(file.path(dir_out, "time_of_night_by_elevation_richness.png"), width = 9, height = 5, units = "in", res = 300, bg = "white")
print(p_rich_panel)
dev.off()
png(file.path(dir_out, "time_of_night_by_elevation_rate.png"), width = 9, height = 5, units = "in", res = 300, bg = "white")
print(p_rate_panel)
dev.off()
message("Written: time_of_night_by_elevation_richness.png, time_of_night_by_elevation_rate.png")

# Optional: write session-level dataset for reuse
write.csv(session_richness, file.path(dir_out, "session_richness_rate.csv"), row.names = FALSE)
message("Written: session_richness_rate.csv")

message("Variation partitioning done. Outputs in ", dir_out)
