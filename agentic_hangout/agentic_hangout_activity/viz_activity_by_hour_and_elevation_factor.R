# Overlay figure: activity (detections per photo) by time of night, with per-elevation
# trends and an overall LMM fit. Dolson-style single panel (coloured points + thin lines
# per elevation + black overall trend + grey 95% CI ribbon).
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# Run prep_time_of_night.R first. Outputs to agentic_hangout/agentic_hangout_activity/output/.

library(dplyr)
library(ggplot2)
library(viridis)
library(lme4)

dir_out <- "agentic_hangout/agentic_hangout_activity/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

hour_levels <- c("19h", "21h", "23h", "2h", "4h")

#' Population-level predictions at each hour for rate ~ hour LMM (marginal mean + 95% CI).
predict_hour_lmm <- function(model, hour_levels) {
  newdata <- data.frame(hour = factor(hour_levels, levels = hour_levels))
  pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
  fit_vals <- as.numeric(pred$fit)
  se <- as.numeric(pred$se.fit)
  data.frame(
    hour = factor(hour_levels, levels = hour_levels),
    fit = fit_vals,
    ymin = fit_vals - 1.96 * se,
    ymax = fit_vals + 1.96 * se
  )
}

# ---- Data prep (match code/2-visualization.R Figure 6) ----
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")

data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = hour_levels)

session_effort <- session_effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)

data <- data %>% filter(as.numeric(elevation) != 1416)
site_nights_ok <- unique(data$site_night)
session_effort <- session_effort %>% filter(site_night %in% site_nights_ok)

session_rate <- data %>%
  group_by(site_night, hour) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(
    session_effort %>% select(site_night, hour, n_photos, elevation, elevation_n),
    by = c("site_night", "hour")
  ) %>%
  mutate(
    rate = detections / n_photos,
    site = sub("_.*", "", site_night)
  ) %>%
  filter(n_photos > 0)

elev_hour_means <- session_rate %>%
  group_by(elevation_n, hour) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    n_sessions = n(),
    .groups = "drop"
  )

write.csv(
  elev_hour_means,
  file.path(dir_out, "activity_by_hour_and_elevation_summary.csv"),
  row.names = FALSE
)

# ---- Overall LMM: rate ~ hour + random site and site_night ----
m_hour <- lmer(rate ~ hour + (1 | site) + (1 | site_night), data = session_rate, REML = TRUE)
pred_overall <- predict_hour_lmm(m_hour, hour_levels)

# ---- Plot ----
p <- ggplot(session_rate, aes(x = hour, y = rate, colour = elevation_n)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_line(
    data = elev_hour_means,
    aes(x = hour, y = mean_rate, colour = elevation_n, group = factor(elevation_n)),
    linewidth = 0.5,
    alpha = 0.85
  ) +
  geom_ribbon(
    data = pred_overall,
    aes(x = hour, ymin = ymin, ymax = ymax, group = 1),
    inherit.aes = FALSE,
    fill = "grey40",
    alpha = 0.2
  ) +
  geom_line(
    data = pred_overall,
    aes(x = hour, y = fit, group = 1),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.3
  ) +
  scale_colour_viridis_c(option = "viridis", direction = -1, name = "Elevation (m)") +
  labs(
    x = "Time of night",
    y = "Mean detections per photo"
  ) +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = NA)
  )

out_png <- file.path(dir_out, "activity_by_hour_and_elevation_factor.png")
png(out_png, width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p)
dev.off()

out_tif <- file.path(dir_out, "activity_by_hour_and_elevation_factor.tif")
tiff(out_tif, width = 12, height = 7.5, units = "in", res = 300, compression = "lzw", bg = "white")
print(p)
dev.off()

message("Written: ", out_png)
message("Written: ", out_tif)
message("Written: ", file.path(dir_out, "activity_by_hour_and_elevation_summary.csv"))
