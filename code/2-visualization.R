#load packages
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)

#load data
dataA <- read.csv("data_processed/dataA.csv")
hoya_summary <- read.csv("data_processed/hoya_summary.csv")
hoya_data <- read.csv("data_processed/hoya_data.csv")
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("data_processed/session_effort.csv")

# =========================================================
# Figure 1: Detections and elevation
# =========================================================

# Detections per photo by elevation (same effort logic as Figure 4; exclude 1416 m)
photos_per_site <- session_effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416) %>%
  group_by(site_night) %>%
  summarise(n_photos_total = sum(n_photos), .groups = "drop")
hoya_data_fig1 <- hoya_data %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)
summary_fig1 <- hoya_data_fig1 %>%
  select(site_night, elevation, insect_activity) %>%
  left_join(photos_per_site, by = "site_night") %>%
  mutate(rate = insect_activity / n_photos_total) %>%
  group_by(elevation) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    se_rate = sd(rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

p1 <- ggplot(summary_fig1, aes(x = factor(elevation), y = mean_rate, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_rate - se_rate,
        ymax = mean_rate + se_rate),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Mean detections per photo") +
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

p2 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_richness, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_insect_richness- se_insect_richness,
        ymax = mean_insect_richness + se_insect_richness),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Mean Richness") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")


png("output/Richness and elevation.png",
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p2)
dev.off()

# =========================================================
# Figure 3: Shannon diversity and elevation
# =========================================================

p3 <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_insect_shannon, fill = factor(elevation))) + 
  geom_col() +  geom_errorbar(
    aes(ymin = mean_insect_shannon- se_insect_shannon,
        ymax = mean_insect_shannon + se_insect_shannon),
    width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = function(x) {
    ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)}) +  # stagger labels
  labs(x = "Elevation (m)", y = "Mean Shannon Diversity") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text  = element_text(size = 20),
        legend.position = "none")


png("output/shannon and elevation.png",
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p3)
dev.off()


# =========================================================
# Figure 4: Activity by session and elevation bands
# =========================================================
elev_breaks <- c(0, 300, 600, 900, 1300, 2000)
elev_labels <- c("<300 m", "300–600 m", "600–900 m", "900–1300 m", ">1300 m")

# Ensure numeric elevation and apply project exclusion (1416 m)
session_effort <- session_effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416) %>%
  mutate(elevation_band = cut(
    elevation_n,
    breaks = elev_breaks,
    labels = elev_labels,
    include.lowest = TRUE,
    right = FALSE
  ))

# Derive hour and filter to programA windows (19, 21, 23, 2, 4)
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Exclude 1416 m from data (match cleanup)
data <- data %>% filter(as.numeric(elevation) != 1416)

# Restrict session_effort to same site_nights as data (after 1416 exclusion)
site_nights_ok <- unique(data$site_night)
session_effort <- session_effort %>% filter(site_night %in% site_nights_ok)

focal_orders <- c("Lepidoptera", "Coleoptera", "Hemiptera", "Diptera", "Hymenoptera")

# Helper: session-level rates for a given detection dataset, with panel label
session_rates <- function(detections_df, panel_name) {
  counts <- detections_df %>%
    group_by(site_night, hour) %>%
    summarise(detections = n(), .groups = "drop")
  session_effort %>%
    select(site_night, hour, n_photos, elevation_band) %>%
    left_join(counts, by = c("site_night", "hour")) %>%
    mutate(
      detections = replace_na(detections, 0L),
      rate = detections / n_photos,
      panel = panel_name
    )
}

# All orders
all_rates <- session_rates(data, "All orders")

# Per-order panels
lepi_rates   <- session_rates(data %>% filter(order == "Lepidoptera"), "Lepidoptera")
coleo_rates  <- session_rates(data %>% filter(order == "Coleoptera"), "Coleoptera")
hemi_rates   <- session_rates(data %>% filter(order == "Hemiptera"), "Hemiptera")
dip_rates    <- session_rates(data %>% filter(order == "Diptera"), "Diptera")
hymen_rates  <- session_rates(data %>% filter(order == "Hymenoptera"), "Hymenoptera")

# Other orders (exclude focal five and NA/blank)
other_rates <- session_rates(
  data %>% filter(
    !(order %in% focal_orders),
    !is.na(order),
    trimws(as.character(order)) != ""
  ),
  "Other orders"
)

# Combine and set panel order
panel_levels <- c("All orders", "Lepidoptera", "Coleoptera", "Hemiptera", "Diptera", "Hymenoptera", "Other orders")
rates_long <- bind_rows(
  all_rates, lepi_rates, coleo_rates, hemi_rates, dip_rates, hymen_rates, other_rates
) %>%
  mutate(panel = factor(panel, levels = panel_levels))

# Summary by panel, hour, elevation_band
summary_df <- rates_long %>%
  group_by(panel, hour, elevation_band) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    se_rate = sd(rate, na.rm = TRUE) / sqrt(n()),
    n_sessions = n(),
    .groups = "drop"
  )

# Order elevation_band so highest is on top (row 1): >1300 m down to <300 m
elev_band_levels <- c(">1300 m", "900–1300 m", "600–900 m", "300–600 m", "<300 m")
summary_df <- summary_df %>%
  mutate(elevation_band = factor(as.character(elevation_band), levels = elev_band_levels))

# Save summary CSV to output/
write.csv(summary_df, "output/activity_by_session_elevation_bands_summary.csv", row.names = FALSE)

# Panels to show (exclude Hymenoptera and Other orders)
plot_panels <- c("All orders", "Lepidoptera", "Coleoptera", "Hemiptera", "Diptera")
summary_plot <- summary_df %>% filter(panel %in% plot_panels)

# Plot: rows = elevation_band, columns = panel. Bars colored by elevation band.
p4 <- ggplot(summary_plot, aes(x = hour, y = mean_rate, fill = elevation_band)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate),
    width = 0.2,
    linewidth = 0.5,
    colour = "grey25"
  ) +
  facet_grid(rows = vars(elevation_band), cols = vars(panel), scales = "free_y") +
  scale_fill_viridis_d(option = "viridis", name = "Elevation band") +
  labs(
    x = "Session",
    y = "Mean detections per photo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(size = 13),
    strip.text.x = element_text(size = 14, face = "bold"),
    legend.position = "none",
    panel.spacing.x = unit(0.6, "cm"),
    panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.6)
  )

png("output/activity_by_session_and_elevation_bands.png",
  width = 10,
  height = 9.5,
  units = "in",
  res = 300,
  bg = "white")

print(p4)
dev.off()


tiff("output/activity_by_session_and_elevation_bands.tif",
     width = 10, height = 9.5, units = "in", res = 300, compression = "lzw", bg = "white")
print(p4)
dev.off()

# =========================================================
# Figure 5: Activity by order and hour (four focal orders + Other orders)
# =========================================================
order_four <- c("Lepidoptera", "Coleoptera", "Hemiptera", "Diptera")
order_session <- data %>%
  group_by(site_night, hour, order) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos) %>%
  mutate(order_display = if_else(order %in% order_four, as.character(order), "Other orders"))

order_hour_summary <- order_session %>%
  group_by(order_display, hour) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    se_rate = sd(rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

order_hour_focal <- order_hour_summary %>%
  mutate(order_display = factor(order_display, levels = c(order_four, "Other orders")))

p_order <- ggplot(order_hour_focal, aes(x = hour, y = mean_rate, fill = order_display)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate),
                position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = "Session", y = "Mean detections per photo", fill = "Order") +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = NA))

png("output/activity_by_order_and_hour.png", width = 12, height = 6, units = "in", res = 300, bg = "white")
print(p_order)
dev.off()

##################################
##################################
####### NEW ######################
##################################
##################################
tiff("output/activity_by_order_and_hour.tif",
     width = 12, height = 6, units = "in", res = 300, compression = "lzw", bg = "white")
print(p_order)
dev.off()
##################################
##################################
####### NEW ######################
##################################
##################################

# =========================================================
# Figure 6: Activity by hour and elevation (factor)
# =========================================================
session_rate <- data %>%
  group_by(site_night, hour) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(session_effort %>% select(site_night, hour, n_photos, elevation), by = c("site_night", "hour")) %>%
  mutate(rate = detections / n_photos)

summary_factor <- session_rate %>%
  group_by(hour, elevation) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
  mutate(elevation_f = factor(elevation))

p_factor <- ggplot(summary_factor, aes(x = hour, y = mean_rate, group = 1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate), width = 0.15, linewidth = 0.5) +
  facet_wrap(~ elevation_f, scales = "free_y", ncol = 4) +
  labs(x = "Session", y = "Mean detections per photo") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

png("output/activity_by_hour_and_elevation_factor.png", width = 14, height = 8, units = "in", res = 300, bg = "white")
print(p_factor)
dev.off()

##################################
##################################
####### NEW ######################
##################################
##################################

# Figure 6 overlay: activity by time of night across elevation (reads outputs from 3-statistics.R)
if (!file.exists("data_processed/activity_by_hour_lmm_predictions.csv")) {
  stop("Run code/3-statistics.R before Figure 6 overlay (missing activity_by_hour_lmm_predictions.csv).")
}
session_rate_fig6 <- read.csv("data_processed/activity_by_hour_session_rate.csv")
elev_hour_means_fig6 <- read.csv("data_processed/activity_by_hour_elevation_summary.csv")
pred_fig6_hour <- read.csv("data_processed/activity_by_hour_lmm_predictions.csv")
hour_levels_fig6 <- c("19h", "21h", "23h", "2h", "4h")
session_rate_fig6$hour <- factor(session_rate_fig6$hour, levels = hour_levels_fig6)
elev_hour_means_fig6$hour <- factor(elev_hour_means_fig6$hour, levels = hour_levels_fig6)
pred_fig6_hour$hour <- factor(pred_fig6_hour$hour, levels = hour_levels_fig6)

p_factor_overlay <- ggplot(session_rate_fig6, aes(x = hour, y = rate, colour = elevation_n)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_line(
    data = elev_hour_means_fig6,
    aes(x = hour, y = mean_rate, colour = elevation_n, group = factor(elevation_n)),
    linewidth = 0.5,
    alpha = 0.85
  ) +
  geom_ribbon(
    data = pred_fig6_hour,
    aes(x = hour, ymin = ymin, ymax = ymax, group = 1),
    inherit.aes = FALSE,
    fill = "grey40",
    alpha = 0.2
  ) +
  geom_line(
    data = pred_fig6_hour,
    aes(x = hour, y = fit, group = 1),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.3
  ) +
  scale_colour_viridis_c(option = "viridis", direction = -1, name = "Elevation (m)") +
  labs(x = "Time of night", y = "Mean detections per photo") +
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

png("output/activity_by_hour_and_elevation_factor_overlay.png",
  width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_factor_overlay)
dev.off()

tiff("output/activity_by_hour_and_elevation_factor_overlay.tif",
  width = 12, height = 7.5, units = "in", res = 300, compression = "lzw", bg = "white")
print(p_factor_overlay)
dev.off()

##################################
##################################
####### NEW ######################
##################################
##################################

# Continuous elevation GLMM/LMM figures (site-night points; matches code/3-statistics.R)
library(glmmTMB)
library(lme4)
library(lmerTest)
options(mothbox.elevation.helpers.only = TRUE)
source("code/3-statistics.R")
options(mothbox.elevation.helpers.only = NULL)

elev_axis_breaks <- seq(200, 1400, by = 200)

plot_data_elev <- hoya_data %>%
  mutate(
    elevation_n = as.numeric(elevation),
    site = sub("_.*", "", site_night)
  ) %>%
  filter(!is.na(elevation_n), elevation_n != 1416) %>%
  left_join(photos_per_site, by = "site_night") %>%
  mutate(
    rate = insect_activity / n_photos_total,
    log_offset = log(n_photos_total)
  ) %>%
  filter(n_photos_total > 0)

save_elevation_glm_plot <- function(df_points, y_col, pred_df, ylab, outfile, show_x_axis = FALSE) {
  p <- ggplot(df_points, aes(x = elevation_n, y = .data[[y_col]], colour = elevation_n)) +
    geom_point(size = 3, alpha = 0.85) +
    scale_color_viridis(option = "viridis", direction = -1) +
    scale_x_continuous(breaks = elev_axis_breaks) +
    labs(x = if (show_x_axis) "Elevation (m)" else NULL, y = ylab) +
    base_theme_elevation() +
    theme(
      axis.title.x = if (show_x_axis) element_text(size = 30) else element_blank(),
      axis.text.x  = if (show_x_axis) element_text(size = 20) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank()
    )

  if (!is.null(pred_df) && nrow(pred_df) > 0) {
    if (isTRUE(pred_df$has_ribbon[1])) {
      p <- p +
        geom_ribbon(
          data = pred_df,
          aes(x = elevation_n, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE,
          fill = "grey40", alpha = 0.15
        )
    }
    p <- p +
      geom_line(
        data = pred_df,
        aes(x = elevation_n, y = fit),
        inherit.aes = FALSE,
        linewidth = 1.1,
        colour = "black"
      )
  }

  png(outfile, width = 12, height = 7.5, units = "in", res = 300, bg = "white")
  print(p)
  dev.off()
}

fit_rate_elev <- fit_count_glmm(plot_data_elev, "insect_activity", use_offset = TRUE)
pred_rate_elev <- predict_count_glmm(fit_rate_elev, plot_data_elev, to_rate = TRUE)
save_elevation_glm_plot(
  df_points = plot_data_elev,
  y_col = "rate",
  pred_df = pred_rate_elev,
  ylab = "Mean detections per photo",
  outfile = "output/Detections_and_elevation_continuous.png"
)

fit_rich_elev <- fit_count_glmm(plot_data_elev, "insect_richness", use_offset = TRUE)
pred_rich_elev <- predict_count_glmm(fit_rich_elev, plot_data_elev, to_rate = FALSE)
save_elevation_glm_plot(
  df_points = plot_data_elev,
  y_col = "insect_richness",
  pred_df = pred_rich_elev,
  ylab = "Richness",
  outfile = "output/Richness_and_elevation_continuous.png"
)

fit_shan_elev <- fit_shannon_lmm(plot_data_elev)
pred_shan_elev <- predict_shannon_lmm(fit_shan_elev, plot_data_elev)
save_elevation_glm_plot(
  df_points = plot_data_elev,
  y_col = "insect_shannon",
  pred_df = pred_shan_elev,
  ylab = "Shannon diversity",
  outfile = "output/Shannon_and_elevation_continuous.png",
  show_x_axis = TRUE
)

##################################
##################################
####### NEW ######################
##################################
##################################

