# Fig 3a-style: Layout like paper Fig 3A — rows = elevation band (highest on top), columns = order panel.
# Each cell: Y = mean detections per photo, X = session (time of night). Top-left = All orders, >1000 m; next column = Lepidoptera for >1000 m; etc.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv. Run prep_time_of_night.R first. Outputs to agentic_hangout/output/.

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Elevation band boundaries (m)
elev_breaks <- c(0, 300, 600, 900, 1300, 2000)
elev_labels <- c("<300 m", "300–600 m", "600–900 m", "900–1300 m", ">1300 m")

# Input and output
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

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

# Save summary CSV
write.csv(summary_df, file.path(dir_out, "activity_by_session_elevation_bands_summary.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "activity_by_session_elevation_bands_summary.csv"))

# Panels to show (exclude Hymenoptera and Other orders)
plot_panels <- c("All orders", "Lepidoptera", "Coleoptera", "Hemiptera", "Diptera")
summary_plot <- summary_df %>% filter(panel %in% plot_panels)

# Error bars = standard error of the mean (SEM): sd(rate) / sqrt(n_sessions) for that (panel, hour, elevation_band).
# Plot: rows = elevation_band (cooler viridis at top, warmer at bottom), columns = panel. Bars colored by elevation band.
p <- ggplot(summary_plot, aes(x = hour, y = mean_rate, fill = elevation_band)) +
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
    x = "Session (time of night)",
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

png(
  file.path(dir_out, "activity_by_session_and_elevation_bands.png"),
  width = 10,
  height = 9.5,
  units = "in",
  res = 300,
  bg = "white"
)
print(p)
dev.off()
message("Written: ", file.path(dir_out, "activity_by_session_and_elevation_bands.png"))
