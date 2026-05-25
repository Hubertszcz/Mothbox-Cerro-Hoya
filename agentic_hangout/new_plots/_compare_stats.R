library(dplyr)
source("agentic_hangout/new_plots/R/elevation_model_helpers.R")

hoya <- read.csv("data_processed/hoya_data.csv")
effort <- read.csv("data_processed/session_effort.csv")
data <- read.csv("data_processed/data.csv")

session_effort_s <- effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)
hoya_elev <- hoya %>%
  mutate(elevation_n = as.numeric(elevation), site = sub("_.*", "", site_night)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)
data <- data %>%
  mutate(
    hour_int = as.integer(substr(as.character(eventTime), 1, 2)),
    site = sub("_.*", "", site_night)
  ) %>%
  filter(hour_int %in% c(19, 21, 23, 2, 4)) %>%
  filter(as.numeric(elevation) != 1416)
site_nights_ok <- unique(data$site_night)
session_effort_s <- session_effort_s %>% filter(site_night %in% site_nights_ok)
photos_stats <- session_effort_s %>%
  group_by(site_night) %>%
  summarise(n_photos_total = sum(n_photos), .groups = "drop")

session_effort_p <- effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)
photos_plot <- session_effort_p %>%
  group_by(site_night) %>%
  summarise(n_photos_total = sum(n_photos), .groups = "drop")

cat("site_nights hoya_elev:", nrow(hoya_elev), "\n")
cat("site_nights stats effort:", nrow(photos_stats), "\n")
cat("site_nights plot effort:", nrow(photos_plot), "\n")
only_plot <- setdiff(photos_plot$site_night, photos_stats$site_night)
only_stats <- setdiff(photos_stats$site_night, photos_plot$site_night)
cat("Only in plot (not stats):", paste(only_plot, collapse = ", "), "\n")
cat("Only in stats (not plot):", paste(only_stats, collapse = ", "), "\n")
diff_n <- inner_join(photos_stats, photos_plot, by = "site_night", suffix = c("_stats", "_plot")) %>%
  filter(n_photos_total_stats != n_photos_total_plot)
cat("Different photo totals:", nrow(diff_n), "\n")
if (nrow(diff_n) > 0) print(diff_n)

plot_data <- hoya_elev %>%
  left_join(photos_plot, by = "site_night") %>%
  mutate(rate = insect_activity / n_photos_total, log_offset = log(n_photos_total)) %>%
  filter(n_photos_total > 0)
fig1_stats <- hoya_elev %>%
  select(site_night, site, elevation_n, insect_activity) %>%
  left_join(photos_stats, by = "site_night") %>%
  mutate(log_offset = log(n_photos_total)) %>%
  filter(n_photos_total > 0)
hoya_glmm_stats <- hoya_elev %>%
  left_join(photos_stats, by = "site_night") %>%
  mutate(log_offset = log(n_photos_total)) %>%
  filter(n_photos_total > 0)

cat("\nRows plot vs stats:", nrow(plot_data), nrow(fig1_stats), "\n")
f1p <- fit_count_glmm(plot_data, "insect_activity")
f1s <- fit_count_glmm(fig1_stats, "insect_activity")
cat("Activity family:", f1p$family, "/", f1s$family, "\n")
cat("Activity elev est:", coef(summary(f1p$model))$cond["elevation_n", "Estimate"],
    coef(summary(f1s$model))$cond["elevation_n", "Estimate"], "\n")
cat("Activity elev p:", coef(summary(f1p$model))$cond["elevation_n", "Pr(>|z|)"],
    coef(summary(f1s$model))$cond["elevation_n", "Pr(>|z|)"], "\n")
