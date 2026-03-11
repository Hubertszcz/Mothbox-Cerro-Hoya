# Identify Coleoptera families (and genera) that drive early-night activity.
# Session-level rate by family/genus; LMM p-value for hour; early % and peak hour; "driver" flag.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# All outputs to agentic_hangout/output/time_of_night/.

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

# Restrict to Coleoptera and programA hours
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(order == "Coleoptera", hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Standardise family/genus: NA or blank -> "Unknown"
data <- data %>%
  mutate(
    family = if_else(is.na(family) | trimws(as.character(family)) == "", "Unknown", as.character(family)),
    genus  = if_else(is.na(genus)  | trimws(as.character(genus)) == "", "Unknown", as.character(genus))
  )

sessions <- session_effort %>%
  select(site_night, hour, n_photos) %>%
  mutate(site = sub("_.*", "", site_night))

# ---- By family ----
# Observed detections per (site_night, hour, family)
fam_obs <- data %>%
  group_by(site_night, hour, family) %>%
  summarise(detections = n(), .groups = "drop")
families_vec <- unique(fam_obs$family)
session_fam_full <- tidyr::expand_grid(
  sessions %>% distinct(site_night, hour, n_photos, site),
  family = families_vec
)
session_fam_rate <- session_fam_full %>%
  left_join(fam_obs, by = c("site_night", "hour", "family")) %>%
  mutate(detections = replace_na(detections, 0L), rate = detections / n_photos)

# LMM p-value for hour (same as order script)
fit_hour <- function(df) {
  if (length(unique(df$hour)) < 2) return(NA_real_)
  if (var(df$rate, na.rm = TRUE) == 0) return(NA_real_)
  m_full <- tryCatch(
    lmer(rate ~ hour + (1 | site) + (1 | site_night), data = df, REML = TRUE),
    error = function(e) NULL
  )
  if (is.null(m_full)) return(NA_real_)
  m_null <- tryCatch(
    lmer(rate ~ (1 | site) + (1 | site_night), data = df, REML = TRUE),
    error = function(e) NULL
  )
  if (is.null(m_null)) return(NA_real_)
  a <- anova(m_null, m_full)
  a[2, "Pr(>Chisq)"]
}

# Early = 19h + 21h; late = 23h, 2h, 4h. Proportion of detections in early hours.
early_hours <- c("19h", "21h")
family_totals <- data %>% group_by(family) %>% summarise(total_detections = n(), .groups = "drop")
family_early <- family_totals %>%
  left_join(
    data %>% filter(hour %in% early_hours) %>%
      group_by(family) %>% summarise(early_detections = n(), .groups = "drop"),
    by = "family"
  ) %>%
  mutate(early_detections = replace_na(early_detections, 0L), early_pct = 100 * early_detections / total_detections)

# Mean rate per family per hour (for peak hour)
fam_hour_means <- session_fam_rate %>%
  group_by(family, hour) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop")
peak_hour <- fam_hour_means %>%
  group_by(family) %>%
  slice_max(mean_rate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(family, peak_hour = hour)

# Fit LMM per family
fam_list <- unique(session_fam_rate$family)
fam_p <- setNames(rep(NA_real_, length(fam_list)), fam_list)
for (f in fam_list) {
  fam_p[f] <- fit_hour(session_fam_rate %>% filter(family == f))
}

family_results <- family_totals %>%
  left_join(family_early %>% select(family, early_pct), by = "family") %>%
  left_join(peak_hour, by = "family") %>%
  mutate(
    p_value_hour = unname(fam_p[family]),
    significant = !is.na(p_value_hour) & p_value_hour < 0.05,
    early_driver = significant & (peak_hour %in% early_hours | (early_pct >= 50))
  ) %>%
  arrange(desc(total_detections))

write.csv(
  family_results %>% select(family, total_detections, p_value_hour, peak_hour, early_pct, early_driver),
  file.path(dir_out, "Coleoptera_family_early_drivers.csv"),
  row.names = FALSE
)
message("Written: ", file.path(dir_out, "Coleoptera_family_early_drivers.csv"))

# ---- By genus (top genera by detections) ----
gen_obs <- data %>%
  group_by(site_night, hour, genus) %>%
  summarise(detections = n(), .groups = "drop")
genus_totals <- data %>% group_by(genus) %>% summarise(total_detections = n(), .groups = "drop")
# Keep genera with at least 20 detections for LMM
genus_keep <- genus_totals %>% filter(total_detections >= 20) %>% pull(genus)
gen_obs <- gen_obs %>% filter(genus %in% genus_keep)
if (length(genus_keep) > 0) {
  session_gen_full <- tidyr::expand_grid(
    sessions %>% distinct(site_night, hour, n_photos, site),
    genus = genus_keep
  )
  session_gen_rate <- session_gen_full %>%
    left_join(gen_obs, by = c("site_night", "hour", "genus")) %>%
    mutate(detections = replace_na(detections, 0L), rate = detections / n_photos)

  genus_early <- data %>% filter(genus %in% genus_keep) %>%
    group_by(genus) %>% summarise(total_detections = n(), .groups = "drop") %>%
    left_join(
      data %>% filter(genus %in% genus_keep, hour %in% early_hours) %>%
        group_by(genus) %>% summarise(early_detections = n(), .groups = "drop"),
      by = "genus"
    ) %>%
    mutate(early_detections = replace_na(early_detections, 0L), early_pct = 100 * early_detections / total_detections)

  gen_hour_means <- session_gen_rate %>%
    group_by(genus, hour) %>%
    summarise(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop")
  gen_peak <- gen_hour_means %>%
    group_by(genus) %>%
    slice_max(mean_rate, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(genus, peak_hour = hour)

  gen_p <- setNames(rep(NA_real_, length(genus_keep)), genus_keep)
  for (g in genus_keep) {
    gen_p[g] <- fit_hour(session_gen_rate %>% filter(genus == g))
  }
  genus_results <- genus_totals %>% filter(genus %in% genus_keep) %>%
    left_join(genus_early %>% select(genus, early_pct), by = "genus") %>%
    left_join(gen_peak, by = "genus") %>%
    mutate(
      p_value_hour = unname(gen_p[genus]),
      significant = !is.na(p_value_hour) & p_value_hour < 0.05,
      early_driver = significant & (peak_hour %in% early_hours | (early_pct >= 50))
    ) %>%
    arrange(desc(total_detections))
  write.csv(
    genus_results %>% select(genus, total_detections, p_value_hour, peak_hour, early_pct, early_driver),
    file.path(dir_out, "Coleoptera_genus_early_drivers.csv"),
    row.names = FALSE
  )
  message("Written: ", file.path(dir_out, "Coleoptera_genus_early_drivers.csv"))
}

# ---- Optional: family-by-hour figure with driver annotation ----
family_hour_summary <- session_fam_rate %>%
  group_by(family, hour) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
  left_join(family_results %>% select(family, early_driver), by = "family")
p_fam <- ggplot(family_hour_summary, aes(x = hour, y = mean_rate, fill = family)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate),
                position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = "Time of night", y = "Detections per photo", fill = "Family",
       title = "Coleoptera activity by family and hour (early drivers: significant & peak in 19h/21h)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "right")
png(file.path(dir_out, "Coleoptera_activity_by_family_and_hour_drivers.png"), width = 12, height = 6, units = "in", res = 300, bg = "white")
print(p_fam)
dev.off()
message("Written: ", file.path(dir_out, "Coleoptera_activity_by_family_and_hour_drivers.png"))
