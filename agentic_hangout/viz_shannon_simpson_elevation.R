# Shannon and Simpson diversity vs elevation (all insects).
# Includes effort (n_photos), activity per photo, and rarefied richness/diversity.
# Reads data_processed/data.csv and agentic_hangout/output/site_night_effort.csv.
# Outputs to agentic_hangout/output/.

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
effort_table <- read.csv("agentic_hangout/output/site_night_effort.csv")
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Replicate analysis flow from code/1-data_analysis.R
dataB <- data %>%
  group_by(site_night, name) %>%
  summarise(freq = n(), .groups = "drop")

data_wide <- dataB %>% pivot_wider(names_from = name, values_from = freq, values_fill = 0)
data_wide <- as.data.frame(data_wide)
data_wide[is.na(data_wide)] <- 0

data_counts <- data_wide[, -1, drop = FALSE]

insect_activity <- rowSums(data_counts)
insect_shannon <- diversity(data_counts, index = "shannon")
insect_simpson <- diversity(data_counts, index = "simpson")

# Rarefaction by individuals (common size = min row sum)
size_rare <- min(rowSums(data_counts))
if (size_rare < 1) size_rare <- 1
mat_rare <- rrarefy(data_counts, size_rare)
insect_richness_rare <- specnumber(mat_rare)
insect_shannon_rare <- diversity(mat_rare, index = "shannon")
insect_simpson_rare <- diversity(mat_rare, index = "simpson")

# Write rarefaction target for reproducibility
writeLines(
  sprintf("Rarefaction (all insects): %d individuals per site_night (min row sum).", size_rare),
  file.path(dir_out, "rarefaction_info.txt")
)

elev_table <- data %>% select(site_night, elevation) %>% distinct()
hoya_data <- data_wide[, 1, drop = FALSE] %>%
  left_join(elev_table, by = "site_night") %>%
  left_join(effort_table %>% select(site_night, n_photos), by = "site_night") %>%
  mutate(
    insect_activity = insect_activity,
    activity_per_photo = insect_activity / n_photos,
    insect_shannon = insect_shannon,
    insect_simpson = insect_simpson,
    insect_richness_rare = insect_richness_rare,
    insect_shannon_rare = insect_shannon_rare,
    insect_simpson_rare = insect_simpson_rare
  ) %>%
  arrange(elevation)

hoya_summary <- hoya_data %>%
  group_by(elevation) %>%
  summarise(
    mean_activity_per_photo = mean(activity_per_photo, na.rm = TRUE),
    se_activity_per_photo   = sd(activity_per_photo, na.rm = TRUE) / sqrt(n()),
    mean_richness_rare = mean(insect_richness_rare, na.rm = TRUE),
    se_richness_rare   = sd(insect_richness_rare, na.rm = TRUE) / sqrt(n()),
    mean_shannon = mean(insect_shannon, na.rm = TRUE),
    se_shannon   = sd(insect_shannon, na.rm = TRUE) / sqrt(n()),
    mean_simpson = mean(insect_simpson, na.rm = TRUE),
    se_simpson   = sd(insect_simpson, na.rm = TRUE) / sqrt(n()),
    mean_shannon_rare = mean(insect_shannon_rare, na.rm = TRUE),
    se_shannon_rare   = sd(insect_shannon_rare, na.rm = TRUE) / sqrt(n()),
    mean_simpson_rare = mean(insect_simpson_rare, na.rm = TRUE),
    se_simpson_rare   = sd(insect_simpson_rare, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Shared plot style (matches code/2-visualization.R)
base_theme <- function() {
  theme_minimal(base_family = "Arial", base_size = 18) +
    theme(
      axis.title = element_text(size = 30),
      axis.text  = element_text(size = 20),
      legend.position = "none"
    )
}
stagger_x <- function(x) ifelse(seq_along(x) %% 2 == 0, paste0("\n\n", x), x)

# Format p-value for display; return list(label, fontface)
pval_annot <- function(pval) {
  label <- if (pval < 0.001) "p < 0.001" else sprintf("p = %.3f", pval)
  list(label = label, fontface = if (pval < 0.05) "bold" else "plain")
}

# ANOVA: elevation effect
aov_shannon <- summary(aov(insect_shannon ~ elevation, data = hoya_data))[[1]]
aov_simpson <- summary(aov(insect_simpson ~ elevation, data = hoya_data))[[1]]
p_shannon_val <- aov_shannon["elevation", "Pr(>F)"]
p_simpson_val <- aov_simpson["elevation", "Pr(>F)"]
ann_shannon <- pval_annot(p_shannon_val)
ann_simpson <- pval_annot(p_simpson_val)

# Figure: Activity per photo vs elevation
aov_rate <- summary(aov(activity_per_photo ~ elevation, data = hoya_data))[[1]]
ann_rate <- pval_annot(aov_rate["elevation", "Pr(>F)"])
p_rate <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_activity_per_photo, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_activity_per_photo - se_activity_per_photo, ymax = mean_activity_per_photo + se_activity_per_photo), width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Detections per photo") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_rate$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_rate$fontface)
png(file.path(dir_out, "Activity_per_photo_and_elevation.png"), width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_rate)
dev.off()
message("Written: ", file.path(dir_out, "Activity_per_photo_and_elevation.png"))

# Figure: Rarefied richness vs elevation
aov_rare_rich <- summary(aov(insect_richness_rare ~ elevation, data = hoya_data))[[1]]
ann_rare_rich <- pval_annot(aov_rare_rich["elevation", "Pr(>F)"])
p_rare_rich <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_richness_rare, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_richness_rare - se_richness_rare, ymax = mean_richness_rare + se_richness_rare), width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Mean rarefied richness") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_rare_rich$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_rare_rich$fontface)
png(file.path(dir_out, "Rarefied_richness_and_elevation.png"), width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_rare_rich)
dev.off()
message("Written: ", file.path(dir_out, "Rarefied_richness_and_elevation.png"))

# Figure 1: Shannon diversity vs elevation
p_shannon <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_shannon, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_shannon - se_shannon, ymax = mean_shannon + se_shannon),
    width = 0.1, linewidth = 0.6
  ) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Mean Shannon diversity") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_shannon$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_shannon$fontface)

png(file.path(dir_out, "Shannon and elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_shannon)
dev.off()
message("Written: ", file.path(dir_out, "Shannon and elevation.png"))

# Figure 2: Simpson diversity vs elevation
p_simpson <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_simpson, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_simpson - se_simpson, ymax = mean_simpson + se_simpson),
    width = 0.1, linewidth = 0.6
  ) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Mean Simpson diversity") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_simpson$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_simpson$fontface)

png(file.path(dir_out, "Simpson and elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_simpson)
dev.off()
message("Written: ", file.path(dir_out, "Simpson and elevation.png"))

# Figure: Rarefied Shannon vs elevation
aov_shannon_rare <- summary(aov(insect_shannon_rare ~ elevation, data = hoya_data))[[1]]
ann_shannon_rare <- pval_annot(aov_shannon_rare["elevation", "Pr(>F)"])
p_shannon_rare <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_shannon_rare, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_shannon_rare - se_shannon_rare, ymax = mean_shannon_rare + se_shannon_rare), width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Mean rarefied Shannon diversity") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_shannon_rare$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_shannon_rare$fontface)
png(file.path(dir_out, "Rarefied_Shannon_and_elevation.png"), width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_shannon_rare)
dev.off()
message("Written: ", file.path(dir_out, "Rarefied_Shannon_and_elevation.png"))

# Figure: Rarefied Simpson vs elevation
aov_simpson_rare <- summary(aov(insect_simpson_rare ~ elevation, data = hoya_data))[[1]]
ann_simpson_rare <- pval_annot(aov_simpson_rare["elevation", "Pr(>F)"])
p_simpson_rare <- ggplot(hoya_summary, aes(x = factor(elevation), y = mean_simpson_rare, fill = factor(elevation))) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_simpson_rare - se_simpson_rare, ymax = mean_simpson_rare + se_simpson_rare), width = 0.1, linewidth = 0.6) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  scale_x_discrete(labels = stagger_x) +
  labs(x = "Elevation (m)", y = "Mean rarefied Simpson diversity") +
  base_theme() +
  annotate("text", x = Inf, y = Inf, label = ann_simpson_rare$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann_simpson_rare$fontface)
png(file.path(dir_out, "Rarefied_Simpson_and_elevation.png"), width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_simpson_rare)
dev.off()
message("Written: ", file.path(dir_out, "Rarefied_Simpson_and_elevation.png"))
