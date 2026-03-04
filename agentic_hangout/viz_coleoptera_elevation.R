# Coleoptera-only: abundance, richness, Shannon, Simpson vs elevation.
# Includes effort (n_photos), detections per photo, and rarefied metrics.
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
coleo <- data %>% filter(order == "Coleoptera")
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Site_night x name (morphospecies) wide matrix
dataB <- coleo %>%
  group_by(site_night, name) %>%
  summarise(freq = n(), .groups = "drop")

data_wide <- dataB %>% pivot_wider(names_from = name, values_from = freq, values_fill = 0)
data_wide <- as.data.frame(data_wide)
data_wide[is.na(data_wide)] <- 0

data_counts <- data_wide[, -1, drop = FALSE]

activity <- rowSums(data_counts)
richness <- specnumber(data_counts)
shannon  <- diversity(data_counts, index = "shannon")
simpson  <- diversity(data_counts, index = "simpson")

# Rarefaction by individuals
size_rare <- min(rowSums(data_counts))
if (size_rare < 1) size_rare <- 1
mat_rare <- rrarefy(data_counts, size_rare)
richness_rare <- specnumber(mat_rare)
shannon_rare  <- diversity(mat_rare, index = "shannon")
simpson_rare  <- diversity(mat_rare, index = "simpson")

# Append Coleoptera rarefaction info
cat("\nColeoptera: ", size_rare, " individuals per site_night (min row sum).\n",
    file = file.path(dir_out, "rarefaction_info.txt"), append = TRUE)

elev_table <- coleo %>% select(site_night, elevation) %>% distinct()
hoya_data <- data_wide[, 1, drop = FALSE] %>%
  left_join(elev_table, by = "site_night") %>%
  left_join(effort_table %>% select(site_night, n_photos), by = "site_night") %>%
  mutate(
    activity = activity,
    activity_per_photo = activity / n_photos,
    richness = richness,
    shannon  = shannon,
    simpson  = simpson,
    richness_rare = richness_rare,
    shannon_rare  = shannon_rare,
    simpson_rare  = simpson_rare
  ) %>%
  arrange(elevation)

hoya_summary <- hoya_data %>%
  group_by(elevation) %>%
  summarise(
    mean_activity = mean(activity, na.rm = TRUE),
    se_activity   = sd(activity, na.rm = TRUE) / sqrt(n()),
    mean_activity_per_photo = mean(activity_per_photo, na.rm = TRUE),
    se_activity_per_photo   = sd(activity_per_photo, na.rm = TRUE) / sqrt(n()),
    mean_richness = mean(richness, na.rm = TRUE),
    se_richness   = sd(richness, na.rm = TRUE) / sqrt(n()),
    mean_richness_rare = mean(richness_rare, na.rm = TRUE),
    se_richness_rare   = sd(richness_rare, na.rm = TRUE) / sqrt(n()),
    mean_shannon  = mean(shannon, na.rm = TRUE),
    se_shannon    = sd(shannon, na.rm = TRUE) / sqrt(n()),
    mean_shannon_rare = mean(shannon_rare, na.rm = TRUE),
    se_shannon_rare   = sd(shannon_rare, na.rm = TRUE) / sqrt(n()),
    mean_simpson  = mean(simpson, na.rm = TRUE),
    se_simpson    = sd(simpson, na.rm = TRUE) / sqrt(n()),
    mean_simpson_rare = mean(simpson_rare, na.rm = TRUE),
    se_simpson_rare   = sd(simpson_rare, na.rm = TRUE) / sqrt(n()),
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

# Format p-value for display (handle NA)
pval_annot <- function(pval) {
  label <- if (is.na(pval)) "p = NA" else if (pval < 0.001) "p < 0.001" else sprintf("p = %.3f", pval)
  list(label = label, fontface = if (!is.na(pval) && pval < 0.05) "bold" else "plain")
}

# Helper to save one elevation bar plot (with ANOVA p-value)
save_elev_plot <- function(summary_df, data_df, response_var, y_var, se_var, y_lab, filename) {
  aov_fit <- aov(as.formula(paste(response_var, "~ elevation")), data = data_df)
  pval <- summary(aov_fit)[[1]]["elevation", "Pr(>F)"]
  ann <- pval_annot(pval)
  p <- ggplot(summary_df, aes(x = factor(elevation), y = .data[[y_var]], fill = factor(elevation))) +
    geom_col() +
    geom_errorbar(
      aes(ymin = .data[[y_var]] - .data[[se_var]], ymax = .data[[y_var]] + .data[[se_var]]),
      width = 0.1, linewidth = 0.6
    ) +
    scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
    scale_x_discrete(labels = stagger_x) +
    labs(x = "Elevation (m)", y = y_lab) +
    base_theme() +
    annotate("text", x = Inf, y = Inf, label = ann$label, hjust = 1.1, vjust = 1.5, size = 6, fontface = ann$fontface)
  png(file.path(dir_out, filename), width = 12, height = 7.5, units = "in", res = 300, bg = "white")
  print(p)
  dev.off()
  message("Written: ", file.path(dir_out, filename))
}

# Raw figures (four)
save_elev_plot(hoya_summary, hoya_data, "activity", "mean_activity", "se_activity",
               "Coleoptera total detections", "Coleoptera_detections_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "richness", "mean_richness", "se_richness",
               "Coleoptera mean richness", "Coleoptera_richness_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "shannon", "mean_shannon", "se_shannon",
               "Coleoptera mean Shannon diversity", "Coleoptera_Shannon_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "simpson", "mean_simpson", "se_simpson",
               "Coleoptera mean Simpson diversity", "Coleoptera_Simpson_elevation.png")

# Effort-corrected and rarefied figures (four)
save_elev_plot(hoya_summary, hoya_data, "activity_per_photo", "mean_activity_per_photo", "se_activity_per_photo",
               "Coleoptera detections per photo", "Coleoptera_detections_per_photo_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "richness_rare", "mean_richness_rare", "se_richness_rare",
               "Coleoptera mean rarefied richness", "Coleoptera_rarefied_richness_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "shannon_rare", "mean_shannon_rare", "se_shannon_rare",
               "Coleoptera mean rarefied Shannon diversity", "Coleoptera_rarefied_Shannon_elevation.png")
save_elev_plot(hoya_summary, hoya_data, "simpson_rare", "mean_simpson_rare", "se_simpson_rare",
               "Coleoptera mean rarefied Simpson diversity", "Coleoptera_rarefied_Simpson_elevation.png")
