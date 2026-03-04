# Coleoptera by family: abundance, richness, Shannon vs elevation.
# Filters to order == "Coleoptera", keeps top families by detections (min detections
# and max number of families), computes per-family metrics by site_night and elevation,
# plots three faceted figures. Outputs to agentic_hangout/output/.

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

# Treat NA/blank family as "Unknown"
coleo <- coleo %>%
  mutate(family = if_else(is.na(family) | trimws(as.character(family)) == "", "Unknown", as.character(family)))

# Family totals and filter: at least 30 detections, then top 10 families
family_totals <- coleo %>% count(family, name = "total_detections")
families_keep <- family_totals %>%
  filter(total_detections >= 30) %>%
  slice_max(total_detections, n = 10, with_ties = FALSE) %>%
  pull(family)

coleo_fam <- coleo %>% filter(family %in% families_keep)
if (length(families_keep) == 0) stop("No Coleoptera families with >= 30 detections.")

# Per-family: site_night x name matrix, then metrics
summarise_family <- function(fam_df) {
  dataB <- fam_df %>%
    group_by(site_night, name) %>%
    summarise(freq = n(), .groups = "drop")
  data_wide <- dataB %>% pivot_wider(names_from = name, values_from = freq, values_fill = 0)
  data_wide <- as.data.frame(data_wide)
  data_wide[is.na(data_wide)] <- 0
  data_counts <- data_wide[, -1, drop = FALSE]
  elev_table <- fam_df %>% select(site_night, elevation) %>% distinct()
  tibble(
    site_night = data_wide[[1]],
    activity  = rowSums(data_counts),
    richness  = specnumber(data_counts),
    shannon   = diversity(data_counts, index = "shannon")
  ) %>%
    left_join(elev_table, by = "site_night")
}

# Site_night-level data per family (for ANOVA); add effort (n_photos) per site_night
family_site_data <- coleo_fam %>%
  group_by(family) %>%
  group_modify(~ summarise_family(.x)) %>%
  ungroup() %>%
  left_join(effort_table %>% select(site_night, n_photos), by = "site_night")

# Summary by elevation for each family (mean and SE)
family_summaries <- family_site_data %>%
  group_by(family) %>%
  group_modify(~ {
    .x %>%
      group_by(elevation) %>%
      summarise(
        mean_activity = mean(activity, na.rm = TRUE),
        se_activity   = sd(activity, na.rm = TRUE) / sqrt(n()),
        mean_richness = mean(richness, na.rm = TRUE),
        se_richness   = sd(richness, na.rm = TRUE) / sqrt(n()),
        mean_shannon  = mean(shannon, na.rm = TRUE),
        se_shannon    = sd(shannon, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
  }) %>%
  ungroup()

# ANOVA p-value per family per metric (for plot annotations)
pval_annot <- function(pval) {
  label <- if (is.na(pval)) "p = NA" else if (pval < 0.001) "p < 0.001" else sprintf("p = %.3f", pval)
  list(label = label, fontface = if (!is.na(pval) && pval < 0.05) "bold" else "plain")
}
pval_by_family <- function(metric) {
  fams <- unique(family_site_data$family)
  tibble(
    family = fams,
    pval = vapply(fams, function(f) {
      d <- filter(family_site_data, family == f)
      m <- tryCatch(aov(as.formula(paste(metric, "~ elevation")), data = d), error = function(e) NULL)
      if (is.null(m)) NA_real_ else summary(m)[[1]]["elevation", "Pr(>F)"]
    }, FUN.VALUE = numeric(1))
  ) %>%
    rowwise() %>%
    mutate(
      label = pval_annot(pval)$label,
      fontface = pval_annot(pval)$fontface
    ) %>%
    ungroup() %>%
    select(family, label, fontface)
}
pval_activity <- pval_by_family("activity")
pval_richness <- pval_by_family("richness")
pval_shannon  <- pval_by_family("shannon")

# Shared plot style; faceted so stagger_x applied per panel is tricky — use default labels or simple stagger
base_theme <- function() {
  theme_minimal(base_family = "Arial", base_size = 18) +
    theme(
      axis.title = element_text(size = 30),
      axis.text  = element_text(size = 20),
      legend.position = "none",
      strip.text = element_text(size = 14)
    )
}

# Faceted plot helper: one figure per metric (with per-panel ANOVA p-value)
save_family_plot <- function(summary_df, pval_df, y_var, se_var, y_lab, filename) {
  p <- ggplot(summary_df, aes(x = factor(elevation), y = .data[[y_var]], fill = factor(elevation))) +
    geom_col() +
    geom_errorbar(
      aes(ymin = .data[[y_var]] - .data[[se_var]], ymax = .data[[y_var]] + .data[[se_var]]),
      width = 0.1, linewidth = 0.6
    ) +
    scale_fill_viridis(discrete = TRUE, option = "viridis", direction = -1) +
    labs(x = "Elevation (m)", y = y_lab) +
    facet_wrap(~ family, scales = "free_y", ncol = 2) +
    base_theme() +
    geom_text(
      data = pval_df, aes(x = Inf, y = Inf, label = label, fontface = fontface),
      hjust = 1.1, vjust = 1.5, size = 5, inherit.aes = FALSE
    )
  # Larger figure to accommodate facets
  png(file.path(dir_out, filename), width = 14, height = 10, units = "in", res = 300, bg = "white")
  print(p)
  dev.off()
  message("Written: ", file.path(dir_out, filename))
}

save_family_plot(family_summaries, pval_activity, "mean_activity", "se_activity",
                 "Mean detections", "Coleoptera_family_abundance_elevation.png")
save_family_plot(family_summaries, pval_richness, "mean_richness", "se_richness",
                 "Mean richness", "Coleoptera_family_richness_elevation.png")
save_family_plot(family_summaries, pval_shannon, "mean_shannon", "se_shannon",
                 "Mean Shannon diversity", "Coleoptera_family_Shannon_elevation.png")
