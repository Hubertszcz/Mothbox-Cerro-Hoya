# Ordination of Coleoptera community vs elevation: NMDS (Bray-Curtis) and PERMANOVA.
# Includes raw and rarefied (by individuals) ordination; effort (n_photos) joined for PERMANOVA.
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

# Site_night x name (morphospecies) abundance matrix
dataB <- coleo %>%
  group_by(site_night, name) %>%
  summarise(freq = n(), .groups = "drop")

data_wide <- dataB %>% pivot_wider(names_from = name, values_from = freq, values_fill = 0)
data_wide <- as.data.frame(data_wide)
data_wide[is.na(data_wide)] <- 0

site_nights <- data_wide[, 1, drop = TRUE]
mat <- as.matrix(data_wide[, -1, drop = FALSE])
rownames(mat) <- site_nights

# Elevation and effort per site_night (row order must match mat for PERMANOVA)
elev_table <- coleo %>% select(site_night, elevation) %>% distinct()
elev_for_perm <- data.frame(site_night = rownames(mat)) %>%
  left_join(elev_table, by = "site_night") %>%
  left_join(effort_table %>% select(site_night, n_photos), by = "site_night")

# NMDS (Bray-Curtis) on raw matrix; try to get 2 dimensions
set.seed(42)
nmds <- metaMDS(mat, distance = "bray", k = 2, trymax = 100)

# Site scores and elevation for plotting
scores_sites <- as.data.frame(scores(nmds, display = "sites"))
scores_sites$site_night <- rownames(scores_sites)
scores_sites <- scores_sites %>%
  left_join(elev_table, by = "site_night")

# PERMANOVA: elevation effect on community composition (rows of elev_for_perm match mat)
perm <- adonis2(mat ~ elevation, data = elev_for_perm, permutations = 999, method = "bray")
perm_tab <- as.data.frame(perm)
r2   <- perm_tab[1, "R2"]
pval <- perm_tab[1, "Pr(>F)"]
perm_df <- data.frame(R2 = r2, p_value = pval)

# Save site scores and PERMANOVA result
write.csv(scores_sites, file.path(dir_out, "Coleoptera_NMDS_site_scores.csv"), row.names = FALSE)
write.csv(perm_df, file.path(dir_out, "Coleoptera_PERMANOVA_elevation.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "Coleoptera_NMDS_site_scores.csv"))
message("Written: ", file.path(dir_out, "Coleoptera_PERMANOVA_elevation.csv"))

# Ordination plot: points colored by elevation (continuous viridis)
p <- ggplot(scores_sites, aes(x = NMDS1, y = NMDS2, color = elevation)) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_viridis(option = "viridis", direction = -1) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Elevation (m)",
    title = "Coleoptera community ordination (Bray-Curtis NMDS)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(
    axis.title = element_text(size = 30),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 22, hjust = 0.5)
  ) +
  coord_fixed()

# Add PERMANOVA annotation (bold if p < 0.05)
pval_label <- if (perm_df$p_value < 0.001) "p < 0.001" else sprintf("p = %.3f", perm_df$p_value)
perm_label <- sprintf("PERMANOVA (elevation): R² = %.3f, %s", perm_df$R2, pval_label)
p <- p + annotate(
  "text",
  x = min(scores_sites$NMDS1),
  y = max(scores_sites$NMDS2),
  hjust = 0, vjust = 1, size = 5,
  label = perm_label,
  fontface = if (perm_df$p_value < 0.05) "bold" else "plain"
)

png(file.path(dir_out, "Coleoptera_NMDS_elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p)
dev.off()
message("Written: ", file.path(dir_out, "Coleoptera_NMDS_elevation.png"))

# Rarefied matrix and ordination (rarefy to min row sum)
size_rare <- min(rowSums(mat))
if (size_rare < 1) size_rare <- 1
mat_rare <- rrarefy(mat, size_rare)
set.seed(42)
nmds_rare <- metaMDS(mat_rare, distance = "bray", k = 2, trymax = 100)

scores_rare <- as.data.frame(scores(nmds_rare, display = "sites"))
scores_rare$site_night <- rownames(scores_rare)
scores_rare <- scores_rare %>%
  left_join(elev_table, by = "site_night")

perm_rare <- adonis2(mat_rare ~ elevation, data = elev_for_perm, permutations = 999, method = "bray")
perm_rare_tab <- as.data.frame(perm_rare)
perm_rare_df <- data.frame(R2 = perm_rare_tab[1, "R2"], p_value = perm_rare_tab[1, "Pr(>F)"])
write.csv(perm_rare_df, file.path(dir_out, "Coleoptera_PERMANOVA_rarefied_elevation.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "Coleoptera_PERMANOVA_rarefied_elevation.csv"))

p_rare <- ggplot(scores_rare, aes(x = NMDS1, y = NMDS2, color = elevation)) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_viridis(option = "viridis", direction = -1) +
  labs(
    x = "NMDS1", y = "NMDS2", color = "Elevation (m)",
    title = "Coleoptera community ordination (rarefied, Bray-Curtis NMDS)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 18) +
  theme(
    axis.title = element_text(size = 30), axis.text = element_text(size = 20),
    legend.title = element_text(size = 18), legend.text = element_text(size = 14),
    plot.title = element_text(size = 22, hjust = 0.5)
  ) +
  coord_fixed()

pval_label_rare <- if (perm_rare_df$p_value < 0.001) "p < 0.001" else sprintf("p = %.3f", perm_rare_df$p_value)
p_rare <- p_rare + annotate(
  "text", x = min(scores_rare$NMDS1), y = max(scores_rare$NMDS2),
  hjust = 0, vjust = 1, size = 5, label = sprintf("PERMANOVA (elevation): R² = %.3f, %s", perm_rare_df$R2, pval_label_rare),
  fontface = if (perm_rare_df$p_value < 0.05) "bold" else "plain"
)

png(file.path(dir_out, "Coleoptera_NMDS_rarefied_elevation.png"),
    width = 12, height = 7.5, units = "in", res = 300, bg = "white")
print(p_rare)
dev.off()
message("Written: ", file.path(dir_out, "Coleoptera_NMDS_rarefied_elevation.png"))

# Optional: PERMANOVA with effort as covariate on raw matrix
perm_effort <- adonis2(mat ~ elevation + n_photos, data = elev_for_perm, permutations = 999, method = "bray")
perm_effort_tab <- as.data.frame(perm_effort)
write.csv(perm_effort_tab, file.path(dir_out, "Coleoptera_PERMANOVA_elevation_and_effort.csv"), row.names = TRUE)
message("Written: ", file.path(dir_out, "Coleoptera_PERMANOVA_elevation_and_effort.csv"))
