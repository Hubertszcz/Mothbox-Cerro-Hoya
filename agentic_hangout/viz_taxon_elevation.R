# Taxon-elevation visualizations: ordination biplots (A + B), RDA, heatmap, response curves, slopes.
# Reads data_processed/data.csv and agentic_hangout/output/site_night_effort.csv.
# All outputs to agentic_hangout/output/taxon_elevation/.

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
effort <- read.csv("agentic_hangout/output/site_night_effort.csv")
dir_out <- "agentic_hangout/output/taxon_elevation"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Use only rows with order
data <- data %>% filter(!is.na(order) & trimws(as.character(order)) != "")

# Site_night x order count matrix
order_counts <- data %>%
  group_by(site_night, order) %>%
  summarise(freq = n(), .groups = "drop")
mat_orders <- order_counts %>%
  pivot_wider(names_from = order, values_from = freq, values_fill = 0) %>%
  as.data.frame()
rownames(mat_orders) <- mat_orders$site_night
mat_orders <- as.matrix(mat_orders[, -1])

# Elevation and n_photos per site_night (match row order)
elev <- data %>% select(site_night, elevation) %>% distinct()
effort_site <- effort %>% select(site_night, n_photos)
site_info <- data.frame(site_night = rownames(mat_orders)) %>%
  left_join(elev, by = "site_night") %>%
  left_join(effort_site, by = "site_night")
# Rate matrix (detections per photo per site per order)
n_photos_vec <- site_info$n_photos
mat_rates <- sweep(mat_orders, 1, n_photos_vec, "/")
mat_rates[!is.finite(mat_rates)] <- 0

# ---------- 1A: Ordination biplot with species scores ----------
set.seed(42)
ord <- metaMDS(mat_orders, distance = "bray", k = 2, trymax = 80)
sites <- as.data.frame(scores(ord, display = "sites"))
sites$site_night <- rownames(sites)
sites <- sites %>% left_join(site_info %>% select(site_night, elevation), by = "site_night")
spp <- as.data.frame(scores(ord, display = "species"))
spp$order <- rownames(spp)
# Scale species for visible arrows
mul <- 0.8 * max(abs(sites[, c("NMDS1", "NMDS2")])) / max(sqrt(spp$NMDS1^2 + spp$NMDS2^2), 1e-6)
spp_scaled <- spp %>% mutate(NMDS1 = NMDS1 * mul, NMDS2 = NMDS2 * mul)

p1a <- ggplot() +
  geom_point(data = sites, aes(x = NMDS1, y = NMDS2, colour = elevation), size = 3, alpha = 0.9) +
  scale_color_viridis_c(option = "viridis", name = "Elevation (m)") +
  geom_segment(data = spp_scaled, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.6, colour = "darkred") +
  geom_text(data = spp_scaled, aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = order), size = 3.5, hjust = 0) +
  labs(title = "Ordination biplot (Option A: species scores)", x = "NMDS1", y = "NMDS2") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_fixed()
png(file.path(dir_out, "ordination_biplot_species_scores.png"), width = 10, height = 8, units = "in", res = 300, bg = "white")
print(p1a)
dev.off()
message("Written: ordination_biplot_species_scores.png")

# ---------- 1B: Ordination with envfit vectors (elevation + taxa) ----------
env_df <- data.frame(elevation = site_info$elevation)
env_df <- cbind(env_df, as.data.frame(mat_orders))
ef <- envfit(ord, env_df, permutations = 999)
sites$elevation <- site_info$elevation
p1b <- ggplot(sites, aes(x = NMDS1, y = NMDS2, colour = elevation)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_viridis_c(option = "viridis", name = "Elevation (m)") +
  labs(title = "Ordination biplot (Option B: envfit vectors)", x = "NMDS1", y = "NMDS2") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_fixed()
# Add envfit arrows in base then save; vegan ordiplot adds to existing plot. Use ggvegan or manual arrows.
# Extract envfit scores for arrows
ef_scores <- scores(ef, display = "vectors")
if (!is.null(ef_scores)) {
  ef_df <- as.data.frame(ef_scores)
  ef_df$var <- rownames(ef_df)
  arr_scale <- 0.5 * max(abs(sites[, c("NMDS1", "NMDS2")])) / max(sqrt(ef_df[,1]^2 + ef_df[,2]^2), 1e-6)
  ef_df[, 1] <- ef_df[, 1] * arr_scale
  ef_df[, 2] <- ef_df[, 2] * arr_scale
  names(ef_df)[1:2] <- c("NMDS1", "NMDS2")
  p1b <- p1b +
    geom_segment(data = ef_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.15, "cm")), linewidth = 0.5, colour = "black", inherit.aes = FALSE) +
    geom_text(data = ef_df, aes(x = NMDS1 * 1.15, y = NMDS2 * 1.15, label = var), size = 3, hjust = 0, inherit.aes = FALSE)
}
png(file.path(dir_out, "ordination_biplot_envfit_vectors.png"), width = 10, height = 8, units = "in", res = 300, bg = "white")
print(p1b)
dev.off()
message("Written: ordination_biplot_envfit_vectors.png")

# ---------- 2: RDA with elevation ----------
site_info <- site_info %>% filter(complete.cases(.))
mat_ord_ok <- mat_orders[rownames(mat_orders) %in% site_info$site_night, , drop = FALSE]
mat_ord_ok <- mat_ord_ok[order(rownames(mat_ord_ok)), ]
elev_ok <- site_info %>% arrange(site_night) %>% pull(elevation)
rda_fit <- rda(mat_ord_ok ~ elev_ok)
rda_sites <- as.data.frame(scores(rda_fit, display = "sites"))
rda_sites$site_night <- rownames(rda_sites)
rda_sites$elevation <- elev_ok
rda_spp <- as.data.frame(scores(rda_fit, display = "species"))
rda_spp$order <- rownames(rda_spp)
# RDA with one constraint has RDA1; second axis may be PC1
ax1 <- names(rda_spp)[1]
ax2 <- names(rda_spp)[2]
sc <- 0.4 * max(abs(rda_sites[, 1:2])) / max(sqrt(rda_spp[[ax1]]^2 + rda_spp[[ax2]]^2), 1e-6)
rda_spp <- rda_spp %>% mutate(!!ax1 := .data[[ax1]] * sc, !!ax2 := .data[[ax2]] * sc)

p2 <- ggplot() +
  geom_point(data = rda_sites, aes(x = .data[[ax1]], y = .data[[ax2]], colour = elevation), size = 3, alpha = 0.9) +
  scale_color_viridis_c(option = "viridis", name = "Elevation (m)") +
  geom_segment(data = rda_spp, aes(x = 0, y = 0, xend = .data[[ax1]], yend = .data[[ax2]]),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.6, colour = "darkred") +
  geom_text(data = rda_spp, aes(x = .data[[ax1]] * 1.1, y = .data[[ax2]] * 1.1, label = order), size = 3.5, hjust = 0) +
  labs(title = "RDA: orders ~ elevation", x = paste0(ax1, " (elevation)"), y = ax2, subtitle = "Taxa to the right increase with elevation") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 9)) + coord_fixed()
png(file.path(dir_out, "RDA_orders_elevation.png"), width = 10, height = 8, units = "in", res = 300, bg = "white")
print(p2)
dev.off()
message("Written: RDA_orders_elevation.png")

# ---------- 3: Heatmap orders + Coleoptera families ----------
# Orders: elevation x order, mean rate
rate_long <- as.data.frame(mat_rates)
rate_long$site_night <- rownames(rate_long)
rate_long <- rate_long %>% left_join(site_info %>% select(site_night, elevation), by = "site_night")
rate_long <- rate_long %>% pivot_longer(-c(site_night, elevation), names_to = "order", values_to = "rate")
heat_order <- rate_long %>% group_by(elevation, order) %>% summarise(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop")

p3a <- ggplot(heat_order, aes(x = factor(elevation), y = order, fill = mean_rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Mean det./photo") +
  labs(x = "Elevation (m)", y = "Order", title = "Orders x elevation (mean detections per photo)") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5))
png(file.path(dir_out, "heatmap_orders_elevation.png"), width = 10, height = 6, units = "in", res = 300, bg = "white")
print(p3a)
dev.off()
message("Written: heatmap_orders_elevation.png")

# Coleoptera families
coleo <- data %>% filter(order == "Coleoptera") %>%
  mutate(family = if_else(is.na(family) | trimws(as.character(family)) == "", "Unknown", as.character(family)))
fam_session <- coleo %>%
  group_by(site_night, family) %>%
  summarise(det = n(), .groups = "drop") %>%
  left_join(effort_site, by = "site_night") %>%
  mutate(rate = det / n_photos) %>%
  left_join(elev, by = "site_night")
heat_fam <- fam_session %>% group_by(elevation, family) %>% summarise(mean_rate = mean(rate, na.rm = TRUE), .groups = "drop")

p3b <- ggplot(heat_fam, aes(x = factor(elevation), y = family, fill = mean_rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Mean det./photo") +
  labs(x = "Elevation (m)", y = "Family", title = "Coleoptera families x elevation") +
  theme_minimal() + theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5))
png(file.path(dir_out, "heatmap_Coleoptera_families_elevation.png"), width = 10, height = 6, units = "in", res = 300, bg = "white")
print(p3b)
dev.off()
message("Written: heatmap_Coleoptera_families_elevation.png")

# ---------- 4: Response curves (mean rate vs elevation per order) ----------
resp <- rate_long %>% group_by(elevation, order) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE), se_rate = sd(rate, na.rm = TRUE) / sqrt(n()), .groups = "drop")
# Top 5 most abundant orders (by total detections)
top5_orders <- order_counts %>% group_by(order) %>% summarise(tot = sum(freq), .groups = "drop") %>%
  slice_max(tot, n = 5) %>% pull(order)
resp_top5 <- resp %>% filter(order %in% top5_orders)
p4 <- ggplot(resp_top5, aes(x = elevation, y = mean_rate, colour = order)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_rate - se_rate, ymax = mean_rate + se_rate), width = 0, linewidth = 0.5) +
  scale_color_viridis_d(option = "turbo", name = "Order") +
  labs(x = "Elevation (m)", y = "Mean detections per photo", title = "Order response to elevation (top 5 abundant)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
png(file.path(dir_out, "response_curves_orders_elevation.png"), width = 10, height = 6, units = "in", res = 300, bg = "white")
print(p4)
dev.off()
message("Written: response_curves_orders_elevation.png")

# ---------- 5: Slope bar chart (rate ~ elevation per order) ----------
slopes <- rate_long %>%
  group_by(order) %>%
  summarise(
    slope = if (n() >= 3) coef(lm(rate ~ elevation))[2] else NA_real_,
    .groups = "drop"
  ) %>%
  filter(!is.na(slope)) %>%
  mutate(order = reorder(order, slope))
p5 <- ggplot(slopes, aes(x = order, y = slope, fill = slope > 0)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkorange"), name = "Direction", labels = c("Decrease", "Increase")) +
  labs(x = "Order", y = "Slope (rate ~ elevation)", title = "Taxon-elevation slope (detections per photo)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
png(file.path(dir_out, "taxon_elevation_slopes.png"), width = 10, height = 6, units = "in", res = 300, bg = "white")
print(p5)
dev.off()
message("Written: taxon_elevation_slopes.png")

message("All taxon-elevation figures written to ", dir_out)
