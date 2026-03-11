# Community turnover across monitoring sessions (time of night).
# Session-level community matrix; PERMANOVA (hour, strata = site_night); NMDS by hour.
# Optional: pairwise Bray-Curtis between consecutive hours within site_night.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# All outputs to agentic_hangout/output/time_of_night/.

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Input and output
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")
dir_out <- "agentic_hangout/output/time_of_night"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Derive hour and filter to programA windows
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))
data <- data %>% filter(!is.na(order) & trimws(as.character(order)) != "")

# ---- Session x order matrix (counts per session) ----
order_session <- data %>%
  group_by(site_night, hour, order) %>%
  summarise(freq = n(), .groups = "drop")
# Session ID = site_night + hour (unique row per session)
session_effort <- session_effort %>%
  mutate(session_id = paste(site_night, hour, sep = "__"))
order_session <- order_session %>%
  mutate(session_id = paste(site_night, hour, sep = "__"))

# Sessions data frame for env and row names
sessions_df <- session_effort %>% select(session_id, site_night, hour)

# Wide: one row per session_id, columns = orders (only session_id + orders for clean join)
mat_orders_wide <- order_session %>%
  select(session_id, order, freq) %>%
  pivot_wider(names_from = order, values_from = freq, values_fill = 0)
# All sessions (including those with zero detections) so row order matches session_effort
mat_orders <- sessions_df %>%
  left_join(mat_orders_wide, by = "session_id") %>%
  select(-session_id, -site_night, -hour)
mat_orders[is.na(mat_orders)] <- 0
mat_orders <- as.matrix(mat_orders)
rownames(mat_orders) <- sessions_df$session_id
env_orders <- sessions_df

# PERMANOVA: hour effect, permutations stratified by site_night
perm_orders_full <- adonis2(mat_orders ~ hour, data = env_orders, permutations = 999, method = "bray", strata = env_orders$site_night)
perm_orders_tab <- as.data.frame(perm_orders_full)
perm_orders_tab$term <- rownames(perm_orders_tab)
write.csv(perm_orders_tab, file.path(dir_out, "PERMANOVA_hour_orders.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "PERMANOVA_hour_orders.csv"))

# Second half only: 23h, 2h, 4h
env_second <- env_orders %>% filter(hour %in% c("23h", "2h", "4h"))
mat_second <- mat_orders[env_second$session_id, , drop = FALSE]
perm_orders_second <- adonis2(mat_second ~ hour, data = env_second, permutations = 999, method = "bray", strata = env_second$site_night)
perm_second_tab <- as.data.frame(perm_orders_second)
perm_second_tab$term <- rownames(perm_second_tab)
write.csv(perm_second_tab, file.path(dir_out, "PERMANOVA_hour_orders_second_half.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "PERMANOVA_hour_orders_second_half.csv"))

# NMDS: orders, all sessions, coloured by hour
set.seed(42)
nmds_orders <- metaMDS(mat_orders, distance = "bray", k = 2, trymax = 100)
scores_ord <- as.data.frame(scores(nmds_orders, display = "sites"))
scores_ord$session_id <- rownames(scores_ord)
scores_ord <- scores_ord %>% left_join(env_orders, by = "session_id")
p_nmds_orders <- ggplot(scores_ord, aes(x = NMDS1, y = NMDS2, colour = hour)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_colour_viridis_d(option = "viridis", name = "Time of night") +
  labs(title = "Session-level community ordination (orders) by time of night") +
  theme_minimal(base_size = 12) +
  coord_fixed()
png(file.path(dir_out, "NMDS_sessions_by_hour_orders.png"), width = 8, height = 6, units = "in", res = 300, bg = "white")
print(p_nmds_orders)
dev.off()
message("Written: ", file.path(dir_out, "NMDS_sessions_by_hour_orders.png"))

# ---- Session x Coleoptera family matrix ----
coleo <- data %>% filter(order == "Coleoptera") %>%
  mutate(family = if_else(is.na(family) | trimws(as.character(family)) == "", "Unknown", as.character(family)))
fam_session <- coleo %>%
  group_by(site_night, hour, family) %>%
  summarise(freq = n(), .groups = "drop") %>%
  mutate(session_id = paste(site_night, hour, sep = "__"))
fam_wide <- fam_session %>%
  select(session_id, family, freq) %>%
  pivot_wider(names_from = family, values_from = freq, values_fill = 0)
mat_coleo <- sessions_df %>%
  left_join(fam_wide, by = "session_id") %>%
  select(-session_id, -site_night, -hour)
mat_coleo[is.na(mat_coleo)] <- 0
mat_coleo <- as.matrix(mat_coleo)
rownames(mat_coleo) <- sessions_df$session_id
# Drop sessions with zero Coleoptera (all-zero rows break Bray-Curtis)
row_sums <- rowSums(mat_coleo)
mat_coleo_ok <- mat_coleo[row_sums > 0, , drop = FALSE]
env_coleo <- env_orders %>% filter(session_id %in% rownames(mat_coleo_ok))
mat_coleo_ok <- mat_coleo_ok[env_coleo$session_id, , drop = FALSE]
perm_coleo_full <- adonis2(mat_coleo_ok ~ hour, data = env_coleo, permutations = 999, method = "bray", strata = env_coleo$site_night)
perm_coleo_tab <- as.data.frame(perm_coleo_full)
perm_coleo_tab$term <- rownames(perm_coleo_tab)
write.csv(perm_coleo_tab, file.path(dir_out, "PERMANOVA_hour_coleoptera_family.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "PERMANOVA_hour_coleoptera_family.csv"))

# NMDS Coleoptera family
set.seed(42)
nmds_coleo <- metaMDS(mat_coleo_ok, distance = "bray", k = 2, trymax = 100)
scores_coleo <- as.data.frame(scores(nmds_coleo, display = "sites"))
scores_coleo$session_id <- rownames(scores_coleo)
scores_coleo <- scores_coleo %>% left_join(env_coleo, by = "session_id")
p_nmds_coleo <- ggplot(scores_coleo, aes(x = NMDS1, y = NMDS2, colour = hour)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_colour_viridis_d(option = "viridis", name = "Time of night") +
  labs(title = "Session-level Coleoptera (family) ordination by time of night") +
  theme_minimal(base_size = 12) +
  coord_fixed()
png(file.path(dir_out, "NMDS_sessions_by_hour_coleoptera_family.png"), width = 8, height = 6, units = "in", res = 300, bg = "white")
print(p_nmds_coleo)
dev.off()
message("Written: ", file.path(dir_out, "NMDS_sessions_by_hour_coleoptera_family.png"))

# ---- Pairwise beta diversity: mean Bray-Curtis between consecutive hours within site_night ----
hour_levels <- c("19h", "21h", "23h", "2h", "4h")
pairs_list <- list(
  "19h_21h" = c("19h", "21h"), "21h_23h" = c("21h", "23h"), "23h_2h" = c("23h", "2h"), "2h_4h" = c("2h", "4h")
)
site_nights_vec <- unique(env_orders$site_night)
turnover_by_pair <- vector("list", length(pairs_list))
names(turnover_by_pair) <- names(pairs_list)
for (p in names(pairs_list)) {
  h1 <- pairs_list[[p]][1]
  h2 <- pairs_list[[p]][2]
  bc_vec <- numeric(0)
  for (sn in site_nights_vec) {
    id1 <- paste(sn, h1, sep = "__")
    id2 <- paste(sn, h2, sep = "__")
    if (id1 %in% rownames(mat_orders) && id2 %in% rownames(mat_orders)) {
      d <- vegdist(rbind(mat_orders[id1, ], mat_orders[id2, ]), method = "bray")
      bc_vec <- c(bc_vec, as.numeric(d))
    }
  }
  turnover_by_pair[[p]] <- data.frame(hour_pair = p, mean_BrayCurtis = mean(bc_vec), n_site_nights = length(bc_vec))
}
turnover_df <- bind_rows(turnover_by_pair)
write.csv(turnover_df, file.path(dir_out, "pairwise_turnover_consecutive_hours.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "pairwise_turnover_consecutive_hours.csv"))

message("Community turnover analysis done. Outputs in ", dir_out)
