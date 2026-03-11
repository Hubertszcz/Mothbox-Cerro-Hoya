# Test whether activity (detections per photo) varies by monitoring session (hour) per order.
# LMM per order: rate ~ hour + (1|site) + (1|site_night). Output: table + optional plot.
# Reads data_processed/data.csv and agentic_hangout/output/time_of_night/session_effort.csv.
# All outputs to agentic_hangout/output/time_of_night/.

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

# Input and output
data <- read.csv("data_processed/data.csv")
session_effort <- read.csv("agentic_hangout/output/time_of_night/session_effort.csv")
dir_out <- "agentic_hangout/output/time_of_night"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Derive hour and filter to programA windows
data$hour_int <- as.integer(substr(data$eventTime, 1, 2))
data <- data %>% filter(hour_int %in% c(19, 21, 23, 2, 4))
data$hour <- factor(data$hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h"))

# Only orders with valid value
data <- data %>% filter(!is.na(order) & trimws(as.character(order)) != "")

# All sessions (with site for random effect)
sessions <- session_effort %>%
  select(site_night, hour, n_photos) %>%
  mutate(site = sub("_.*", "", site_night))

# Observed detections per (site_night, hour, order)
order_obs <- data %>%
  group_by(site_night, hour, order) %>%
  summarise(detections = n(), .groups = "drop")

# Full grid: every session x every order; join observed detections and effort
orders_vec <- unique(order_obs$order)
session_order_full <- tidyr::expand_grid(
  sessions %>% distinct(site_night, hour, n_photos, site),
  order = orders_vec
)

session_order_rate <- session_order_full %>%
  left_join(order_obs, by = c("site_night", "hour", "order")) %>%
  mutate(detections = replace_na(detections, 0L)) %>%
  mutate(rate = detections / n_photos)

# Fit LMM per order; p-value for hour via likelihood ratio test (drop hour vs full model)
fit_order <- function(df) {
  if (length(unique(df$hour)) < 2) return(list(p_value_hour = NA_real_, converged = FALSE))
  if (var(df$rate, na.rm = TRUE) == 0) return(list(p_value_hour = NA_real_, converged = TRUE))
  m_full <- tryCatch(
    lmer(rate ~ hour + (1 | site) + (1 | site_night), data = df, REML = TRUE),
    error = function(e) NULL
  )
  if (is.null(m_full)) return(list(p_value_hour = NA_real_, converged = FALSE))
  m_null <- tryCatch(
    lmer(rate ~ (1 | site) + (1 | site_night), data = df, REML = TRUE),
    error = function(e) NULL
  )
  if (is.null(m_null)) return(list(p_value_hour = NA_real_, converged = FALSE))
  a <- anova(m_null, m_full)
  # Second row is effect of adding hour
  p_val <- a[2, "Pr(>Chisq)"]
  list(p_value_hour = p_val, converged = TRUE)
}

order_results <- session_order_rate %>%
  group_by(order) %>%
  summarise(
    n_sessions_with_detections = sum(detections > 0),
    n_sessions_total = n(),
    .groups = "drop"
  )

order_list <- order_results %>% pull(order)
p_vals <- vector("list", length(order_list))
for (i in seq_along(order_list)) {
  ord <- order_list[i]
  p_vals[[i]] <- fit_order(session_order_rate %>% filter(order == ord))
}
order_results$p_value_hour <- vapply(p_vals, function(x) x[["p_value_hour"]], NA_real_)
order_results$converged <- vapply(p_vals, function(x) x[["converged"]], NA)

order_results <- order_results %>%
  mutate(
    significant = !is.na(p_value_hour) & p_value_hour < 0.05,
    significant_yes_no = if_else(significant, "yes", "no")
  )

# Write table
write.csv(
  order_results %>% select(order, n_sessions_with_detections, n_sessions_total, p_value_hour, significant_yes_no, converged),
  file.path(dir_out, "order_hour_significance.csv"),
  row.names = FALSE
)
message("Written: ", file.path(dir_out, "order_hour_significance.csv"))

# Optional: dot plot of p-values by order
order_plot <- order_results %>%
  filter(!is.na(p_value_hour)) %>%
  mutate(order = reorder(order, -p_value_hour))
p <- ggplot(order_plot, aes(x = order, y = p_value_hour, colour = significant_yes_no)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("yes" = "red", "no" = "black"), name = "Hour effect (p < 0.05)") +
  labs(x = "Order", y = "p-value (hour)", title = "Effect of monitoring session on activity per order") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
png(file.path(dir_out, "order_hour_significance.png"), width = 10, height = 6, units = "in", res = 300, bg = "white")
print(p)
dev.off()
message("Written: ", file.path(dir_out, "order_hour_significance.png"))
