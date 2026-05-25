# Bar chart: order-level Mothbot accuracy for all ORDER_* predictions and by human-validated order.
# Styling matches taxonomic-rank validation figure (viridis fills, Cambria, light grids).
# Input: data_processed/data.csv
# Output: agentic_hangout/accuracy/output/

library(dplyr)
library(ggplot2)
library(readr)
library(forcats)

input_path <- "data_processed/data.csv"
out_dir <- "agentic_hangout/accuracy/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path)
}

raw <- read_csv(input_path, show_col_types = FALSE)

required_cols <- c("original_mothbox_identifciation", "order")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
}

order_level <- raw %>%
  transmute(
    AI_raw = trimws(as.character(original_mothbox_identifciation)),
    human_order = trimws(as.character(.data$order))
  ) %>%
  filter(!is.na(AI_raw), AI_raw != "", grepl("^ORDER_", AI_raw)) %>%
  mutate(
    AI_order = trimws(sub("^ORDER_", "", AI_raw)),
    human_order = if_else(is.na(human_order) | human_order == "", "Unknown", human_order),
    correct = AI_order == human_order
  )

if (nrow(order_level) == 0) {
  stop("No ORDER_* records found in original_mothbox_identifciation.")
}

summarise_acc <- function(df) {
  n_ <- nrow(df)
  n_correct <- sum(df$correct, na.rm = TRUE)
  tibble(
    n = n_,
    n_correct = n_correct,
    accuracy_pct = if (n_ > 0) 100 * n_correct / n_ else NA_real_
  )
}

insect_orders <- c("Lepidoptera", "Coleoptera", "Hemiptera", "Diptera", "Hymenoptera")

overall <- summarise_acc(order_level)
overall_row <- tibble(
  group = "All detections",
  n = overall$n,
  n_correct = overall$n_correct,
  accuracy_pct = overall$accuracy_pct
)

by_order_rows <- bind_rows(lapply(insect_orders, function(ord) {
  sub <- filter(order_level, human_order == ord)
  s <- summarise_acc(sub)
  tibble(
    group = ord,
    n = s$n,
    n_correct = s$n_correct,
    accuracy_pct = s$accuracy_pct
  )
}))

plot_df <- bind_rows(overall_row, by_order_rows) %>%
  mutate(
    group = fct_relevel(factor(group), "All detections", insect_orders)
  )

write_csv(plot_df, file.path(out_dir, "order_level_accuracy_by_insect_group.csv"))

n_total <- sum(plot_df$n[plot_df$group == "All detections"])

# Theme: Cambria, viridis fills, major grid every 10%, minor every 5%, light verticals at categories
base_family <- "Cambria"

p <- ggplot(plot_df, aes(x = group, y = accuracy_pct, fill = group)) +
  geom_col(width = 0.62, color = NA) +
  geom_text(
    aes(label = paste0(round(accuracy_pct, 1), "%")),
    vjust = -0.35,
    size = 4.8,
    family = base_family
  ) +
  scale_fill_viridis_d(option = "D", begin = 0.08, end = 0.95, guide = "none") +
  scale_y_continuous(
    name = "Accuracy (%) at order level",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    minor_breaks = seq(0, 100, by = 5),
    expand = expansion(mult = c(0, 0.06)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_discrete(expand = expansion(add = 0.35)) +
  labs(
    title = "Mothbot order-level accuracy by insect group",
    subtitle = paste0("n = ", format(n_total, big.mark = ","), " detections"),
    x = NULL
  ) +
  theme_minimal(base_size = 15, base_family = base_family) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, colour = "grey25", margin = margin(b = 16)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text = element_text(colour = "grey15", size = 13),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.minor.y = element_line(colour = "grey90", linewidth = 0.25),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = file.path(out_dir, "order_level_accuracy_by_insect_group.png"),
  plot = p,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

message("Written: ", file.path(out_dir, "order_level_accuracy_by_insect_group.png"))
message("Written: ", file.path(out_dir, "order_level_accuracy_by_insect_group.csv"))
