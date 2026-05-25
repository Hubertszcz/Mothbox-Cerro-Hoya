# Visualize order-level Mothbox detection accuracy against human-validated order IDs.
# Input: data_processed/data.csv
# Output: agentic_hangout/accuracy/output/

library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(scales)

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
    human_order = trimws(as.character(order))
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

overall_accuracy <- order_level %>%
  summarise(
    n_order_level = n(),
    n_correct = sum(correct, na.rm = TRUE),
    pct_correct = 100 * n_correct / n_order_level
  )

by_human_order <- order_level %>%
  group_by(human_order) %>%
  summarise(
    n = n(),
    n_correct = sum(correct, na.rm = TRUE),
    pct_correct = 100 * n_correct / n,
    .groups = "drop"
  ) %>%
  arrange(desc(n))

confusion <- order_level %>%
  count(human_order, AI_order, name = "n") %>%
  group_by(human_order) %>%
  mutate(row_pct = 100 * n / sum(n)) %>%
  ungroup()

write_csv(overall_accuracy, file.path(out_dir, "overall_order_level_accuracy.csv"))
write_csv(by_human_order, file.path(out_dir, "order_level_accuracy_by_human_order.csv"))
write_csv(confusion, file.path(out_dir, "order_level_confusion_matrix_long.csv"))

p_overall <- ggplot(overall_accuracy, aes(x = "Order-level", y = pct_correct)) +
  geom_col(fill = "#2C7FB8", width = 0.5) +
  geom_text(aes(label = paste0(round(pct_correct, 1), "%")), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(
    title = "Mothbox Order-level Accuracy",
    subtitle = paste0("Correct = ", overall_accuracy$n_correct, " / ", overall_accuracy$n_order_level),
    x = NULL,
    y = "Accuracy (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, "overall_order_level_accuracy.png"),
  plot = p_overall,
  width = 7,
  height = 4,
  dpi = 300
)

p_by_order <- by_human_order %>%
  mutate(human_order = fct_reorder(human_order, pct_correct)) %>%
  ggplot(aes(x = human_order, y = pct_correct, fill = n)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct_correct, 1), "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  scale_fill_viridis_c(option = "C", end = 0.9) +
  labs(
    title = "Order-level Accuracy by Human-validated Order",
    subtitle = "Fill indicates sample size per order",
    x = "Human-validated order",
    y = "Accuracy (%)",
    fill = "N"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, "order_level_accuracy_by_human_order.png"),
  plot = p_by_order,
  width = 9,
  height = 7,
  dpi = 300
)

top_human <- confusion %>%
  count(human_order, wt = n, name = "total") %>%
  arrange(desc(total)) %>%
  slice_head(n = 12) %>%
  pull(human_order)

top_ai <- confusion %>%
  count(AI_order, wt = n, name = "total") %>%
  arrange(desc(total)) %>%
  slice_head(n = 12) %>%
  pull(AI_order)

confusion_plot_data <- confusion %>%
  filter(human_order %in% top_human, AI_order %in% top_ai) %>%
  mutate(
    human_order = fct_reorder(human_order, row_pct, .fun = max, .desc = TRUE),
    AI_order = fct_reorder(AI_order, n, .fun = sum, .desc = TRUE)
  )

p_confusion <- ggplot(confusion_plot_data, aes(x = AI_order, y = human_order, fill = row_pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3) +
  scale_fill_viridis_c(option = "B", end = 0.95, labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "Order-level Confusion Matrix (Top Orders)",
    subtitle = "Tile color is within-row percentage; labels are counts",
    x = "AI-predicted order",
    y = "Human-validated order",
    fill = "Row %"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(out_dir, "order_level_confusion_heatmap_top_orders.png"),
  plot = p_confusion,
  width = 10,
  height = 8,
  dpi = 300
)

message("Wrote outputs to: ", out_dir)
