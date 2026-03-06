# Percentage of order-level identifications from the AI (Mothbox) that are correct.
# Validates original_mothbox_identifciation (ORDER_*) against the user-assigned 'order' column.
# Reads data_processed/data.csv. Writes summary to agentic_hangout/output/.

library(dplyr)

# Input
data <- read.csv("data_processed/data.csv")

# Keep only rows where the AI gave an order-level identification (ORDER_*)
data$orig <- as.character(data$original_mothbox_identifciation)
order_level <- data %>%
  filter(!is.na(orig), trimws(orig) != "", grepl("^ORDER_", orig))

# Extract order name from AI string (e.g. ORDER_Lepidoptera -> Lepidoptera)
order_level <- order_level %>%
  mutate(
    AI_order = trimws(sub("^ORDER_", "", orig)),
    user_order = trimws(as.character(order))
  )

# Match: AI order equals user-assigned order
order_level <- order_level %>% mutate(correct = (AI_order == user_order))

# Counts
n_order_level <- nrow(order_level)
n_correct <- sum(order_level$correct, na.rm = TRUE)
pct_correct <- if (n_order_level > 0) 100 * n_correct / n_order_level else NA_real_

# Output
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

result <- data.frame(
  n_order_level_AI = n_order_level,
  n_correct = n_correct,
  pct_correct = round(pct_correct, 2)
)
write.csv(result, file.path(dir_out, "AI_order_level_accuracy.csv"), row.names = FALSE)
message("Written: ", file.path(dir_out, "AI_order_level_accuracy.csv"))

# Print result
message("\n--- Order-level AI accuracy (validated by 'order' column) ---")
message("Rows with order-level AI ID (ORDER_*): ", n_order_level)
message("Correct (AI order == user order):       ", n_correct)
message("Percentage correct:                    ", round(pct_correct, 2), "%")

# Optional: by-order breakdown
by_order <- order_level %>%
  group_by(user_order, AI_order) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(match = (user_order == AI_order))
write.csv(by_order, file.path(dir_out, "AI_order_level_accuracy_by_order.csv"), row.names = FALSE)
message("\nBy-order breakdown written to AI_order_level_accuracy_by_order.csv")
