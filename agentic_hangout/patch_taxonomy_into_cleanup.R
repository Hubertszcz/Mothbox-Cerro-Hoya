# Show what adding the taxonomic filter into code/0-data_cleanup.R would look like.
# This file only DEMONSTRATES the change; it does not modify code/0-data_cleanup.R.

# ---- CODE BLOCK TO ADD (after the elevation 1416 exclusion, before export) ----
# Insert after line 70:  data <- data[data$elevation != "1416", ]
# and before line 77:   write.csv(data, "data_processed/data.csv", ...)
#
# No new packages needed (dplyr already loaded).

cat("=== Code block you would add in code/0-data_cleanup.R ===\n\n")
cat("# Standardize taxonomic level for analysis:\n")
cat("# - Non-Coleoptera: set name to order (analyze at order level).\n")
cat("# - Coleoptera: keep finer taxonomy (do not change name).\n")
cat("# - No order (e.g. only Insecta): leave name unchanged.\n\n")
cat("data <- data %>%\n")
cat("  mutate(\n")
cat("    has_order = !is.na(order) & trimws(as.character(order)) != \"\",\n")
cat("    name = if_else(has_order & order != \"Coleoptera\", as.character(order), name)\n")
cat("  ) %>%\n")
cat("  select(-has_order)\n\n")
cat("=== End of block (place it after 1416 exclusion, before write.csv) ===\n\n")

# ---- Simulate outcome using current data_processed/data.csv ----
cat("=== Simulated outcome (before/after for a few rows) ===\n\n")
library(dplyr)
data <- read.csv("data_processed/data.csv")
data$name_original <- data$name
data <- data %>%
  mutate(
    has_order = !is.na(order) & trimws(as.character(order)) != "",
    name = if_else(has_order & order != "Coleoptera", as.character(order), name)
  )
ex <- data[, c("order", "name_original", "name", "has_order")]
cat("Non-Coleoptera (name -> order):\n")
print(head(ex[ex$order != "Coleoptera" & ex$has_order, c("order", "name_original", "name")], 4))
cat("\nColeoptera (unchanged):\n")
print(head(ex[ex$order == "Coleoptera", ], 4))
# Drop has_order for display
data <- data %>% select(-has_order)
