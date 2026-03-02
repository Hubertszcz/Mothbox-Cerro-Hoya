# Standardize taxonomic level for analysis
# Run from project root (e.g. in RStudio: setwd to repo root, then source this file).
#
# Reads: data_processed/data.csv (output of code/0-data_cleanup.R)
# Writes: agentic_hangout/data_taxonomy_standardized.csv (all agentic_hangout outputs stay here)
#
# Rule:
# - By default analyze at ORDER level: for any row where order is NOT "Coleoptera",
#   set name to order (so all non-Coleoptera are analyzed at order level).
# - For Coleoptera only: keep finer taxonomy (do not change name).
# - If not identified to Order (e.g. only class Insecta): keep higher taxonomy,
#   do not make up an order (leave name unchanged).

library(dplyr)
library(readr)

# Paths (run from project root). Output stays in agentic_hangout/.
path_in  <- "data_processed/data.csv"
path_out <- "agentic_hangout/data_taxonomy_standardized.csv"

# Optional: set to TRUE to write to main pipeline (only after you approve)
# OVERWRITE_DATA_CSV <- FALSE
# if (OVERWRITE_DATA_CSV) path_out <- "data_processed/data_taxonomy_standardized.csv"

# Read cleaned data
data <- read_csv(path_in, show_col_types = FALSE)

# Keep original name for sanity checks
data$name_original <- data$name

# Standardize: only set name to order when we have a valid order AND it's not Coleoptera
# (NA, empty string, or "Insecta"-only cases have no order → leave name as-is)
data <- data %>%
  mutate(
    has_order = !is.na(order) & trimws(as.character(order)) != "",
    name = if_else(has_order & order != "Coleoptera", as.character(order), name)
  )

# Sanity checks
non_col <- data %>% filter(has_order, order != "Coleoptera")
coleoptera <- data %>% filter(order == "Coleoptera")

check1 <- all(non_col$name == non_col$order, na.rm = TRUE)
check2 <- all(coleoptera$name == coleoptera$name_original, na.rm = TRUE)

message("Sanity checks:")
message("  (1) Non-Coleoptera (with order) have name == order: ", check1)
message("  (2) Coleoptera rows unchanged (name == name_original): ", check2)
if (!check1 || !check2) warning("At least one check failed. Inspect the data before using output.")

# Drop only the helper column; keep name_original in CSV for verification
data <- data %>% select(-has_order)

# Write output
write_csv(data, path_out)
message("Written: ", path_out)

# Optional: verify in R after sourcing
# table(data$order, data$name, useNA = "ifany")  # non-Coleoptera: name should match order
# all(data[data$order != "Coleoptera" & !is.na(data$order) & data$order != "", "name"] == data[data$order != "Coleoptera" & !is.na(data$order) & data$order != "", "order"])
