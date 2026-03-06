# Count detections with class == "Insecta" and no value for order (NA or blank).
# Run from project root.

library(vroom)

d <- vroom("data_processed/data.csv", show_col_types = FALSE)
cl <- as.character(d$class)
ord <- as.character(d$order)

no_order <- is.na(ord) | is.na(d$order) | trimws(ord) == "" | ord == "NA"
n <- sum(cl == "Insecta" & no_order, na.rm = TRUE)

cat("Detections with class Insecta and no order (NA or blank):", n, "\n")
