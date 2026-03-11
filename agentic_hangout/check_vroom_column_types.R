# One-off diagnostic: report column types vroom() infers when loading occurrence CSVs
# without col_types. Identifies columns that may be mis-guessed as logical (like genus was).
# Run from project root.

library(vroom)
library(readr)

files <- list.files("data_raw/occurence_data", pattern = "\\.csv$", full.names = TRUE)
# Load WITHOUT col_types so we see vroom's default guessing
raw <- vroom(files, id = "source_file", show_col_types = FALSE)

types <- vapply(raw, function(x) paste(class(x), collapse = ", "), character(1))
n_na <- vapply(raw, function(x) sum(is.na(x)), integer(1))
n_non_na <- nrow(raw) - n_na

report <- data.frame(
  column = names(raw),
  class = types,
  n_na = n_na,
  n_non_na = n_non_na,
  stringsAsFactors = FALSE
)

# Flag columns guessed as logical (same bug as genus had)
report$possibly_wrong <- report$class == "logical" & report$n_non_na > 0

print(report)
cat("\n--- Columns inferred as LOGICAL (check if they should be character) ---\n")
print(report[report$class == "logical", ])
