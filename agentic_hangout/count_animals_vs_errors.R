# Count how many raw detections were animals (have taxonID) vs errors (no taxonID).
# Replicates code/0-data_cleanup.R logic. Does not modify any data.
# Optionally: count detections where AI flagged as ERROR in original_mothbox_identifciation.

library(vroom)
library(dplyr)

# Load raw occurrence data (same as cleanup)
data_raw <- vroom(
  list.files("data_raw/occurence_data", pattern = "\\.csv$", full.names = TRUE),
  id = "source_file"
)

# Pipeline definition: errors = no taxonID (removed); animals = have taxonID (kept)
has_taxonID <- complete.cases(data_raw[, c("taxonID")])
n_total_raw <- nrow(data_raw)
n_animals   <- sum(has_taxonID)
n_errors    <- n_total_raw - n_animals

# AI self-flagged as ERROR (e.g. ERROR, ERROR_background)
orig <- as.character(data_raw$original_mothbox_identifciation)
n_AI_flagged_error <- sum(grepl("ERROR", orig, ignore.case = TRUE), na.rm = TRUE)

# Output
dir_out <- "agentic_hangout/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
out <- data.frame(
  total_raw_detections = n_total_raw,
  animals_has_taxonID = n_animals,
  errors_no_taxonID = n_errors,
  pct_animals = round(100 * n_animals / n_total_raw, 2),
  pct_errors = round(100 * n_errors / n_total_raw, 2),
  n_AI_flagged_ERROR = n_AI_flagged_error,
  pct_AI_flagged_ERROR = round(100 * n_AI_flagged_error / n_total_raw, 2)
)
write.csv(out, file.path(dir_out, "count_animals_vs_errors.csv"), row.names = FALSE)

message("--- Animals vs errors (raw occurrence data) ---")
message("Total raw detections:     ", n_total_raw)
message("Animals (have taxonID):  ", n_animals, " (", out$pct_animals, "%)")
message("Errors (no taxonID):     ", n_errors, " (", out$pct_errors, "%)")
message("")
message("AI-flagged as ERROR in original_mothbox_identifciation: ", n_AI_flagged_error, " (", out$pct_AI_flagged_ERROR, "%)")
message("")
message("Written: ", file.path(dir_out, "count_animals_vs_errors.csv"))
