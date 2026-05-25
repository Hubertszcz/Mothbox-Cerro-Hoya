# Helpers to load GLMM p-value annotations for figures.
# Requires agentic_hangout/elevation_glmm/analysis_elevation_statistics.R to have been run.

read_plot_annotations <- function(
    path = "agentic_hangout/elevation_glmm/output/statistics_plot_annotations.csv"
) {
  if (!file.exists(path)) {
    warning("Run analysis_elevation_statistics.R first; missing ", path)
    return(data.frame(test = character(), p_value = numeric(), label = character(), fontface = character()))
  }
  read.csv(path, stringsAsFactors = FALSE)
}

get_plot_annot <- function(test_id, annot_df = NULL) {
  if (is.null(annot_df)) annot_df <- read_plot_annotations()
  row <- annot_df[annot_df$test == test_id, , drop = FALSE]
  if (nrow(row) == 0) {
    return(list(label = "p = NA", fontface = "plain", p_value = NA_real_))
  }
  list(label = row$label[1], fontface = row$fontface[1], p_value = row$p_value[1])
}
