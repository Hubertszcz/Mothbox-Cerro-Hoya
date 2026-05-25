# Continuous elevation scatter plots with GLMM/LMM trend lines.
# Site-night points; elevation as continuous predictor; Poisson/NB or Gaussian LMM.
# Outputs: agentic_hangout/new_plots/output/
# Run from project root.

library(dplyr)
library(ggplot2)
library(viridis)
library(glmmTMB)
library(lme4)
library(lmerTest)

source("agentic_hangout/new_plots/R/elevation_model_helpers.R")

dir_out <- "agentic_hangout/new_plots/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# ---- Data prep (match code/3-statistics.R) ----
hoya_data <- read.csv("data_processed/hoya_data.csv")
session_effort <- read.csv("data_processed/session_effort.csv")

session_effort <- session_effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)

photos_per_site <- session_effort %>%
  group_by(site_night) %>%
  summarise(n_photos_total = sum(n_photos), .groups = "drop")

plot_data <- hoya_data %>%
  mutate(
    elevation_n = as.numeric(elevation),
    site = sub("_.*", "", site_night)
  ) %>%
  filter(!is.na(elevation_n), elevation_n != 1416) %>%
  left_join(photos_per_site, by = "site_night") %>%
  mutate(
    rate = insect_activity / n_photos_total,
    log_offset = log(n_photos_total)
  ) %>%
  filter(n_photos_total > 0)

# X-axis ticks and grid every 200 m (200–1400 m; no 1600 label)
elev_axis_breaks <- seq(200, 1400, by = 200)

# ---- Plot helper (same layout for all; x title/labels only when show_x_axis = TRUE) ----
save_elevation_glm_plot <- function(df_points, y_col, pred_df, ylab, outfile, show_x_axis = FALSE) {
  p <- ggplot(df_points, aes(x = elevation_n, y = .data[[y_col]], colour = elevation_n)) +
    geom_point(size = 3, alpha = 0.85) +
    scale_color_viridis(option = "viridis", direction = -1) +
    scale_x_continuous(breaks = elev_axis_breaks) +
    labs(x = if (show_x_axis) "Elevation (m)" else NULL, y = ylab) +
    base_theme_elevation() +
    theme(
      axis.title.x = if (show_x_axis) element_text(size = 30) else element_blank(),
      axis.text.x  = if (show_x_axis) element_text(size = 20) else element_blank(),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank()
    )

  if (!is.null(pred_df) && nrow(pred_df) > 0) {
    if (isTRUE(pred_df$has_ribbon[1])) {
      p <- p +
        geom_ribbon(
          data = pred_df,
          aes(x = elevation_n, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE,
          fill = "grey40", alpha = 0.15
        )
    }
    p <- p +
      geom_line(
        data = pred_df,
        aes(x = elevation_n, y = fit),
        inherit.aes = FALSE,
        linewidth = 1.1,
        colour = "black"
      )
  }

  png(outfile, width = 12, height = 7.5, units = "in", res = 300, bg = "white")
  print(p)
  dev.off()
  message("Written: ", outfile)
}

# ---- Figure 1: Detections per photo ----
fit_rate <- fit_count_glmm(plot_data, "insect_activity", use_offset = TRUE)
pred_rate <- predict_count_glmm(fit_rate, plot_data, to_rate = TRUE)
save_elevation_glm_plot(
  df_points = plot_data,
  y_col = "rate",
  pred_df = pred_rate,
  ylab = "Mean detections per photo",
  outfile = file.path(dir_out, "Detections_and_elevation_continuous.png")
)

# ---- Figure 2: Richness (GLMM with offset, matches code/3-statistics.R) ----
fit_rich <- fit_count_glmm(plot_data, "insect_richness", use_offset = TRUE)
pred_rich <- predict_count_glmm(fit_rich, plot_data, to_rate = FALSE)
save_elevation_glm_plot(
  df_points = plot_data,
  y_col = "insect_richness",
  pred_df = pred_rich,
  ylab = "Richness",
  outfile = file.path(dir_out, "Richness_and_elevation_continuous.png")
)

# ---- Figure 3: Shannon diversity ----
fit_shan <- fit_shannon_lmm(plot_data)
pred_shan <- predict_shannon_lmm(fit_shan, plot_data)
save_elevation_glm_plot(
  df_points = plot_data,
  y_col = "insect_shannon",
  pred_df = pred_shan,
  ylab = "Shannon diversity",
  outfile = file.path(dir_out, "Shannon_and_elevation_continuous.png"),
  show_x_axis = TRUE
)

# =============================================================================
# DHARMa residual diagnostics (same models as figures / code/3-statistics.R)
# =============================================================================

if (!requireNamespace("DHARMa", quietly = TRUE)) {
  warning("Install DHARMa for residual diagnostics: install.packages('DHARMa')")
} else {
  dir_diag <- file.path(dir_out, "diagnostics")
  if (!dir.exists(dir_diag)) dir.create(dir_diag, recursive = TRUE)

  dharma_test_row <- function(test_obj, test_name, response) {
    data.frame(
      response = response,
      test = test_name,
      statistic = unname(test_obj$statistic),
      p_value = test_obj$p.value,
      interpretation = if (test_obj$p.value < 0.05) "significant deviation (p < 0.05)" else "no significant deviation",
      stringsAsFactors = FALSE
    )
  }

  run_dharma_diagnostics <- function(model, response_label, family_label, outfile_stub) {
    sim <- DHARMa::simulateResiduals(model, plot = FALSE, n = 250)

    png(
      file.path(dir_diag, paste0(outfile_stub, "_DHARMa.png")),
      width = 10, height = 8, units = "in", res = 150, bg = "white"
    )
    plot(sim)
    dev.off()

    tests <- list(
      uniformity = DHARMa::testUniformity(sim, plot = FALSE),
      dispersion = DHARMa::testDispersion(sim, plot = FALSE),
      outliers   = DHARMa::testOutliers(sim, plot = FALSE)
    )

    rows <- bind_rows(
      dharma_test_row(tests$uniformity, "Uniformity (KS)", response_label),
      dharma_test_row(tests$dispersion, "Dispersion", response_label)
    )

    out <- tests$outliers
    if (!is.null(out$observedOutliers) && length(out$observedOutliers) > 0) {
      out_p <- if (!is.null(out$p.value) && length(out$p.value) > 0) unname(out$p.value)[1] else NA_real_
      rows <- bind_rows(
        rows,
        data.frame(
          response = response_label,
          test = "Outliers",
          statistic = as.numeric(out$observedOutliers)[1],
          p_value = out_p,
          interpretation = sprintf(
            "%s outliers observed (%s expected at 95%% simulation interval)",
            out$observedOutliers[1],
            format(out$expectedOutliers[1], digits = 3)
          ),
          stringsAsFactors = FALSE
        )
      )
    }
    rows$family <- family_label
    message("DHARMa: ", response_label, " (", family_label, ") -> ", outfile_stub, "_DHARMa.png")
    rows
  }

  dharma_results <- bind_rows(
    run_dharma_diagnostics(
      fit_rate$model, "Detections (insect_activity)",
      fit_rate$family, "detections"
    ),
    run_dharma_diagnostics(
      fit_rich$model, "Richness (insect_richness)",
      fit_rich$family, "richness"
    ),
    run_dharma_diagnostics(
      fit_shan$model, "Shannon (insect_shannon)",
      fit_shan$family, "shannon"
    )
  )

  write.csv(dharma_results, file.path(dir_diag, "DHARMa_tests.csv"), row.names = FALSE)

  summary_lines <- c(
    "DHARMa residual checks for elevation GLMM/LMM (site-night data, 1416 m excluded).",
    "Tests: Uniformity (QQ/KS) = overall distribution; Dispersion = variance vs simulation;",
    "Outliers = count of points outside simulated 95% interval.",
    "",
    apply(dharma_results, 1, function(r) {
      sprintf("%s | %s | p = %s | %s", r["response"], r["test"], format.pval(as.numeric(r["p_value"]), digits = 3), r["interpretation"])
    })
  )
  writeLines(summary_lines, file.path(dir_diag, "DHARMa_summary.txt"))
  message("Written: ", file.path(dir_diag, "DHARMa_tests.csv"))
  message("Written: ", file.path(dir_diag, "DHARMa_summary.txt"))
}
