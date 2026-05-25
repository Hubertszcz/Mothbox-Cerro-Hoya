# Helpers for continuous-elevation GLMM/LMM fits and prediction curves.
# Logic mirrors code/3-statistics.R (Poisson vs NB by AIC; Gaussian LMM for Shannon).

#' Fit count GLMM with optional photo offset; return best of Poisson vs NB by AIC.
fit_count_glmm <- function(df, count_col, use_offset = TRUE) {
  if (use_offset) {
    fml <- as.formula(paste(count_col, "~ elevation_n + offset(log_offset) + (1 | site)"))
  } else {
    fml <- as.formula(paste(count_col, "~ elevation_n + (1 | site)"))
  }
  m_pois <- tryCatch(glmmTMB::glmmTMB(fml, data = df, family = poisson), error = function(e) NULL)
  m_nb <- tryCatch(glmmTMB::glmmTMB(fml, data = df, family = nbinom2), error = function(e) NULL)
  if (is.null(m_pois) && is.null(m_nb)) {
    return(list(model = NULL, family = NA_character_))
  }
  if (!is.null(m_nb) && !is.null(m_pois) && AIC(m_nb) + 2 < AIC(m_pois)) {
    list(model = m_nb, family = "Negative binomial GLMM")
  } else {
    list(model = m_pois, family = "Poisson GLMM")
  }
}

#' Fit Shannon Gaussian LMM.
fit_shannon_lmm <- function(df) {
  m <- lme4::lmer(insect_shannon ~ elevation_n + (1 | site), data = df, REML = TRUE)
  list(model = m, family = "Gaussian LMM")
}

#' Population-level predictions on an elevation grid (glmmTMB).
predict_count_glmm <- function(fit, df, n_grid = 100, to_rate = FALSE) {
  m <- fit$model
  if (is.null(m)) return(NULL)
  elev_seq <- seq(min(df$elevation_n, na.rm = TRUE), max(df$elevation_n, na.rm = TRUE), length.out = n_grid)
  mean_log_off <- mean(df$log_offset, na.rm = TRUE)
  has_offset <- "log_offset" %in% all.vars(formula(m))
  newdata <- data.frame(elevation_n = elev_seq)
  if (has_offset) newdata$log_offset <- mean_log_off

  pred <- tryCatch(
    predict(m, newdata = newdata, type = "response", se.fit = TRUE, re.form = NA),
    error = function(e) NULL
  )
  if (is.null(pred)) {
    fit_vals <- predict(m, newdata = newdata, type = "response", re.form = NA)
    out <- data.frame(elevation_n = elev_seq, fit = as.numeric(fit_vals))
    if (to_rate && has_offset) out$fit <- out$fit / exp(mean_log_off)
    return(out)
  }

  fit_vals <- as.numeric(pred$fit)
  if (to_rate && has_offset) fit_vals <- fit_vals / exp(mean_log_off)
  se <- as.numeric(pred$se.fit)
  if (to_rate && has_offset) se <- se / exp(mean_log_off)

  data.frame(
    elevation_n = elev_seq,
    fit = fit_vals,
    ymin = pmax(fit_vals - 1.96 * se, 0),
    ymax = fit_vals + 1.96 * se,
    has_ribbon = TRUE
  )
}

#' Population-level predictions for Gaussian LMM (Shannon).
predict_shannon_lmm <- function(fit, df, n_grid = 100) {
  m <- fit$model
  if (is.null(m)) return(NULL)
  elev_seq <- seq(min(df$elevation_n, na.rm = TRUE), max(df$elevation_n, na.rm = TRUE), length.out = n_grid)
  newdata <- data.frame(elevation_n = elev_seq)
  pred <- predict(m, newdata = newdata, re.form = NA, se.fit = TRUE)
  fit_vals <- as.numeric(pred$fit)
  se <- as.numeric(pred$se.fit)
  data.frame(
    elevation_n = elev_seq,
    fit = fit_vals,
    ymin = fit_vals - 1.96 * se,
    ymax = fit_vals + 1.96 * se,
    has_ribbon = TRUE
  )
}

#' Shared ggplot theme (matches agentic_hangout bar elevation plots).
#' Vertical grid at x breaks only (200 m); no minor x grid.
base_theme_elevation <- function() {
  ggplot2::theme_minimal(base_family = "Arial", base_size = 18) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 30),
      axis.text  = ggplot2::element_text(size = 20),
      legend.position = "none",
      panel.grid.minor.x = ggplot2::element_blank()
    )
}
