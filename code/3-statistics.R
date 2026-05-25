###########################################################################################################################
# Statistics: elevation and session effects (manuscript Methods)
#   Fig 1 — activity per photo ~ elevation
#   Fig 2 — richness ~ elevation
#   Fig 3 — Shannon ~ elevation
#   Fig 4 — session, elevation, interaction (focal orders; panels use elevation bands for display only)
# Methods paragraph: agentic_hangout/elevation_glmm/STATISTICS_METHODS.md
###########################################################################################################################

#load packages
library(dplyr)
library(tidyr)

#load data
hoya_data <- read.csv("data_processed/hoya_data.csv")
session_effort <- read.csv("data_processed/session_effort.csv")
data <- read.csv("data_processed/data.csv")


###########################################################################################################################
# Setup: same filters as code/2-visualization.R (effort, hours, 1416 m exclusion)
###########################################################################################################################

# Exclude deployment at 1416 m (failed unit; see code/0-data_cleanup.R).
session_effort <- session_effort %>%
  mutate(elevation_n = as.numeric(elevation)) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)
hoya_data_elev <- hoya_data %>%
  mutate(
    elevation_n = as.numeric(elevation),
    site = sub("_.*", "", site_night)
  ) %>%
  filter(!is.na(elevation_n), elevation_n != 1416)

# Program A hours only so statistics match bar plots in 2-visualization.R.
data <- data %>%
  mutate(
    hour_int = as.integer(substr(as.character(eventTime), 1, 2)),
    site = sub("_.*", "", site_night)
  ) %>%
  filter(hour_int %in% c(19, 21, 23, 2, 4)) %>%
  mutate(hour = factor(hour_int, levels = c(19, 21, 23, 2, 4), labels = c("19h", "21h", "23h", "2h", "4h")))
data <- data %>% filter(as.numeric(elevation) != 1416)
site_nights_ok <- unique(data$site_night)
session_effort <- session_effort %>% filter(site_night %in% site_nights_ok)

# elevation_band: display-only grouping for Fig 4 panels in 2-visualization.R (not used in Test 6 inference).
elev_breaks <- c(0, 300, 600, 900, 1300, 2000)
elev_labels <- c("<300 m", "300–600 m", "600–900 m", "900–1300 m", ">1300 m")
session_effort <- session_effort %>%
  mutate(elevation_band = cut(
    elevation_n,
    breaks = elev_breaks,
    labels = elev_labels,
    include.lowest = TRUE,
    right = FALSE
  ))

focal_orders <- c("Lepidoptera", "Coleoptera", "Hemiptera", "Diptera")

dir_out <- "agentic_hangout/elevation_glmm/output"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create("agentic_hangout/output", showWarnings = FALSE, recursive = TRUE)
# Significance threshold (matches Methods: assessed at alpha = 0.05).
alpha <- 0.05


###########################################################################################################################
# Data prep: site-night rates (Figs 1–3, Test 4) and session-level rates by order (Figs 4–5, Test 6)
###########################################################################################################################

# Site-night totals: mean detections per photo = count / n_photos; log(n_photos) is the GLMM offset.
photos_per_site <- session_effort %>%
  group_by(site_night) %>%
  summarise(n_photos_total = sum(n_photos), .groups = "drop")
fig1_data <- hoya_data_elev %>%
  select(site_night, site, elevation, elevation_n, insect_activity) %>%
  left_join(photos_per_site, by = "site_night") %>%
  mutate(
    rate = insect_activity / n_photos_total,
    log_offset = log(n_photos_total)
  ) %>%
  filter(n_photos_total > 0)

hoya_glmm <- hoya_data_elev %>%
  left_join(photos_per_site, by = "site_night") %>%
  mutate(log_offset = log(n_photos_total)) %>%
  filter(n_photos_total > 0)

# Session x site-night x order: detections per photo per hour (Fig 4, Tests 5–6).
session_rates_by_order <- function(detections_df, order_name) {
  counts <- detections_df %>%
    group_by(site_night, hour) %>%
    summarise(detections = n(), .groups = "drop")
  session_effort %>%
    select(site_night, hour, n_photos, elevation, elevation_n, elevation_band) %>%
    left_join(counts, by = c("site_night", "hour")) %>%
    mutate(
      site = sub("_.*", "", site_night),
      detections = replace_na(detections, 0L),
      rate = detections / n_photos,
      order = order_name
    )
}
rates_session_lepi   <- session_rates_by_order(data %>% filter(order == "Lepidoptera"), "Lepidoptera")
rates_session_coleo  <- session_rates_by_order(data %>% filter(order == "Coleoptera"), "Coleoptera")
rates_session_hemi   <- session_rates_by_order(data %>% filter(order == "Hemiptera"), "Hemiptera")
rates_session_dip    <- session_rates_by_order(data %>% filter(order == "Diptera"), "Diptera")
rates_session_long   <- bind_rows(
  rates_session_lepi, rates_session_coleo, rates_session_hemi, rates_session_dip
)

# Site-night detections per focal order (order panels; Test 4).
order_detections_sitenight <- data %>%
  filter(order %in% focal_orders) %>%
  group_by(site_night, order) %>%
  summarise(detections = n(), .groups = "drop")
fig4_order_data <- photos_per_site %>%
  left_join(order_detections_sitenight, by = "site_night") %>%
  left_join(
    session_effort %>%
      mutate(site = sub("_.*", "", site_night)) %>%
      select(site_night, elevation, elevation_n, site) %>%
      distinct(site_night, .keep_all = TRUE),
    by = "site_night"
  ) %>%
  mutate(
    detections = replace_na(detections, 0L),
    rate = detections / n_photos_total,
    log_offset = log(n_photos_total)
  )

##################################
##################################
####### NEW ######################
##################################
##################################

library(lme4)
library(lmerTest)
library(glmmTMB)

# Helpers: continuous elevation (m); (1|site) for repeated nights at one elevation per site.
format_p <- function(p) {
  if (length(p) != 1L || !is.finite(p)) return("NA")
  format.pval(p, digits = 2, eps = 0.001)
}

# Count responses: Poisson vs negative binomial by AIC (Methods); Gaussian for Shannon.
elev_slope <- function(df, count_col = NULL, gaussian_col = NULL) {
  if (!is.null(count_col)) {
    fml <- as.formula(paste(count_col, "~ elevation_n + offset(log_offset) + (1 | site)"))
    m_pois <- tryCatch(glmmTMB(fml, data = df, family = poisson), error = function(e) NULL)
    m_nb <- tryCatch(glmmTMB(fml, data = df, family = nbinom2), error = function(e) NULL)
    if (is.null(m_pois) && is.null(m_nb)) {
      return(list(est = NA, p = NA, family = NA_character_))
    }
    use_nb <- !is.null(m_nb) && !is.null(m_pois) && AIC(m_nb) + 2 < AIC(m_pois)
    m <- if (use_nb) m_nb else m_pois
    fam <- if (use_nb) "nbinom2" else "poisson"
    sm <- summary(m)$coefficients$cond
    list(est = sm["elevation_n", "Estimate"], p = sm["elevation_n", "Pr(>|z|)"], family = fam)
  } else {
    m <- lmer(
      as.formula(paste(gaussian_col, "~ elevation_n + (1 | site)")),
      data = df,
      REML = TRUE
    )
    sm <- summary(m)$coefficients
    list(est = sm["elevation_n", "Estimate"], p = sm["elevation_n", "Pr(>|t|)"], family = "gaussian")
  }
}

elev_statement <- function(label, est, p) {
  sig <- is.finite(p) && p < alpha
  dir <- if (!sig) "did not vary significantly" else if (est > 0) "increased" else "decreased"
  sprintf("%s %s with elevation (p = %s).", label, dir, format_p(p))
}

sig_word <- function(p) {
  if (is.finite(p) && p < alpha) "significant" else "not significant"
}


###########################################################################################################################
# Test 1: Mean detections per photo ~ elevation (Figure 1)
#   GLMM with log(n_photos) offset; report direction of elevation slope.
###########################################################################################################################

s1 <- elev_slope(fig1_data, count_col = "insect_activity")
p1 <- s1$p
result_1 <- elev_statement("Mean detections per photo", s1$est, p1)


###########################################################################################################################
# Test 2: Morphospecies richness ~ elevation (Figure 2)
###########################################################################################################################

s2 <- elev_slope(hoya_glmm, count_col = "insect_richness")
p2 <- s2$p
result_2 <- elev_statement("Morphospecies richness", s2$est, p2)


###########################################################################################################################
# Test 3: Shannon diversity ~ elevation (Figure 3)
#   Gaussian LMM (Shannon is continuous, not a count).
###########################################################################################################################

s3 <- elev_slope(hoya_glmm, gaussian_col = "insect_shannon")
p3 <- s3$p
result_3 <- elev_statement("Shannon diversity", s3$est, p3)


###########################################################################################################################
# Test 4: Focal-order detections per photo ~ elevation (site-night GLMMs)
###########################################################################################################################

result_4 <- character(length(focal_orders))
names(result_4) <- focal_orders
order_p <- list()
order_family <- list()
for (ord in focal_orders) {
  df4 <- fig4_order_data %>% filter(order == ord)
  s4 <- elev_slope(df4, count_col = "detections")
  order_p[[ord]] <- s4$p
  order_family[[ord]] <- s4$family
  result_4[ord] <- elev_statement(paste0(ord, " detections per photo"), s4$est, s4$p)
}


###########################################################################################################################
# Test 5: Does sampling session (hour) predict detections per photo? (Figure 4 — session main effect)
#   Nested LMMs; likelihood-ratio test for adding hour (Methods).
###########################################################################################################################

result_5 <- character(length(focal_orders))
names(result_5) <- focal_orders
p5_by_order <- list()
for (ord in focal_orders) {
  df5 <- rates_session_long %>% filter(order == ord)
  m5_full <- lmer(rate ~ hour + (1 | site) + (1 | site_night), data = df5, REML = FALSE)
  m5_null <- lmer(rate ~ 1 + (1 | site) + (1 | site_night), data = df5, REML = FALSE)
  p5 <- anova(m5_null, m5_full)[2, "Pr(>Chisq)"]
  p5_by_order[[ord]] <- p5
  sig5 <- if (p5 < alpha) "was" else "was not"
  result_5[ord] <- sprintf(
    "Session (hour) %s a significant predictor of %s detections per photo (p = %s).",
    sig5, ord, format_p(p5)
  )
}


###########################################################################################################################
# Test 6: Session x elevation on detections per photo (Figure 4 caption statistics)
#   Continuous elevation_n (not elevation_band); random effects: site + site_night.
#   hour: omnibus F (anova); elevation_n: slope t-test; interaction: F-test.
###########################################################################################################################

result_6 <- character(length(focal_orders))
names(result_6) <- focal_orders
test6_p <- list()
for (ord in focal_orders) {
  df6 <- rates_session_long %>% filter(order == ord)
  m6 <- lmer(rate ~ hour * elevation_n + (1 | site) + (1 | site_night), data = df6, REML = FALSE)
  sm6 <- summary(m6)$coefficients
  a6 <- anova(m6)
  p_hour6 <- a6["hour", "Pr(>F)"]
  p_elev6 <- sm6["elevation_n", "Pr(>|t|)"]
  p_int6 <- if ("hour:elevation_n" %in% rownames(a6)) a6["hour:elevation_n", "Pr(>F)"] else NA_real_
  test6_p[[ord]] <- list(hour = p_hour6, elevation = p_elev6, interaction = p_int6)
  result_6[ord] <- sprintf(
    "%s: session %s (p = %s), elevation %s (p = %s), interaction %s (p = %s).",
    ord, sig_word(p_hour6), format_p(p_hour6),
    sig_word(p_elev6), format_p(p_elev6),
    sig_word(p_int6), format_p(p_int6)
  )
}

# Draft sentences for Figure 4 caption (focal orders only; left "All orders" panel is descriptive).
fig4_caption_parts <- vapply(focal_orders, function(ord) {
  p <- test6_p[[ord]]
  sprintf(
    "%s: session %s (p = %s), elevation (continuous) %s (p = %s), session x elevation %s (p = %s)",
    ord, sig_word(p$hour), format_p(p$hour),
    sig_word(p$elevation), format_p(p$elevation),
    sig_word(p$interaction), format_p(p$interaction)
  )
}, character(1))
fig4_caption_text <- paste(
  "For focal orders (mixed models, continuous elevation, alpha = 0.05):",
  paste(fig4_caption_parts, collapse = "; "),
  sep = " "
)


###########################################################################################################################
# Export: one-line results, plot annotations, model families, Fig 4 caption draft
###########################################################################################################################

results_df <- tibble(
  test = c(
    "1_mean_detections_per_photo_elevation",
    "2_richness_elevation",
    "3_shannon_elevation",
    paste0("4_order_", focal_orders, "_elevation"),
    paste0("5_session_", focal_orders),
    paste0("6_session_x_elevation_", focal_orders)
  ),
  statement = c(
    result_1,
    result_2,
    result_3,
    result_4[focal_orders],
    result_5[focal_orders],
    result_6[focal_orders]
  )
)

families_df <- tibble(
  test = c(
    "1_mean_detections_per_photo_elevation",
    "2_richness_elevation",
    "3_shannon_elevation",
    paste0("4_order_", focal_orders, "_elevation")
  ),
  family = c(s1$family, s2$family, s3$family, unlist(order_family[focal_orders], use.names = FALSE))
)

cat("\n========== Statistical results (one-line statements) ==========\n\n")
cat(result_1, "\n")
cat(result_2, "\n")
cat(result_3, "\n\n")
cat("Per order (elevation):\n")
for (ord in focal_orders) cat("  ", result_4[ord], "\n")
cat("\nPer order (session):\n")
for (ord in focal_orders) cat("  ", result_5[ord], "\n")
cat("\nPer order (session x elevation):\n")
for (ord in focal_orders) cat("  ", result_6[ord], "\n")
cat("\n--- Figure 4 caption draft (Test 6) ---\n")
cat(fig4_caption_text, "\n")
cat("\n================================================================\n")

write.csv(results_df, file.path(dir_out, "statistics_one_line_results.csv"), row.names = FALSE)
write.csv(results_df, file.path(dir_out, "results_summary.csv"), row.names = FALSE)
write.csv(results_df, "agentic_hangout/output/statistics_one_line_results.csv", row.names = FALSE)
write.csv(families_df, file.path(dir_out, "model_families.csv"), row.names = FALSE)
writeLines(fig4_caption_text, file.path(dir_out, "figure4_caption_stats_draft.txt"))

pval_label <- function(p) {
  if (!is.finite(p)) "p = NA" else if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
}
plot_annot <- tibble(
  test = c(
    "activity_per_photo_elevation",
    "2_richness_elevation",
    "3_shannon_elevation",
    paste0("4_order_", focal_orders, "_elevation")
  ),
  p_value = c(p1, p2, p3, unlist(order_p[focal_orders], use.names = FALSE)),
  label = c(
    pval_label(p1), pval_label(p2), pval_label(p3),
    vapply(focal_orders, function(o) pval_label(order_p[[o]]), character(1))
  ),
  fontface = c(
    if (p1 < alpha) "bold" else "plain",
    if (p2 < alpha) "bold" else "plain",
    if (p3 < alpha) "bold" else "plain",
    vapply(focal_orders, function(o) if (order_p[[o]] < alpha) "bold" else "plain", character(1))
  )
)
write.csv(plot_annot, file.path(dir_out, "statistics_plot_annotations.csv"), row.names = FALSE)

##################################
##################################
####### NEW ######################
##################################
##################################

###########################################################################################################################
# Figure 6 support: all-insect session rates and hour LMM predictions (2-visualization.R overlay)
#   Not part of Tests 1–6; supplies data_processed CSVs for the hour x elevation factor plot.
###########################################################################################################################

session_rate_fig6 <- data %>%
  group_by(site_night, hour) %>%
  summarise(detections = n(), .groups = "drop") %>%
  left_join(
    session_effort %>% select(site_night, hour, n_photos, elevation, elevation_n),
    by = c("site_night", "hour")
  ) %>%
  mutate(
    rate = detections / n_photos,
    site = sub("_.*", "", site_night)
  ) %>%
  filter(n_photos > 0)

elev_hour_means_fig6 <- session_rate_fig6 %>%
  group_by(elevation_n, hour) %>%
  summarise(
    mean_rate = mean(rate, na.rm = TRUE),
    n_sessions = n(),
    .groups = "drop"
  )

predict_hour_lmm <- function(model, hour_levels) {
  newdata <- data.frame(hour = factor(hour_levels, levels = hour_levels))
  pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
  fit_vals <- as.numeric(pred$fit)
  se <- as.numeric(pred$se.fit)
  data.frame(
    hour = factor(hour_levels, levels = hour_levels),
    fit = fit_vals,
    ymin = fit_vals - 1.96 * se,
    ymax = fit_vals + 1.96 * se
  )
}

hour_levels_fig6 <- levels(session_rate_fig6$hour)
m_fig6_hour <- lmer(
  rate ~ hour + (1 | site) + (1 | site_night),
  data = session_rate_fig6,
  REML = TRUE
)
pred_fig6_hour <- predict_hour_lmm(m_fig6_hour, hour_levels_fig6)

write.csv(session_rate_fig6, "data_processed/activity_by_hour_session_rate.csv", row.names = FALSE)
write.csv(elev_hour_means_fig6, "data_processed/activity_by_hour_elevation_summary.csv", row.names = FALSE)
write.csv(pred_fig6_hour, "data_processed/activity_by_hour_lmm_predictions.csv", row.names = FALSE)

##################################
##################################
####### NEW ######################
##################################
##################################


###########################################################################################################################
### OLD: categorical ANOVA (replaced by continuous GLMM/LMM above)
###########################################################################################################################

# m1 <- lm(rate ~ factor(elevation), data = fig1_data)
# m2 <- lm(insect_richness ~ factor(elevation), data = hoya_data_elev)
# m3 <- lm(insect_shannon ~ factor(elevation), data = hoya_data_elev)
# m4 <- lm(rate ~ factor(elevation), data = df4)
# m5 <- lm(rate ~ hour, data = df5)
# m6 <- lm(rate ~ hour * elevation_band, data = df6)
