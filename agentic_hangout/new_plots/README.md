# Continuous elevation GLM scatter plots

Site-night scatter plots with **continuous elevation** on the x-axis and **GLMM/LMM** trend lines (population-level predictions). Replaces the categorical bar-chart view for manuscript figures responding to reviewer feedback.

## Models

Same design as [`elevation_glmm/STATISTICS_METHODS.md`](../elevation_glmm/STATISTICS_METHODS.md) and `code/3-statistics.R`:

- **Detections per photo**: Poisson or negative binomial GLMM on counts with `log(n_photos)` offset; trend shown on per-photo scale.
- **Richness**: Poisson or negative binomial GLMM with photo offset and `(1 | site)`.
- **Shannon**: Gaussian linear mixed model, `(1 | site)`.

1416 m is excluded. One point per site-night.

## Run

From the project root:

```bash
Rscript agentic_hangout/new_plots/viz_elevation_continuous_glm.R
```

For inference (p-values, model summaries), run `agentic_hangout/elevation_glmm/analysis_elevation_statistics.R` separately.

## Residual diagnostics (DHARMa)

The visualization script runs **DHARMa** checks on the same three models (requires `install.packages("DHARMa")`). Outputs:

- `output/diagnostics/*_DHARMa.png` — QQ and residual plots
- `output/diagnostics/DHARMa_tests.csv` — uniformity and dispersion tests
- `output/diagnostics/DHARMa_summary.txt` — short text summary

## Outputs

`agentic_hangout/new_plots/output/`:

- `Detections_and_elevation_continuous.png`
- `Richness_and_elevation_continuous.png`
- `Shannon_and_elevation_continuous.png`
