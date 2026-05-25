# Elevation GLMM statistics (simplified)

Manuscript-focused significance tests: continuous elevation, site random effects, count GLMMs for detections/richness, LMM for Shannon.

## Run (project root)

```bash
Rscript code/3-statistics.R
```

Development wrapper (same analysis):

```bash
Rscript agentic_hangout/elevation_glmm/analysis_elevation_statistics.R
```

Canonical implementation: [`code/3-statistics.R`](../../code/3-statistics.R).

## Contents

| File | Purpose |
|------|---------|
| `analysis_elevation_statistics.R` | All tests 1–6 + optional hour×elevation tables (~300 lines) |
| `STATISTICS_METHODS.md` | Draft Methods paragraph for the manuscript |
| `R/stats_annotations.R` | Load p-values for `viz_*` scripts |
| `output/results_summary.csv` | Main results table |
| `output/manuscript_statements.txt` | Copy-paste statements |

## Packages

`dplyr`, `tidyr`, `lme4`, `lmerTest`, `glmmTMB`

## Figures

After running the analysis, annotation CSV is read by:

- `agentic_hangout/viz_shannon_simpson_elevation.R`
- `agentic_hangout/viz_coleoptera_elevation.R`

Coleoptera family panels may show `p = NA` (family-level tests not run in the simplified script).
