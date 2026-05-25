# Statistical methods — manuscript text

Draft **Methods** paragraph (replaces the one-way ANOVA / elevation-band wording). Inference uses **continuous elevation** and mixed models; bar plots may still show means by elevation for display.

## Methods paragraph (copy into manuscript)

We calculated species richness and Shannon diversity index using the ‘vegan’ package (Oksanen et al. 2026) in R v. 4.5.0 (R Core Team 2026). We tested the effects of **elevation (continuous, metres)** and **sampling session** (time of night) on insect activity, richness, and Shannon diversity. Activity was expressed as mean detections per photo (total detections divided by number of photos per site-night, or per site per session) to account for sampling effort.

For site-night-level responses, we used **generalized linear mixed models** (package `glmmTMB`) for detection counts and morphospecies richness, with **log(number of photos) as an offset**, elevation as a linear predictor, and a **random intercept for site** to account for repeated sampling nights at each elevation. Poisson and negative binomial families were compared by AIC; the better-fitting family was retained. **Shannon diversity** was analyzed with a **Gaussian linear mixed model** (`lme4`, `lmerTest`): Shannon index ~ elevation + (1 | site).

For the most frequently detected orders (Lepidoptera, Coleoptera, Hemiptera, and Diptera), we applied the same count mixed-model design to order-specific detections per site-night. We then tested the effect of sampling session on mean detections per photo for each focal order using **linear mixed models** with session as a fixed factor and random intercepts for **site** and **site-night**. Finally, we fitted **session × elevation** mixed models (continuous elevation, not elevation bands) for each focal order to test main effects of session and elevation and their interaction on mean detections per photo. Significance was assessed at α = 0.05 from model summaries and likelihood-ratio tests where appropriate.

## Design notes (for reviewers)

- Each **site** occurs at one elevation; multiple **nights** per site → `(1 | site)` at site-night level.
- Session models: `(1 | site) + (1 | site_night)`.
- Elevation is **not** treated as a categorical factor in primary inference.

## Run analysis

From project root:

```bash
Rscript code/3-statistics.R
```

Or the development wrapper (sources the same script):

```bash
Rscript agentic_hangout/elevation_glmm/analysis_elevation_statistics.R
```

## Outputs

`agentic_hangout/elevation_glmm/output/`:

- `results_summary.csv` — test and one-line statement
- `statistics_one_line_results.csv` — same table (legacy path: `agentic_hangout/output/statistics_one_line_results.csv`)
- `statistics_plot_annotations.csv` — p-values for agentic viz scripts
- `model_families.csv` — Poisson vs negative binomial (or Gaussian) chosen per elevation GLMM
- `figure4_caption_stats_draft.txt` — Test 6 sentences for Figure 4 caption (focal orders)
- `model_families.csv` — Poisson vs negative binomial (or Gaussian) chosen per elevation GLMM
- `figure4_caption_stats_draft.txt` — Test 6 sentences for Figure 4 caption (focal orders)

## Packages

`dplyr`, `tidyr`, `lme4`, `lmerTest`, `glmmTMB`; `vegan` optional (Simpson index for figure annotations only).
