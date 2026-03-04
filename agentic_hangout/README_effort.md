# Sampling-effort correction (agentic_hangout)

Effort = number of photos per site_night (`n_distinct(eventID)`). All outputs stay in `agentic_hangout/output/`.

## Run order

1. **`prep_effort.R`** — Run first. Reads `data_processed/data.csv`, computes `n_photos` per site_night, writes:
   - `site_night_effort.csv` (site_night, elevation, n_photos)
   - `effort_summary.txt`
   - `effort_vs_elevation.png`

2. **Viz/analysis scripts** — Read `agentic_hangout/output/site_night_effort.csv` and join effort where needed:
   - `viz_shannon_simpson_elevation.R` — activity per photo, rarefied richness/Shannon/Simpson (all insects)
   - `viz_coleoptera_elevation.R` — raw + detections per photo + rarefied metrics (Coleoptera)
   - `viz_coleoptera_by_family.R` — joins n_photos to family_site_data
   - `analysis_coleoptera_ordination.R` — raw NMDS + rarefied NMDS + PERMANOVA (elevation; elevation + n_photos)

## Rarefaction

- **By individuals:** each matrix is rarefied to `min(rowSums)` (minimum total detections per site_night). See `agentic_hangout/output/rarefaction_info.txt` for the numeric target(s).

## Effort-corrected outputs

| Output | Description |
|--------|-------------|
| Activity_per_photo_and_elevation.png | All insects: detections per photo vs elevation |
| Rarefied_richness_and_elevation.png | All insects: rarefied richness vs elevation |
| Rarefied_Shannon_and_elevation.png | All insects: rarefied Shannon vs elevation |
| Rarefied_Simpson_and_elevation.png | All insects: rarefied Simpson vs elevation |
| Coleoptera_detections_per_photo_elevation.png | Coleoptera: detections per photo vs elevation |
| Coleoptera_rarefied_richness_elevation.png | Coleoptera: rarefied richness vs elevation |
| Coleoptera_rarefied_Shannon_elevation.png | Coleoptera: rarefied Shannon vs elevation |
| Coleoptera_rarefied_Simpson_elevation.png | Coleoptera: rarefied Simpson vs elevation |
| Coleoptera_NMDS_rarefied_elevation.png | Coleoptera: NMDS on rarefied matrix |
| Coleoptera_PERMANOVA_rarefied_elevation.csv | PERMANOVA on rarefied matrix (elevation) |
| Coleoptera_PERMANOVA_elevation_and_effort.csv | PERMANOVA on raw matrix (elevation + n_photos) |

Raw (uncorrected) figures are unchanged and still produced.
