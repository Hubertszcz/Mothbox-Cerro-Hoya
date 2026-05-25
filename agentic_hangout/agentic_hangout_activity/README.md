# Activity by hour and elevation (overlay figure)

Single-panel figure: session-level detections per photo across time of night, with one thin trend line per elevation, plus an overall mixed-model trend (black line) and 95% CI ribbon (grey).

## Run from project root

```r
source("agentic_hangout/prep_time_of_night.R")
source("agentic_hangout/agentic_hangout_activity/viz_activity_by_hour_and_elevation_factor.R")
```

## Outputs

- `output/activity_by_hour_and_elevation_factor.png`
- `output/activity_by_hour_and_elevation_factor.tif` (300 dpi, LZW compression)
- `output/activity_by_hour_and_elevation_summary.csv` (mean rate by elevation × hour)

## Notes

- All insect orders; same logic as Figure 6 in `code/2-visualization.R`.
- Excludes 1416 m; programA hours only (19h, 21h, 23h, 2h, 4h).
- Overall line: `lmer(rate ~ hour + (1 | site) + (1 | site_night))`, marginal predictions (`re.form = NA`). Grey band is a 95% CI for the population mean at each hour, not a prediction interval for new sessions.
