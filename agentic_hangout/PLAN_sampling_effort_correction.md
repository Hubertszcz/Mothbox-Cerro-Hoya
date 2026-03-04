# Plan: Sampling-effort correction (agentic_hangout only)

This plan describes how to add sampling-effort correction to the Mothbox Cerro Hoya analysis workflow **without modifying** `code/0-data_cleanup.R`, `code/1-data_analysis.R`, or `code/2-visualization.R`. All new or changed logic and outputs live under **`agentic_hangout/`** until explicitly approved for the main pipeline.

---

## 1. Definition and source of effort

**Definition**

- **Effort** at the site_night level = number of distinct photos (images) for that site_night.
- In the occurrence data, each row is one detection; **`eventID`** identifies the image (photo). So one eventID = one photo; the same photo can have multiple rows (multiple detections).
- Effort per site_night = **`n_distinct(eventID)`** when grouping `data` by `site_night`.
- Store this as a column named **`n_photos`** (or **`effort`**) in any site_night-level table. Use one name consistently (e.g. `n_photos`) across scripts.

**Data source**

- **`data_processed/data.csv`** (read-only). This is the cleaned occurrence data produced by the pipeline and already contains `eventID`, `site_night`, and `elevation`.
- Effort is **derived** from this table; it is not written back to `data_processed/`. It is computed whenever we build a site_night-level dataset (in agentic scripts) or in a dedicated prep step that writes to `agentic_hangout/output/`.

**When it is computed**

- **Option A (recommended):** In a single, reusable step that runs first. That step reads `data_processed/data.csv`, computes per site_night: `n_photos = n_distinct(eventID)`, and optionally `elevation` (from the same data). It writes a site_night-level table to e.g. **`agentic_hangout/output/site_night_effort.csv`** with columns: `site_night`, `elevation`, `n_photos`. Downstream agentic scripts then read this file (and optionally still read `data.csv` for detection-level metrics).
- **Option B:** Each agentic script that builds a site_night-level table computes effort inline from `data` (e.g. `effort_table <- data %>% group_by(site_night) %>% summarise(n_photos = n_distinct(eventID), .groups = "drop")`) and joins it to the site_night table. No separate effort file.
- Plan assumes **Option A** for consistency and so effort (and any future effort-related variables) are defined in one place. Option B is valid if you prefer to avoid an extra file.

---

## 2. Adding effort to the dataset(s) used for analysis

**Site_night-level table (all insects)**

- Where scripts currently build a table with one row per `site_night` (e.g. `hoya_data` with columns such as `site_night`, `elevation`, `insect_activity`, `insect_richness`, `insect_shannon`):
  - Join the effort table so that every row has **`n_photos`** (and optionally `elevation` if not already present).
  - Example: after building `hoya_data` from the wide matrix and joining `elev_table`, add:  
    `hoya_data <- hoya_data %>% left_join(effort_table, by = "site_night")`  
    so that `hoya_data` has a column `n_photos`.

**Coleoptera-only and by-family**

- For Coleoptera-only analyses, the same effort applies: effort is per site_night, not per taxon. So the same **`site_night_effort.csv`** (or the same inline computation) is joined to the Coleoptera site_night-level table (e.g. the table that has `site_night`, `elevation`, `activity`, `richness`, `shannon`, `simpson` for beetles).
- For by-family scripts, the site_night-level data per family (e.g. `family_site_data` with `site_night`, `family`, `elevation`, `activity`, `richness`, `shannon`) should also get **`n_photos`** via a join on `site_night`, so that effort-corrected metrics or diagnostics can be computed per family if needed.

**Summary**

- Every site_night-level analysis table used for activity, richness, diversity, or ordination should include a column **`n_photos`** (or `effort`) so that downstream steps can use it for rates, offsets, or rarefaction.

---

## 3. Using effort in activity, richness, diversity, and ordination

### 3.1 Activity (total detections)

**Current:** Activity = total detections per site_night (e.g. `rowSums(data_counts)`), with no effort.

**Effort-corrected options:**

1. **Rate (detections per photo)**  
   - New variable: **`activity_per_photo`** = `activity / n_photos` (or `activity / effort`).  
   - Use this as the response in plots and models (e.g. “Detections per photo” vs elevation).  
   - Elevation summaries: mean and SE of `activity_per_photo` by elevation (same style as current bar plots).  
   - ANOVA: `aov(activity_per_photo ~ elevation, data = hoya_data)` (optionally check homogeneity of variance / residuals).

2. **Model with offset (optional, for more advanced use)**  
   - If using a count model (e.g. Poisson/negative binomial) in the future: include **`offset(log(n_photos))`** so that the model interprets the response as a rate.  
   - Not required for the current bar plots and ANOVAs; the rate approach above is enough for the planned figures.

**Scripts to touch (agentic_hangout):**

- **`viz_shannon_simpson_elevation.R`** (if/when effort is added to the “all insects” workflow): add effort join; compute and plot **mean activity per photo** vs elevation (and optionally keep a raw-activity figure for comparison).  
- **`viz_coleoptera_elevation.R`**: add effort join to the Coleoptera site_night table; add a figure “Coleoptera detections per photo vs elevation” (and optionally keep raw detections figure).  
- Any new or existing script that plots “total detections” vs elevation should have an effort-corrected counterpart (rate) or document why raw counts are retained.

### 3.2 Richness

**Current:** Richness = number of morphospecies (or taxa) per site_night, e.g. `specnumber(data_counts)`.

**Effort-corrected options:**

1. **Rarefaction (recommended for comparability)**  
   - Rarefy the site × taxon abundance matrix to a common sample size (number of individuals) per site_night.  
   - In R/vegan: **`rrarefy()`** rarefies by number of individuals (total detections), not by number of photos. So “effort” for rarefaction can be either:  
     - **By individuals:** rarefy to e.g. `min(rowSums(matrix))` or a chosen quantile (e.g. 5th percentile of row sums) so that richness is comparable across site_nights with different total detections.  
     - **By photos:** first aggregate counts “per photo” (e.g. divide or scale by n_photos), or rarefy to a common number of *photos* by sub-sampling photos (eventIDs) then pooling detections—more involved; see “Rarefaction by photos” below.  
   - Simplest and standard: **rarefy by number of individuals** to e.g. `min(rowSums(data_counts))` (or a fixed value if min is too small). This controls for total sampling effort in terms of detections.  
   - Add a column **`richness_rare`** (or similar) to the site_night table: for each row, rarefy that row’s counts to the chosen size and compute `specnumber` on the rarefied vector.  
   - Plot “Rarefied richness vs elevation” (mean ± SE by elevation) in the same style as current richness plot. Keep raw richness as optional comparison.

2. **Effort as covariate**  
   - In models: e.g. `richness ~ elevation + n_photos` or `richness ~ elevation + log(n_photos)` to partial out effort.  
   - Useful for regression; for simple elevation bar plots, rarefaction is more interpretable.

**Rarefaction by photos (optional, more complex)**

- To rarefy by “number of photos” rather than “number of individuals”: for each site_night, sub-sample a fixed number of eventIDs (e.g. without replacement), pool detections from those photos only, then compute richness (and optionally diversity) on that subset. Repeat multiple times and average (e.g. mean rarefied richness).  
- This requires detection-level data and grouping by `eventID` and `site_night`; possible in agentic scripts but more code. Can be a later extension.

**Scripts to touch (agentic_hangout):**

- **`viz_shannon_simpson_elevation.R`**: after building the wide matrix and site_night table, add rarefied richness (rarefy by individuals to a common size); add to summary by elevation; add a figure “Rarefied richness vs elevation” (and optionally keep raw richness).  
- **`viz_coleoptera_elevation.R`**: same for Coleoptera (rarefied richness column + figure).  
- **`viz_coleoptera_by_family.R`**: per family, rarefaction is trickier (smaller counts); either rarefy per family to a common size or report raw richness and add a note about effort. If effort is joined, at least plot effort per site_night in diagnostics.

### 3.3 Diversity (Shannon, Simpson)

**Current:** Shannon and Simpson per site_night from the raw count vector (e.g. `diversity(data_counts, index = "shannon")`).

**Effort-corrected options:**

1. **Rarefaction (recommended)**  
   - Same as richness: rarefy each site_night’s count vector to a common number of individuals (e.g. `min(rowSums)` or a set value).  
   - Compute Shannon and Simpson on the **rarefied** vector per site_night; store as e.g. **`shannon_rare`**, **`simpson_rare`**.  
   - Plot “Mean rarefied Shannon (or Simpson) vs elevation” in the same style as current diversity figures. Optionally keep raw diversity figures for comparison.

2. **Effort as covariate**  
   - In ANOVA or regression: e.g. `shannon ~ elevation + n_photos` to account for effort.  
   - Complements rarefaction; rarefaction is more directly comparable across site_nights.

**Scripts to touch (agentic_hangout):**

- **`viz_shannon_simpson_elevation.R`**: add rarefied Shannon and Simpson; add figures “Rarefied Shannon vs elevation” and “Rarefied Simpson vs elevation” (and optionally keep raw).  
- **`viz_coleoptera_elevation.R`**: same for Coleoptera (rarefied Shannon and Simpson figures).  
- **`viz_coleoptera_by_family.R`**: optionally rarefy per family (if sample size allows) or keep raw and document effort in figure/diagnostics.

### 3.4 Ordination (Coleoptera)

**Current:** NMDS on the site_night × morphospecies abundance matrix (Bray–Curtis); PERMANOVA with elevation.

**Effort-corrected options:**

1. **Rarefy the community matrix**  
   - Rarefy each row (site_night) to a common number of individuals (e.g. `min(rowSums(mat))` or a chosen quantile).  
   - Run **`metaMDS()`** on the rarefied matrix.  
   - PERMANOVA: **`adonis2(rarefied_mat ~ elevation, ...)`** so the test is on effort-standardized composition.  
   - Keep the current (raw) NMDS as optional comparison (e.g. “Raw” vs “Rarefied” in the same script or two figures).

2. **Effort as covariate in PERMANOVA**  
   - **`adonis2(mat ~ elevation + n_photos, data = ..., permutations = 999, method = "bray")`** to test elevation after partialling out effort.  
   - Does not change the ordination display; only the test. Can be used in addition to rarefaction.

**Script to touch (agentic_hangout):**

- **`analysis_coleoptera_ordination.R`**:  
  - Add effort to the site_night table (join `n_photos`).  
  - Build a rarefied matrix (same rows/columns, rarefied row-wise to common size).  
  - Run NMDS and PERMANOVA on the rarefied matrix; save a second figure (e.g. “Coleoptera_NMDS_rarefied_elevation.png”) and optionally a second PERMANOVA table.  
  - Optionally add PERMANOVA with `elevation + n_photos` on the raw matrix for comparison.

---

## 4. New outputs and diagnostics (transparency)

All of these should be written under **`agentic_hangout/output/`** (or a subfolder such as `agentic_hangout/output/effort/`).

1. **Effort per site_night table**  
   - **`site_night_effort.csv`** (or similar): columns at least `site_night`, `elevation`, `n_photos`.  
   - Produced by the “effort prep” step (Option A in §1). Enables checking and reuse.

2. **Effort summary statistics**  
   - Simple summary: min, max, mean, median, SD of `n_photos` (overall and optionally by elevation).  
   - Can be printed in the prep script or written to **`agentic_hangout/output/effort_summary.txt`** (or CSV).

3. **Effort vs elevation plot**  
   - Figure: **Effort (n_photos) vs elevation** (e.g. boxplot or points per site_night, or mean ± SE by elevation).  
   - File e.g. **`agentic_hangout/output/effort_vs_elevation.png`**.  
   - Makes it clear whether effort varies with elevation and motivates effort correction.

4. **Rarefaction target (if used)**  
   - Document or save the chosen rarefaction size (e.g. “rarefied to X individuals”) in a short comment in the script or in **`agentic_hangout/output/rarefaction_info.txt`**, so results are reproducible.

5. **Optional: comparison figures**  
   - Side-by-side or overlaid “raw vs rate” (activity) or “raw vs rarefied” (richness/diversity) in one figure or two, so reviewers can see the effect of correction.

---

## 5. Implementation order (suggested)

1. **Effort definition and prep**  
   - Add script (e.g. **`agentic_hangout/prep_effort.R`**) that: reads `data_processed/data.csv`; computes `effort_table` with `site_night`, `elevation`, `n_photos`; writes **`agentic_hangout/output/site_night_effort.csv`**; optionally prints or writes effort summary and produces **`effort_vs_elevation.png`**.

2. **Add effort to existing analyses**  
   - In each script that builds a site_night-level table, join `site_night_effort.csv` (or compute effort inline if Option B) so that `n_photos` is present.

3. **Activity: rate**  
   - In the relevant scripts, compute `activity_per_photo` and add figures “Detections per photo vs elevation” (all insects and Coleoptera). Keep or drop raw detections figures as desired.

4. **Richness and diversity: rarefaction**  
   - Add rarefaction (by individuals) in **`viz_shannon_simpson_elevation.R`** and **`viz_coleoptera_elevation.R`**; add rarefied richness and rarefied Shannon/Simpson columns; add corresponding figures. Optionally in **`viz_coleoptera_by_family.R`** for families with enough counts.

5. **Ordination: rarefied NMDS and PERMANOVA**  
   - In **`analysis_coleoptera_ordination.R`**, add rarefied matrix, rarefied NMDS figure, and rarefied PERMANOVA; optionally PERMANOVA with effort as covariate on raw matrix.

6. **Documentation**  
   - Short note in **`agentic_hangout/README_effort.md`** (or inside this plan): which scripts use effort, which outputs are effort-corrected, and what the rarefaction target is.

---

## 6. What stays unchanged (until you approve)

- **`code/0-data_cleanup.R`**, **`code/1-data_analysis.R`**, **`code/2-visualization.R`**: no edits.  
- **`data_processed/`** and **`output/`**: no writes from agentic scripts.  
- All new or modified scripts and outputs remain under **`agentic_hangout/`** until you explicitly approve moving logic or outputs into the main pipeline.

This plan is concrete enough to implement step-by-step in agentic_hangout only, without touching the approved pipeline.
