# Mothbox Cerro Hoya — Project Brief

Read this at the start of any task in this repo.

## What this project is

- **Mothbox expedition data**: occurrence records plus metadata from the Cerro Hoya expedition (Hoya project only).
- **Elevation gradient**: sites span an elevation gradient; analyses use elevation as a key variable.
- **Insect detections**: data are insect detections (identifications, counts, etc.) from Mothbox traps.

## Pipeline (run in order)

1. **`code/0-data_cleanup.R`** — Reads from `data_raw/`, writes cleaned data to `data_processed/`.
2. **`code/1-data_analysis.R`** — Reads from `data_processed/`, writes analysis outputs (e.g. CSVs) to `data_processed/` (or as agreed).
3. **`code/2-visualization.R`** — Reads from `data_processed/` (and analysis outputs), writes figures (e.g. PNGs) to `output/`.

All paths are relative to the **project root**; run scripts from the project root so `data_processed/` and `output/` resolve correctly.

## Conventions

- **New or experimental scripts** and **any docs for working with the AI** go in **`agentic_hangout/`**.
- **Outputs** of those scripts (data files, figures, tables) also stay in **`agentic_hangout/`** (or a subfolder like `agentic_hangout/output/`) until you approve them for the main pipeline. Do not write to `data_processed/` or `output/` from agentic_hangout scripts unless you explicitly ask.
- **Approved, stable changes** go in the existing **`code/*.R`** files.
- Prefer **R**; use existing packages (e.g. `dplyr`, `vegan`, `ggplot2`, `viridis`) where possible.
- Run from project root so paths like `data_processed/` and `output/` work.

## Expansion goals

- **Cleaning/validation**: more robust cleanup and validation (e.g. handling corrupted images; elevation 1204 m has “corrupted images” not yet excluded; 1416 m is excluded).
- **Analyses**: e.g. % identified to species, order-level summaries, other diversity/community metrics.
- **Visualizations**: e.g. Shannon diversity, time-of-night effects, additional elevation/diversity plots.
