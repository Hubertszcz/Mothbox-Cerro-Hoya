# [Project name] — Project Brief

Read this at the start of any task in this repo.

## What this project is

[One paragraph: data type, goal. E.g. "Cleaning and combining species lists from ..." or "Occurrence data from ... with focus on ..."]

## Pipeline / scripts

[E.g. "Single script: scripts/clean_species_lists.R" or "Order: code/1-clean.R → code/2-combine.R"]

All paths are relative to the **project root**; run scripts from the project root so paths resolve correctly.

## Conventions

- **New or experimental scripts** and **any docs for working with the AI** go in **`agentic_hangout/`**.
- **Outputs** of those scripts (data files, figures, tables) also stay in **`agentic_hangout/`** (or a subfolder like `agentic_hangout/output/`) until you approve them for the main pipeline. Do not write to [data_processed/ or output/ — replace with your main output folders] from agentic_hangout scripts unless you explicitly ask.
- **Approved, stable changes** go in [code/ or scripts/ — replace with your main script folder].
- Prefer **R**; use existing packages where possible.
- Run from project root so relative paths work.

## Goals / expansion

[Short list of what you might add later. E.g. "More list sources, validation checks, output formats."]
