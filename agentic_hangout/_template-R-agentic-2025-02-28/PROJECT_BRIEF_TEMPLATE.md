# [Project name] — Project Brief

Read this at the start of any task in this repo.

## What this project is

[Short project description: data type, domain, and main question.  
Example: "Cleaning and combining species lists from ...", or "Occurrence data from ... with focus on ...".]

## Data inputs

- **Primary language(s)**: [Primary language(s): R, Python, both, etc.]
- **Main script folder**: [Main script folder: e.g. `code/` or `scripts/`]
- **Raw data location(s)**: [Key input files and paths: e.g. `data_raw/my_data.csv`, `data_raw/metadata.csv`]
- **Key columns / identifiers**: [Brief list of important columns such as IDs, dates, groups, response variables.]

## Pipeline / scripts

[Describe the main pipeline or scripts used in this project.  
Examples:  
- "Single script: `scripts/clean_species_lists.R`"  
- "Order: `code/0-data_cleanup.R` → `code/1-data_analysis.R` → `code/2-visualization.R`"  
- "One or more notebooks in `code/` or `agentic_hangout/`".]

All paths are relative to the **project root**; run scripts from the project root so paths resolve correctly.

## Outputs

- **Main output folders**: [Main output folders: e.g. `data_processed/`, `output/`, `agentic_hangout/output/`]
- **Key outputs you care about**: [Short list of expected outputs such as cleaned CSVs, summary tables, and core figures.]

## Conventions

- **New or experimental scripts** and **any docs for working with the AI** go in **`agentic_hangout/`**.
- **Outputs** of those scripts (data files, figures, tables) also stay in **`agentic_hangout/`** (or a subfolder like `agentic_hangout/output/`) until you approve them for the main pipeline. Do not write to [Main output folders: data_processed/, output/, etc.] from `agentic_hangout/` scripts unless you explicitly ask.
- **Approved, stable changes** go in [Main script folder: code/ or scripts/].
- Prefer **R** and tidyverse-style libraries where they fit; use other languages or tools as needed.
- Run from the project root so relative paths work.

## Goals / expansion

[Short list of what you might add later.  
Examples: "More list sources, validation checks, output formats", "More diversity indices", "Additional figures or reports".]

## Project specifics (optional)

[Optional free-text notes for quirks, constraints, or important decisions.  
Examples: "Do not modify file X", "Filter out rows where Y", "These columns are known to be messy".]
