# Analysis notes (template)

Use this file in new projects (copied as `agentic_hangout/ANALYSIS_NOTES.md`) to record important analysis decisions and quirks in the data.

## How to use

- Keep entries short and dated.
- Focus on decisions that will matter later (filters, exclusions, derived variables, column renames, etc.).
- You can refer to this file in chats with the agent so it understands past choices.

## Example entries

- 2025-03-01 — Excluded rows where `quality_flag == "bad"` from all analyses.
- 2025-03-02 — Aggregated counts by `site_id` and `date` before computing diversity metrics.
- 2025-03-05 — Treated missing `temperature_c` as `NA` and did not impute.

