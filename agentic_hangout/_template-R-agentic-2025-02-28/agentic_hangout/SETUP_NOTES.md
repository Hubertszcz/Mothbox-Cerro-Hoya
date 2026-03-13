# Setup notes for new template-based projects

Use this as a quick, human-facing checklist when starting a new project from this template.

## Minimal checklist

1. **Copy the template**
   - Copy the folder `_template-R-agentic-2025-02-28/` from the source repo into your new project.

2. **Ensure the project root has the expected folders**
   - `agentic_hangout/`
   - main script folder (usually `code/`, but `scripts/` is fine too)
   - `data_raw/`
   - `data_processed/`
   - `output/`
   - `.cursor/rules/`

3. **Wire up the brief**
   - Copy `PROJECT_BRIEF_TEMPLATE.md` from the template into `agentic_hangout/PROJECT_BRIEF.md` in the new project (overwrite if you already have a brief you want to replace).

4. **Install the Cursor rule**
   - Copy `cursor-rule-template.mdc` from the template into `.cursor/rules/<project-name>.mdc` in the new project.

5. **Open the project in Cursor and run the bootstrap Plan**
   - Open the new project in Cursor.
   - In a fresh chat, paste the **“Standard Plan bootstrap prompt for a new project”** from the template’s `README.md`.
   - Answer the questions so the agent can:
     - Validate the folder structure.
     - Fill in `agentic_hangout/PROJECT_BRIEF.md`.
     - Fill in the `.cursor/rules/*.mdc` rule.
     - Capture any important project specifics.

## Optional notes

- You can keep the template folder in the project for reference or delete it after setup.
- If you prefer a script folder name other than `code/` (for example `scripts/`), just make sure both the brief and the Cursor rule use the same folder name.
- If you want a log of analysis decisions, create or keep `agentic_hangout/ANALYSIS_NOTES.md` and add short dated entries when you make meaningful changes to the pipeline or filters.

