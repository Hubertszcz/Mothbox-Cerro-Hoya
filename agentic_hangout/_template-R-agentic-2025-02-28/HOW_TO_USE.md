# How to use this R + agentic template (2025-02-28)

Use this checklist when starting a **new data-analysis project** so the agent follows the same workflow (`agentic_hangout/`, `PROJECT_BRIEF`, Cursor rule), whether the main analysis is in R, Python, or a mix (R is the primary example).

**Where this template lives (in this repo):** `agentic_hangout/_template-R-agentic-2025-02-28/`

**First time in a new project?** See the **“Standard Plan bootstrap prompt for a new project”** section in `README.md`; copy that prompt into your first message to Cursor Plan.

## 1. Copy template and get the project structure

- Copy the entire folder **`_template-R-agentic-2025-02-28`** (from this project’s `agentic_hangout/`) into your new project.
  - You can put it at the **new project’s root** (e.g. `newproject/_template-R-agentic-2025-02-28/`) or inside the new project’s **`agentic_hangout/`** (e.g. `newproject/agentic_hangout/_template-R-agentic-2025-02-28/`).
- Ensure the **new project root** has: `agentic_hangout/`, a main script folder (usually `code/`, but `scripts/` also works), `data_raw/`, `data_processed/`, `output/`, `.cursor/rules/`. If any are missing, create them (empty) or copy the folder from inside the template to the project root.
- You can keep the date in the name or rename the folder; the important part is the files inside.

Quick human checklist (also see `agentic_hangout/SETUP_NOTES.md` in the template):

1. Copy `_template-R-agentic-2025-02-28/` into the new project.
2. Make sure the project root has the folders listed above.
3. Copy `PROJECT_BRIEF_TEMPLATE.md` from the template into `agentic_hangout/PROJECT_BRIEF.md` in the new project (if it is not already there).
4. Copy `cursor-rule-template.mdc` from the template into `.cursor/rules/<project-name>.mdc` in the new project.
5. Open the project in Cursor and paste the standard Plan bootstrap prompt from `README.md`.

## 2. Fill in the project brief

- The template includes an **inner `agentic_hangout/`** (with `README.txt`, `output/`, and setup/analysis notes). Use it as the skeleton: ensure your **project root** has an **`agentic_hangout/`**, then copy **`PROJECT_BRIEF_TEMPLATE.md`** from the template to **`agentic_hangout/PROJECT_BRIEF.md`** in the new project (overwrite if you already have a brief).
- Open `agentic_hangout/PROJECT_BRIEF.md`. It contains structured placeholders such as:
  - `[Short project description]`
  - `[Primary language(s): R, Python, etc.]`
  - `[Main script folder: e.g. code/ or scripts/]`
  - `[Key input files and paths]`
  - `[Main output folders: data_processed/, output/, etc.]`
  - “Goals / expansion” and optional “Project specifics”
- When you use the standard Plan bootstrap prompt from `README.md`, the agent will:
  - Detect any remaining placeholders.
  - Ask you targeted questions.
  - Propose exact text to fill them in, and update the brief after you confirm.

## 3. Install the Cursor rule

- Copy **`cursor-rule-template.mdc`** into **`.cursor/rules/`** in the new project.
- Rename it (e.g. `my-project.mdc`) so it’s clear which project it belongs to.
- Open the file and replace:
  - **`[Project name]`** → your project name (e.g. "Species list cleaner").
  - **`[path to PROJECT_BRIEF, e.g. agentic_hangout/PROJECT_BRIEF.md]`** → usually `agentic_hangout/PROJECT_BRIEF.md`.
  - **`[Main script folder: code/ or scripts/]`** → the folder where approved, stable scripts live (e.g. `code/` or `scripts/`).
- Keep `alwaysApply: true` in the frontmatter so the rule applies in that repo.

When you use the standard Plan bootstrap prompt, the agent will double-check these placeholders and can propose the exact values for you.

### Placeholders to replace

- **In `PROJECT_BRIEF.md`:**
  - `[Project name]` (title).
  - `[Short project description]`.
  - `[Primary language(s): R, Python, etc.]`.
  - `[Main script folder: e.g. code/ or scripts/]`.
  - `[Key input files and paths]`.
  - `[Main output folders: data_processed/, output/, etc.]`.
  - Pipeline / scripts, “Goals / expansion”, and optional “Project specifics”.
- **In the Cursor rule (`.mdc`):**
  - `[Project name]`.
  - `[path to PROJECT_BRIEF, e.g. agentic_hangout/PROJECT_BRIEF.md]`.
  - `[Main script folder: code/ or scripts/]` (same as in the brief).

## 4. Optional: add a starter script

- The folders **`code/`** or **`scripts/`** already exist from step 1. If you want a starter script, copy **`script_stub.R`** from the template into that folder or into the new project’s `agentic_hangout/`, then rename and fill in paths and logic. Do **not** copy R code from other projects (e.g. Mothbox); keep domain logic project-specific.

## 5. Do not copy

- **Do not copy** R or Python scripts from other projects as-is. Use this template for **structure and rules** only; write or adapt analysis code for the new project’s purpose.

---

After this, the agent will: read the brief, put new work in `agentic_hangout/`, keep outputs there until you approve, and only edit the main script folder when you ask.
