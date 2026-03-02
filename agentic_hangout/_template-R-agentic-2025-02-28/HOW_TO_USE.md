# How to use this R + agentic template (2025-02-28)

Use this checklist when starting a **new R project** so the agent follows the same workflow (agentic_hangout, PROJECT_BRIEF, Cursor rule).

**Where this template lives (in this repo):** `agentic_hangout/_template-R-agentic-2025-02-28/`

**First time in a new project?** See the "First prompt to use in a new project" section in **README.md**; copy that prompt into your first message to the agent.

## 1. Copy template and get the project structure

- Copy the entire folder **`_template-R-agentic-2025-02-28`** (from this project’s `agentic_hangout/`) into your new project.
  - You can put it at the **new project’s root** (e.g. `newproject/_template-R-agentic-2025-02-28/`) or inside the new project’s **`agentic_hangout/`** (e.g. `newproject/agentic_hangout/_template-R-agentic-2025-02-28/`).
- Ensure the **new project root** has: `agentic_hangout/`, `code/`, `data_raw/`, `data_processed/`, `output/`, `.cursor/rules/`. If any are missing, create them (empty) or copy the folder from inside the template to the project root.
- You can keep the date in the name or rename the folder; the important part is the files inside.

## 2. Fill in the project brief

- The template includes an **inner `agentic_hangout/`** (with README.txt and output/). Use it as the skeleton: ensure your **project root** has an **`agentic_hangout/`**, then copy **`PROJECT_BRIEF_TEMPLATE.md`** from the template to **`agentic_hangout/PROJECT_BRIEF.md`** in the new project (overwrite if you already have a brief).
- Open `agentic_hangout/PROJECT_BRIEF.md` and replace every **`[placeholder]`** (see “Placeholders to replace” below for a quick list).

## 3. Install the Cursor rule

- Copy **`cursor-rule-template.mdc`** into **`.cursor/rules/`** in the new project.
- Rename it (e.g. `my-project.mdc`) so it’s clear which project it belongs to.
- Open the file and replace:
  - **`[Project name]`** → your project name (e.g. "Species list cleaner").
  - **`[path to PROJECT_BRIEF]`** → usually `agentic_hangout/PROJECT_BRIEF.md`.
  - **`[code/ or scripts/]`** → the folder where approved, stable scripts live (e.g. `scripts/` or `code/`).
- Keep `alwaysApply: true` in the frontmatter so the rule applies in that repo.

### Placeholders to replace

- **In PROJECT_BRIEF.md:** project name (title), “What this project is” (one paragraph), “Pipeline / scripts”, `[data_processed/ or output/]` (your main output folders), `[code/ or scripts/]` (your main script folder), “Goals / expansion”.
- **In the Cursor rule (.mdc):** `[Project name]`, `[path to PROJECT_BRIEF]` (usually `agentic_hangout/PROJECT_BRIEF.md`), `[code/ or scripts/]` (same as in the brief).

## 4. Optional: add a starter script

- The folders **`code/`** or **`scripts/`** already exist from step 1. If you want a starter script, copy **`script_stub.R`** from the template into that folder or into the new project’s `agentic_hangout/`, then rename and fill in paths and logic. Do **not** copy R code from other projects (e.g. Mothbox); keep domain logic project-specific.

## 5. Do not copy

- **Do not copy** R scripts from other projects (e.g. Mothbox) as-is. Use this template for **structure and rules** only; write or adapt R code for the new project’s purpose.

---

After this, the agent will: read the brief, put new work in `agentic_hangout/`, keep outputs there until you approve, and only edit the main script folder when you ask.
