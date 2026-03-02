# R + agentic template (2025-02-28)

This folder is a reusable template for starting R projects that use an AI agent (e.g. Cursor).

**Quick setup:** Copy this folder into your new project → ensure the project root has `agentic_hangout/`, `code/`, `data_raw/`, `data_processed/`, `output/`, and `.cursor/rules/` (create them or copy from inside this template) → use the first prompt below in the agent to complete the brief and rule. Details in **HOW_TO_USE.md**.

## Getting the project structure

After copying this template folder into the new project, the **new project root** must have: `agentic_hangout/`, `code/`, `data_raw/`, `data_processed/`, `output/`, `.cursor/rules/`. If any are missing, create them (empty) or copy the corresponding folder from inside this template to the project root.

## Folder structure

| Folder | Purpose |
|--------|--------|
| **`agentic_hangout/`** | New or experimental scripts and AI-related docs. Outputs from here stay here until you approve; then you move or copy into the main pipeline. |
| **`code/`** | Approved, stable R scripts (main pipeline). Not modified by the agent unless you explicitly ask. |
| **`data_raw/`** | Raw input data. Read-only for the pipeline; never modify or overwrite unless you ask. |
| **`data_processed/`** | Cleaned or derived data produced by the pipeline. Not written to from agentic_hangout unless you approve. |
| **`output/`** | Figures, tables, and other final outputs. Not written to from agentic_hangout unless you approve. |
| **`.cursor/rules/`** | Cursor rule file(s) for this project. Not modified unless you explicitly ask. |

The agent is instructed not to create, modify, or delete files in `code/`, `data_raw/`, `data_processed/`, `output/`, or `.cursor/rules/` without your explicit permission.

This template also contains an **inner `agentic_hangout/`** (with README.txt and output/). Use it as the skeleton for the project’s `agentic_hangout/`: ensure your project root has an `agentic_hangout/`, then copy **PROJECT_BRIEF_TEMPLATE.md** there as **PROJECT_BRIEF.md** and fill in the placeholders.

## Target state after setup

When setup is complete, the **project root** should contain: `agentic_hangout/` (with PROJECT_BRIEF.md filled in), `code/`, `data_raw/`, `data_processed/`, `output/`, and `.cursor/rules/` (with one .mdc rule, placeholders replaced). The template folder (`_template-R-agentic-2025-02-28/`) can stay for reference or be removed.

## First prompt to use in a new project

Copy the following into your first message to the agent:

---

This project was set up from an R + agentic template. The template folder is **_template-R-agentic-2025-02-28/** at the project root (or **agentic_hangout/_template-R-agentic-2025-02-28/** if you placed it there). Please read HOW_TO_USE.md (and PROJECT_BRIEF_TEMPLATE.md if you need it) from that folder.

Then:
1. Check that the project has the expected structure: **agentic_hangout/** (with PROJECT_BRIEF.md), **code/**, **data_raw/**, **data_processed/**, **output/**, and **.cursor/rules/** with the project rule. Tell me if anything from the template checklist is missing.
2. Help me fill in agentic_hangout/PROJECT_BRIEF.md: I'll describe the project (e.g. purpose, main script, where data lives), and you suggest the exact text for each section so the brief is ready. If PROJECT_BRIEF.md still has [placeholders], we can replace them together.
3. If the Cursor rule has [placeholders] (project name, path to brief, scripts folder), suggest the values for this project so I can fill them in (or you can note what to replace).

Once the brief and rule are filled in, I'll use this project for [one sentence: e.g. "cleaning and combining species lists in R"].
