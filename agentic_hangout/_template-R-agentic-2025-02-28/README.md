# R + agentic template (2025-02-28)

This folder is a reusable template for starting data-analysis projects that use an AI agent (e.g. Cursor). It is **R-first**, but the structure also works for Python or mixed-language projects.

**Quick setup:** Copy this folder into your new project → ensure the project root has `agentic_hangout/`, `code/` (or `scripts/`), `data_raw/`, `data_processed/`, `output/`, and `.cursor/rules/` (create them or copy from inside this template) → use the **standard Plan bootstrap prompt** below in the agent to complete the brief and rule. Details in **HOW_TO_USE.md**.

## Getting the project structure

After copying this template folder into the new project, the **new project root** must have: `agentic_hangout/`, `code/` (or your main script folder), `data_raw/`, `data_processed/`, `output/`, `.cursor/rules/`. If any are missing, create them (empty) or copy the corresponding folder from inside this template to the project root.

If your main script folder is not `code/` (for example, you prefer `scripts/`), you can still use this template: just make sure the project brief and Cursor rule refer to the correct folder.

## Folder structure

| Folder | Purpose |
|--------|--------|
| **`agentic_hangout/`** | New or experimental scripts, notebooks, and AI-related docs. Outputs from here stay here until you approve; then you move or copy into the main pipeline. |
| **`code/`** | Approved, stable analysis scripts (main pipeline). Can contain `.R`, `.py`, notebooks, etc. Not modified by the agent unless you explicitly ask. |
| **`data_raw/`** | Raw input data (e.g. CSVs). Read-only for the pipeline; never modify or overwrite unless you ask. |
| **`data_processed/`** | Cleaned or derived data produced by the pipeline (e.g. processed CSVs, intermediate analysis tables). Not written to from `agentic_hangout/` unless you approve. |
| **`output/`** | Figures, tables, and other final outputs (e.g. PNGs, JPGs, summary CSVs). Not written to from `agentic_hangout/` unless you approve. |
| **`.cursor/rules/`** | Cursor rule file(s) for this project. Not modified unless you explicitly ask. |

The agent is instructed not to create, modify, or delete files in `code/`, `data_raw/`, `data_processed/`, `output/`, or `.cursor/rules/` without your explicit permission.

This template also contains an **inner `agentic_hangout/`** (with `README.txt`, `output/`, and setup notes). Use it as the skeleton for the project’s `agentic_hangout/`: ensure your project root has an `agentic_hangout/`, then copy **PROJECT_BRIEF_TEMPLATE.md** there as **PROJECT_BRIEF.md** and fill in the placeholders.

## Where to put raw, processed, and final outputs

- Put **raw CSVs and other unmodified inputs** in `data_raw/`.
- Put **cleaned or derived tables** that are part of the main pipeline in `data_processed/`.
- Put **final figures and tables** for reports or papers in `output/`.
- During experimentation, prefer writing all new outputs from `agentic_hangout/` scripts into `agentic_hangout/` (or `agentic_hangout/output/`) until you are happy with them.

## Target state after setup

When setup is complete, the **project root** should contain:

- `agentic_hangout/` with `PROJECT_BRIEF.md` filled in (and optional `ANALYSIS_NOTES.md`).
- A main script folder (usually `code/`, but you can use `scripts/` if you prefer).
- `data_raw/`, `data_processed/`, `output/`.
- `.cursor/rules/` with one `.mdc` rule file whose placeholders are replaced and whose `PROJECT_BRIEF` path is correct.

The template folder (for example, `_template-R-agentic-2025-02-28/`) can stay for reference or be removed once everything is wired up.

## Standard Plan bootstrap prompt for a new project

Copy the following into your **first message to Cursor Plan** after you have copied the template into a new project and placed your raw data in `data_raw/`:

---

This project was set up from an R + agentic template. The unaltered template folder (if present) is `agentic_hangout/_template-R-agentic-2025-02-28/`. The rest of the repository either matches that template or has been modified according to the setup instructions.

Please do the following steps as a Plan, asking me questions where needed and waiting for confirmation before editing files:

1. **Validate the project structure**
   - Check that the project root contains: `agentic_hangout/`, a main script folder (usually `code/` or `scripts/`), `data_raw/`, `data_processed/`, `output/`, and `.cursor/rules/`.
   - Tell me if any of these are missing or if something looks obviously mis-placed.

2. **Inspect and help fill `agentic_hangout/PROJECT_BRIEF.md`**
   - Open `agentic_hangout/PROJECT_BRIEF.md` (copied from the template).
   - List any remaining placeholders such as `[Project name]`, `[Short project description]`, `[Primary language(s): ...]`, `[Main script folder: ...]`, `[Main output folders: ...]`, and `[Key input files and paths]`.
   - For each placeholder, ask me targeted questions to gather the information you need. Examples:
     - Which CSVs or other files in `data_raw/` are the primary inputs?
     - What are the key columns or identifiers (e.g. ID, date, group, response variables)?
     - Which folder do I consider the main script folder (`code/`, `scripts/`, something else)?
     - What kinds of outputs (cleaned CSVs, analysis tables, figures) do I expect, and where should they live?
   - Propose **exact text replacements** for each placeholder in `PROJECT_BRIEF.md` and wait for my confirmation before editing the file.

3. **Inspect and help fill the Cursor rule**
   - Find the `.mdc` rule file in `.cursor/rules/` that was copied from `cursor-rule-template.mdc`.
   - Identify any placeholders like `[Project name]`, `[path to PROJECT_BRIEF, e.g. agentic_hangout/PROJECT_BRIEF.md]`, and `[Main script folder: code/ or scripts/]`.
   - Based on our answers from step 2, propose concrete values for each placeholder (for example: project name, brief path, main script folder).
   - After I confirm, update the rule file so that:
     - The project name is set.
     - The `PROJECT_BRIEF` path points to `agentic_hangout/PROJECT_BRIEF.md` (or the correct path if we move it).
     - The main script folder placeholder matches this project (e.g. `code/` or `scripts/`).

4. **Capture project-specific details for future work**
   - Ask me a short series of questions to capture important project details that will be useful later, such as:
     - Any known quirks in the data (missing values, encodings, units).
     - Important identifiers (e.g. site, date, individual ID).
     - Priority outputs I care about most (particular cleaned tables, summary CSVs, or key figures).
     - Any constraints or rules (for example: \"never modify this raw file\", \"respect this exclusion\", \"these columns are known to be messy\").
   - Append a short **“Project specifics”** section to `agentic_hangout/PROJECT_BRIEF.md` (or, if present, to `agentic_hangout/ANALYSIS_NOTES.md`), summarizing those answers.

5. **Confirm that setup is complete**
   - Summarize back to me:
     - What this project is and what language(s) it uses.
     - Where the raw data live and which files are most important.
     - Where new experimental work should go (`agentic_hangout/`).
     - Which folder is the main script folder (e.g. `code/` or `scripts/`).
   - Then state explicitly: **\"Setup complete; you can now ask for specific analyses or scripts.\"**

After these steps, I will start giving you concrete analysis or scripting tasks within this project.
