#' Create a standardized MetaLab project directory structure
#'
#' This function generates a complete directory structure for a new MetaLab project,
#' including folders for experiments, with nested data, analysis, code, and other subfolders.
#' It also creates an example experiment folder, template files (README, LICENSE,
#' analysis plan, manuscript draft), and a data dictionary template in JSON format
#' within the experiment's data/metadata/ folder.
#'
#' @param project_name A string. The name of the project folder to create.
#' @param experiment_name A string. The short name for the example experiment (default is "example-experiment").
#' @param base_path A string. The path where the project folder should be created (default is the current working directory).
#'
#' @return No return value. Creates folders and files in the specified location.
#'
#' @details
#' The directory structure includes:
#' \itemize{
#'   \item experiments/YYYY-MM-DD_experiment-name (with design/, stimuli/, scripts/, data/metadata/, analysis/, code/, and notes.md)
#'   \item manuscripts/YYYY-MM-DD_draft_manuscript.md
#'   \item reports, preregistration, presentations
#'   \item README.md, LICENSE, .gitignore
#' }
#'
#' The function automatically stamps the experiment folder and manuscript draft with the current date.
#'
#' @examples
#' \dontrun{
#' create_MetaLab_project("Metacognition_Study")
#' }
#'
#' @export
create_MetaLab_project <- function(project_name,
                                   experiment_name = "example-experiment",
                                   base_path = getwd()) {

  # --- Today's date ---
  today <- format(Sys.Date(), "%Y-%m-%d")
  experiment_folder <- paste0(experiment_name)

  # --- Root project directory ---
  root_dir <- file.path(base_path, project_name)

  # --- Main project-level folders ---
  folders <- c(
    "",  # root
    "experiments",
    "manuscripts",
    "reports",
    "preregistration",
    "presentations"
  )

  # Create main directories
  for (folder in folders) {
    dir.create(file.path(root_dir, folder), recursive = TRUE, showWarnings = FALSE)
  }

  # --- Create experiment folder structure ---
  exp_path <- file.path(root_dir, "experiments", experiment_folder)

  exp_subfolders <- c(
    "design",
    "stimuli",
    "scripts",
    "data/metadata",
    "data/processed",
    "data/raw",
    "analysis/scripts",
    "analysis/results",
    "analysis/notebooks",
    "code"
  )

  for (sub in exp_subfolders) {
    dir.create(file.path(exp_path, sub), recursive = TRUE, showWarnings = FALSE)
  }

  # --- Add notes.md to experiment folder ---
  notes_path <- file.path(exp_path, "notes.md")
  if (!file.exists(notes_path)) {
    writeLines(c(
      "# Experiment Notes",
      "",
      paste("Experiment created on", today),
      "",
      "## Design",
      "- Describe the experimental design here.",
      "",
      "## Changes",
      "- Document changes or issues here."
    ), con = notes_path)
  }

  # --- Add analysis plan in experiment analysis folder ---
  analysis_plan <- file.path(exp_path, "analysis", "analysis_plan.Rmd")
  if (!file.exists(analysis_plan)) {
    writeLines(c(
      "---",
      "title: 'Analysis Plan'",
      "output: html_document",
      "---",
      "",
      "## Objectives",
      "",
      "Describe the main hypotheses and analysis plan here."
    ), con = analysis_plan)
  }

  # --- Add README.md ---
  readme_path <- file.path(root_dir, "README.md")
  if (!file.exists(readme_path)) {
    readme_content <- c(
      paste0("# ", project_name),
      "",
      "## Overview",
      "Describe the project here.",
      "",
      "## Citation",
      "Add citation information if needed.",
      "",
      "## Experiment codes",
      "- experiment_name: Description of study",
      "",
      "## Folder Structure",
      "",
      "```plaintext",
      "Project_Name/",
      "├── README.md                # Project overview and folder structure",
      "├── LICENSE                  # Licensing information",
      "├── .gitignore               # Files to ignore in version control",
      "├── experiments/",
      "│   └── YYYY-MM-DD_example-experiment/",
      "│       ├── design/          # Task design files, protocols",
      "│       ├── stimuli/         # Images, sounds, or other stimuli",
      "│       ├── scripts/         # Code for running the experiment (e.g., jsPsych, MATLAB)",
      "│       ├── data/",
      "│       │   ├── metadata/    # Data dictionary and metadata files",
      "│       │   ├── processed/   # Cleaned data ready for analysiss",
      "│       │   └── raw/         # Untouched raw data",
      "│       ├── analysis/",
      "│       │   ├── scripts/     # R, Python, MATLAB analysis scripts",
      "│       │   ├── results/     # Figures, stats outputs, tables",
      "│       │   ├── notebooks/   # RMarkdown or Jupyter notebooks",
      "│       │   └── analysis_plan.Rmd # Pre-planned analysis description",
      "│       ├── code/            # Shared functions or custom packages for this experiment",
      "│       └── notes.md         # Lab notes or changelog for the experiment",
      "├── manuscripts/",
      "│   └── YYYY-MM-DD_draft_manuscript.md  # Draft manuscript for the project",
      "├── reports/                 # Internal reports or summaries",
      "├── preregistration/         # Preregistration documents",
      "└── presentations/           # Conference slides, posters, etc.",
      "```",
      "",
      "## Notes",
      "- Use the `notes.md` file in each experiment folder to document changes, decisions, and progress.",
      "- Place raw data in the experiment's `data/raw` folder. Keep the `metadata/` updated.",
      "- Analysis scripts and outputs stay inside the experiment's `analysis/` folder.",
      "- Modular structure keeps each experiment self-contained."
    )
    writeLines(readme_content, con = readme_path)
  }


  # --- Add LICENSE ---
  license_path <- file.path(root_dir, "LICENSE")
  if (!file.exists(license_path)) {
    writeLines("Specify your license here (e.g., MIT, CC BY 4.0).", con = license_path)
  }

  # --- Add .gitignore ---
  gitignore_path <- file.path(root_dir, ".gitignore")
  if (!file.exists(gitignore_path)) {
    writeLines(c(
      ".DS_Store",
      "*.Rhistory",
      "*.RData",
      "*.Rproj.user"
    ), con = gitignore_path)
  }

  # --- Create manuscript draft ---
  manuscript_draft <- file.path(root_dir, "manuscripts", paste0(today, "_draft_manuscript.md"))
  if (!file.exists(manuscript_draft)) {
    writeLines(c(
      "# Draft Manuscript",
      "",
      "## Abstract",
      "",
      "## Introduction",
      "",
      "## Methods",
      "",
      "## Results",
      "",
      "## Discussion"
    ), con = manuscript_draft)
  }

  # --- Add template data dictionary JSON in experiment data/metadata ---
  datadict_path <- file.path(exp_path, "data", "metadata", "data_dictionary_template.json")
  if (!file.exists(datadict_path)) {
    json_content <- '{
    "@context": "http://schema.org/",
    "@type": "Dataset",
    "name": "Example Study Dataset",
    "description": "A description of the dataset. Update this to reflect your study.",
    "variableMeasured": [
        {
            "@type": "PropertyValue",
            "name": "participant_id",
            "description": "Unique identifier for each participant",
            "unitText": "string"
        },
        {
            "@type": "PropertyValue",
            "name": "condition",
            "description": "Experimental condition assigned to the participant",
            "valuePattern": "control|treatment"
        },
        {
            "@type": "PropertyValue",
            "name": "response_time",
            "description": "Response time from stimulus onset to button press",
            "unitText": "milliseconds"
        },
        {
            "@type": "PropertyValue",
            "name": "accuracy",
            "description": "Binary indicator of response correctness (1 = correct, 0 = incorrect)",
            "unitText": "binary"
        }
    ]
}'
    writeLines(json_content, con = datadict_path)
  }

  cat("✅ Project directory created at:", root_dir, "\n")
  cat("✅ Example experiment folder created:", experiment_folder, "\n")
}



