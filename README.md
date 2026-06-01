# About

# Project Execution Strategy: R & Power BI Pipeline

This document outlines the collaborative workflow for our data science project. The goal is to ensure a reproducible pipeline while providing a structured environment for learning and development.

---

## 1. Roles and Responsibilities

| Phase | Lead Oversight (Project Lead) | Team Execution (Contributors) |
| :--- | :--- | :--- |
| **Setup** | Define repo structure, `.gitignore`, and `renv` environment. | Clone repository and run `renv::restore()`. |
| **Development** | Write function headers and logic outlines (scaffolding). | Fill in function logic using `dplyr` and `tidyverse`. |
| **Review** | Review Pull Requests; suggest optimisations; check edge cases. | Address review comments, fix bugs, and document code. |
| **Integration** | Configure GitHub Actions for periodic refresh and data exports. | Build visualisations in Power BI using exported data. |

---

## 2. Phase Details

### Phase 1: Setup
*   **Repository:** The central source of truth. All work happens in branches.
*   **Environment Management:** We use the `renv` package to ensure everyone uses the exact same versions of R libraries.
*   **Verification:** Success is defined as all team members being able to run a "Hello World" script without library errors.

### Phase 2: Development (The "Hand-holding" Phase)
*   **Function Scaffolding:** To assist with R logic, the Lead provides function "skeletons."
    *   *Example:*
    ```r
    # clean_dates
    clean_date_column <- function(df) {
      # TODO: Use lubridate to standardise the 'Date' column
      return(df)
    }

## 3. Getting Started

To ensure a consistent development environment, please follow these steps:

1.  **Pull latest changes**: Always run `git pull` before starting work.
2.  **Open Project**: Open the `hhp-dashboard.Rproj` file in RStudio.
3.  **Restore Environment**: Run `renv::restore()` in the R console to install all required dependencies.

## 4. Repository Map

```text
HHP/
├── Evaluation.docx             # Project evaluation documentation
├── AGENT.md                    # Assistant instructions
├── Healthy Hearts update..pptx # Steering Group presentation
└── hhp-dashboard/              # Main R project directory
    ├── extracts/               # Raw data extracts
    ├── outputs/                # Processed data and outputs
    ├── scripts/                # R scripts
    │   ├── ini.R               # Environment initialization and libraries
    │   └── etl.R               # Main ETL pipeline skeleton
    ├── renv.lock               # R package lockfile
    └── hhp-dashboard.Rproj     # RStudio project file
```

## 5. Workflow

The project follows a standard ETL (Extract, Transform, Load) pattern:

-   **Initialize**: `scripts/ini.R` loads required libraries (`tidyverse`, `data.table`, `plotly`, etc.).
-   **ETL**: `scripts/etl.R` is the entry point for data processing.
