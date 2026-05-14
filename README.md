# Healthy Hearts Project (HHP) Dashboard

This repository contains the data processing pipeline and dashboard for the Healthy Hearts Project.

## Getting Started

To ensure a consistent development environment, please follow these steps:

1.  **Pull latest changes**: Always run `git pull` before starting work.
2.  **Open Project**: Open the `hhp-dashboard.Rproj` file in RStudio.
3.  **Restore Environment**: Run `renv::restore()` in the R console to install all required dependencies.

## Repository Map

```text
HHP/
├── Evaluation.docx             # Project evaluation documentation
├── GEMINI.md                   # Assistant instructions
├── Healthy Hearts update...pptx # Steering Group presentation
└── hhp-dashboard/              # Main R project directory
    ├── extracts/               # Raw data extracts
    ├── outputs/                # Processed data and outputs
    ├── scripts/                # R scripts
    │   ├── ini.R               # Environment initialization and libraries
    │   └── etl.R               # Main ETL pipeline skeleton
    ├── renv.lock               # R package lockfile
    └── hhp-dashboard.Rproj     # RStudio project file
```

## Workflow

The project follows a standard ETL (Extract, Transform, Load) pattern:

-   **Initialize**: `scripts/ini.R` loads required libraries (`tidyverse`, `data.table`, `plotly`, etc.).
-   **ETL**: `scripts/etl.R` is the entry point for data processing.
