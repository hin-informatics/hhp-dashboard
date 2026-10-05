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
    ```

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
    ├── extracts/               # Raw EMIS practice extract subfolders
    │   └── <Practice_Name>/    # 3-report EMIS extracts per practice
    ├── data/                   # Processed outputs for Power BI
    │   ├── facts.csv           # Master patient-level fact table
    │   └── patient_bp_readings.csv # Longitudinal post-target BP readings
    ├── scripts/                # Modular R ETL pipeline
    │   ├── ini.R               # Environment initialization and libraries
    │   ├── func_practice_loader.R # Multi-practice discovery and 3-report validation
    │   ├── func_clean_vars.R   # Header normalization, date parsing, and demographics
    │   ├── func_optimisation.R # Clinical optimisation rules (HTN, CKD, T2D)
    │   └── etl.R               # Pipeline orchestrator
    ├── renv.lock               # R package lockfile
    └── hhp-dashboard.Rproj     # RStudio project file
```

## 5. Workflow

The project follows a standard ETL (Extract, Transform, Load) pattern:

-   **Initialize**: `scripts/ini.R` loads required libraries (`tidyverse`, `data.table`, `readxl`, `lubridate`, etc.).
-   **ETL Orchestration**: `scripts/etl.R` discovers practice folders, validates report manifests, normalises data structures, applies clinical optimisation logic, and exports analysis-ready datasets.

---

## 6. Core Datasets

The ETL pipeline automatically discovers, validates, and harmonises three complementary EMIS Web extracts across all GP practices. Below is a summary of the three core internal datasets consumed during processing:

### 1. `ptts_dt` — Patient Cohort & Cross-Sectional Clinical Baseline
- **Source:** EMIS Web *Healthy Hearts Evaluation Report 1* (85 columns).
- **Granularity:** Patient-level (exactly one row per registered patient; keyed by patient identifier and practice).
- **Key Fields & Purpose:**
  - **Demographics:** Age, biological sex, ethnic category, Lower Layer Super Output Area (LSOA) code used for Index of Multiple Deprivation (IMD) decile mapping.
  - **Diagnostic Registers:** Binary indicators and earliest recorded diagnosis dates for Hypertension, Type 2 Diabetes (T2D), and Chronic Kidney Disease (CKD).
  - **Clinical Baselines & Frailty:** Electronic Frailty Index (eFI) classification, Body Mass Index (BMI), and cardiovascular risk scores (QRISK2/QRISK3).
  - **Latest Laboratory Biochemistry:** Most recent values for Glycated Haemoglobin (HbA1c), estimated Glomerular Filtration Rate (eGFR), Urine Albumin-to-Creatinine Ratio (ACR), and Lipid profiles (Total Cholesterol, HDL, LDL, Non-HDL).
- **Pipeline Role:** Forms the master patient spine. All disease-specific clinical optimisation status flags (`optimised_htn`, `optimised_ckd`, `optimised_t2d`, `optimised_other`) and demographic stratifications are appended directly to this table before loading into `data/facts.csv`.

---

### 2. `appt_dt` — Longitudinal Consultations & Prescriptions
- **Source:** EMIS Web *Healthy Hearts Evaluation Report 2* (80 columns).
- **Granularity:** Event-level transactional log (multiple rows per patient capturing a 5-year retrospective activity window).
- **Key Fields & Purpose:**
  - **Primary Care Consultations:** Consultation dates, clinician type (General Practitioner, Practice Nurse, Healthcare Assistant, Clinical Pharmacist), appointment slot types, and attendance statuses (Attended, Did Not Attend / DNA, Cancelled).
  - **Prescribing History:** Drug issue dates and quantities for cardioprotective regimens, including lipid-lowering therapies (statins: Atorvastatin, Rosuvastatin, Simvastatin), antihypertensive agents (ACEi/ARBs, CCBs, thiazide-like diuretics), and SGLT2 inhibitors.
  - **Clinical Exceptions & Contraindications:** Coded medical exceptions, drug intolerances, patient refusal, or contraindications used to inform disease optimisation rules (e.g. statin contraindications in CKD/CVD).
- **Pipeline Role:** Enriches the patient spine with primary care utilisation counts, last seen clinician disciplines, and therapeutic adherence/contraindication flags.

---

### 3. `base_dt` — Longitudinal Blood Pressure Measurement Log
- **Source:** EMIS Web *Healthy Hearts Baseline Report 3* (17 columns).
- **Granularity:** Event-level measurement log (multiple historical BP readings per patient over time).
- **Key Fields & Purpose:**
  - **Blood Pressure Time Series:** Observation dates, Systolic Blood Pressure (SBP), Diastolic Blood Pressure (DBP), and clinical code terms (e.g. *O/E - Blood pressure reading*, *Home blood pressure monitoring*, *Ambulatory BP monitoring*).
  - **Post-Target Assessment Window:** Tracks all BP readings after the patient's first in-range reading within the baseline intervention period.
  - **Longitudinal Optimisation Metric:** Evaluates sustained blood pressure control based on whether $\ge 50\%$ of post-target BP readings are within age- and comorbidity-adjusted clinical targets:
    - *Standard Clinic:* $\text{SBP} < 140\text{ mmHg}$ and $\text{DBP} < 90\text{ mmHg}$ (or $<150/90\text{ mmHg}$ for patients aged $\ge 80$).
    - *Home / Ambulatory:* $\text{SBP} < 135\text{ mmHg}$ and $\text{DBP} < 85\text{ mmHg}$ (or $<145/85\text{ mmHg}$ for patients aged $\ge 80$).
- **Pipeline Role:** Mandatory input for `optimised_htn(ptts_dt, base_dt)`. Generates the longitudinal hypertension control flag and exports all individual post-target readings to `data/patient_bp_readings.csv` for drill-down visualizations in Power BI.
