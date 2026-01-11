# Sleep_And_Light_Exposure_Behaviour (SALEB)

This repository contains the full reproducible analysis pipeline and all non-restricted derived datasets used in the manuscript
“Associations between habitual light exposure–related behaviors and sleep timing and sleep complaints in an international community sample”. The repository serves as the archival location for all computational materials associated with this manuscript.

## Reproducibility notes

- The pipeline relies on a project-level working directory, open the `Sleep_And_Light_Exposure_Behaviour.Rproj` file and run scripts from there.
- Ensure your R environment has the required packages installed.
- All file paths should be relative. If absolute paths are used, adapt them locally.

## Code structure

The analysis code is organized into four sequential stages. Run scripts in numeric folder order.

1. `01_dataimport` — data import from csv, saving into rda
2. `02_data_wrangling` — cleaning, harmonisation, reformatting and scale score computations
3. `03_demographics` — descriptive statistics and demographic summaries
4. `04_data_analysis` — inferential analyses, modelling, figures/tables, reporting


If scripts within a folder have a numeric prefix, execute them in that order.

---

## 01_dataimport — Import

`01_dataimport/dataimport.R`  
**What it does:**
- Imports the raw survey export (`SpitschanSleepSurvey_DATA_2024-08-07_1229.csv`) and removes incomplete cases
- filters out data with incorrect attention check items
- Adds variable labels and creates factor-coded versions of key variables
- variable selection for downstream analysis
- Saves the resulting cleaned/labelled dataset as `./02_data_wrangling/data.rda` for downstream scripts.

---

## 02_data_wrangling

`01_dataimport/data_wrangling.R`  
**What it does:**
- Imports the cleaned/labelled dataset `./02_data_wrangling/data.rda` 
- Computes scale scores for
  - PAQ (Photophilia / Photophobia),
  - ASE (Assessment of Sleep Environment),
  - PROMIS sleep disturbance and sleep-related impairment scales,
  - Pubertal Development Scale (PDS) with sex-specific scoring,
  - LEBA questionnaire scores (F1–F5).
  - Munich Chronotype Questionnaire (MCTQ) metrics including cleaning and corrections
    - Parses and corrects implausible bed and sleep-preparation times, 12-hour clock entry errors, bedtime–sleep inversions,
    - Flags implausible records.
- Exports the cleaned subdatasets for demographic analysis (`demvars.data`; `country_tz.data`)
- Exports the cleaned, analysis-ready dataset as ` 04_data_analysis/analysis.data.rda`

---

## 03_demographics — 

`01_dataimport/demographics.R`  

**What it does:**
- Loads preprocessed demographic (`demvars.data`) and country/time-zone (`country_tz.data`) datasets.
- Creates **Table 1 (Demographics)** using `gtsummary`:
  - Continuous variables reported as *mean (SD)*.
  - Categorical variables reported as *n (%)*.
  - Applies labels and formatting.
- Performs a basic quality check of participant age range.
- Generates a frequency table for **Time zone – Country** distribution (`tz_table`).


## 04_data_analysis

- `04_data_analysis/<FILENAME_1>.R`  
  **What it does:**  


- `04_data_analysis/<FILENAME_2>.R`  
  **What it does:**  


## 05_output

-

---



