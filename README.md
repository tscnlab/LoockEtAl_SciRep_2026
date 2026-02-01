# Sleep_And_Light_Exposure_Behaviour (SALEB)

This repository contains the full reproducible analysis pipeline and all non-restricted derived datasets used in the manuscript
“Associations between habitual light exposure-related behaviors and sleep timing and sleep complaints in an international community sample”. The repository serves as the archival location for all computational materials associated with this manuscript.

## Reproducibility notes

- The pipeline relies on a project-level working directory. Open the `Sleep_And_Light_Exposure_Behaviour.Rproj` file and run scripts from there.
- Ensure your R environment has the required packages installed.
- All file paths should be relative. If absolute paths are used, adapt them locally.

## Code structure

The analysis code is organized into four sequential stages. Run scripts in numeric folder order.

1. `01_dataimport`: Data import from CSV, saving into RDA
2. `02_data_wrangling`: Cleaning, harmonization, reformatting and scale score computations
3. `03_demographics`: Descriptive statistics and demographic summaries
4. `04_data_analysis`: Inferential analyses, modelling, reporting and figures
5. `05_output`: Saved figures generated for reporting and manuscript preparation


If scripts within a folder have a numeric prefix, execute them in that order.

---

## 01_dataimport

`01_dataimport/dataimport.R`  
**What it does:**
- Imports the raw survey export (`SpitschanSleepSurvey_DATA_2024-11-24-2125.csv`) and removes incomplete cases
- Filters out data with incorrect attention check items
- Adds variable labels and creates factor-coded versions of key variables
- Variable selection for downstream analysis
- Saves the resulting cleaned/labelled dataset as `./02_data_wrangling/data.rda` for downstream scripts.

**Additional files:**
- `raw_data/SpitschanSleepSurvey_DATA_2024-11-24-2125.csv`: Raw REDCap survey export used as input for the data import script
- `SLYPOS_Codebook.pdf`: REDCap survey codebook documenting all variables and response encoding

---

## 02_data_wrangling

`02_data_wrangling/data_wrangling.R`  
**What it does:**
- Loads the cleaned/labelled dataset `./02_data_wrangling/data.rda` 
- Computes scale scores for
  - PAQ (Photophilia / Photophobia),
  - ASE (Assessment of Sleep Environment),
  - PROMIS sleep disturbance and sleep-related impairment scales,
  - Pubertal Development Scale (PDS) with sex-specific scoring,
  - MCTQ (Munich Chronotype Questionnaire) metrics midsleep time and light exposure 
    - Includes cleaning and corrections of implausible bed- / sleep times, incorrect time input, implausible entries
  - LEBA questionnaire scores (F1–F5),
- Exports the cleaned subdatasets for demographic analysis to `03_demographics` (`demvars.data`; `country_tz.data`)
- Exports the cleaned, analysis-ready dataset as `04_data_analysis/analysis.data.rda`

---

## 03_demographics

`03_demographics/demographics.R`  

**What it does:**
- Loads preprocessed demographic variables (`demvars.data`) and country/time-zone information (`country_tz.data`)
- Creates a descriptive summary of demographic variables using `gtsummary`:
  - Continuous variables are summarized as mean (SD),
  - Categorical variables are summarized as counts and percentages.
- Performs a basic quality check of participant age range (minimum and maximum age)
- Creates a descriptive summary of time zone-country combinations using `gtsummary`
- Computes counts and proportions of participants per time zone and per country using `data.table` for descriptive inspection

## 04_data_analysis

- `04_data_analysis/data_analysis.R`  
  **What it does:**  
  - Loads the preprocessed analysis dataset (`analysis.data.rda`)
  - Creates numeric chronotype variables (e.g. MSF in hours)
  - Performs confirmatory Bayes Factor analyses using `BayesFactor::lmBF` for multiple outcomes:
    - Outcomes include chronotype (MSF, MSFsc), PROMIS sleep disturbances, and PROMIS sleep-related impairment
    - For each outcome, constructs a common null model including covariates (age, sex, work setting)
    - Fits alternative models adding one LEBA factor (F2-F5) at a time to the null model
    - Computes Bayes Factors as ratios of alternative models against the corresponding null model
    - Interprets Bayes Factors using the helper function `BFA_interpret.R`
    - Draws posterior samples (10000 iterations) and summarizes posterior distributions
  - Extracts Bayes Factor results, posterior estimates, credible intervals, and effect direction into a unified results table for downstream reporting
  - Saves extracted confirmatory results as `04_data_analysis/results.rda`
  - Conducts exploratory analyses: 
    - Computes a Spearman correlation matrix using `corr.test` with pairwise complete observations and FDR-adjusted p-values
    - Visualizes correlations using `ggcorrplot`, suppressing non-significant associations
    - Performs principal component analysis (PCA) on selected questionnaire and sleep variables:
      - Uses Spearman correlation matrices
      - Assesses sampling adequacy and factorability using KMO and Bartlett’s test
      - Determines the number of components using parallel analysis and MAP
      - Fits oblimin-rotated PCA solutions with and without weekly light exposure (le_week) to account for missingness differences

- `04_data_analysis/BFA_interpret.R`  
  **What it does:**
  - Defines a helper function (`BFA_interpret`) to interpret Bayes Factors using `effectsize::interpret_bf`
  - Applies Jeffreys’ (1961) evidence categories for Bayes Factor interpretation
  
- `04_data_analysis/reporting.R`  
  **What it does:**
  - Loads the preprocessed analysis dataset (`analysis.data.rda`)
  - Computes descriptive statistics for numeric variables using `psych::describe` and formats a preview table with `gt`
  - Loads extracted Bayes Factor results (`results.rda`) and prepares a labelled coefficient dataset for plotting
  - Creates manuscript figures and saves them to `05_output`:
    - Forest plot figure for selected LEBA factors (F2, F3) across selected outcomes, saved as `Fig_forest_F2F3.png`
    - Scatter plot figure for associations of F2 and F3 with MSF and PROMIS outcomes, saved as `Fig_scatter_all.png`
    - Spearman correlation matrix figure with FDR-adjusted p-values and custom labels, saved as `cor_matrix.png`
    - Descriptive statistics table formatted for manuscript use and saved as `Table_descriptives.docx`

---

## 05_output

This folder contains analysis outputs generated for reporting and manuscript preparation.

**Contents:**
- `Fig_forest_F2F3.png`: Forest plot of posterior regression coefficients (with 95% credible intervals) for selected LEBA factors (F2, F3) across selected sleep outcomes
- `Fig_scatter_all.png`: Scatter plot figure showing associations of LEBA factors F2 and F3 with MSF and PROMIS sleep outcomes
- `cor_matrix.png`: Spearman correlation matrix of selected questionnaire, light exposure, and sleep variables with FDR-adjusted significance
- `Table_descriptives.docx`: Descriptive statistics table for demographic, LEBA, sleep outcome, and additional measures variables

---



