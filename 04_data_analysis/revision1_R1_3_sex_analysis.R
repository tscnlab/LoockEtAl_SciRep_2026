# =============================================================================
# Revision Script: Reviewer 1, Comment 3 (R1.3)
# Sex/gender differences in light exposure behavior (LEBA) 
# and sex as a predictor or sleep outcomes
# =============================================================================
# This script provides:
# (1) Sex-inclusion Bayes factors ("Does adding sex improve model fit for the 4 
#     sleep outcomes, over and above the covariates already used (age + occupational setting)?")
#     --> tests whether sex matters for SLEEP
# (2) Exploratory Bayesian t-tests ("Do females/males differ on 4 LEBA subscales?")
#     --> answers reviewer's question about sex differences in light exposure BEHAVIOR. 

# Note: (1) retains all 3 sex levels (female, male, other), as it tests the sex factor exactly
# as it entered the confirmatory models. (2) is restricted to female vs. male because the 'other'
# group (n = 15) is too small for a stable group comparison. 

# Code Author: Ann-Sophie Loock    
# =============================================================================

library(dplyr)
library(tidyr)
library(BayesFactor)
library(effsize)


load(file = "./04_data_analysis/analysis.data.rda")

# Confirm factor levels
levels(analysis.data$slypos_demographics_sex.factor) # "Female" "Male" "Other"

# Create numeric MSF (consistent with main analysis)
analysis.data <- analysis.data %>%
  mutate(msf_num = as.numeric(msf) / 3600)



# (1) Sex-inclusion Bayes factors for the four sleep outcomes -------------

# For each outcome: compare a model WITH sex against a model WITHOUT sex,
# both controlling for age + occupational setting (the covariate structure
# used throughout the confirmatory analysis). BF10 > 1 favors including sex (≥ 3 moderate evidence)

outcomes <- c("msf_num", "msf_sc_num", "Promis_sd_sum", "Promis_sri_sum")

sex_inclusion_bf <- lapply(outcomes, function(outc) {
  
  # Complete-case data for this outcome + all covariates
  dat <- analysis.data %>%
    drop_na(all_of(outc),
            slypos_demographics_age,
            slypos_demographics_sex.factor,
            slypos_demographics_school.factor)
  
  # Model WITHOUT sex (age + work only)
  bf_no_sex <- lmBF(
    as.formula(paste(outc, "~ slypos_demographics_age + slypos_demographics_school.factor")), data = dat)
  
  # Model WITH sex (age + work + sex)
  bf_with_sex <- lmBF(
    as.formula(paste(outc, "~ slypos_demographics_age + slypos_demographics_school.factor + slypos_demographics_sex.factor")), data = dat)
  
  # BF10 for including sex = (with sex) / (without sex)
  bf_ratio <- bf_with_sex / bf_no_sex
  
  data.frame(
    outcome = outc,
    n       = nrow(dat),
    BF10_sex_inclusion = round(as.numeric(extractBF(bf_ratio)$bf), 3))})

sex_inclusion_bf <- bind_rows(sex_inclusion_bf)
sex_inclusion_bf



# (2) Exploratory Bayesian t-tests: sex differences in LEBA subscales --------
# Restricted to Female vs. Male (Other group n = 15 too small).

sex_leba <- analysis.data %>%
  filter(slypos_demographics_sex.factor %in% c("Female", "Male")) %>%
  select(slypos_demographics_sex.factor, F2_leba, F3_leba, F4_leba, F5_leba) %>%
  drop_na()

# Drop unused "Other" level
sex_leba$slypos_demographics_sex.factor <- droplevels(sex_leba$slypos_demographics_sex.factor)

female_idx <- sex_leba$slypos_demographics_sex.factor == "Female"
male_idx   <- sex_leba$slypos_demographics_sex.factor == "Male"

cat("\nn Female:", sum(female_idx), "  n Male:", sum(male_idx), "\n")

leba_factors <- c("F2_leba", "F3_leba", "F4_leba", "F5_leba")

# Descriptive statistics by sex
desc_stats_fm <- do.call(rbind, lapply(leba_factors, function(f) {
  data.frame(
    factor    = f,
    female_M  = round(mean(sex_leba[female_idx, f]), 2),
    female_SD = round(sd(sex_leba[female_idx, f]),   2),
    male_M    = round(mean(sex_leba[male_idx, f]),   2),
    male_SD   = round(sd(sex_leba[male_idx, f]),     2))}))
desc_stats_fm

# Bayesian independent-samples t-tests (default Cauchy prior, r = sqrt(2)/2)
# No multiplicity correction: Bayesian model comparison does not require it.
bf_sex_leba <- lapply(leba_factors, function(f) {
  bf <- ttestBF(x = sex_leba[female_idx, f],
                y = sex_leba[male_idx,   f])
  data.frame(factor = f, BF10   = round(as.numeric(extractBF(bf)$bf), 3))})

bf_sex_leba <- bind_rows(bf_sex_leba)
cat("\n--- (2) Bayesian t-test BF10 (Female vs. Male) ---\n")
bf_sex_leba


# =============================================================================
# Interpretation guide:
#   BF10 >= 10  : strong evidence for a difference
#   3-10        : moderate evidence for a difference
#   1-3         : anecdotal evidence for a difference
#   1/3 - 1     : anecdotal evidence against
#   <= 1/3      : moderate evidence against
#   <= 1/10     : strong evidence against
# Direction of any difference is read from desc_stats_fm.
# =============================================================================


# Cohen's d for each LEBA factor, Female vs. Male
# Input: numeric LEBA score ~ two-level sex factor
cohens_d_results <- lapply(leba_factors, function(f) {
  d <- cohen.d(sex_leba[[f]] ~ sex_leba$slypos_demographics_sex.factor)
  data.frame(
    factor      = f,
    cohens_d    = round(d$estimate, 3),
    ci_lower    = round(d$conf.int[1], 3),
    ci_upper    = round(d$conf.int[2], 3))})
cohens_d_results <- bind_rows(cohens_d_results)
cohens_d_results





###############################################################################

# Optional look at covariate numbers --------------------------------------
# Load pre-extracted covariate posterior summaries
# (generated from summary(posterior(BF_model, iterations = 10000)) for each model)
load(file = "./04_data_analysis/covariate_results.rda")

# Inspect structure
str(covariate_results)

# Full covariate table for Supplemental Material
suppl_covariate_table <- covariate_results[
  , c("outcome", "predictor", "covariate", "beta_mean", "beta_sd",
      "ci_lower", "ci_upper", "ci_includes_zero")]
suppl_covariate_table <- suppl_covariate_table[
  order(suppl_covariate_table$outcome, suppl_covariate_table$covariate), ]

print(suppl_covariate_table)

# Save as CSV for table formatting
# write.csv(suppl_covariate_table,
#           file = "./05_output/suppl_covariate_posteriors.csv", row.names = FALSE)