#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Ann-Sophie Loock  & Rafael Lazar                                                               


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(lme4, stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq, psych, ggcorrplot)


# load data ----------------------------------------------------------------

load(file="./04_data_analysis/analysis.data.rda")

# Get all column names
colnames(analysis.data)

# AL: Aug - 07 ~ playing around with MSF...
# We have 20 missing values in MSF and 280 missing values in MSFsc
analysis.data <- analysis.data %>% mutate(msf_num = as.numeric(msf)/3600) # create numeric value for MSF


## Bayes Factor Analyses-------------------------------------------------

### ---- Chronotype (msf_sc_num) ----

# we need a "clean" (i.e., no missing or infinite values) dataset for predictor and dependent variables,
MSF <- analysis.data %>% 
  drop_na(msf_num, 
          slypos_demographics_age, slypos_demographics_sex.factor, slypos_demographics_school.factor,
          F2_leba, F3_leba, F4_leba, F5_leba)
# nrow(MSF) = 751

# Define the common null model (without the Leba factor) for MSF as the outcome
## MSF
bf_msf_null <- lmBF(msf_num ~ slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF)

# Bayes Factor for the model including F2_leba
bf_msf_f2 <- lmBF(msf_num ~ F2_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF)


(BF_msf_f2 <- bf_msf_f2 / bf_msf_null)

## Interpret the BF
source("/Users/loock0000/switchdrive/PhD/Projekte_Collabs/SLYPOS/Analyses/Sleep_And_Light_Exposure_Behaviour/04_data_analysis/BFA_interpret.R")
BFA_interpret(BF_msf_f2) 
estimatesBF_msf2 <- posterior(BF_msf_f2, iterations = 10000)
summary(estimatesBF_msf2)


# Bayes Factor for the model including F5_leba
bf_msf_f5 <- lmBF(msf_num ~ F5_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF)
(BF_msf_f5 <- bf_msf_f5 / bf_msf_null)

## Interpret the BF
BFA_interpret(BF_msf_f5) 
estimatesBF_msf5 <- posterior(BF_msf_f5, iterations = 10000)
summary(estimatesBF_msf5)


# Bayes Factor for the model including F3_leba
bf_msf_f3 <- lmBF(msf_num ~ F3_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF)

(BF_msf_f3 <- bf_msf_f3 / bf_msf_null)

## Interpret the BF
BFA_interpret(BF_msf_f3) 
estimatesBF_msf3 <- posterior(BF_msf_f3, iterations = 10000)
summary(estimatesBF_msf3)


# Bayes Factor for the model including F4_leba
bf_msf_f4 <- lmBF(msf_num ~ F4_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF)

(BF_msf_f4 <- bf_msf_f4 / bf_msf_null)

## Interpret the BF
BFA_interpret(BF_msf_f4) 
estimatesBF_msf4 <- posterior(BF_msf_f4, iterations = 10000)
summary(estimatesBF_msf4)

##############################################################################################################

## Do all the models for MSFsc (i.e., a much smaller dataset)

MSFsc <- analysis.data %>% 
  drop_na(msf_sc_num,
          slypos_demographics_age, 
          slypos_demographics_sex.factor, 
          slypos_demographics_school.factor,
          F2_leba, F3_leba, F4_leba, F5_leba)
# nrow(MSFsc) = 491

# Null model
bf_msfsc_null <- lmBF(msf_sc_num ~ slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSFsc)

# Bayes Factor for the model including F2_leba
bf_msfsc_f2 <- lmBF(msf_sc_num ~ F2_leba + slypos_demographics_age + 
                    slypos_demographics_sex.factor + 
                    slypos_demographics_school.factor, 
                  data = MSFsc)


(BF_msfsc_f2 <- bf_msfsc_f2 / bf_msfsc_null)
BFA_interpret(BF_msfsc_f2) 
estimatesBF_msfsc2 <- posterior(BF_msfsc_f2, iterations = 10000)
summary(estimatesBF_msfsc2)


# Bayes Factor for the model including F5_leba
bf_msfsc_f5 <- lmBF(msf_sc_num ~ F5_leba + slypos_demographics_age + 
                    slypos_demographics_sex.factor + 
                    slypos_demographics_school.factor, 
                  data = MSFsc)
(BF_msfsc_f5 <- bf_msfsc_f5 / bf_msfsc_null)
BFA_interpret(BF_msfsc_f5) 
estimatesBF_msfsc5 <- posterior(BF_msfsc_f5, iterations = 10000)
summary(estimatesBF_msfsc5)


# Bayes Factor for the model including F3_leba
bf_msfsc_f3 <- lmBF(msf_sc_num ~ F3_leba + slypos_demographics_age + 
                      slypos_demographics_sex.factor + 
                      slypos_demographics_school.factor, 
                    data = MSFsc)
(BF_msfsc_f3 <- bf_msfsc_f3 / bf_msfsc_null)
BFA_interpret(BF_msfsc_f3) 
estimatesBF_msfsc3 <- posterior(BF_msfsc_f3, iterations = 10000)
summary(estimatesBF_msfsc3)

# Bayes Factor for the model including F4_leba
bf_msfsc_f4 <- lmBF(msf_sc_num ~ F4_leba + slypos_demographics_age + 
                      slypos_demographics_sex.factor + 
                      slypos_demographics_school.factor, 
                    data = MSFsc)
(BF_msfsc_f4 <- bf_msfsc_f4 / bf_msfsc_null)
BFA_interpret(BF_msfsc_f4) 
estimatesBF_msfsc4 <- posterior(BF_msfsc_f4, iterations = 10000)
summary(estimatesBF_msfsc4)



### ---- Sleep Disturbances (Promis_sd_sum) ----

# clean dataset for predictor and dependent variables
PROMIS_clean <- analysis.data %>% 
  drop_na(Promis_sd_sum,
          Promis_sri_sum,
          slypos_demographics_age, 
          slypos_demographics_sex.factor, 
          slypos_demographics_school.factor,
          F2_leba, F3_leba, F4_leba, F5_leba)

# Common null model for PROMIS sleep quality
bf_sleepdis_null <- lmBF(Promis_sd_sum ~ slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

# Model with F2_leba
bf_sleepdis_fac2 <- lmBF(Promis_sd_sum ~ F2_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
(BF_sleepdis_fac2 <- bf_sleepdis_fac2 / bf_sleepdis_null)
BFA_interpret(BF_sleepdis_fac2) 

estimatesBF_SD_2 <- posterior(BF_sleepdis_fac2, iterations = 10000)
summary(estimatesBF_SD_2)


# Model with F5_leba
bf_sleepdis_fac5 <- lmBF(Promis_sd_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepdis_fac5 <- bf_sleepdis_fac5 / bf_sleepdis_null)
BFA_interpret(BF_sleepdis_fac5) 

estimatesBF_SD_5 <- posterior(BF_sleepdis_fac5, iterations = 10000)
summary(estimatesBF_SD_5)


# Model with F3_leba
bf_sleepdis_fac3 <- lmBF(Promis_sd_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepdis_fac3 <- bf_sleepdis_fac3 / bf_sleepdis_null)
BFA_interpret(BF_sleepdis_fac3) 

estimatesBF_SD_3 <- posterior(BF_sleepdis_fac3, iterations = 10000)
summary(estimatesBF_SD_3)

# Model with F4_leba
bf_sleepdis_fac4 <- lmBF(Promis_sd_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepdis_fac4 <- bf_sleepdis_fac4 / bf_sleepdis_null)
BFA_interpret(BF_sleepdis_fac4) 

estimatesBF_SD_4 <- posterior(BF_sleepdis_fac4, iterations = 10000)
summary(estimatesBF_SD_4)


### ---- Sleep-Related Impairment (Promis_sri_sum) ----

# Common null model for PROMIS daytime sleepiness
bf_sleepimp_null <- lmBF(Promis_sri_sum ~ slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

# Model with F2_leba
bf_sleepimp_fac2 <- lmBF(Promis_sri_sum ~ F2_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepimp_fac2 <- bf_sleepimp_fac2 / bf_sleepimp_null)
BFA_interpret(BF_sleepimp_fac2) 

estimatesBF_sleepimp_f2 <- posterior(BF_sleepimp_fac2, iterations = 10000)
summary(estimatesBF_sleepimp_f2)


# Model with F5_leba
bf_sleepimp_fac5 <- lmBF(Promis_sri_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepimp_fac5 <- bf_sleepimp_fac5 / bf_sleepimp_null)
BFA_interpret(BF_sleepimp_fac5) 

estimatesBF_sleepimp_f5 <- posterior(BF_sleepimp_fac5, iterations = 10000)
summary(estimatesBF_sleepimp_f5)


# Model with F3_leba
bf_sleepimp_fac3 <- lmBF(Promis_sri_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepimp_fac3 <- bf_sleepimp_fac3 / bf_sleepimp_null)
BFA_interpret(BF_sleepimp_fac3) 

estimatesBF_sleepimp_f3 <- posterior(BF_sleepimp_fac3, iterations = 10000)
summary(estimatesBF_sleepimp_f3)


# Model with F4_leba
bf_sleepimp_fac4 <- lmBF(Promis_sri_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)

(BF_sleepimp_fac4 <- bf_sleepimp_fac4 / bf_sleepimp_null)
BFA_interpret(BF_sleepimp_fac4) 

estimatesBF_sleepimp_f4 <- posterior(BF_sleepimp_fac4, iterations = 10000)
summary(estimatesBF_sleepimp_f4)


# Extract for reporting ---------------------------------------------------------

## Helper function to extract results from confirmatory results ------------
extract_from_existing <- function(outcome,
                                  predictor,      # e.g. "F2_leba"
                                  bf_object,      # e.g. BF_msf_f2  (BFBayesFactor)
                                  post_object) {  # e.g. estimatesBF_msf2 (mcmc object)
  
  summ <- summary(post_object)
  
  # Get numeric BF10; extractBF() returns a length-1 numeric vector for length-1 BFBayesFactor
  BF10 <- as.numeric(BayesFactor::extractBF(bf_object, onlybf = TRUE))
  
  # Capture textual interpretation from your BFA_interpret() helper
  evidence_label <- paste(capture.output(BFA_interpret(bf_object)), collapse = " ")
  
  # Find the row corresponding to the predictor in the posterior summary
  # (BayesFactor names it e.g. "F2_leba-F2_leba")
  pred_row <- grep(predictor, rownames(summ$statistics), value = TRUE)[1]
  
  beta_mean <- summ$statistics[pred_row, "Mean"]
  beta_sd   <- summ$statistics[pred_row, "SD"]
  ci_lower  <- summ$quantiles[pred_row, "2.5%"]
  ci_upper  <- summ$quantiles[pred_row, "97.5%"]
  
  # Basic direction classification
  direction <- ifelse(beta_mean > 0, "positive",
                      ifelse(beta_mean < 0, "negative", "zero"))
  ci_includes_zero <- (ci_lower < 0 & ci_upper > 0)
  
  data.frame(
    outcome          = outcome,
    predictor        = predictor,
    BF10             = BF10,
    evidence_label   = evidence_label,
    beta_mean        = beta_mean,
    beta_sd          = beta_sd,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    direction        = direction,
    ci_includes_zero = ci_includes_zero,
    stringsAsFactors = FALSE
  )
}

all_results <- bind_rows(
  
  # ---- MSF (msf_num) ----
  extract_from_existing("msf_num",       "F2_leba", BF_msf_f2,        estimatesBF_msf2),
  extract_from_existing("msf_num",       "F5_leba", BF_msf_f5,        estimatesBF_msf5),
  extract_from_existing("msf_num",       "F3_leba", BF_msf_f3,        estimatesBF_msf3),
  extract_from_existing("msf_num",       "F4_leba", BF_msf_f4,        estimatesBF_msf4),
  
  # ---- MSFsc (msf_sc_num) ----
  extract_from_existing("msf_sc_num",    "F2_leba", BF_msfsc_f2,      estimatesBF_msfsc2),
  extract_from_existing("msf_sc_num",    "F5_leba", BF_msfsc_f5,      estimatesBF_msfsc5),
  extract_from_existing("msf_sc_num",    "F3_leba", BF_msfsc_f3,      estimatesBF_msfsc3),
  extract_from_existing("msf_sc_num",    "F4_leba", BF_msfsc_f4,      estimatesBF_msfsc4),
  
  # ---- Sleep Disturbances (PROMIS_sd_sum) ----
  extract_from_existing("Promis_sd_sum", "F2_leba", BF_sleepdis_fac2, estimatesBF_SD_2),
  extract_from_existing("Promis_sd_sum", "F5_leba", BF_sleepdis_fac5, estimatesBF_SD_5),
  extract_from_existing("Promis_sd_sum", "F3_leba", BF_sleepdis_fac3, estimatesBF_SD_3),
  extract_from_existing("Promis_sd_sum", "F4_leba", BF_sleepdis_fac4, estimatesBF_SD_4),
  
  # ---- Sleep-Related Impairment (PROMIS_sri_sum) ----
  extract_from_existing("Promis_sri_sum","F2_leba", BF_sleepimp_fac2, estimatesBF_sleepimp_f2),
  extract_from_existing("Promis_sri_sum","F5_leba", BF_sleepimp_fac5, estimatesBF_sleepimp_f5),
  extract_from_existing("Promis_sri_sum","F3_leba", BF_sleepimp_fac3, estimatesBF_sleepimp_f3),
  extract_from_existing("Promis_sri_sum","F4_leba", BF_sleepimp_fac4, estimatesBF_sleepimp_f4)
)

#write.csv(all_results, "BF_confirmatory_results.csv", row.names = FALSE)
#save(all_results, file="./04_data_analysis/results.rda")


# Extract covariate information -------------------------------------------
extract_covariates_from_existing <- function(outcome,
                                             predictor,      # e.g. "F2_leba"
                                             posterior) {    # e.g. estimatesBF_msf2
  
  summ <- summary(posterior)
  rows <- rownames(summ$statistics)
  
  # Keep *only* the covariate rows (age, sex, work env)
  cov_rows <- rows[grepl("^slypos_demographics", rows)]
  
  # Build one row per covariate
  cov_list <- lapply(cov_rows, function(r) {
    beta_mean <- summ$statistics[r, "Mean"]
    beta_sd   <- summ$statistics[r, "SD"]
    ci_lower  <- summ$quantiles[r, "2.5%"]
    ci_upper  <- summ$quantiles[r, "97.5%"]
    
    direction <- ifelse(beta_mean > 0, "positive",
                        ifelse(beta_mean < 0, "negative", "zero"))
    ci_includes_zero <- (ci_lower < 0 & ci_upper > 0)
    
    # nicer covariate label: everything after the last "-"
    cov_label <- sub(".*-", "", r)
    
    data.frame(
      outcome          = outcome,
      predictor        = predictor,
      covariate        = cov_label,
      beta_mean        = beta_mean,
      beta_sd          = beta_sd,
      ci_lower         = ci_lower,
      ci_upper         = ci_upper,
      direction        = direction,
      ci_includes_zero = ci_includes_zero,
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(cov_list)
}

covariate_results <- dplyr::bind_rows(
  # ---- MSF (msf_num) ----
  extract_covariates_from_existing("msf_num", "F2_leba", estimatesBF_msf2),
  extract_covariates_from_existing("msf_num", "F5_leba", estimatesBF_msf5),
  extract_covariates_from_existing("msf_num", "F3_leba", estimatesBF_msf3),
  extract_covariates_from_existing("msf_num", "F4_leba", estimatesBF_msf4),
  
  # ---- MSFsc (msf_sc_num) ----
  extract_covariates_from_existing("msf_sc_num", "F2_leba", estimatesBF_msfsc2),
  extract_covariates_from_existing("msf_sc_num", "F5_leba", estimatesBF_msfsc5),
  extract_covariates_from_existing("msf_sc_num", "F3_leba", estimatesBF_msfsc3),
  extract_covariates_from_existing("msf_sc_num", "F4_leba", estimatesBF_msfsc4),
  
  # ---- Sleep Disturbances (PROMIS_sd_sum) ----
  extract_covariates_from_existing("Promis_sd_sum", "F2_leba", estimatesBF_SD_2),
  extract_covariates_from_existing("Promis_sd_sum", "F5_leba", estimatesBF_SD_5),
  extract_covariates_from_existing("Promis_sd_sum", "F3_leba", estimatesBF_SD_3),
  extract_covariates_from_existing("Promis_sd_sum", "F4_leba", estimatesBF_SD_4),
  
  # ---- Sleep-Related Impairment (PROMIS_sri_sum) ----
  extract_covariates_from_existing("Promis_sri_sum", "F2_leba", estimatesBF_sleepimp_f2),
  extract_covariates_from_existing("Promis_sri_sum", "F5_leba", estimatesBF_sleepimp_f5),
  extract_covariates_from_existing("Promis_sri_sum", "F3_leba", estimatesBF_sleepimp_f3),
  extract_covariates_from_existing("Promis_sri_sum", "F4_leba", estimatesBF_sleepimp_f4)
)

# Optional: write to CSV
# write.csv(covariate_results, "BF_covariate_results.csv", row.names = FALSE)


## Create table ------------------------------------------------------------
# library(knitr)
# 
# apa_table <- all_results %>%
#   mutate(
#     Outcome   = outcome,
#     Predictor = predictor,
#     BF10_fmt  = sprintf("%.2f", BF10),
#     beta_fmt  = sprintf("%.3f", beta_mean),
#     CI_fmt    = sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
#   ) %>%
#   select(
#     Outcome,
#     Predictor,
#     BF10      = BF10_fmt,
#     beta      = beta_fmt,
#     `95% CI`  = CI_fmt,
#     Direction = direction,
#     Evidence  = evidence_label
#   )

# # View nicely in RStudio / R Markdown
# kable(
#   apa_table,
#   booktabs = TRUE,
#   caption = "Bayesian regression results for associations between LEBA factors and sleep outcomes."
# )

# Optional: save APA-style table to CSV for Word
# write.csv(apa_table, "BF_confirmatory_APA_table.csv", row.names = FALSE)


# Exploratory analysis----------------------------------------------------------

## correlation matrix-----------------------------------------------------------
cor_data <- analysis.data %>%
  #select_if(where(is.numeric))
  select(F1_leba, F2_leba, F3_leba, F4_leba, F5_leba,
         msf_num, msf_sc_num, 
         le_week_num, 
         Photophila_score, 
         Photophobia_score,
         ASE_score, 
         Promis_sd_sum, 
         Promis_sri_sum, 
         PDS_score_m,
         PDS_score_f)

cor_matrix <- cor(cor_data, use="pairwise.complete.obs", method = "spearman")
# use: for each pair of variables both values are present
# method: pearson for normally distributed, spearman for skewed / ordinal data
cor_matrix

cor <- corr.test(cor_data, use = "pairwise", method = "spearman", adjust= "fdr")

# Basic
ggcorrplot(cor$r, 
           p.mat = cor$p,
           sig.level = 0.05,
           insig = "blank", 
           lab = TRUE, 
           lab_size = 2.5, 
           hc.order = FALSE)

# Non sig correlations blank
ggcorrplot(
  cor$r,
  type = "upper",
  p.mat = cor$p,
  sig.level = 0.05,
  insig = "blank",
  lab = TRUE
  #hc.order = TRUE
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Pca--------------------------------------------------------------------------

# pca_data <- analysis.data %>%
#   select(where(is.numeric)) %>% 
#   filter(if_all(everything(), ~ !is.na(.) & !is.infinite(.))) 

vars <- c("F1_leba","F2_leba","F3_leba","F4_leba","F5_leba",
          "msf_num","le_week_num",
          "Photophila_score","Photophobia_score",
          "ASE_score","Promis_sd_sum","Promis_sri_sum")

pca_selected <- analysis.data %>%
  select(all_of(vars))

# check missingness 
colSums(is.na(pca_selected))

# option A: keep complete cases only on these vars
pca_cc <- pca_selected %>% drop_na(all_of(vars))

# run PCA
pca_result <- prcomp(pca_cc, center = TRUE, scale. = TRUE)
summary(pca_result)

# add on: do pca without Light exposure (has 203 NAs)
vars2 <- c("F1_leba","F2_leba","F3_leba","F4_leba","F5_leba",
           "msf_num",
           "Photophila_score","Photophobia_score",
           "ASE_score","Promis_sd_sum","Promis_sri_sum")
pca_nolight <- analysis.data %>% select(all_of(vars2))
pca_nolight <- pca_nolight %>% drop_na(all_of(vars2))
pca_result_nolight <- prcomp(pca_nolight, center = TRUE, scale. = TRUE)
summary(pca_result_nolight)


# PCA plots

# Plot a scree plot to visualize variance explained by each PC
plot(pca_result, type = "l")
plot(pca_result_nolight, type = "l")

# Extract variable loadings
loadings <- as.data.frame(pca_result$rotation)
loadings$variable <- rownames(loadings)

print(loadings, digits = 3, cutoff = 0.3) 

pca_result_nolight$rotation

# Plot the loadings for the first two PCs
ggplot(loadings, aes(x = PC1, y = PC2, label = variable)) +
  geom_point() +
  ggrepel::geom_text_repel() +  # Requires ggrepel for non-overlapping labels
  labs(title = "Variable Loadings Plot", x = "PC1", y = "PC2")

# Create a correlation circle plot for the variables
factoextra::fviz_pca_var(pca_result, 
                         col.var = "contrib",  # Color by contributions
                         repel = TRUE,         # Avoid label overlap
                         title = "PCA Variable Correlation Circle")



# PCA_version 2_for-submission --------------------------------------------
# --- PCA with le_week (smaller N) ----

# Select variables for PCA
vars <- c("F1_leba","F2_leba","F3_leba","F4_leba","F5_leba",
          "msf_num","le_week_num",
          "Photophila_score","Photophobia_score",
          "ASE_score","Promis_sd_sum","Promis_sri_sum")

pca_selected <- analysis.data %>% dplyr::select(all_of(vars)) %>%
  # remove "labelled" attributes but keep numeric values
  dplyr::mutate(across(everything(), ~ as.numeric(.)))

# Check missingness (bc/ that's an issue w/ pca)
colSums(is.na(pca_selected))

# Select only complete cases
pca_cc <- pca_selected %>% tidyr::drop_na()
nrow(pca_cc)

# Spearman correlation matrix
p_mat <- cor(pca_selected, use = "pairwise.complete.obs", method = "spearman")

# KMO
kmo <- psych::KMO(p_mat)
kmo

# Bartlett (for pairwise cor this is approximate, but acceptable for a check)
bart <- psych::cortest.bartlett(p_mat, n = nrow(pca_cc))
bart

# Parallel analysis
set.seed(123)

pa <- psych::fa.parallel(
  p_mat,
  n.obs = nrow(pca_cc),
  fm = "pc",        # principal components
  fa = "pc",
  n.iter = 100,
  error.bars = FALSE,
  main = "Parallel analysis (Spearman correlations)"
)

# MAP
vss <- psych::VSS(p_mat, n.obs = nrow(pca_cc), rotate = "none", fm = "pc", plot = FALSE)
vss

# Fit rotated solution and extract loadings
k <- 3  # <- update after inspection of PA + MAP

# PCA
pc_rot <- psych::principal(
  p_mat,
  nfactors = k,
  rotate = "oblimin",
  scores = FALSE)
pc_rot

# Loadings table with cutoff
print(pc_rot$loadings, cutoff = 0.30, digits = 2)

# Alternative (but less fitting): EFA
# fa_rot <- psych::fa(p_mat, nfactors = k, rotate = "oblimin", fm = "minres")   # robust default for EFA
# fa_rot
# print(fa_rot$loadings, cutoff = 0.30, digits = 2)


# --- PCA w/o le_week (larger N) ----
vars2 <- c("F1_leba","F2_leba","F3_leba","F4_leba","F5_leba",
           "msf_num",
           "Photophila_score","Photophobia_score",
           "ASE_score","Promis_sd_sum","Promis_sri_sum")

pca_selected2 <- analysis.data %>%
  dplyr::select(all_of(vars2)) %>%
  dplyr::mutate(across(everything(), ~ as.numeric(.)))

pca_cc2 <- pca_selected2 %>% tidyr::drop_na()
p_mat2 <- cor(pca_selected2, use = "pairwise.complete.obs", method = "spearman")

psych::KMO(p_mat2)
psych::cortest.bartlett(p_mat2, n = nrow(pca_cc2))

set.seed(123)
psych::fa.parallel(p_mat2, n.obs = nrow(pca_cc2), fm = "pc", fa = "pc", n.iter = 100,
                   error.bars = FALSE, main = "Parallel analysis (no weekly LE)")

psych::VSS(p_mat2, n.obs = nrow(pca_cc2), rotate = "none", fm = "pc", plot = FALSE)

k2 <- 2  # update based on outputs
pc_rot2 <- psych::principal(p_mat2, nfactors = k2, rotate = "oblimin", scores = FALSE)
print(pc_rot2$loadings, digits = 2)
print(pc_rot2$loadings, cutoff = 0.40, digits = 2)
