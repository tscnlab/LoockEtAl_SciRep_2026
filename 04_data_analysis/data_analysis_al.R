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

ggcorrplot(cor$r, 
           p.mat = cor$p,
           sig.level = 0.05,
           insig = "blank", 
           lab = TRUE, 
           lab_size = 2.5, 
           hc.order = FALSE)


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

