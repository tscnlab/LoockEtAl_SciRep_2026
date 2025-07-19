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


## Bayes Factor Analyses-------------------------------------------------

### ---- Chronotype (msf_sc_num) ----

# we need a "clean" (i.e., no missing or infinite values) dataset for predictor and dependent variables,
MSF_clean <- analysis.data %>% 
  drop_na(msf_sc_num,
          slypos_demographics_age, 
          slypos_demographics_sex.factor, 
          slypos_demographics_school.factor,
          F2_leba, F3_leba, F4_leba, F5_leba)

# Define the common null model (without the Leba factor)

bf_chrono_null <- lmBF(msf_sc_num ~ slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF_clean)

# Bayes Factor for the model including F2_leba
bf_chrono_fac2 <- lmBF(msf_sc_num ~ F2_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF_clean)


BF_chrono_fac2 <- bf_chrono_fac2 / bf_chrono_null
print(BF_chrono_fac2)

# Bayes Factor for the model including F5_leba
bf_chrono_fac5 <- lmBF(msf_sc_num ~ F5_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF_clean)
BF_chrono_fac5 <- bf_chrono_fac5 / bf_chrono_null
BF_chrono_fac5

# Bayes Factor for the model including F3_leba
bf_chrono_fac3 <- lmBF(msf_sc_num ~ F3_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF_clean)
BF_chrono_fac3 <- bf_chrono_fac3 / bf_chrono_null
BF_chrono_fac3

# Bayes Factor for the model including F4_leba
bf_chrono_fac4 <- lmBF(msf_sc_num ~ F4_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = MSF_clean)
BF_chrono_fac4 <- bf_chrono_fac4 / bf_chrono_null
BF_chrono_fac4


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
BF_sleepdis_fac2 <- bf_sleepdis_fac2 / bf_sleepdis_null
print(BF_sleepdis_fac2)

# Model with F5_leba
bf_sleepdis_fac5 <- lmBF(Promis_sd_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepdis_fac5 <- bf_sleepdis_fac5 / bf_sleepdis_null
print(BF_sleepdis_fac5)

# Model with F3_leba
bf_sleepdis_fac3 <- lmBF(Promis_sd_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepdis_fac3 <- bf_sleepdis_fac3 / bf_sleepdis_null
print(BF_sleepdis_fac3)

# Model with F4_leba
bf_sleepdis_fac4 <- lmBF(Promis_sd_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepdis_fac4 <- bf_sleepdis_fac4 / bf_sleepdis_null
print(BF_sleepdis_fac4)


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
BF_sleepimp_fac2 <- bf_sleepimp_fac2 / bf_sleepimp_null
print(BF_sleepimp_fac2)

# Model with F5_leba
bf_sleepimp_fac5 <- lmBF(Promis_sri_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepimp_fac5 <- bf_sleepimp_fac5 / bf_sleepimp_null
print(BF_sleepimp_fac5)

# Model with F3_leba
bf_sleepimp_fac3 <- lmBF(Promis_sri_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepimp_fac3 <- bf_sleepimp_fac3 / bf_sleepimp_null
print(BF_sleepimp_fac3)

# Model with F4_leba
bf_sleepimp_fac4 <- lmBF(Promis_sri_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = PROMIS_clean)
BF_sleepimp_fac4 <- bf_sleepimp_fac4 / bf_sleepimp_null
print(BF_sleepimp_fac4)


# Exploratory analysis----------------------------------------------------------

## correlation matrix-----------------------------------------------------------
cor_data <- analysis.data %>%
  select_if(where(is.numeric))

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


vars_for_pca <- c("slypos_demogrpahics_age", "slypos_demographics_sex.factor", "slypos_demographics_school.factor",
                  "Photophilia_score", "Photophobia_score", "ASE_score", "Promis_sd_sum", "Promis_sri_sum", 
                  "le_week_num", "msf_sc_num", 
                  "F1_leba", "F2_leba", "F3_leba", "F4_leba", "F5_leba")

pca_selected <- analysis.data %>% select(all_of(vars_for_pca)) %>% drop_na() %>% as.matrix()
prcomp(pca_selected, center = TRUE, scale. = TRUE)


# Compute PCA with centering and scaling
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Display a summary of the PCA results
summary(pca_result)


# PCA plots

# Plot a scree plot to visualize variance explained by each PC
plot(pca_result, type = "l")


# Extract variable loadings
loadings <- as.data.frame(pca_result$rotation)
loadings$variable <- rownames(loadings)

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

