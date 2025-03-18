#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar                                                                 


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(lme4, stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq)


# load data ----------------------------------------------------------------

load(file="./04_data_analysis/analysis.data.rda")


analysis.data <- analysis.data %>% 
  mutate(msf_sc_num = as.numeric(msf_sc)/3600) %>% 
  

# Models Confirmatory hypotheses ----------------------------------------------

## LM linear analysis----------------------------------------------------------


### Models for chronotype (msf_sc from the MCTQ)------------------------------

# Factor 2 & 5 --> more morning & daylight
#Factor 3 --> more evening/night light 
#Factor 4 (Restricting evening and nocturnal light) --> less evening/night light
#

#Models for Chronotype (mid sleep timing corrected for oversleep)
# ß = negative (earlier chronotype if more behaviour)
# msf_sc = F2_leba + age + sex + work/school setting + (1|record_id) 
# msf_sc = F5_leba + age + sex + work/school setting + (1|record_id)
# msf_sc = F4_leba + age + sex + work/school setting + (1|record_id)


# ß = positive (later chronotype if more behaviour)
# msf_sc = F3_leba + age + sex + work/school setting + (1|record_id)




# Model with F2_leba as predictor (ß expected negative)
model_chrono_fac2 <-  lm( msf_sc_num ~ F2_leba +
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)

summary(model_chrono_fac2)

# Model with F5_leba as predictor (ß expected negative)
model_chrono_fac5 <-  lm(msf_sc_num ~ F5_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor  , 
                          data = analysis.data)

summary(model_chrono_fac5)

# Models for Chronotype (msf_sc_num)

# Model with F3_leba as predictor (ß expected positive)
model_chrono_fac3 <- lm(msf_sc_num ~ F3_leba + 
                          slypos_demographics_age + 
                          slypos_demographics_sex.factor + 
                          slypos_demographics_school.factor, 
                        data = analysis.data)
summary(model_chrono_fac3)

# Model with F4_leba as predictor (ß expected negative) 
model_chrono_fac4 <- lm(msf_sc_num ~ F4_leba + 
                          slypos_demographics_age + 
                          slypos_demographics_sex.factor + 
                          slypos_demographics_school.factor, 
                        data = analysis.data)
summary(model_chrono_fac4)


### Models for Sleep Disturbances (PROMIS sleep quality)----------------------------

# Model with F2_leba as predictor (ß = negative: less sleep disturbances if more behaviour)
model_sleepdis_fac2 <- lm(Promis_sd_sum ~ F2_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepdis_fac2)

# Model with F5_leba as predictor (ß = negative: less sleep disturbances if more behaviour)
model_sleepdis_fac5 <- lm(Promis_sd_sum ~ F5_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepdis_fac5)

# Model with F3_leba as predictor (ß expected positive: more sleep disturbances if more behaviour)
model_sleepdis_fac3 <- lm(Promis_sd_sum ~ F3_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepdis_fac3)

# Model with F4_leba as predictor (ß expected negative: less sleep disturbances if more behaviour)
model_sleepdis_fac4 <- lm(Promis_sd_sum ~ F4_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepdis_fac4)


### Models for Sleep-Related Impairment (PROMIS daytime sleepiness)-----------------

# Model with F2_leba as predictor (ß = negative: less impairment if more behaviour)
model_sleepimp_fac2 <- lm(Promis_sri_sum ~ F2_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepimp_fac2)

# Model with F5_leba as predictor (ß = negative: less impairment if more behaviour)
model_sleepimp_fac5 <- lm(Promis_sri_sum ~ F5_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepimp_fac5)

# Model with F3_leba as predictor (ß expected positive: more impairment if more behaviour)
model_sleepimp_fac3 <- lm(Promis_sri_sum ~ F3_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepimp_fac3)

# Model with F4_leba as predictor (ß expected negative: less impairment if more behaviour)
model_sleepimp_fac4 <- lm(Promis_sri_sum ~ F4_leba + 
                            slypos_demographics_age + 
                            slypos_demographics_sex.factor + 
                            slypos_demographics_school.factor, 
                          data = analysis.data)
summary(model_sleepimp_fac4)



## Bayes Factor Analyses-------------------------------------------------

### ---- Chronotype (msf_sc_num) ----

# Define the common null model (without the Leba factor)

analysis.data_MSF <- analysis.data %>%
  filter(!is.na(msf_sc_num) & is.finite(msf_sc_num))




bf_chrono_null <- lmBF(msf_sc_num ~ slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = analysis.data_MSF)

# Bayes Factor for the model including F2_leba
bf_chrono_fac2 <- lmBF(msf_sc_num ~ F2_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = analysis.data_MSF)


BF_chrono_fac2 <- bf_chrono_fac2 / bf_chrono_null
print(BF_chrono_fac2)

# Bayes Factor for the model including F5_leba
bf_chrono_fac5 <- lmBF(msf_sc_num ~ F5_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = analysis.data_MSF)
BF_chrono_fac5 <- bf_chrono_fac5 / bf_chrono_null
print(BF_chrono_fac5)

# Bayes Factor for the model including F3_leba
bf_chrono_fac3 <- lmBF(msf_sc_num ~ F3_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = analysis.data_MSF)
BF_chrono_fac3 <- bf_chrono_fac3 / bf_chrono_null
print(BF_chrono_fac3)

# Bayes Factor for the model including F4_leba
bf_chrono_fac4 <- lmBF(msf_sc_num ~ F4_leba + slypos_demographics_age + 
                         slypos_demographics_sex.factor + 
                         slypos_demographics_school.factor, 
                       data = analysis.data_MSF)
BF_chrono_fac4 <- bf_chrono_fac4 / bf_chrono_null
print(BF_chrono_fac4)


### ---- Sleep Disturbances (Promis_sd_sum) ----

# Common null model for PROMIS sleep quality
bf_sleepdis_null <- lmBF(Promis_sd_sum ~ slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)

# Model with F2_leba
bf_sleepdis_fac2 <- lmBF(Promis_sd_sum ~ F2_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepdis_fac2 <- bf_sleepdis_fac2 / bf_sleepdis_null
print(BF_sleepdis_fac2)

# Model with F5_leba
bf_sleepdis_fac5 <- lmBF(Promis_sd_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepdis_fac5 <- bf_sleepdis_fac5 / bf_sleepdis_null
print(BF_sleepdis_fac5)

# Model with F3_leba
bf_sleepdis_fac3 <- lmBF(Promis_sd_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepdis_fac3 <- bf_sleepdis_fac3 / bf_sleepdis_null
print(BF_sleepdis_fac3)

# Model with F4_leba
bf_sleepdis_fac4 <- lmBF(Promis_sd_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepdis_fac4 <- bf_sleepdis_fac4 / bf_sleepdis_null
print(BF_sleepdis_fac4)


### ---- Sleep-Related Impairment (Promis_sri_sum) ----

# Common null model for PROMIS daytime sleepiness
bf_sleepimp_null <- lmBF(Promis_sri_sum ~ slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)

# Model with F2_leba
bf_sleepimp_fac2 <- lmBF(Promis_sri_sum ~ F2_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepimp_fac2 <- bf_sleepimp_fac2 / bf_sleepimp_null
print(BF_sleepimp_fac2)

# Model with F5_leba
bf_sleepimp_fac5 <- lmBF(Promis_sri_sum ~ F5_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepimp_fac5 <- bf_sleepimp_fac5 / bf_sleepimp_null
print(BF_sleepimp_fac5)

# Model with F3_leba
bf_sleepimp_fac3 <- lmBF(Promis_sri_sum ~ F3_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepimp_fac3 <- bf_sleepimp_fac3 / bf_sleepimp_null
print(BF_sleepimp_fac3)

# Model with F4_leba
bf_sleepimp_fac4 <- lmBF(Promis_sri_sum ~ F4_leba + slypos_demographics_age + 
                           slypos_demographics_sex.factor + 
                           slypos_demographics_school.factor, 
                         data = analysis.data)
BF_sleepimp_fac4 <- bf_sleepimp_fac4 / bf_sleepimp_null
print(BF_sleepimp_fac4)


# Exploratory hypotheses--------------------------------------------------------

# all variables from confirmatory + other questionnaires & interesting demographics

cor_matrix <- cor(analysis.data, use = "complete.obs", method = "pearson")
print(cor_matrix)
