#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar                                                                 


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq)


# load data ----------------------------------------------------------------

load(file="./04_data_analysis/analysis.data.rda")


# Models Confirmatory hypotheses ----------------------------------------------


# Factor 2 & 5 --> more morning & daylight Factor 3 & 4 more evening/night light
#
#Models for Chronotype (mid sleep timing corrected for oversleep)
# ß = negative (earlier chronotype if more behaviour)
# MSFSc = LEBAFACTOR2 + age + sex + work/school setting + (1|record_id) 
# MSFSc = LEBAFACTOR5 + age + sex + work/school setting + (1|record_id)

# ß = positive (earlier chronotype if more behaviour)
# MSFSc = LEBAFACTOR3 + age + sex + work/school setting + (1|record_id)
# MSFSc = LEBAFACTOR4 + age + sex + work/school setting + (1|record_id)


#Models for sleep disturbances (sleep quality, PROMIS)
# ß = negative (earlier chronotype if more behaviour)
# sleep_dis = LEBAFACTOR2 + age + sex + work/school setting + (1|record_id) 
# sleep_dis = LEBAFACTOR5 + age + sex + work/school setting + (1|record_id)

# ß = positive (earlier chronotype if more behaviour)
# sleep_dis = LEBAFACTOR3 + age + sex + work/school setting + (1|record_id)
# sleep_dis = LEBAFACTOR4 + age + sex + work/school setting + (1|record_id)



#Models for sleep-related impairment (daytime sleepiness, PROMIS)
# ß = negative (earlier chronotype if more behaviour)
# sleep_imp = LEBAFACTOR2 + age + sex + work/school setting + (1|record_id) 
# sleep_imp = LEBAFACTOR5 + age + sex + work/school setting + (1|record_id)

# ß = positive (earlier chronotype if more behaviour)
# sleep_imp = LEBAFACTOR3 + age + sex + work/school setting + (1|record_id)
# sleep_imp = LEBAFACTOR4 + age + sex + work/school setting + (1|record_id)


# Exploratory hypotheses--------------------------------------------------------

# all variables from confirmatory + other questionnaires & interesting demographics
