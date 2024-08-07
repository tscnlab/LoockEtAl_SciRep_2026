#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar                                                                 


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(stringr, reshape2, Hmisc, tidyverse, doBy, DescTools, BayesFactor, effectsize, gtsummary)


# load data ----------------------------------------------------------------

load(file="./02_data_wrangling/data.rda")



# compute sum scores for questionnaires--------------------------------------


## Photosensitivity Assessment Questionnaire (PAQ)-----------------------------
#create Photophila and Photophobia score values (if NA values--> sum should be NA)

data <- data %>%
  mutate(Photophila_score = (slypos_paq_1 + slypos_paq_3 + slypos_paq_4 +
                               slypos_paq_5 + slypos_paq_8 + slypos_paq_11 +
                               slypos_paq_15 + slypos_paq_16)/ 8)
data <- data %>%
  mutate(Photophobia_score = (slypos_paq_2 + slypos_paq_6 + slypos_paq_7 +
                                slypos_paq_9 + slypos_paq_10 + slypos_paq_12 +
                                slypos_paq_13 + slypos_paq_14)/ 8)


## Assessment of Sleep Environment (ASE)----------------------------------------

#recode the answers from 1 - 4 to 0 - 3
data<-data %>%
  mutate(slypos_ase_001 = slypos_ase_001 - 1,
         slypos_ase_002 = slypos_ase_002 - 1,
         slypos_ase_003 = slypos_ase_003 - 1,
         slypos_ase_004 = slypos_ase_004 - 1,
         slypos_ase_005 = slypos_ase_005 - 1,
         slypos_ase_006 = slypos_ase_006 - 1,
         slypos_ase_007 = slypos_ase_007 - 1,
         slypos_ase_008 = slypos_ase_008 - 1,
         slypos_ase_009 = slypos_ase_009 - 1,
         slypos_ase_010 = slypos_ase_010 - 1,
         slypos_ase_011 = slypos_ase_011 - 1,
         slypos_ase_012 = slypos_ase_012 - 1,
         slypos_ase_0123 = slypos_ase_0123 - 1)

#create ASE sum score (if NA values --> sum should be NA)
data<-data %>%
  mutate(ASE_score = slypos_ase_001 + 
           slypos_ase_002 + 
           slypos_ase_003 + 
           slypos_ase_004 + 
           slypos_ase_005 + 
           slypos_ase_006 + 
           slypos_ase_007 + 
           slypos_ase_008 + 
           slypos_ase_009 + 
           slypos_ase_010 + 
           slypos_ase_011 + 
           slypos_ase_012 + 
           slypos_ase_0123)

#create levels: score: 0 to 9  =  low; 10 to 19 Moderate; 20 to 39 = High


## PROMIS MEASURES - Sum scores ------------------------------------------------
#PROMIS Sleep Disturbance sum score pediatric

data<-data %>%
  mutate(Promis_sd_ped_sum=slypos_promis_sd_ped_01 + 
           slypos_promis_sd_ped_02 + 
           slypos_promis_sd_ped_03 + 
           slypos_promis_sd_ped_04)

#PROMIS Sleep Disturbance sum score adult

data<-data %>%
  mutate(Promis_sd_ad_sum=slypos_promis_sd_ad_01 + 
           slypos_promis_sd_ad_02 + 
           slypos_promis_sd_ad_03 + 
           slypos_promis_sd_ad_04)

#PROMIS Sleep Related Impairment sum score pediatric
data<-data %>%
  mutate(Promis_si_ped_sum=slypos_promis_si_ped_01 + 
           slypos_promis_si_ped_02 + 
           slypos_promis_si_ped_03 + 
           slypos_promis_si_ped_04)

#PROMIS Sleep Related Impairment sum score adult
data<-data %>%
  mutate(Promis_si_ad_sum=slypos_promis_si_ad_01 + 
           slypos_promis_si_ad_02 + 
           slypos_promis_si_ad_03 + 
           slypos_promis_si_ad_04)



## Self-Administered Rating Scale for Pubertal Development --------------------


#to-dos
# recode the "I don't know" items (5) and the "prefer not to say" items (6)
#recode the yes/no question for girls to (no=1) and (yes=4) 
#& prefer not to say...




#create category score for Puberty scale


# data <- data %>% mutate( 
#   pub_stage = case_when(
#     (dem_sex = 2 & puberty_score_m ==3) | # & !is.na(puberty_score_m)
#       (dem_sex = 1 & puberty_score_f ==2 & puberty_score_f2==1) #& !is.na(puberty_score_f) & !is.na(puberty_score_f2)
#     ~ "Prepubertal",
#     
#     (dem_sex = 2 & puberty_score_m >3 & puberty_score_m<6) |
#       (dem_sex = 1 & puberty_score_f ==3 & puberty_score_f2==1)
#     ~ "Early pubertal",
#     
#     (dem_sex = 2 & puberty_score_m >5 & puberty_score_m<9) |
#       (dem_sex = 1 & puberty_score_f > 3 & puberty_score_f2==1)
#     ~ "Midpubertal",
#     
#     (dem_sex = 2 & puberty_score_m >8 & puberty_score_m<12)|
#       (dem_sex = 1 & puberty_score_f < 8 & puberty_score_f2==4) 
#     ~ "Late pubertal",
#     (dem_sex = 2 & puberty_score_m==12) |
#       (dem_sex = 1 & puberty_score_f ==8 & puberty_score_f2==4)
#     ~ "Postpubertal"
#   ))

# transform into factor
# RC_data$pub_stage <- factor(RC_data$pub_stage,levels = c("Postpubertal",
#                                                          "Late pubertal",
#                                                          "Midpubertal",
#                                                          "Early pubertal",
#                                                          "Prepubertal"
# ) )

#levels(RC_data$pub_stage)


## MCTQ ------------------------------------------------------------------------

#use mctq r package for mctq variables? 
#--> private library, because packe is not peer-reviewed yet.
# what to do with the caffeine and food timing items?




## LEBA factor scores ----------------------------------------------------------



