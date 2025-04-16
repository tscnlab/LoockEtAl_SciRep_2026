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

load(file="./02_data_wrangling/data.rda")

#define functions:

convert_to_decimal_hours <- function(time_string) {
  # Split the string into hours and minutes
  time_parts <- strsplit(time_string, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  
  # Convert to decimal hours
  decimal_hours <- hours + (minutes / 60)
  return(decimal_hours)
}

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


#correlation of the two scores for photophobia and photophilia
cor(data$Photophila_score, data$Photophobia_score)

# Compute as one sum score for full paq?



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
  mutate(ASE_score = 
           slypos_ase_001 + 
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
# high = appropriate sleep environment, low = unappropriate sleeping environment

data <-data %>%
  mutate(ASE_levels =    case_when(
    ASE_score >= 1 & ASE_score <= 9  ~ "low",
   ASE_score >= 10 & ASE_score <= 19  ~ "moderate",
    ASE_score >= 20 & ASE_score <= 39  ~ "high"
          ))


## PROMIS MEASURES - Sum scores ------------------------------------------------
#higher values mean higher impairment and sleep problems
#PROMIS Sleep Disturbance sum score pediatric

# data<-data %>%
#   mutate(Promis_sd_ped_sum=slypos_promis_sd_ped_01 + 
#            slypos_promis_sd_ped_02 + 
#            slypos_promis_sd_ped_03 + 
#            slypos_promis_sd_ped_04)
# 
# #PROMIS Sleep Disturbance sum score adult
# 
# data<-data %>%
#   mutate(Promis_sd_ad_sum=slypos_promis_sd_ad_01 + 
#            slypos_promis_sd_ad_02 + 
#            slypos_promis_sd_ad_03 + 
#            slypos_promis_sd_ad_04)


#PROMIS Sleep Disturbance sum score for both adults and ped

data <- data %>%
  mutate(Promis_sd_sum = rowSums(select(., 
                                        slypos_promis_sd_ped_01, 
                                        slypos_promis_sd_ped_02, 
                                        slypos_promis_sd_ped_03, 
                                        slypos_promis_sd_ped_04,
                                        slypos_promis_sd_ad_01, 
                                        slypos_promis_sd_ad_02, 
                                        slypos_promis_sd_ad_03, 
                                        slypos_promis_sd_ad_04), 
                                 na.rm = TRUE))



# #PROMIS Sleep Related Impairment sum score pediatric
# data<-data %>%
#   mutate(Promis_sri_ped_sum=slypos_promis_si_ped_01 +
#            slypos_promis_si_ped_02 +
#            slypos_promis_si_ped_03 +
#            slypos_promis_si_ped_04)
# 
# #PROMIS Sleep Related Impairment sum score adult
# data<-data %>%
#   mutate(Promis_sri_ad_sum=slypos_promis_si_ad_01 +
#            slypos_promis_si_ad_02 +
#            slypos_promis_si_ad_03 +
#            slypos_promis_si_ad_04)

#PROMIS Sleep Related Impairment sum score for both adults and ped

data <- data %>%
  mutate(Promis_sri_sum = rowSums(select(., 
                                         slypos_promis_si_ped_01, 
                                         slypos_promis_si_ped_02, 
                                         slypos_promis_si_ped_03, 
                                         slypos_promis_si_ped_04,
                                         slypos_promis_si_ad_01, 
                                         slypos_promis_si_ad_02, 
                                         slypos_promis_si_ad_03, 
                                         slypos_promis_si_ad_04), 
                                  na.rm = TRUE))


## Self-Administered Rating Scale for Pubertal Development --------------------


#to-dos
# recode the "I don't know" items (5) and the "prefer not to say" items (6)

#recode the yes/no question for girls to (no=1) and (yes=4) 
#& prefer not to say...

data$slypos_puberty_01 <- ifelse(data$slypos_puberty_01 == 5 | data$slypos_puberty_01 == 6, NA, data$slypos_puberty_01)
data$slypos_puberty_02 <- ifelse(data$slypos_puberty_02 == 5 | data$slypos_puberty_02 == 6, NA, data$slypos_puberty_02)
data$slypos_puberty_03 <- ifelse(data$slypos_puberty_03 == 5 | data$slypos_puberty_03 == 6, NA, data$slypos_puberty_03)
data$slypos_puberty_boys_01 <- ifelse(data$slypos_puberty_boys_01 == 5 | data$slypos_puberty_boys_01 == 6, NA, data$slypos_puberty_boys_01)
data$slypos_puberty_boys_02 <- ifelse(data$slypos_puberty_boys_02 == 5 | data$slypos_puberty_boys_02 == 6, NA, data$slypos_puberty_boys_02)
data$slypos_puberty_girl_01 <- ifelse(data$slypos_puberty_girl_01 == 5 | data$slypos_puberty_girl_01 == 6 , NA, data$slypos_puberty_girl_01)


data$slypos_puberty_girls_02 <-  ifelse(data$slypos_puberty_girls_02 == 3, NA,
                                       ifelse(data$slypos_puberty_girls_02 == 1, 4,
                                              ifelse(data$slypos_puberty_girls_02 == 2, 1, 
                                                     data$slypos_puberty_girls_02)))
          


                             
#Point values are averaged for all items to give a Pubertal Development Scale (PDS) score.
data <- data %>% mutate(
  PDS_score_m = rowMeans(
    select(., slypos_puberty_01, slypos_puberty_02, slypos_puberty_03, slypos_puberty_boys_01, slypos_puberty_boys_02),
    na.rm = FALSE
  ),
  PDS_score_f = rowMeans(
    select(., slypos_puberty_01, slypos_puberty_02, slypos_puberty_03, slypos_puberty_girl_01, slypos_puberty_girls_02),
    na.rm = FALSE
  )
)


#Optional:
#create category score for Puberty scale
#data$slypos_demographics_sex==1 #female
#data$slypos_demographics_sex==2 #male


## MCTQ ------------------------------------------------------------------------

#use mctq r package for mctq variables? 
# what to do with the caffeine and food timing items?



#### create subdata set for mctq --------------------------------------------
# Collect the names of all variables that include "mctq" in the name
mctq_columns <- grep("mctq", colnames(data), value = TRUE)
mctq_columns

# Filter out the column names that contain "ped"
filtered_columns <- grep(".factor", mctq_columns, invert = TRUE, value = TRUE)

# Filter out the column names that contain "ped"
filtered_columns <- grep("attentioncheck", filtered_columns, invert = TRUE, value = TRUE)

# Print the result
filtered_columns

# Filter out the column names that contain "ped"
mctq_ped <- grep("ped", filtered_columns, invert = F, value = TRUE)

# Filter out the column names that contain "ped"
mctq_ad <- grep("ped", filtered_columns, invert = T, value = TRUE)

#create subdataset
mctq_ad.data <-  data %>% dplyr::select(c(record_id, mctq_ad))
#create subdataset
mctq_ped.data <-  data %>% dplyr::select(c(record_id, mctq_ped))

# Items "regular work schedule" and  regular school schedule"  need to be 1 ("Yes")
#for all further calculations
 data$slypos_mctq_01
 data$slypos_mctq_01_ped
 
 
 #### load testdataset for mctq--------------------------------------------------
 
 mctq.testdata <- readr::read_csv(
   mctq::raw_data("vignette_mctq.csv"),
   col_types = readr::cols(.default = "c")
 )


 #### Converting data to mctq form ---------------------------------------------
 
 #renaming according to mctq pacakge and papers
 mctq_ad.data <-  mctq_ad.data %>% rename(
   #id = record_id,
   work = slypos_mctq_01, # need to convert into logical 1=True, 2=No
   #work day variables
   wd = slypos_mctq_02, 
   bt_w = slypos_mctq_03, 
   sprep_w = slypos_mctq_04,
   slat_w = slypos_mctq_05,
   se_w = slypos_mctq_06,
   si_w = slypos_mctq_07,
   alarm_w =slypos_mctq_08, # need to convert into logical 1=True, 0=No
   wake_before_w = slypos_mctq_09, # need to convert into logical 1=True, 0=No
   le_w = slypos_mctq_25,
   # free day variables

   bt_f = slypos_mctq_10, 
   sprep_f = slypos_mctq_11,
   slat_f = slypos_mctq_12,
   se_f = slypos_mctq_13,
   si_f = slypos_mctq_14,
   alarm_f=slypos_mctq_15, # need to convert into logical 1=True, 0=No
   reasons_f = slypos_mctq_16, # need to convert into logical 1=True, 0=No
   reasons_why_f = slypos_mctq_17, # need to convert into factor with 3 levels: 1= "Child(ren)/pet(s), 2="Hobbies", 3="Others"
   le_f = slypos_mctq_26,
 )

 
 
 mctq_ped.data <-  mctq_ped.data %>% rename(
   #id = record_id,
   work = slypos_mctq_01_ped, # need to convert into logical 1=True, 2=No
   #work day variables
   wd = slypos_mctq_02_ped, 
   bt_w = slypos_mctq_03_ped, 
   sprep_w = slypos_mctq_04_ped,
   slat_w = slypos_mctq_05_ped,
   se_w = slypos_mctq_06_ped,
   si_w = slypos_mctq_07_ped,
   alarm_w =slypos_mctq_08_ped, # need to convert into logical 1=True, 0=No
   wake_before_w = slypos_mctq_09_ped, # need to convert into logical 1=True, 0=No
   le_w = slypos_mctq_25_ped,
   # free day variables
   
   bt_f = slypos_mctq_10_ped, 
   sprep_f = slypos_mctq_11_ped,
   slat_f = slypos_mctq_12_ped,
   se_f = slypos_mctq_13_ped,
   si_f = slypos_mctq_14_ped,
   alarm_f=slypos_mctq_15_ped, # need to convert into logical 1=True, 0=No
   reasons_f = slypos_mctq_16_ped, # need to convert into logical 1=True, 0=No
   reasons_why_f = slypos_mctq_17_ped, # need to convert into factor with 3 levels: 1= "Family members/pets, 2="Hobbies", 3="Others"
   le_f = slypos_mctq_26_ped,
 )
 
 # Convert into Logical vars
 mctq_ad.data$work <- mctq_ad.data$work==1
 mctq_ped.data$work <- mctq_ped.data$work==1
 mctq_ad.data$alarm_w <- mctq_ad.data$alarm_w==1
 mctq_ped.data$alarm_w <- mctq_ped.data$alarm_w==1
 mctq_ad.data$wake_before_w <-  mctq_ad.data$wake_before_w==1
 mctq_ped.data$wake_before_w <- mctq_ped.data$wake_before_w==1
 mctq_ad.data$alarm_f <- mctq_ad.data$alarm_f==1
 mctq_ped.data$alarm_f <- mctq_ped.data$alarm_f==1
 mctq_ad.data$reasons_f <- mctq_ad.data$reasons_f==1
 mctq_ped.data$reasons_f <- mctq_ped.data$reasons_f==1
 
 # Convert into Factor vars
 mctq_ad.data$reasons_why_f = as.factor(mctq_ad.data$reasons_why_f)
 levels(mctq_ad.data$reasons_why_f) = c("Child(ren)/pet(s)","Hobbies","Others")
 
 mctq_ped.data$reasons_why_f = as.factor(mctq_ped.data$reasons_why_f)
 levels(mctq_ped.data$reasons_why_f) = c("Family members/pet(s)","Hobbies","Others")
 
 # Convert into hms vars
 mctq_ad.data$bt_w <- hms::parse_hm(mctq_ad.data$bt_w)
 mctq_ad.data$sprep_w <- hms::parse_hm(mctq_ad.data$sprep_w)
 mctq_ad.data$se_w <- hms::parse_hm(mctq_ad.data$se_w)
 mctq_ad.data$bt_f <- hms::parse_hm(mctq_ad.data$bt_f)
 mctq_ad.data$sprep_f <- hms::parse_hm(mctq_ad.data$sprep_f)
 mctq_ad.data$se_f <- hms::parse_hm(mctq_ad.data$se_f)
 
 mctq_ped.data$bt_w <- hms::parse_hm(mctq_ped.data$bt_w)
 mctq_ped.data$sprep_w <- hms::parse_hm(mctq_ped.data$sprep_w)
 mctq_ped.data$se_w <- hms::parse_hm(mctq_ped.data$se_w)
 mctq_ped.data$bt_f <- hms::parse_hm(mctq_ped.data$bt_f)
 mctq_ped.data$sprep_f <- hms::parse_hm(mctq_ped.data$sprep_f)
 mctq_ped.data$se_f <- hms::parse_hm(mctq_ped.data$se_f)
 
 
 
 # Convert into duration vars
 mctq_ad.data$slat_w <-  lubridate::dminutes(mctq_ad.data$slat_w)
 mctq_ad.data$slat_f <-  lubridate::dminutes(mctq_ad.data$slat_f)
 mctq_ad.data$si_w <-  lubridate::dminutes(mctq_ad.data$si_w)
 mctq_ad.data$si_f <-  lubridate::dminutes(mctq_ad.data$si_f)
 
 mctq_ad.data$le_w[mctq_ad.data$le_w== ""] <- NA
 mctq_ad.data$le_w <- sapply(mctq_ad.data$le_w, convert_to_decimal_hours)
 mctq_ad.data$le_w <- lubridate::dhours(mctq_ad.data$le_w)
 mctq_ad.data$le_f[mctq_ad.data$le_f== ""] <- NA
 mctq_ad.data$le_f <- sapply(mctq_ad.data$le_f, convert_to_decimal_hours)
 mctq_ad.data$le_f <- lubridate::dhours(mctq_ad.data$le_f)

 
 # Convert into duration vars
 mctq_ped.data$slat_w <-  lubridate::dminutes(mctq_ped.data$slat_w)
 mctq_ped.data$slat_f <-  lubridate::dminutes(mctq_ped.data$slat_f)
 mctq_ped.data$si_w <-  lubridate::dminutes(mctq_ped.data$si_w)
 mctq_ped.data$si_f <-  lubridate::dminutes(mctq_ped.data$si_f)
 
 mctq_ped.data$le_w[mctq_ped.data$le_w== ""] <- NA
 mctq_ped.data$le_w <- sapply(mctq_ped.data$le_w, convert_to_decimal_hours)
 mctq_ped.data$le_w <- lubridate::dhours(mctq_ped.data$le_w)
 mctq_ped.data$le_f[mctq_ped.data$le_f== ""] <- NA
 mctq_ped.data$le_f <- sapply(mctq_ped.data$le_f, convert_to_decimal_hours)
 mctq_ped.data$le_f <- lubridate::dhours(mctq_ped.data$le_f)
 
 ####  change wrong formatting ----------------------------------------------
  #translate wrong format (e.g. 11 to 23:00)
 
  # Convert hours to seconds
 hours_to_add <- hms::hms(12*3600)
 
 #pediatric Mctq
 
 # Adjusting `sprep_w` based on the condition
 mctq_ped.data$sprep_w <- ifelse(mctq_ped.data$sprep_w >= hms::parse_hm("08:00:00") & 
                                   mctq_ped.data$sprep_w < hms::parse_hm("12:00:00"),
                                 mctq_ped.data$sprep_w + hours_to_add,
                                 mctq_ped.data$sprep_w)
 
 # Adjusting `sprep_w` based on the condition
 mctq_ped.data$sprep_w <- ifelse(mctq_ped.data$sprep_w >= hms::parse_hm("12:00:00") & 
                                   mctq_ped.data$sprep_w < hms::parse_hm("18:00:00"),
                                 mctq_ped.data$sprep_w - hours_to_add,
                                 mctq_ped.data$sprep_w)
 
 mctq_ped.data$sprep_w <- hms::as_hms(mctq_ped.data$sprep_w)
 
 
 # Adjusting `bt_w` based on the condition

 # Adjusting `sprep_w` based on the condition
 mctq_ped.data$bt_w <- ifelse(mctq_ped.data$bt_w >= hms::parse_hm("08:00:00") & 
                                   mctq_ped.data$bt_w < hms::parse_hm("12:00:00"),
                                 mctq_ped.data$bt_w + hours_to_add,
                                 mctq_ped.data$bt_w)
 
 # Adjusting `bt_w` based on the condition
 mctq_ped.data$bt_w <- ifelse(mctq_ped.data$bt_w >= hms::parse_hm("12:00:00") & 
                                   mctq_ped.data$bt_w < hms::parse_hm("18:00:00"),
                                 mctq_ped.data$bt_w - hours_to_add,
                                 mctq_ped.data$bt_w)
 
 mctq_ped.data$bt_w <- hms::as_hms(mctq_ped.data$bt_w)
 
 
 #adult Mctq
 
 # Adjusting `sprep_w` based on the condition
 mctq_ad.data$sprep_w <- ifelse(mctq_ad.data$sprep_w >= hms::parse_hm("08:00:00") & 
                                   mctq_ad.data$sprep_w < hms::parse_hm("12:00:00"),
                                 mctq_ad.data$sprep_w + hours_to_add,
                                 mctq_ad.data$sprep_w)
 
 # Adjusting `sprep_w` based on the condition
 mctq_ad.data$sprep_w <- ifelse(mctq_ad.data$sprep_w >= hms::parse_hm("12:00:00") & 
                                   mctq_ad.data$sprep_w < hms::parse_hm("18:00:00"),
                                 mctq_ad.data$sprep_w - hours_to_add,
                                 mctq_ad.data$sprep_w)
 
 mctq_ad.data$sprep_w <- hms::as_hms(mctq_ad.data$sprep_w)
 
 
 # Adjusting `bt_w` based on the condition
 
 # Adjusting `sprep_w` based on the condition
 mctq_ad.data$bt_w <- ifelse(mctq_ad.data$bt_w >= hms::parse_hm("08:00:00") & 
                                mctq_ad.data$bt_w < hms::parse_hm("12:00:00"),
                              mctq_ad.data$bt_w + hours_to_add,
                              mctq_ad.data$bt_w)
 
 # Adjusting `bt_w` based on the condition
 mctq_ad.data$bt_w <- ifelse(mctq_ad.data$bt_w >= hms::parse_hm("12:00:00") & 
                                mctq_ad.data$bt_w < hms::parse_hm("18:00:00"),
                              mctq_ad.data$bt_w - hours_to_add,
                              mctq_ad.data$bt_w)
 
 mctq_ad.data$bt_w <- hms::as_hms(mctq_ad.data$bt_w)
 
 
 
 ####  exclude invalid data --> set certain vars to NA---------------------------------
 
 #many people misunderstood sprep "getting ready to fall asleep".
 # if getting ready to fall asleep < bedtime --> NA
 # if using an alarm on free days --> already no MSFsc computed
 
 # when to exclude light exposure time?
 # outlier plotting or smth like this?
 
 #work days

 # Applying the condition and setting the specified columns to NA
 mctq_ped.data$sprep_w[!(mctq_ped.data$sprep_w > mctq_ped.data$bt_w |  
                           mctq_ped.data$sprep_w == mctq_ped.data$bt_w |
                           (mctq_ped.data$sprep_w - mctq_ped.data$bt_w)/3600 < -18)] <- NA
 
 # mctq_ped.data$bt_w[!(mctq_ped.data$sprep_w > mctq_ped.data$bt_w |  
 #                        mctq_ped.data$sprep_w == mctq_ped.data$bt_w |
 #                        (mctq_ped.data$sprep_w - mctq_ped.data$bt_w)/3600 < -18)] <- NA
 
 
 #do the same for adult questionnaire
 
 # Applying the condition and setting the specified columns to NA
 mctq_ad.data$sprep_w[!(mctq_ad.data$sprep_w > mctq_ad.data$bt_w |  
                           mctq_ad.data$sprep_w == mctq_ad.data$bt_w |
                           (mctq_ad.data$sprep_w - mctq_ad.data$bt_w)/3600 < -18)] <- NA
 
 # mctq_ad.data$bt_w[!(mctq_ad.data$sprep_w > mctq_ad.data$bt_w |  
 #                        mctq_ad.data$sprep_w == mctq_ad.data$bt_w |
 #                        (mctq_ad.data$sprep_w - mctq_ad.data$bt_w)/3600 < -18)] <- NA
 # 
 # 
 
 
 #free days
 # Applying the condition and setting the specified columns to NA
 mctq_ped.data$sprep_f[!(mctq_ped.data$sprep_f > mctq_ped.data$bt_f |  
                           mctq_ped.data$sprep_f == mctq_ped.data$bt_f |
                           (mctq_ped.data$sprep_f - mctq_ped.data$bt_f)/3600 < -18)] <- NA
 
 # mctq_ped.data$bt_f[!(mctq_ped.data$sprep_f > mctq_ped.data$bt_f |  
 #                        mctq_ped.data$sprep_f == mctq_ped.data$bt_f |
 #                        (mctq_ped.data$sprep_f - mctq_ped.data$bt_f)/3600 < -18)] <- NA
 
 
 #do the same for adult questionnaire
 
 # Applying the condition and setting the specified columns to NA
 mctq_ad.data$sprep_f[!(mctq_ad.data$sprep_f > mctq_ad.data$bt_f |  
                          mctq_ad.data$sprep_f == mctq_ad.data$bt_f |
                          (mctq_ad.data$sprep_f - mctq_ad.data$bt_f)/3600 < -18)] <- NA
 
 # mctq_ad.data$bt_f[!(mctq_ad.data$sprep_f > mctq_ad.data$bt_f |  
 #                       mctq_ad.data$sprep_f == mctq_ad.data$bt_f |
 #                       (mctq_ad.data$sprep_f - mctq_ad.data$bt_f)/3600 < -20)] <- NA
 # 
 
 
 ##### Adult version computations -------------------------------------------------------------
 # compute free days
 mctq_ad.data <- mutate(mctq_ad.data, fd = fd(mctq_ad.data$wd))
 # compute Sleep onset work days
 mctq_ad.data <- mutate(mctq_ad.data, so_w = so(mctq_ad.data$sprep_w, mctq_ad.data$slat_w))
 # compute Sleep onset free days
 mctq_ad.data <- mutate(mctq_ad.data, so_f = so(mctq_ad.data$sprep_f, mctq_ad.data$slat_f))
 # compute get up time on work days 
 mctq_ad.data <- mutate(mctq_ad.data, gu_w = gu(mctq_ad.data$se_w, mctq_ad.data$si_w))
 # compute get up time on free days 
 mctq_ad.data <- mutate(mctq_ad.data, gu_f = gu(mctq_ad.data$se_f, mctq_ad.data$si_f))
 # compute sleep duration on work days 
 mctq_ad.data <- mutate(mctq_ad.data, sd_w = sdu(mctq_ad.data$so_w, mctq_ad.data$se_w))
 # compute sleep duration on free days 
 mctq_ad.data <- mutate(mctq_ad.data, sd_f = sdu(mctq_ad.data$so_f, mctq_ad.data$se_f))
 # compute  total time in bed on work days 
 mctq_ad.data <- mutate(mctq_ad.data, tbt_w = tbt(mctq_ad.data$bt_w, mctq_ad.data$gu_w))
 # compute  total time in bed on work days 
 mctq_ad.data <- mutate(mctq_ad.data, tbt_f = tbt(mctq_ad.data$bt_f, mctq_ad.data$gu_f))
 # compute mid sleep time on work days 
 mctq_ad.data <- mutate(mctq_ad.data, msw = msl(mctq_ad.data$so_w, mctq_ad.data$sd_w))
 # compute mid sleep time on free days 
 mctq_ad.data <- mutate(mctq_ad.data, msf = msl(mctq_ad.data$so_f, mctq_ad.data$sd_f))
 # compute average weekly sleep duration
 mctq_ad.data <- mutate(mctq_ad.data, sd_week = sd_week(mctq_ad.data$sd_w, mctq_ad.data$sd_f,
                                                          mctq_ad.data$wd))
 # compute average weekly light expposure
 mctq_ad.data <- mutate(mctq_ad.data, le_week = le_week(mctq_ad.data$le_w, mctq_ad.data$le_f,
                                                          mctq_ad.data$wd))
 # compute chronotype MSFsc
 mctq_ad.data <- mutate(mctq_ad.data, msf_sc = msf_sc(mctq_ad.data$msf, mctq_ad.data$sd_w, 
                                                        mctq_ad.data$sd_f, mctq_ad.data$sd_week,
                                                        mctq_ad.data$alarm_f))
 
 
##### Ped version computations -------------------------------------------------------------
 # compute free days
 mctq_ped.data <- mutate(mctq_ped.data, fd = fd(mctq_ped.data$wd))
 # compute Sleep onset work days
 mctq_ped.data <- mutate(mctq_ped.data, so_w = so(mctq_ped.data$sprep_w, mctq_ped.data$slat_w))
 # compute Sleep onset free days
 mctq_ped.data <- mutate(mctq_ped.data, so_f = so(mctq_ped.data$sprep_f, mctq_ped.data$slat_f))
 # compute get up time on work days 
 mctq_ped.data <- mutate(mctq_ped.data, gu_w = gu(mctq_ped.data$se_w, mctq_ped.data$si_w))
 # compute get up time on free days 
 mctq_ped.data <- mutate(mctq_ped.data, gu_f = gu(mctq_ped.data$se_f, mctq_ped.data$si_f))
 # compute sleep duration on work days 
 mctq_ped.data <- mutate(mctq_ped.data, sd_w = sdu(mctq_ped.data$so_w, mctq_ped.data$se_w))
 # compute sleep duration on free days 
 mctq_ped.data <- mutate(mctq_ped.data, sd_f = sdu(mctq_ped.data$so_f, mctq_ped.data$se_f))
 # compute  total time in bed on work days 
 mctq_ped.data <- mutate(mctq_ped.data, tbt_w = tbt(mctq_ped.data$bt_w, mctq_ped.data$gu_w))
 # compute  total time in bed on work days 
 mctq_ped.data <- mutate(mctq_ped.data, tbt_f = tbt(mctq_ped.data$bt_f, mctq_ped.data$gu_f))
 # compute mid sleep time on work days 
 mctq_ped.data <- mutate(mctq_ped.data, msw = msl(mctq_ped.data$so_w, mctq_ped.data$sd_w))
 # compute mid sleep time on free days 
 mctq_ped.data <- mutate(mctq_ped.data, msf = msl(mctq_ped.data$so_f, mctq_ped.data$sd_f))
 # compute average weekly sleep duration
 mctq_ped.data <- mutate(mctq_ped.data, sd_week = sd_week(mctq_ped.data$sd_w, mctq_ped.data$sd_f,
                                                          mctq_ped.data$wd))
 # compute average weekly light expposure
 mctq_ped.data <- mutate(mctq_ped.data, le_week = le_week(mctq_ped.data$le_w, mctq_ped.data$le_f,
                                                          mctq_ped.data$wd))
 # compute chronotype MSFsc
 mctq_ped.data <- mutate(mctq_ped.data, msf_sc = msf_sc(mctq_ped.data$msf, mctq_ped.data$sd_w, 
                                                        mctq_ped.data$sd_f, mctq_ped.data$sd_week,
                                                        mctq_ped.data$alarm_f))
 
 
# select all needed mctq vars here: you can add others later
 
mctq_ad.score <- mctq_ad.data %>% select (c(record_id, work, wd, alarm_f, le_w:msf_sc))
mctq_ped.score <- mctq_ped.data %>% select (c(record_id, work, wd, alarm_f, le_w:msf_sc))
  
# Combine dataframes and fill NAs from mctq_ped.score where mctq_ad.score has NAs
mctq_all.score <- mctq_ad.score %>%
  full_join(mctq_ped.score, by = "record_id", suffix = c(".mctq_ad.score", ".mctq_ped.score")) %>%
  mutate(across(ends_with(".mctq_ad.score"), ~ coalesce(.x, get(sub(".mctq_ad.score", ".mctq_ped.score", cur_column())))))

# Remove the now redundant .mctq_ped.score columns
mctq_all.score <- mctq_all.score %>%
  select(-ends_with(".mctq_ped.score"))%>%
  rename_with(~ sub("\\.mctq_ad\\.score$", "", .x), ends_with(".mctq_ad.score")) #return the original names

 
 
 
 ## LEBA factor scores ----------------------------------------------------------


# list all leba items
cols_to_recode <- c(
"slypos_leba_01",
"slypos_leba_02",
"slypos_leba_03",
"slypos_leba_04",
"slypos_leba_05",
"slypos_leba_06",
"slypos_leba_07",
"slypos_leba_08",
"slypos_leba_09",
"slypos_leba_10",
"slypos_leba_11",
"slypos_leba_12",
"slypos_leba_13", 
"slypos_leba_14",
"slypos_leba_15",
"slypos_leba_16",
"slypos_leba_17",
"slypos_leba_18",
"slypos_leba_19",
"slypos_leba_20",
"slypos_leba_21",
"slypos_leba_22",
"slypos_leba_23",
"slypos_leba_24",
"slypos_leba_25",
"slypos_leba_26",
"slypos_leba_27",
"slypos_leba_28",
"slypos_leba_29",
"slypos_leba_30",
"slypos_leba_31",
"slypos_leba_32",
"slypos_leba_33",
"slypos_leba_36",
"slypos_leba_37",
"slypos_leba_38",
"slypos_leba_39",
"slypos_leba_40",
"slypos_leba_41",
"slypos_leba_42",
"slypos_leba_43",
"slypos_leba_44", 
"slypos_leba_45",
"slypos_leba_46",
"slypos_leba_47", 
"slypos_leba_48",
"slypos_leba_49",
"slypos_leba_50")

#recode "I dont know"Does not apply" to "never". (we also did this for the questionnaire development)



data <- data %>%
  mutate(across(all_of(cols_to_recode), ~ as.numeric(replace(., . == 0, 1))))



## Recoding inversed item for sum score

data <- data %>%
  mutate(
    slypos_leba_08R = as.numeric(slypos_leba_08),  # Convert to numeric if labelled
    slypos_leba_08R = recode(slypos_leba_08R,
                            `5` = 1,
                            `4` = 2,
                            `2` = 4,
                            `1` = 5)
  )




data <- data %>%
  mutate(
    slypos_leba_38R = as.numeric(slypos_leba_38),  # Convert to numeric if labelled
    slypos_leba_38R = recode(slypos_leba_38R,
                             `5` = 1,
                             `4` = 2,
                             `2` = 4,
                             `1` = 5)
  )

# compute sum scores for the factors.

# used recoded slypos_leba_38 (5-1) because it goes in the opposite direction 
# regarding filter light on the evening vs. filtering light during the day for 
#the other items.

data <- data %>% mutate(
  F1_leba = slypos_leba_16 + slypos_leba_17 + slypos_leba_38R
  )

data <- data %>% mutate( 
  F2_leba = slypos_leba_08R +  slypos_leba_09 + slypos_leba_10  + slypos_leba_11 +  slypos_leba_12 + slypos_leba_07)


  data <- data %>% mutate( 
    F3_leba = slypos_leba_27 + slypos_leba_03 +  slypos_leba_42  +  slypos_leba_43 +  slypos_leba_30)


  data <- data %>% mutate( 
    F4_leba = slypos_leba_32 + slypos_leba_37 +  slypos_leba_40 +  slypos_leba_41)

  data <- data %>% mutate( 
    F5_leba = slypos_leba_48 + slypos_leba_47 + slypos_leba_25 + slypos_leba_04 + slypos_leba_01 )


  
  
  
  
  #Creating datasubsets ----------------------------------------------------------
  
  ## create and save demographics datasets----------------------------------------
  
  demvars <- c("slypos_demographics_age",
               "slypos_demographics_sex.factor",
               "slypos_demographics_gender.factor" ,
               "slypos_demographics_language.factor", 
               "slypos_demographics_work_or_school.factor",
               "slypos_demographics_school.factor")
  
  
  #separating naming and  reducing data for descriptive Table 
  demvars.data <- data %>% dplyr::select(all_of(demvars))
  
  
  # recode the Gender factor so it will only show "Gender Diverse" in the summary table
  demvars.data$slypos_demographics_gender.factor <-
    recode_factor(demvars.data$slypos_demographics_gender.factor, "No"="Yes",
                  "Yes"= "No")
  
  
  #save demoraphic variables
  save(demvars.data,file="./03_demographics/demvars.data.rda")
  
 
  ## Country & timezone datasets and data wrangling ------------------------------------
  
  #save timezone variables
  country_tz.data <- data %>% dplyr::select(c(slypos_demographics_tz, slypos_demographics_tz.factor))
  
  save(country_tz.data,file="./03_demographics/country_tz.data.rda")
  
  
  #Check for errors with missing Timezone data
  sum(is.na(country_tz.data$slypos_demographics_tz.factor))
  # fix errors
  
  TZ <- as.data.frame(str_split_fixed(country_tz.data$slypos_demographics_tz.factor, "-", 2))
  TZ[TZ$V1=="Tanzania", 2] <- " East Africa/Dodoma (UTC +03:00)"
  TZ[TZ$V1=="Macedonia", 2] <- " European /Skopje (UTC +01:00)"
  TZ[TZ$V1=="Taiwan", 2] <- " Asia /Taipei City (UTC +08:00)"
  TZ[TZ$V1=="Iran", 2] <- " Iran /Tehran (UTC +0:30)"
  
  
  # calculate number of different countries in the dataset
  num.countries <- as.data.frame(unique(TZ$V1))
  nrow(num.countries)
  
  # calculate number of different timezones in the dataset
  UTC <- as.data.frame(str_split_fixed(TZ$V2, "UTC", 2))
  num.UTC <- as.data.frame(unique(UTC$V2))
  nrow(num.UTC)
  
  
  # create time-zone.csv file
  timezone <- TZ %>% 
    group_by(V2) %>% 
    count() %>% 
    as.data.frame()
  
  write.csv(timezone, "./03_demographics/timezone.csv")
  
  ## create date variable from timestamp----------------------------------------
  
  
  data <- data %>% mutate(
    fill_date = as.Date(your_light_behaviour_timestamp))
  
  ## Subdataset Sum scores of questionnaires------------------------------------
  
  data <- merge(data, mctq_all.score, by="record_id")
  
  
  mctqvars <- names(mctq_all.score)
  
  scorevars <- c("Photophila_score", "Photophobia_score", "ASE_score",
                 "ASE_levels", "Promis_sd_sum", "Promis_sri_sum","PDS_score_m",
                 #"Promis_sd_ped_sum", "Promis_sri_ped_sum","Promis_sd_ad_sum", "Promis_sri_ad_sum",
                 "PDS_score_f",  "F1_leba", "F2_leba", "F3_leba", "F4_leba", "F5_leba")
                 #add mctq scores
               
  ## create and save dataset for data analysis ----------------------------------
  
  # select only the data needed for analysis
  
  analysis.data <- data %>% dplyr::select(c(record_id, all_of(demvars), 
                                            slypos_demographics_tz.factor, 
                                            fill_date, all_of(scorevars), 
                                            all_of(mctqvars)))
  
  # create numeric chronotype and weekly light exposure vars
  analysis.data <- analysis.data %>% 
    mutate(msf_sc_num = as.numeric(msf_sc)/3600,
           le_week_num = as.numeric(le_week)/3600) 
  
  
  save(analysis.data, file="./04_data_analysis/analysis.data.rda")
  