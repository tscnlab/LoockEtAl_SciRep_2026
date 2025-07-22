#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar & Ann-Sophie Loock                                                                


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

# convert_to_decimal_hours <- function(time_string) {
#   # Split the string into hours and minutes
#   time_parts <- strsplit(time_string, ":")[[1]]
#   hours <- as.numeric(time_parts[1])
#   minutes <- as.numeric(time_parts[2])
#   
#   # Convert to decimal hours
#   decimal_hours <- hours + (minutes / 60)
#   return(decimal_hours)
# }

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
# -- No, decided to not do this and stick to the original interpretation of scores. 



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

#create levels for ASE sum score: 
# score 0 to 9  =  low; 10 to 19 moderate; 20 to 39 = high
# interpretation: high = appropriate sleep environment, low = inappropriate sleeping environment

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


## Self-Administered Rating Scale for Pubertal Development ---------------------

# recode the "I don't know" items (5) and the "prefer not to say" items (6) to NA
data$slypos_puberty_01 <- ifelse(data$slypos_puberty_01 == 5 | data$slypos_puberty_01 == 6, NA, data$slypos_puberty_01)
data$slypos_puberty_02 <- ifelse(data$slypos_puberty_02 == 5 | data$slypos_puberty_02 == 6, NA, data$slypos_puberty_02)
data$slypos_puberty_03 <- ifelse(data$slypos_puberty_03 == 5 | data$slypos_puberty_03 == 6, NA, data$slypos_puberty_03)
data$slypos_puberty_boys_01 <- ifelse(data$slypos_puberty_boys_01 == 5 | data$slypos_puberty_boys_01 == 6, NA, data$slypos_puberty_boys_01)
data$slypos_puberty_boys_02 <- ifelse(data$slypos_puberty_boys_02 == 5 | data$slypos_puberty_boys_02 == 6, NA, data$slypos_puberty_boys_02)
data$slypos_puberty_girl_01 <- ifelse(data$slypos_puberty_girl_01 == 5 | data$slypos_puberty_girl_01 == 6 , NA, data$slypos_puberty_girl_01)

#recode the yes/no question for girls to (no=1) and (yes=4) 
#& prefer not to say to NA
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

# To do's:
# what to do with the caffeine and food timing items?


#### Sanity check & cleaning --------------------
# i.e., sleep time later than bedtime in mctq (items 3 and 4 entered incorrectly)

# # STEP 1: subset and filter to regular work / school schedule only
# mctq_checks <- data %>% 
#   select(
#     record_id,                    #For later merges
#     slypos_mctq_01.factor,        #"Do you have a regular work schedule?"
#     slypos_mctq_01_ped.factor,    #"Do you have a regular school schedule?"
#     contains("mctq"),             #mctq items
#     -contains("attentioncheck")   #no attention checks
#     ) %>%
#   # Define flags for irregular schedules
#   mutate(
#     slypos_mctq_01.factor = na_if(as.character(slypos_mctq_01.factor), ""), #set empty fields to NA
#     slypos_mctq_01_ped.factor = na_if(as.character(slypos_mctq_01_ped.factor), "") #set empty fields to NA
#   ) %>%
#   mutate(
#     regular = case_when(
#       slypos_mctq_01.factor == "Yes" ~ TRUE, 
#       slypos_mctq_01_ped.factor == "Yes" ~ TRUE, 
#       TRUE ~ FALSE
#     )) %>%
#   # select only regular schedules
#   filter(regular) %>% 
#   select(-regular) # drop helper column 

# STEP 1 (alternative): do NOT filter our irregular schedules. 
mctq_checks <- data %>% 
  select(
    record_id,
    slypos_mctq_01.factor,     
    slypos_mctq_01_ped.factor,
    contains("mctq"), 
    -contains("attentioncheck")
  ) %>%
  # blanks "" → NA
  mutate(across(
    where(~ is.character(.x) || inherits(.x, "labelled")), 
    ~ na_if(as.character(.x), "")
  )) %>%
  # define who has regular schedule
  mutate(
    regular = (slypos_mctq_01.factor %in% "Yes") | (slypos_mctq_01_ped.factor %in% "Yes")
  ) %>%
  # assign Adult vs. Child depending on whether the adult or pediatric form is filled out
  mutate(
    group = case_when(
      !is.na(slypos_mctq_03)        | !is.na(slypos_mctq_04)         ~ "Adult",
      !is.na(slypos_mctq_03_ped)    | !is.na(slypos_mctq_04_ped)     ~ "Child",
      TRUE                                          ~ NA_character_
    )
  )

# STEP 2: fix bed- / sleep times 
mctq_checks <- mctq_checks %>%
  mutate(
    # raw parse to time object
    bed_adult_raw   = hms::parse_hm(slypos_mctq_03),
    sleep_adult_raw = hms::parse_hm(slypos_mctq_04),
    bed_child_raw   = hms::parse_hm(slypos_mctq_03_ped),
    sleep_child_raw = hms::parse_hm(slypos_mctq_04_ped),
    # for free days
    bed_adult_free_raw   = hms::parse_hm(slypos_mctq_10),
    sleep_adult_free_raw = hms::parse_hm(slypos_mctq_11),
    bed_child_free_raw   = hms::parse_hm(slypos_mctq_10_ped),
    sleep_child_free_raw = hms::parse_hm(slypos_mctq_11_ped),
    
    # time corrections: 
    # 00:00/12:00 → 24h   (unifying midnight) 
    # 12:00–12:59 → +12h  (assuming close-to-midnight sleep)
    # 06:01–11:59 → +12h  (assuming 12h entry format instead of 24h)
    
    bed_adult = case_when(
      slypos_mctq_03  %in% c("00:00","12:00")                    ~ hours(24),
      bed_adult_raw  > hms::parse_hm("06:00") & bed_adult_raw  <= hms::parse_hm("11:59") ~ bed_adult_raw + hours(12),
      bed_adult_raw  >= hms::parse_hm("12:00") & bed_adult_raw  <  hms::parse_hm("13:00") ~ bed_adult_raw + hours(12),
      TRUE                                                         ~ bed_adult_raw),
    sleep_adult = case_when(
      slypos_mctq_04  %in% c("00:00","12:00")                    ~ hours(24),
      sleep_adult_raw > hms::parse_hm("06:00") & sleep_adult_raw <= hms::parse_hm("11:59")~ sleep_adult_raw + hours(12),
      sleep_adult_raw >= hm("12:00") & sleep_adult_raw <  hm("13:00")~ sleep_adult_raw + hours(12),
      TRUE                                                         ~ sleep_adult_raw),
    bed_child = case_when(
      slypos_mctq_03_ped %in% c("00:00","12:00")                    ~ hours(24),
      bed_child_raw  > hm("06:00") & bed_child_raw  <= hm("11:59")   ~ bed_child_raw + hours(12),
      bed_child_raw  >= hm("12:00") & bed_child_raw  <  hm("13:00")   ~ bed_child_raw + hours(12),
      TRUE                                                           ~ bed_child_raw),
    sleep_child = case_when(
      slypos_mctq_04_ped %in% c("00:00","12:00")                    ~ hours(24),
      sleep_child_raw > hm("06:00") & sleep_child_raw <= hm("11:59") ~ sleep_child_raw + hours(12),
      sleep_child_raw >= hm("12:00") & sleep_child_raw <  hm("13:00") ~ sleep_child_raw + hours(12),
      TRUE                                                           ~ sleep_child_raw),
    # for free days
    bed_adult_free = case_when(
      slypos_mctq_10  %in% c("00:00","12:00")                    ~ hours(24),
      bed_adult_free_raw  > hm("06:00") & bed_adult_free_raw  <= hm("11:59") ~ bed_adult_free_raw + hours(12),
      bed_adult_free_raw  >= hm("12:00") & bed_adult_free_raw  <  hm("13:00") ~ bed_adult_free_raw + hours(12),
      TRUE                                                               ~ bed_adult_free_raw), 
    sleep_adult_free = case_when(
      slypos_mctq_11  %in% c("00:00","12:00")                    ~ hours(24),
      sleep_adult_free_raw > hm("06:00") & sleep_adult_free_raw <= hm("11:59") ~ sleep_adult_free_raw + hours(12),
      sleep_adult_free_raw >= hm("12:00") & sleep_adult_free_raw <  hm("13:00") ~ sleep_adult_free_raw + hours(12),
      TRUE                                                                    ~ sleep_adult_free_raw), 
    bed_child_free = case_when(
      slypos_mctq_10_ped %in% c("00:00","12:00")                    ~ hours(24),
      bed_child_free_raw  > hm("06:00") & bed_child_free_raw  <= hm("11:59")   ~ bed_child_free_raw + hours(12),
      bed_child_free_raw  >= hm("12:00") & bed_child_free_raw  <  hm("13:00")   ~ bed_child_free_raw + hours(12),
      TRUE                                                                      ~ bed_child_free_raw), 
    sleep_child_free = case_when(
      slypos_mctq_11_ped %in% c("00:00","12:00")                    ~ hours(24),
      sleep_child_free_raw > hm("06:00") & sleep_child_free_raw <= hm("11:59") ~ sleep_child_free_raw + hours(12),
      sleep_child_free_raw >= hm("12:00") & sleep_child_free_raw <  hm("13:00") ~ sleep_child_free_raw + hours(12),
      TRUE                                                                      ~ sleep_child_free_raw),
    
    # combine child / adult into one variable for bed- and sleep time
    bed   = coalesce(bed_adult,   bed_child), # given a set of vectors, find the first non-missing value
    sleep = coalesce(sleep_adult, sleep_child),
    # free days
    bed_free = coalesce(bed_adult_free,   bed_child_free), 
    sleep_free = coalesce(sleep_adult_free, sleep_child_free)) %>%
  
  # next day bump
  mutate(
    sleep       = case_when(
      sleep < bed            &  sleep <= hours(6)     ~ sleep + hours(24),
      TRUE                                            ~ sleep),
    sleep_free  = case_when(
      sleep_free  < bed_free & sleep_free <= hours(6) ~ sleep_free  + hours(24),
      TRUE                                            ~ sleep_free)) %>%
  
  # tag inverted bed- / sleep times 
  # accept inversion of 2h
  # greater differences between bed/sleep times are considered invalid
  mutate(
    inverted = bed > sleep,
    diff     = bed - sleep,             
    status   = case_when(
      !inverted                   ~ "ok",
      inverted & diff <= hours(2) ~ "corrected", 
      TRUE                        ~ "flag"),
    # for free days
    inverted_free = bed_free > sleep_free,
    diff_free     = bed_free - sleep_free,             
    status_free   = case_when(
      !inverted_free                        ~ "ok",
      inverted_free & diff_free <= hours(2) ~ "corrected", 
      TRUE                                  ~ "flag")) %>%
  
  # swap 2h inversions
  mutate(
    bed_swapped   = if_else(status == "corrected", sleep, bed),
    sleep_swapped = if_else(status == "corrected", bed, sleep),
    # free days
    bed_swapped_free   = if_else(status_free == "corrected", sleep_free, bed_free),
    sleep_swapped_free = if_else(status_free == "corrected", bed_free, sleep_free)) %>%
  
  # STEP 4: Build final data set
  mutate(
    bed_new        = case_when(
      status     == "flag"  ~ hms::as_hms(NA),
      TRUE                   ~ bed_swapped),
    sleep_new      = case_when(
      status     == "flag"  ~ hms::as_hms(NA),
      TRUE                   ~ sleep_swapped),
    bed_free_new   = case_when(
      status_free== "flag"  ~ hms::as_hms(NA),
      TRUE                   ~ bed_swapped_free),
    sleep_free_new = case_when(
      status_free== "flag"  ~ hms::as_hms(NA),
      TRUE                   ~ sleep_swapped_free)) %>%
  select(
    record_id, group, regular, status, status_free,
    bed_new, sleep_new, bed_free_new, sleep_free_new,
    slypos_mctq_03_new, slypos_mctq_04_new,
    slypos_mctq_03_ped_new, slypos_mctq_04_ped_new,
    slypos_mctq_10_new, slypos_mctq_11_new,
    slypos_mctq_10_ped_new, slypos_mctq_11_ped_new)
  
 

# merge back onto `data` by record_id:
data <- data %>%
  left_join(
    mctq_final,
    by = "record_id"
  )


# Compute MSFsc for analysis ------------------------------------
msf <- data %>%
  # Select columns needed for MSFsc (keeping record_id, group, status)
  select(
    record_id, status, status_free, group,
    wd             = slypos_mctq_02,         # work days
    bt_w           = slypos_mctq_03_new,     # bedtime on work days
    sprep_w        = slypos_mctq_04_new,     # sleep time on work days
    slat_w         = slypos_mctq_05,         # sleep onset latency on work days
    se_w           = slypos_mctq_06,         # sleep end on work days
    si_w           = slypos_mctq_07,         # sleep inertia on work days
    # free-day equivalents:
    bt_f           = slypos_mctq_10_new,
    sprep_f        = slypos_mctq_11_new,     # sleep time  
    slat_f         = slypos_mctq_12,         # sleep onset latency
    se_f           = slypos_mctq_13,         # sleep end
    si_f           = slypos_mctq_14,         # sleep inertia
    alarm_f        = slypos_mctq_15,         # alarm 
    
    # and for pediatric sample: 
    wd_ped             = slypos_mctq_02_ped,         
    bt_w_ped           = slypos_mctq_03_ped_new,     
    sprep_w_ped        = slypos_mctq_04_ped_new,     
    slat_w_ped         = slypos_mctq_05_ped,         
    se_w_ped           = slypos_mctq_06_ped,         
    si_w_ped           = slypos_mctq_07_ped,         
    # free-day equivalents:
    bt_f_ped           = slypos_mctq_10_ped_new,
    sprep_f_ped        = slypos_mctq_11_ped_new,       
    slat_f_ped         = slypos_mctq_12_ped,         
    se_f_ped           = slypos_mctq_13_ped,         
    si_f_ped           = slypos_mctq_14_ped,         
    alarm_f_ped        = slypos_mctq_15_ped,          
  ) %>%
  mutate(across(
    c(slat_w, slat_w_ped, slat_f, slat_f_ped, si_w, si_w_ped, si_f, si_f_ped), 
    as.numeric
  )) %>%
  # Drop any flagged rows
  filter(status != "flag", status_free != "flag_free") %>%
  # Parse to hms / durations
  mutate(
    # pick adult vs child answers
    wd      = if_else(group=="Adult", as.integer(wd), as.integer(wd_ped)), 
    bt_w    = if_else(group=="Adult", hms::parse_hm(as.character(bt_w)), hms::parse_hm(as.character(bt_w_ped))),
    sprep_w = if_else(group=="Adult", hms::parse_hm(as.character(sprep_w)), hms::parse_hm(as.character(sprep_w_ped))),
    bt_f    = if_else(group=="Adult", hms::parse_hm(as.character(bt_f)), hms::parse_hm(as.character(bt_f_ped))),
    sprep_f = if_else(group=="Adult", hms::parse_hm(as.character(sprep_f)), hms::parse_hm(as.character(sprep_f_ped))),
    alarm_f = if_else(group=="Adult", as.logical(alarm_f == "1"), as.logical(alarm_f_ped == "1")),
    slat_w  = if_else(group=="Adult", slat_w, slat_w_ped),
    se_w    = if_else(group=="Adult", se_w, se_w_ped),
    si_w    = if_else(group=="Adult", si_w, si_w_ped),
    slat_f  = if_else(group=="Adult", slat_f, slat_f_ped),
    se_f    = if_else(group=="Adult", se_f, se_f_ped),
    si_f    = if_else(group=="Adult", si_f, si_f_ped)
    ) %>%
  # Format conversions
  mutate(
    slat_w = dminutes(as.numeric(slat_w)), # sleep onset latency
    slat_f = dminutes(as.numeric(slat_f)),
    se_w   = hms::parse_hm(as.character(se_w)), # sleep end
    se_f   = hms::parse_hm(as.character(se_f)),
    si_w   = dminutes(as.numeric(si_w)), # sleep inertia
    si_f   = dminutes(as.numeric(si_f))
  ) %>%
  # Compute the MCTQ intermediates & msf_sc in one go
  mutate(
    so_w    = so(sprep_w, slat_w), # sleep onset
    so_f    = so(sprep_f, slat_f),
    sd_w    = sdu(so_w, se_w),     # sleep duration
    sd_f    = sdu(so_f, se_f),
    msf     = msl(so_f, sd_f),     # midsleep
    sd_week = sd_week(sd_w, sd_f, wd),
    msf_sc  = msf_sc(msf, sd_w, sd_f, sd_week, alarm_f) # midsleep corrected
  )

data <- data %>%
  left_join(
    msf %>% select(record_id, msf, msf_sc), # only MSFsc selected for now
    by = "record_id"
  )

# Add light exposure from MCTQ
light_expo <- data %>% select(
  record_id, group, 
  wd = slypos_mctq_02, 
  le_w = slypos_mctq_25,
  le_f = slypos_mctq_26,
  
  wd_ped = slypos_mctq_02_ped, 
  le_w_ped = slypos_mctq_25_ped,
  le_f_ped = slypos_mctq_26_ped
  ) %>%
  # Convert to correct format: Blanks -> NAs
  mutate(across(
    c(le_w, le_f, le_w_ped, le_f_ped),
    ~ na_if(as.character(.x), "")
  )) %>%
  # Select corresponding variables
  filter(!is.na(group)) %>%
  mutate(
    wd   = if_else(group=="Adult", as.integer(wd), as.integer(wd_ped)),
    le_w = if_else(group=="Adult",
                   as.duration(hm(le_w)),
                   as.duration(hm(le_w_ped))),
    le_f = if_else(group=="Adult",
                   as.duration(hm(le_f)),
                   as.duration(hm(le_f_ped)))
  ) %>%
  # 4) compute weekly average light exposure
  mutate(
    le_week = le_week(le_w, le_f, wd)
  ) %>%
  select(record_id, le_week)

data <- data %>% left_join(light_expo, by = "record_id")


# Why are we losing so many cases in calculating MSF and MSF sc?
## --> We filter for status and select only ok and corrected, NA rows are dropped. 
## From the entire survey dataset of N = 774, 
## we only obtained valid MCTQ data from N = 551 participants. 

irregulars <- data %>%  filter(
    (is.na(slypos_mctq_01)     | slypos_mctq_01     == 2),
    (is.na(slypos_mctq_01_ped) | slypos_mctq_01_ped == 2)) 
# we have 203 cases, in which participants do NOT have a regular work/school schedule

# There is still 20 cases unaccounted for in the difference between data's length and msf's length
msf_ids <- msf %>% pull(record_id)
irregular_ids <- irregulars %>% pull(record_id)
missing_ids <- data %>% filter(
    !record_id %in% msf_ids,
    !record_id %in% irregular_ids) %>% pull(record_id)

# Look at the remaining 20 cases
View(data %>% filter(record_id %in% missing_ids))






# RAFA's MCTQ code (not currently/no longer implemented) ------------------

#### create subdata set for mctq --------------------------------------------
# Collect the names of all variables that include "mctq" in the name
# mctq_columns <- grep("mctq", colnames(data), value = TRUE)
# mctq_columns
# 
# # Filter out the column names that contain "ped"
# filtered_columns <- grep(".factor", mctq_columns, invert = TRUE, value = TRUE)
# 
# # Filter out the column names that contain "ped"
# filtered_columns <- grep("attentioncheck", filtered_columns, invert = TRUE, value = TRUE)
# 
# # Print the result
# filtered_columns
# 
# # Filter out the column names that contain "ped"
# mctq_ped <- grep("ped", filtered_columns, invert = F, value = TRUE)
# 
# # Filter out the column names that contain "ped"
# mctq_ad <- grep("ped", filtered_columns, invert = T, value = TRUE)
# 
# #create subdataset
# mctq_ad.data <-  data %>% dplyr::select(c(record_id, mctq_ad))
# #create subdataset
# mctq_ped.data <-  data %>% dplyr::select(c(record_id, mctq_ped))

#### load testdataset for mctq--------------------------------------------------
# 
# mctq.testdata <- readr::read_csv(
#   mctq::raw_data("vignette_mctq.csv"),
#   col_types = readr::cols(.default = "c")
# )
# 
#  #### Converting data to mctq form ---------------------------------------------
#  
#  #renaming according to mctq pacakge and papers
#  mctq_ad.data <-  mctq_ad.data %>% rename(
#    #id = record_id,
#    work = slypos_mctq_01, # need to convert into logical 1=True, 2=No
#    #work day variables
#    wd = slypos_mctq_02, 
#    bt_w = slypos_mctq_03, 
#    sprep_w = slypos_mctq_04,
#    slat_w = slypos_mctq_05,
#    se_w = slypos_mctq_06,
#    si_w = slypos_mctq_07,
#    alarm_w =slypos_mctq_08, # need to convert into logical 1=True, 0=No
#    wake_before_w = slypos_mctq_09, # need to convert into logical 1=True, 0=No
#    le_w = slypos_mctq_25,
#    # free day variables
# 
#    bt_f = slypos_mctq_10, 
#    sprep_f = slypos_mctq_11,
#    slat_f = slypos_mctq_12,
#    se_f = slypos_mctq_13,
#    si_f = slypos_mctq_14,
#    alarm_f=slypos_mctq_15, # need to convert into logical 1=True, 0=No
#    reasons_f = slypos_mctq_16, # need to convert into logical 1=True, 0=No
#    reasons_why_f = slypos_mctq_17, # need to convert into factor with 3 levels: 1= "Child(ren)/pet(s), 2="Hobbies", 3="Others"
#    le_f = slypos_mctq_26,
#  )
# 
# 
#  mctq_ped.data <-  mctq_ped.data %>% rename(
#    #id = record_id,
#    work = slypos_mctq_01_ped, # need to convert into logical 1=True, 2=No
#    #work day variables
#    wd = slypos_mctq_02_ped, 
#    bt_w = slypos_mctq_03_ped, 
#    sprep_w = slypos_mctq_04_ped,
#    slat_w = slypos_mctq_05_ped,
#    se_w = slypos_mctq_06_ped,
#    si_w = slypos_mctq_07_ped,
#    alarm_w =slypos_mctq_08_ped, # need to convert into logical 1=True, 0=No
#    wake_before_w = slypos_mctq_09_ped, # need to convert into logical 1=True, 0=No
#    le_w = slypos_mctq_25_ped,
#    # free day variables
#    
#    bt_f = slypos_mctq_10_ped, 
#    sprep_f = slypos_mctq_11_ped,
#    slat_f = slypos_mctq_12_ped,
#    se_f = slypos_mctq_13_ped,
#    si_f = slypos_mctq_14_ped,
#    alarm_f=slypos_mctq_15_ped, # need to convert into logical 1=True, 0=No
#    reasons_f = slypos_mctq_16_ped, # need to convert into logical 1=True, 0=No
#    reasons_why_f = slypos_mctq_17_ped, # need to convert into factor with 3 levels: 1= "Family members/pets, 2="Hobbies", 3="Others"
#    le_f = slypos_mctq_26_ped,
#  )
#  
#  # Convert into Logical vars
#  mctq_ad.data$work <- mctq_ad.data$work==1
#  mctq_ped.data$work <- mctq_ped.data$work==1
#  mctq_ad.data$alarm_w <- mctq_ad.data$alarm_w==1
#  mctq_ped.data$alarm_w <- mctq_ped.data$alarm_w==1
#  mctq_ad.data$wake_before_w <-  mctq_ad.data$wake_before_w==1
#  mctq_ped.data$wake_before_w <- mctq_ped.data$wake_before_w==1
#  mctq_ad.data$alarm_f <- mctq_ad.data$alarm_f==1
#  mctq_ped.data$alarm_f <- mctq_ped.data$alarm_f==1
#  mctq_ad.data$reasons_f <- mctq_ad.data$reasons_f==1
#  mctq_ped.data$reasons_f <- mctq_ped.data$reasons_f==1
#  
#  # Convert into Factor vars
#  mctq_ad.data$reasons_why_f = as.factor(mctq_ad.data$reasons_why_f)
#  levels(mctq_ad.data$reasons_why_f) = c("Child(ren)/pet(s)","Hobbies","Others")
#  
#  mctq_ped.data$reasons_why_f = as.factor(mctq_ped.data$reasons_why_f)
#  levels(mctq_ped.data$reasons_why_f) = c("Family members/pet(s)","Hobbies","Others")
#  
#  # Convert into hms vars
#  mctq_ad.data$bt_w <- hms::parse_hm(mctq_ad.data$bt_w)
#  mctq_ad.data$sprep_w <- hms::parse_hm(mctq_ad.data$sprep_w)
#  mctq_ad.data$se_w <- hms::parse_hm(mctq_ad.data$se_w)
#  mctq_ad.data$bt_f <- hms::parse_hm(mctq_ad.data$bt_f)
#  mctq_ad.data$sprep_f <- hms::parse_hm(mctq_ad.data$sprep_f)
#  mctq_ad.data$se_f <- hms::parse_hm(mctq_ad.data$se_f)
#  
#  mctq_ped.data$bt_w <- hms::parse_hm(mctq_ped.data$bt_w)
#  mctq_ped.data$sprep_w <- hms::parse_hm(mctq_ped.data$sprep_w)
#  mctq_ped.data$se_w <- hms::parse_hm(mctq_ped.data$se_w)
#  mctq_ped.data$bt_f <- hms::parse_hm(mctq_ped.data$bt_f)
#  mctq_ped.data$sprep_f <- hms::parse_hm(mctq_ped.data$sprep_f)
#  mctq_ped.data$se_f <- hms::parse_hm(mctq_ped.data$se_f)
#  
#  
#  # Convert into duration vars
#  mctq_ad.data$slat_w <-  lubridate::dminutes(mctq_ad.data$slat_w)
#  mctq_ad.data$slat_f <-  lubridate::dminutes(mctq_ad.data$slat_f)
#  mctq_ad.data$si_w <-  lubridate::dminutes(mctq_ad.data$si_w)
#  mctq_ad.data$si_f <-  lubridate::dminutes(mctq_ad.data$si_f)
#  
#  mctq_ad.data$le_w[mctq_ad.data$le_w== ""] <- NA
#  mctq_ad.data$le_w <- sapply(mctq_ad.data$le_w, convert_to_decimal_hours)
#  mctq_ad.data$le_w <- lubridate::dhours(mctq_ad.data$le_w)
#  mctq_ad.data$le_f[mctq_ad.data$le_f== ""] <- NA
#  mctq_ad.data$le_f <- sapply(mctq_ad.data$le_f, convert_to_decimal_hours)
#  mctq_ad.data$le_f <- lubridate::dhours(mctq_ad.data$le_f)
# 
#  
#  # Convert into duration vars
#  mctq_ped.data$slat_w <-  lubridate::dminutes(mctq_ped.data$slat_w)
#  mctq_ped.data$slat_f <-  lubridate::dminutes(mctq_ped.data$slat_f)
#  mctq_ped.data$si_w <-  lubridate::dminutes(mctq_ped.data$si_w)
#  mctq_ped.data$si_f <-  lubridate::dminutes(mctq_ped.data$si_f)
#  
#  mctq_ped.data$le_w[mctq_ped.data$le_w== ""] <- NA
#  mctq_ped.data$le_w <- sapply(mctq_ped.data$le_w, convert_to_decimal_hours)
#  mctq_ped.data$le_w <- lubridate::dhours(mctq_ped.data$le_w)
#  mctq_ped.data$le_f[mctq_ped.data$le_f== ""] <- NA
#  mctq_ped.data$le_f <- sapply(mctq_ped.data$le_f, convert_to_decimal_hours)
#  mctq_ped.data$le_f <- lubridate::dhours(mctq_ped.data$le_f)
#  
#  ####  change wrong formatting ----------------------------------------------
#   #translate wrong format (e.g. 11 to 23:00)
#  
#   # Convert hours to seconds
#  hours_to_add <- hms::hms(12*3600)
#  
#  #pediatric Mctq
#  
#  # Adjusting `sprep_w` based on the condition
#  mctq_ped.data$sprep_w <- ifelse(mctq_ped.data$sprep_w >= hms::parse_hm("08:00:00") & 
#                                    mctq_ped.data$sprep_w < hms::parse_hm("12:00:00"),
#                                  mctq_ped.data$sprep_w + hours_to_add,
#                                  mctq_ped.data$sprep_w)
#  
#  # Adjusting `sprep_w` based on the condition
#  mctq_ped.data$sprep_w <- ifelse(mctq_ped.data$sprep_w >= hms::parse_hm("12:00:00") & 
#                                    mctq_ped.data$sprep_w < hms::parse_hm("18:00:00"),
#                                  mctq_ped.data$sprep_w - hours_to_add,
#                                  mctq_ped.data$sprep_w)
#  
#  mctq_ped.data$sprep_w <- hms::as_hms(mctq_ped.data$sprep_w)
#  
#  
#  # Adjusting `bt_w` based on the condition
# 
#  # Adjusting `sprep_w` based on the condition
#  mctq_ped.data$bt_w <- ifelse(mctq_ped.data$bt_w >= hms::parse_hm("08:00:00") & 
#                                    mctq_ped.data$bt_w < hms::parse_hm("12:00:00"),
#                                  mctq_ped.data$bt_w + hours_to_add,
#                                  mctq_ped.data$bt_w)
#  
#  # Adjusting `bt_w` based on the condition
#  mctq_ped.data$bt_w <- ifelse(mctq_ped.data$bt_w >= hms::parse_hm("12:00:00") & 
#                                    mctq_ped.data$bt_w < hms::parse_hm("18:00:00"),
#                                  mctq_ped.data$bt_w - hours_to_add,
#                                  mctq_ped.data$bt_w)
#  
#  mctq_ped.data$bt_w <- hms::as_hms(mctq_ped.data$bt_w)
#  
#  
#  #adult Mctq
#  
#  # Adjusting `sprep_w` based on the condition
#  mctq_ad.data$sprep_w <- ifelse(mctq_ad.data$sprep_w >= hms::parse_hm("08:00:00") & 
#                                    mctq_ad.data$sprep_w < hms::parse_hm("12:00:00"),
#                                  mctq_ad.data$sprep_w + hours_to_add,
#                                  mctq_ad.data$sprep_w)
#  
#  # Adjusting `sprep_w` based on the condition
#  mctq_ad.data$sprep_w <- ifelse(mctq_ad.data$sprep_w >= hms::parse_hm("12:00:00") & 
#                                    mctq_ad.data$sprep_w < hms::parse_hm("18:00:00"),
#                                  mctq_ad.data$sprep_w - hours_to_add,
#                                  mctq_ad.data$sprep_w)
#  
#  mctq_ad.data$sprep_w <- hms::as_hms(mctq_ad.data$sprep_w)
#  
#  
#  # Adjusting `bt_w` based on the condition
#  
#  # Adjusting `sprep_w` based on the condition
#  mctq_ad.data$bt_w <- ifelse(mctq_ad.data$bt_w >= hms::parse_hm("08:00:00") & 
#                                 mctq_ad.data$bt_w < hms::parse_hm("12:00:00"),
#                               mctq_ad.data$bt_w + hours_to_add,
#                               mctq_ad.data$bt_w)
#  
#  # Adjusting `bt_w` based on the condition
#  mctq_ad.data$bt_w <- ifelse(mctq_ad.data$bt_w >= hms::parse_hm("12:00:00") & 
#                                 mctq_ad.data$bt_w < hms::parse_hm("18:00:00"),
#                               mctq_ad.data$bt_w - hours_to_add,
#                               mctq_ad.data$bt_w)
#  
#  mctq_ad.data$bt_w <- hms::as_hms(mctq_ad.data$bt_w)
 
 
 
 ####  exclude invalid data --> set certain vars to NA---------------------------------
 
 #many people misunderstood sprep "getting ready to fall asleep".
 # if getting ready to fall asleep < bedtime --> NA
 # if using an alarm on free days --> already no MSFsc computed
 
 # when to exclude light exposure time?
 # outlier plotting or smth like this?
 
 #work days
# 
#  # Applying the condition and setting the specified columns to NA
#  mctq_ped.data$sprep_w[!(mctq_ped.data$sprep_w > mctq_ped.data$bt_w |  
#                            mctq_ped.data$sprep_w == mctq_ped.data$bt_w |
#                            (mctq_ped.data$sprep_w - mctq_ped.data$bt_w)/3600 < -18)] <- NA
#  
#  # mctq_ped.data$bt_w[!(mctq_ped.data$sprep_w > mctq_ped.data$bt_w |  
#  #                        mctq_ped.data$sprep_w == mctq_ped.data$bt_w |
#  #                        (mctq_ped.data$sprep_w - mctq_ped.data$bt_w)/3600 < -18)] <- NA
#  
#  
#  #do the same for adult questionnaire
#  
#  # Applying the condition and setting the specified columns to NA
#  mctq_ad.data$sprep_w[!(mctq_ad.data$sprep_w > mctq_ad.data$bt_w |  
#                            mctq_ad.data$sprep_w == mctq_ad.data$bt_w |
#                            (mctq_ad.data$sprep_w - mctq_ad.data$bt_w)/3600 < -18)] <- NA
#  
#  # mctq_ad.data$bt_w[!(mctq_ad.data$sprep_w > mctq_ad.data$bt_w |  
#  #                        mctq_ad.data$sprep_w == mctq_ad.data$bt_w |
#  #                        (mctq_ad.data$sprep_w - mctq_ad.data$bt_w)/3600 < -18)] <- NA
#  # 
#  # 
#  
#  
#  #free days
#  # Applying the condition and setting the specified columns to NA
#  mctq_ped.data$sprep_f[!(mctq_ped.data$sprep_f > mctq_ped.data$bt_f |  
#                            mctq_ped.data$sprep_f == mctq_ped.data$bt_f |
#                            (mctq_ped.data$sprep_f - mctq_ped.data$bt_f)/3600 < -18)] <- NA
#  
#  # mctq_ped.data$bt_f[!(mctq_ped.data$sprep_f > mctq_ped.data$bt_f |  
#  #                        mctq_ped.data$sprep_f == mctq_ped.data$bt_f |
#  #                        (mctq_ped.data$sprep_f - mctq_ped.data$bt_f)/3600 < -18)] <- NA
#  
#  
#  #do the same for adult questionnaire
#  
#  # Applying the condition and setting the specified columns to NA
#  mctq_ad.data$sprep_f[!(mctq_ad.data$sprep_f > mctq_ad.data$bt_f |  
#                           mctq_ad.data$sprep_f == mctq_ad.data$bt_f |
#                           (mctq_ad.data$sprep_f - mctq_ad.data$bt_f)/3600 < -18)] <- NA
#  
#  # mctq_ad.data$bt_f[!(mctq_ad.data$sprep_f > mctq_ad.data$bt_f |  
#  #                       mctq_ad.data$sprep_f == mctq_ad.data$bt_f |
#  #                       (mctq_ad.data$sprep_f - mctq_ad.data$bt_f)/3600 < -20)] <- NA
 # 
 
#  
#  ##### Adult version computations -------------------------------------------------------------
#  # compute free days
#  mctq_ad.data <- mutate(mctq_ad.data, fd = fd(mctq_ad.data$wd))
#  # compute Sleep onset work days
#  mctq_ad.data <- mutate(mctq_ad.data, so_w = so(mctq_ad.data$sprep_w, mctq_ad.data$slat_w))
#  # compute Sleep onset free days
#  mctq_ad.data <- mutate(mctq_ad.data, so_f = so(mctq_ad.data$sprep_f, mctq_ad.data$slat_f))
#  # compute get up time on work days 
#  mctq_ad.data <- mutate(mctq_ad.data, gu_w = gu(mctq_ad.data$se_w, mctq_ad.data$si_w))
#  # compute get up time on free days 
#  mctq_ad.data <- mutate(mctq_ad.data, gu_f = gu(mctq_ad.data$se_f, mctq_ad.data$si_f))
#  # compute sleep duration on work days 
#  mctq_ad.data <- mutate(mctq_ad.data, sd_w = sdu(mctq_ad.data$so_w, mctq_ad.data$se_w))
#  # compute sleep duration on free days 
#  mctq_ad.data <- mutate(mctq_ad.data, sd_f = sdu(mctq_ad.data$so_f, mctq_ad.data$se_f))
#  # compute  total time in bed on work days 
#  mctq_ad.data <- mutate(mctq_ad.data, tbt_w = tbt(mctq_ad.data$bt_w, mctq_ad.data$gu_w))
#  # compute  total time in bed on work days 
#  mctq_ad.data <- mutate(mctq_ad.data, tbt_f = tbt(mctq_ad.data$bt_f, mctq_ad.data$gu_f))
#  # compute mid sleep time on work days 
#  mctq_ad.data <- mutate(mctq_ad.data, msw = msl(mctq_ad.data$so_w, mctq_ad.data$sd_w))
#  # compute mid sleep time on free days 
#  mctq_ad.data <- mutate(mctq_ad.data, msf = msl(mctq_ad.data$so_f, mctq_ad.data$sd_f))
#  # compute average weekly sleep duration
#  mctq_ad.data <- mutate(mctq_ad.data, sd_week = sd_week(mctq_ad.data$sd_w, mctq_ad.data$sd_f,
#                                                           mctq_ad.data$wd))
#  # compute average weekly light expposure
#  mctq_ad.data <- mutate(mctq_ad.data, le_week = le_week(mctq_ad.data$le_w, mctq_ad.data$le_f,
#                                                           mctq_ad.data$wd))
#  # compute chronotype MSFsc
#  mctq_ad.data <- mutate(mctq_ad.data, msf_sc = msf_sc(mctq_ad.data$msf, mctq_ad.data$sd_w, 
#                                                         mctq_ad.data$sd_f, mctq_ad.data$sd_week,
#                                                         mctq_ad.data$alarm_f))
#  
#  
# ##### Ped version computations -------------------------------------------------------------
#  # compute free days
#  mctq_ped.data <- mutate(mctq_ped.data, fd = fd(mctq_ped.data$wd))
#  # compute Sleep onset work days
#  mctq_ped.data <- mutate(mctq_ped.data, so_w = so(mctq_ped.data$sprep_w, mctq_ped.data$slat_w))
#  # compute Sleep onset free days
#  mctq_ped.data <- mutate(mctq_ped.data, so_f = so(mctq_ped.data$sprep_f, mctq_ped.data$slat_f))
#  # compute get up time on work days 
#  mctq_ped.data <- mutate(mctq_ped.data, gu_w = gu(mctq_ped.data$se_w, mctq_ped.data$si_w))
#  # compute get up time on free days 
#  mctq_ped.data <- mutate(mctq_ped.data, gu_f = gu(mctq_ped.data$se_f, mctq_ped.data$si_f))
#  # compute sleep duration on work days 
#  mctq_ped.data <- mutate(mctq_ped.data, sd_w = sdu(mctq_ped.data$so_w, mctq_ped.data$se_w))
#  # compute sleep duration on free days 
#  mctq_ped.data <- mutate(mctq_ped.data, sd_f = sdu(mctq_ped.data$so_f, mctq_ped.data$se_f))
#  # compute  total time in bed on work days 
#  mctq_ped.data <- mutate(mctq_ped.data, tbt_w = tbt(mctq_ped.data$bt_w, mctq_ped.data$gu_w))
#  # compute  total time in bed on work days 
#  mctq_ped.data <- mutate(mctq_ped.data, tbt_f = tbt(mctq_ped.data$bt_f, mctq_ped.data$gu_f))
#  # compute mid sleep time on work days 
#  mctq_ped.data <- mutate(mctq_ped.data, msw = msl(mctq_ped.data$so_w, mctq_ped.data$sd_w))
#  # compute mid sleep time on free days 
#  mctq_ped.data <- mutate(mctq_ped.data, msf = msl(mctq_ped.data$so_f, mctq_ped.data$sd_f))
#  # compute average weekly sleep duration
#  mctq_ped.data <- mutate(mctq_ped.data, sd_week = sd_week(mctq_ped.data$sd_w, mctq_ped.data$sd_f,
#                                                           mctq_ped.data$wd))
#  # compute average weekly light expposure
#  mctq_ped.data <- mutate(mctq_ped.data, le_week = le_week(mctq_ped.data$le_w, mctq_ped.data$le_f,
#                                                           mctq_ped.data$wd))
#  # compute chronotype MSFsc
#  mctq_ped.data <- mutate(mctq_ped.data, msf_sc = msf_sc(mctq_ped.data$msf, mctq_ped.data$sd_w, 
#                                                         mctq_ped.data$sd_f, mctq_ped.data$sd_week,
#                                                         mctq_ped.data$alarm_f))
#  
#  
# # select all needed mctq vars here: you can add others later
#  
# mctq_ad.score <- mctq_ad.data %>% select (c(record_id, work, wd, alarm_f, le_w:msf_sc))
# mctq_ped.score <- mctq_ped.data %>% select (c(record_id, work, wd, alarm_f, le_w:msf_sc))
#   
# # Combine dataframes and fill NAs from mctq_ped.score where mctq_ad.score has NAs
# mctq_all.score <- mctq_ad.score %>%
#   full_join(mctq_ped.score, by = "record_id", suffix = c(".mctq_ad.score", ".mctq_ped.score")) %>%
#   mutate(across(ends_with(".mctq_ad.score"), ~ coalesce(.x, get(sub(".mctq_ad.score", ".mctq_ped.score", cur_column())))))
# 
# # Remove the now redundant .mctq_ped.score columns
# mctq_all.score <- mctq_all.score %>%
#   select(-ends_with(".mctq_ped.score"))%>%
#   rename_with(~ sub("\\.mctq_ad\\.score$", "", .x), ends_with(".mctq_ad.score")) #return the original names

 # 
# # Items "regular work schedule" and  regular school schedule"  need to be 1 ("Yes")
# #for all further calculations
# data$slypos_mctq_01
# data$slypos_mctq_01_ped
 


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
  
  
  #save demographic variables
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
  
  #data <- merge(data, mctq_all.score, by="record_id")
  
  
  #mctqvars <- names(mctq_all.score)
  
  scorevars <- c("Photophila_score", "Photophobia_score", "ASE_score",
                 "ASE_levels", "Promis_sd_sum", "Promis_sri_sum","PDS_score_m",
                 #"Promis_sd_ped_sum", "Promis_sri_ped_sum","Promis_sd_ad_sum", "Promis_sri_ad_sum",
                 "PDS_score_f",  "msf", "msf_sc", "le_week", "F1_leba", "F2_leba", "F3_leba", "F4_leba", "F5_leba")
                
               
  ## create and save dataset for data analysis ----------------------------------
  
  # select only the data needed for analysis
  
  analysis.data <- data %>% dplyr::select(c(record_id, group, 
                                            all_of(demvars), 
                                            slypos_demographics_tz.factor, 
                                            fill_date, 
                                            all_of(scorevars)))
  
  # create numeric chronotype and weekly light exposure vars
  analysis.data <- analysis.data %>% 
    mutate(msf_sc_num = as.numeric(msf_sc)/3600,
           le_week_num = as.numeric(le_week)/3600) 
  
  
  save(analysis.data, file="./04_data_analysis/analysis.data.rda")
  