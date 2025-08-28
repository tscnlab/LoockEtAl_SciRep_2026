#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Ann-Sophie Loock  & Rafael Lazar                                                              


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(stringr, reshape2, Hmisc, tidyverse, lubridate, hms, doBy, DescTools, 
               BayesFactor, effectsize, gtsummary, mctq)


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
mctq_cleaned <- data %>%
  # 1) Keep only MCTQ + record_id --------------------------------
  select(
    record_id,
    slypos_mctq_01.factor,
    slypos_mctq_01_ped.factor,
    contains("mctq"),
    -contains("attentioncheck")
  ) %>%
  
  # 2) Blanks / labelled missings → NA ---------------------------
  mutate(across(
    where(~ is.character(.x) || inherits(.x,"labelled")),
    ~ na_if(as.character(.x), "")
  )) %>%
  
  # 3) Flag “regular” & Adult/Child form -------------------------
  mutate(
    regular = slypos_mctq_01.factor == "Yes"
    | slypos_mctq_01_ped.factor == "Yes",
    group   = case_when(
      !is.na(slypos_mctq_03)     | !is.na(slypos_mctq_04)     ~ "Adult",
      !is.na(slypos_mctq_03_ped) | !is.na(slypos_mctq_04_ped) ~ "Child",
      TRUE                                                   ~ NA_character_
    )
  ) %>%
  
  # 4) Parse all HH:MM → seconds (_sec) --------------------------
  mutate(across(
    c(slypos_mctq_03, slypos_mctq_04,
      slypos_mctq_03_ped, slypos_mctq_04_ped,
      slypos_mctq_10, slypos_mctq_11,
      slypos_mctq_10_ped, slypos_mctq_11_ped),
    ~ as.numeric(parse_hm(.x)),
    .names = "{.col}_sec"
  )) %>%
  
  # 5) Fix 12h typos for 06:00–12:59 entries --------------------
  mutate(across(
    ends_with("_sec"),
    ~ if_else(. >  6*3600 & . < 13*3600, . + 12*3600, .)
  )) %>%
  
  # 6) Coalesce Adult vs Child into bed_sec / sleep_sec ----------
  mutate(
    bed_sec        = coalesce(
      if_else(group=="Adult",   slypos_mctq_03_sec,     slypos_mctq_03_ped_sec)
    ),
    sleep_sec      = coalesce(
      if_else(group=="Adult",   slypos_mctq_04_sec,     slypos_mctq_04_ped_sec)
    ),
    bed_free_sec   = coalesce(
      if_else(group=="Adult",   slypos_mctq_10_sec,     slypos_mctq_10_ped_sec)
    ),
    sleep_free_sec = coalesce(
      if_else(group=="Adult",   slypos_mctq_11_sec,     slypos_mctq_11_ped_sec)
    )
  ) %>%
    
  # 7) **Bump** any bed ≤06:00 → +86400 s (so “1AM” is treated as next‐day)  
  mutate(
    bed2_sec       = if_else(bed_sec       <= 6*3600, bed_sec       + 86400, bed_sec),
    bed2_free_sec  = if_else(bed_free_sec  <= 6*3600, bed_free_sec  + 86400, bed_free_sec)
  ) %>%
  
  # 8) Tag tiny inversions ≤2 h (BEFORE bumping sleep) ------------
  mutate(
    small_inv       = sleep_sec      <  bed2_sec       & (bed2_sec       - sleep_sec     ) <= 2*3600,
    small_inv_free  = sleep_free_sec <  bed2_free_sec  & (bed2_free_sec  - sleep_free_sec) <= 2*3600
  ) %>%
    
  # 9) Swap those tiny inversions immediately --------------------
  mutate(
    bed_s       = if_else(small_inv,      sleep_sec,       bed2_sec),
    sleep_s     = if_else(small_inv,      bed2_sec,        sleep_sec),
    bed_s_free  = if_else(small_inv_free, sleep_free_sec,  bed2_free_sec),
    sleep_s_free= if_else(small_inv_free, bed2_free_sec,   sleep_free_sec)
  ) %>%
  
  # 10) Bump true next‐day sleeps by +86400 s --------------------
  mutate(
    sleep2      = if_else(sleep_s      < bed_s      & sleep_s      <= 6*3600,
                          sleep_s + 86400, sleep_s),
    sleep2_free = if_else(sleep_s_free < bed_s_free & sleep_s_free <= 6*3600,
                          sleep_s_free + 86400, sleep_s_free)
  ) %>%
  
  # 11) Tag / swap any remaining inversion >2 h -------------------
  mutate(
    inverted       = bed_s       > sleep2,
    diff_sec       = abs(sleep2      - bed_s),
    status         = case_when(
      !inverted                   ~ "ok",
      inverted & diff_sec <= 2*3600  ~ "corrected",
      TRUE                           ~ "flag"
    ),
    
    inverted_free  = bed_s_free  > sleep2_free,
    diff_free_sec  = abs(sleep2_free - bed_s_free),
    status_free    = case_when(
      !inverted_free                ~ "ok",
      inverted_free & diff_free_sec <= 2*3600 ~ "corrected",
      TRUE                                   ~ "flag"
    )
  ) %>%
  
  mutate(
    final_bed_sec       = if_else(status      == "corrected", sleep2,      bed_s),
    final_sleep_sec     = if_else(status      == "corrected", bed_s,        sleep2),
    final_bed_free_sec  = if_else(status_free == "corrected", sleep2_free,  bed_s_free),
    final_sleep_free_sec= if_else(status_free == "corrected", bed_s_free,    sleep2_free)
  ) %>%
  
  # 12) Back to 0–86399s, then to hms() for your *_new columns ----
  mutate(
    bed_new        = as_hms(final_bed_sec       %% 86400),
    sleep_new      = as_hms(final_sleep_sec     %% 86400),
    bed_free_new   = as_hms(final_bed_free_sec  %% 86400),
    sleep_free_new = as_hms(final_sleep_free_sec%% 86400)
  ) %>%
  select(
    record_id, group, regular,
    status, status_free,
    bed_new, sleep_new,
    bed_free_new, sleep_free_new
  )

# quick sanity checks to ensure everything worked
nrow(mctq_cleaned); nrow(data) #sample size
mctq_cleaned %>% summarise(
  flagged_work = sum(status == "flag", na.rm = TRUE), #dropped in work day times
  flagged_free = sum(status_free == "flag", na.rm = TRUE) #dropped in free day times
)

weird_midday <- mctq_cleaned %>%
  filter(
    # work-day bed or sleep in [13:00,16:00)
    (bed_new       >= hms(hours = 13) & bed_new       < hms(hours = 16))
    | (sleep_new     >= hms(hours = 13) & sleep_new     < hms(hours = 16))
    # free-day bed or sleep in [13:00,16:00)
    | (bed_free_new  >= hms(hours = 13) & bed_free_new  < hms(hours = 16))
    | (sleep_free_new>= hms(hours = 13) & sleep_free_new< hms(hours = 16))
  ) %>%
  select(
    record_id, group, regular, 
    status, status_free, 
    bed_new, sleep_new, bed_free_new, sleep_free_new
  )
weird_midday

# merge back onto `data` by record_id:
data <- data %>%
  left_join(
    mctq_cleaned,
    by = "record_id"
  )


# Compute MSFsc for analysis ------------------------------------
msf <- data %>%
  # Select columns needed for MSFsc (keeping record_id, group, status)
  select(
    record_id, status, status_free, group, regular,
    bt_w           = bed_new,     # bedtime on work days
    sprep_w        = sleep_new,     # sleep time on work days
    wd             = slypos_mctq_02,         # work days
    slat_w         = slypos_mctq_05,         # sleep onset latency on work days
    se_w           = slypos_mctq_06,         # sleep end on work days
    si_w           = slypos_mctq_07,         # sleep inertia on work days
    # free-day equivalents:
    bt_f           = bed_free_new,
    sprep_f        = sleep_free_new,     # sleep time  
    slat_f         = slypos_mctq_12,         # sleep onset latency
    se_f           = slypos_mctq_13,         # sleep end
    si_f           = slypos_mctq_14,         # sleep inertia
    alarm_f        = slypos_mctq_15,         # alarm 
    
    # and for pediatric sample: 
    wd_ped             = slypos_mctq_02_ped,         
    slat_w_ped         = slypos_mctq_05_ped,         
    se_w_ped           = slypos_mctq_06_ped,         
    si_w_ped           = slypos_mctq_07_ped,         
    # free-day equivalents:
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
  # filter(status != "flag", status_free != "flag") %>%
  
  # Parse to hms / durations
  mutate(
    # pick adult vs child answers
    wd      = if_else(group=="Adult", as.integer(wd), as.integer(wd_ped)), 
    #bt_w    = hms :: parse_hm(as.character(bt_w)),
    #sprep_w = hms::parse_hm(as.character(sprep_w)),
    #bt_f    = hms :: parse_hm(as.character(bt_f)),
    #sprep_f = hms::parse_hm(as.character(sprep_f)),
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
    si_f   = dminutes(as.numeric(si_f))) %>%
  # When status is FLAG -> set to NA (work days and free days)
  mutate(
    across(c(bt_w, sprep_w), ~ if_else(status == "flag", as_hms(NA_real_), .)),
    across(c(bt_f, sprep_f), ~ if_else(status_free == "flag", as_hms(NA_real_), .))) %>%
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
    msf %>% select(record_id, msf, msf_sc), 
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




# TROUBLESHOOTING (complete - Aug 06, 25)
# Why are we losing so many cases in calculating MSF and MSF sc?
## --> We filter for status and select only ok and corrected, NA rows are dropped. 
## From the entire survey dataset of N = 774, 
## we only obtained valid MCTQ data from N = 551 participants. 

# irregulars <- data %>%  filter(
#     (is.na(slypos_mctq_01)     | slypos_mctq_01     == 2),
#     (is.na(slypos_mctq_01_ped) | slypos_mctq_01_ped == 2)) 
# # we have 203 cases, in which participants do NOT have a regular work/school schedule
# 
# # There is still 20 cases unaccounted for in the difference between data's length and msf's length
# msf_ids <- msf %>% pull(record_id)
# irregular_ids <- irregulars %>% pull(record_id)
# missing_ids <- data %>% filter(
#     !record_id %in% msf_ids,
#     !record_id %in% irregular_ids) %>% pull(record_id)
# 
# # Look at the remaining 20 cases
# View(data %>% filter(record_id %in% missing_ids))

# Take out "alarm_f" bc/ we are loosing too much data otherwise. 
# So, instead of using the implemented function to calculate MSFsc, 
# I do the calculation manually with the formula provided in the publication:
# https://www.researchgate.net/publication/340541904_Chronotype_and_Social_Jetlag_-_a_self-critical_review






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
  
  analysis.data <- data %>% dplyr::select(c(record_id, group, regular, status, status_free,
                                            all_of(demvars), 
                                            slypos_demographics_tz.factor, 
                                            fill_date, 
                                            all_of(scorevars)))
  
  # create numeric chronotype and weekly light exposure vars
  analysis.data <- analysis.data %>% 
    mutate(msf_sc_num = as.numeric(msf_sc)/3600,
           le_week_num = as.numeric(le_week)/3600) 
  
  
  save(analysis.data, file="./04_data_analysis/analysis.data.rda")
  