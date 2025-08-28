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

set.seed(123)

# load demographic data --------------------------------------------------------

load(file="./03_demographics/demvars.data.rda")

load(file="./03_demographics/country_tz.data.rda")



#Dem Table 1--------------------------------------------------------------------


# create descriptive table for the demographic vars 
#(excluding tz & Country) with gtsummary

demvars.data %>%
  tbl_summary(
    statistic=list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
    digits=all_continuous() ~ 2,
    label=list(slypos_demographics_age ~ "Age",
               slypos_demographics_sex.factor ~ "Sex",
               slypos_demographics_gender.factor ~ "Gender-Variant Identity",
               slypos_demographics_language.factor ~ "Native English Speaker",
               slypos_demographics_work_or_school.factor ~ "Occupational Status",
               slypos_demographics_school.factor ~ "Occupational setting"
    ),
    
    missing="no"
  ) %>% bold_labels() %>% 
  # add_p() %>% add_q() %>%
  modify_header(label ~ "**Variable**")  -> dem_table 

#  %>% separate_p_footnotes()
# %>%   #modify_caption("Demographic Characteristics of Participants") 



dem_table

min(demvars.data$slypos_demographics_age)
max(demvars.data$slypos_demographics_age)

# Country/time zone data----------------------------------------------

## summarise country/time zone data----------------------------------------------

#Problem: This contains all country/timezones with 0s

country_tz.data%>%
  tbl_summary(
    label=slypos_demographics_tz.factor ~ "Time zone - Country",
    statistic=list(all_categorical() ~ "{n} ({p}%)"),
    missing="no",
    sort=all_categorical() ~ "frequency",
    include="slypos_demographics_tz.factor"
  )  %>% bold_labels() %>%
  modify_header(label ~ "") -> tz_table
  #as_tibble(format='pipe') 
tz_table

# Demographics of timezones and countries
library(data.table)
tz <- setDT(country_tz.data)
tz_n <- tz[ , uniqueN(slypos_demographics_tz)] # n of timezones
tz_count <- tz[ , .N, by = slypos_demographics_tz][order(-N)] 
cty_n <- tz[ , uniqueN(slypos_demographics_tz.factor)] # n of countries
cty_count <- tz[ , .N, by = slypos_demographics_tz.factor][order(-N)] 

# for percentages
tz_count[ , pct := N/sum(N)]
cty_count[ , pct := N/sum(N)]






