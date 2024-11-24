#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar                                                                 

# clean workspace and unload all packages--------------------------------------
rm(list=ls())
graphics.off()


#unload packages that were loaded before (run function twice to "catch" all pkgs)
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))





#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq)


# Read-in data ----------------------------------------------------------------

#Read the whole dataset
rawdata=read.csv('01_dataimport/raw_data/SpitschanSleepSurvey_DATA_2024-11-24_2125.csv')

# filter out incomplete data
complete_df <- rawdata %>% filter(!is.na(slypos_leba_50))


# reduce dataset 

# take sample of the full dataset
#uncomment this later
set.seed(123)
complete_df <- complete_df %>% sample_n(50)



# set data for analysis to sample data (30 obs.)
data <- complete_df

rm(complete_df, rawdata)



save(data, file="01_dataimport/raw_data/data.rda")

