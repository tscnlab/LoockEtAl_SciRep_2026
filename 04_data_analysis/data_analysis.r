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


