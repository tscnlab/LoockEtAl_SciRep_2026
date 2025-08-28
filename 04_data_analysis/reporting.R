#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Ann-Sophie Loock                                                                


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(lme4, stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq, psych, ggcorrplot, gt)


# load data ----------------------------------------------------------------

load(file="./04_data_analysis/analysis.data.rda")
colnames(analysis.data) # Get all column names


# Calculate descriptive stats ---------------------------------------------
dt <- as.data.table(analysis.data) # convert to data table format

# Filter out numeric variables
num_cols <- names(dt)[vapply(dt, is.numeric, FUN.VALUE = logical(1))]
num_cols <- setdiff(num_cols, c("record_id")) # exclude record id column

# obtain data table with only numeric variables
dt_num <- dt[ , ..num_cols] 

# calculate descriptive stats
desc <- as.data.table(psych :: describe(dt_num), keep.rownames = "variable") 

# keep selected descriptive stats columns
keep <- c("variable", "n", "mean","sd","median", "min","max","range")  
desc <- desc[ , ..keep]

# round numeric values
col_to_round <- setdiff(names(desc), c("variable", "n"))
desc[ , (col_to_round) := lapply(.SD, round, 1), .SDcols = col_to_round]

# make a pretty table
desc %>%
  gt() %>%
  fmt_number(columns = -c(variable, n), decimals = 1) %>% 
  tab_header(title = "Descriptive Stats...", subtitle = paste0("N = ", nrow(dt_num), " rows; ", length(num_cols), " numeric variables")) %>%
  cols_label(n = "N", sd = "SD") %>%
  tab_options(table.font.size = px(12))



# Average Mid-sleep Time --------------------------------------------------

# Deal with MSF and MSFsc hms objects
dt[ , msf_num := as.numeric(msf)/3600] # convert MSF to numeric (already done for MSF sc)

# Calculate mean and sd on numeric level
# for MSF
mean_msf <- dt[ , mean(msf_num, na.rm = TRUE)]
sd_msf <- dt[ , sd(msf_num, na.rm = TRUE)]
hms :: hms(seconds = mean_msf * 3600)
hms :: hms(seconds = sd_msf * 3600)

# for MSFsc
mean_msfsc <- dt[ , mean(msf_sc_num, na.rm = TRUE)]
sd_msfsc <- dt[ , sd(msf_sc_num, na.rm = TRUE)]
hms :: hms(seconds = mean_msfsc * 3600)
hms :: hms(seconds = sd_msfsc * 3600)
