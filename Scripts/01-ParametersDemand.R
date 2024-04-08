# Set key parameters for Demand Module Runs
# Mineral Demand Module
# PBH Dec 2023

library(tidyverse)



# LDV Survival Parameters --------------



# Battery Survival Parameters ------
mat_recovery_recycling <- 0.9 # % of material recovery in recycling
# change to:
# 90% for cobalt, nickel, copper and lead by the end of 2027, rising to 95% in 2031; 
# and 50% for lithium by 2027, rising to 80% in 2031.
# mat_recovery_recycling <- tibble(
#   Mineral=c("Lithium","Nickel","Cobalt","Manganese","Phosphorus"),
#   mat_recov_recyc=c(0.8,0.95,0.95,??,??))


delay_recycling_year <- 1 # 1 year to recycle to get new demand
statHealth_SPSS <- 0.7 # state of health of batteries going to SSPS
cathode_scrap <- 0.04



