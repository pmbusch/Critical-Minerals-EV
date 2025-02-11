# Set key parameters for Demand Module Runs
# Mineral Demand Module
# PBH Dec 2023

library(tidyverse)

# LDV Survival Parameters --------------

recycling_scenarios <- 
  tibble(recycling_scenario = c("Baseline","Enhanced recycling","Enhanced SSPS",
                                "USA Recycling","Recycling Medium"),
         ssps_perc = c(0.5,0.3,0.7,0.5,0.3)) %>% 
  mutate(recycling_perc = 1-ssps_perc)


# Battery Survival Parameters ------
delay_recycling_year <- 1 # 1 year to recycle to get new demand
statHealth_deg <- 0.02 # state of health of batteries degradation per year, for SSPS purposes
cathode_scrap <- 0.04 # 


# % of material recovery in recycling
# 90% for cobalt, nickel, copper and lead by the end of 2027, rising to 95% in 2031; 
# and 50% for lithium by 2027, rising to 80% in 2031.
EU_targets <- tibble(
  Mineral=c("Lithium","Nickel","Cobalt","Manganese","Phosphorus","Copper","Graphite"),
  mat_recov_recyc1=c(0.5,0.9,0.9,0,0,0.05,0),
  mat_recov_recyc2=c(0.8,0.95,0.95,0,0,0.05,0),
  dummy=1) %>% 
  left_join(tibble(Year=2022:2070,dummy=1),
            relationship = "many-to-many") %>% 
  # linear adoption of targets
  mutate(EU_mat_recov_recyc=case_when(
    Year<2029 ~ mat_recov_recyc1/7*(Year-2021),
    Year<2033 ~mat_recov_recyc1+(mat_recov_recyc2-mat_recov_recyc1)/5*(Year-2027),
    T ~ mat_recov_recyc2)) %>% 
  dplyr::select(-mat_recov_recyc1,-mat_recov_recyc2,-dummy)

# Levels right now
China_targets <- tibble(
  Mineral=c("Lithium","Nickel","Cobalt","Manganese","Phosphorus","Copper","Graphite"),
  China_mat_recove=c(0.85,0.98,0.98,0.98,0,0.05,0))


mat_recovery_recycling <- tibble(recycling_scenarios=recycling_scenarios$recycling_scenario) %>% 
  mutate(dummy=1) %>% left_join(mutate(EU_targets,dummy=1), relationship = "many-to-many") %>% 
  mutate(dummy=1) %>% left_join(mutate(China_targets,dummy=1), relationship = "many-to-many") %>% 
  mutate(mat_recov_recyc=case_when(
    recycling_scenarios=="Baseline" ~ 0.05,
    recycling_scenarios=="Enhanced recycling" ~ EU_mat_recov_recyc,
    recycling_scenarios=="Enhanced SSPS" ~ 0.05)) %>% 
  # EU and China Baseline with regulations
  left_join(tibble(Region=region_level,dummy=1), relationship = "many-to-many") %>% dplyr::select(-dummy) %>% 
  mutate(mat_recov_recyc=case_when(
    Region %in% c("European Union") ~ EU_mat_recov_recyc,
    Region %in% c("China") ~ China_mat_recove,
    T ~mat_recov_recyc),
    EU_mat_recov_recyc=NULL,China_mat_recove=NULL)

# add Copper enhanced recycling 64% today to 95% 2031
mat_recovery_recycling <- mat_recovery_recycling %>% 
  mutate(mat_recov_recyc=case_when(
    Mineral!="Copper"~mat_recov_recyc,
    recycling_scenarios!="Enhanced recycling"~mat_recov_recyc,
    Year>2031 ~ 0.95,
    T ~0.64+(0.95-0.64)/9*(Year-2022))) # linear increase


# Scenario for USA adopting EU standards
# copy/paste above code and change USA
mat_recovery_recycling_USA <- tibble(recycling_scenarios=recycling_scenarios$recycling_scenario) %>% 
  mutate(dummy=1) %>% left_join(mutate(EU_targets,dummy=1), relationship = "many-to-many") %>% 
  mutate(dummy=1) %>% left_join(mutate(China_targets,dummy=1), relationship = "many-to-many") %>% 
  mutate(mat_recov_recyc=case_when(
    recycling_scenarios=="Baseline" ~ 0.05,
    recycling_scenarios=="Enhanced recycling" ~ EU_mat_recov_recyc,
    recycling_scenarios=="Enhanced SSPS" ~ 0.05)) %>% 
  # EU and China Baseline with regulations
  left_join(tibble(Region=region_level,dummy=1), relationship = "many-to-many") %>% dplyr::select(-dummy) %>% 
  mutate(mat_recov_recyc=case_when(
    Region %in% c("European Union","United States") ~ EU_mat_recov_recyc,
    Region %in% c("China") ~ China_mat_recove,
    T ~mat_recov_recyc),
    EU_mat_recov_recyc=NULL,China_mat_recove=NULL) %>% 
  filter(recycling_scenarios=="Baseline")


# Global recovery scenarios -----
# not included for EU and China specific targets, same level for all minerals
# Model all regions like Europe

# constraint of max recycling for each region
rec_levels <- seq(0.05,0.95,0.1)


global_rec_scenarios <- tibble(mat_recov_recyc1=rec_levels,
                               recycling_scenarios=paste0("Recycling percentage ",round(rec_levels*100,0)),
                               dummy=1) %>%
  left_join(mutate(EU_targets,dummy=1), relationship="many-to-many") %>% 
  mutate(dummy=1) %>% left_join(mutate(China_targets,dummy=1), relationship = "many-to-many") %>% 
  # EU and China Baseline with regulations
  left_join(tibble(Region=region_level,dummy=1), relationship = "many-to-many") %>% dplyr::select(-dummy) %>% 
  mutate(mat_recov_recyc=case_when(
    Region %in% c("European Union") ~ EU_mat_recov_recyc,
    Region %in% c("China") ~ China_mat_recove,
    T ~pmin(EU_mat_recov_recyc,mat_recov_recyc1)),
    EU_mat_recov_recyc=NULL,China_mat_recove=NULL,mat_recov_recyc1=NULL)


rm(EU_targets,China_targets)



# EoF