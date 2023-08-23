# Load and pre-process ICCT Data
# Mineral Demand Module
# PBH August 2023

####################
# LOAD DATA ---------
####################
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# ICCT on-road demand projections
icct <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")

# Battery for LDV by country
bat <- read.csv("Results/battery_size_country.csv")
# By region
bat_region <- read.csv("Results/battery_size_region.csv")
bat$Vehicle <- "Car";bat_region$Vehicle <- "Car";

# Battery for other applications
bat_others <- read_excel("Data/Battery_Size.xlsx",sheet = "Battery_Size")

# Mineral intensity
mineral <- read_excel("Data/Mineral_Intensity.xlsx",sheet = "Dunn2021")
mineral <- mineral %>% mutate(chemistry=if_else(chemistry=="NMC 111","NMC",chemistry))


####################
# JOIN DATA  ---------
####################

df <- icct %>% 
  filter(Powertrain!="ICE")
  # filter(Scenario=="Baseline")


## Add Battery size and chemistry ----------

df_ldv <- df %>% filter(Vehicle=="Car")

# For LDV
# Join by country name
bat$Year <- NULL # for all years use the same battery
df_ldv <- df_ldv %>% left_join(bat)

# get NA terms to join them by region
df_ldv_na <- df_ldv %>% filter(is.na(kwh_veh)) %>% dplyr::select(-chemistry,-MWh,-unit,-share_units,-kwh_veh,-kwh_veh_total)
df_ldv <- df_ldv %>% filter(!is.na(kwh_veh)) # to join later
# add the regional estimates
bat_region$Year <- NULL # for all years use the same battery
df_ldv_na <- df_ldv_na %>% left_join(bat_region)
# NO MISSING no
df_ldv <- rbind(df_ldv,df_ldv_na); rm(df_ldv_na);
# Check
length(unique(df_ldv$Country))*length(unique(df_ldv$Powertrain)) # 187 * 2 = 374
df_ldv$MWh <- df_ldv$unit <- df_ldv$share_units <- df_ldv$kwh_veh_total <- NULL  

# Add battery for others
bat_others$chemistry <- "LFP"
bat_others$Year <- NULL # for all years use the same battery
df_rest <- df %>% filter(Vehicle!="Car")
df_rest <- df_rest %>% left_join(bat_others)

# Join them
df <- rbind(df_ldv,df_rest); rm(df_ldv,df_rest);


## Add Mineral Intensity --------


# use same chemistries for now
df$chemistry %>% unique()
mineral$chemistry %>% unique()
df <- df %>% mutate(chemistry=case_when(
  chemistry %in% c("NMC 89:4:4:3","NMC 721") ~ "NMC 811",
  chemistry=="Other" ~ "LFP",
  T ~ chemistry))

df <- df %>% left_join(mineral)

####################
# CALCULATIONS ---------
####################

## kWH ----
df <- df %>% 
  mutate(kwh_required=Sales*kwh_veh)

# total GWh
sum(df$kwh_required)/1e6

## Minerals ----

df <- df %>% mutate(tons_mineral=kwh_required*kg_per_kwh/1e3)


## Save results -----

write.csv(df,"Results/MineralDemand.csv",row.names = F)


# EoF