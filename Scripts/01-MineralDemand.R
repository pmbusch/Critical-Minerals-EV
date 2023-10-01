# Load and pre-process ICCT Data
# Mineral Demand Module
# PBH August 2023

####################
# LOAD DATA ---------
####################
source("Scripts/00-Libraries.R", encoding = "UTF-8")

## ICCT on-road demand projections -----
icct <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")

eq_country_region <- icct %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)


## Battery size -----
# for LDV by country 
bat <- read.csv("Results/Battery/battery_size_country.csv")
# By region
bat_region <- read.csv("Results/Battery/battery_size_region.csv")
bat_region <- bat_region %>% filter(Year==2022)
bat$Vehicle <- "Car";bat_region$Vehicle <- "Car";


# Battery for other on-road transport 
bat_others <- read_excel("Data/Battery_Size.xlsx",sheet = "Battery_Size")

## Transport chemistry share ------------

# Use regional chemistry share forecasts, BY 3 scenarios
chem <- read.csv("Results/Battery/Chemistry_Scenarios/bat_share_2050_region.csv")
chem$chem_scenario <- "Benchmark"
chem_LFP <- read.csv("Results/Battery/Chemistry_Scenarios/LFP_scen_region.csv")
chem_LFP$chem_scenario <- "Double LFP"
chem_NMC <- read.csv("Results/Battery/Chemistry_Scenarios/NMC811_scen_region.csv")
chem_NMC$chem_scenario <- "Double NMC 811"
chem <- rbind(chem,chem_LFP,chem_NMC);rm(chem_LFP,chem_NMC)
chem <- chem %>% rename(Year=year)

# Battery for stationary power storage -----
stationary <- read.csv("Results/stationaryPower.csv")
stationary <- stationary %>% rename(Country=ICCT_Country,Region=ICCT_Region)
stationary$Vehicle <- "Stationary Power Storage"

# Mineral intensity
mineral <- read_excel("Data/Mineral_Intensity.xlsx",sheet = "BatPac")



####################
# JOIN DATA  ---------
####################

df <- icct %>% 
  filter(Powertrain!="ICE")
  # filter(Scenario=="Baseline")


## Add Battery size and chemistry ----------

df_ldv <- df %>% filter(Vehicle=="Car")

# For LDV
# merge size and chem
bat <- bat %>% 
  mutate(Year=NULL) %>% # for all years use the same battery size (FOR NOW)
  left_join(eq_country_region) %>% # add region
  left_join(chem) %>% mutate(kwh_veh=kwh_veh_total*share_units)
# Join by country name
df_ldv <- df_ldv %>% left_join(bat)

# get NA terms to join them by region
df_ldv_na <- df_ldv %>% filter(is.na(kwh_veh)) %>% dplyr::select(-chemistry,-MWh,-unit,-share_units,-kwh_veh,-kwh_veh_total,-chem_scenario)
bat_region <- bat_region %>% mutate(Year=NULL) %>% left_join(chem) %>% 
  mutate(kwh_veh=kwh_veh_total*share_units)
df_ldv <- df_ldv %>% filter(!is.na(kwh_veh)) # to join later
# add the regional estimates
df_ldv_na <- df_ldv_na %>% left_join(bat_region)
# NO MISSING no
df_ldv <- rbind(df_ldv,df_ldv_na); rm(df_ldv_na);
# Check
length(unique(df_ldv$Country))*length(unique(df_ldv$Powertrain)) # 187 * 2 = 374
df_ldv$MWh <- df_ldv$unit <- df_ldv$share_units <- df_ldv$kwh_veh_total <- NULL  


bat_others$Year <- NULL # for all years use the same battery
df_rest <- df %>% filter(Vehicle!="Car")
df_rest <- df_rest %>% left_join(bat_others)

# no chem scenario for rest - FOR NOW
s1 <- s2 <- s3 <- df_rest;
s1$chem_scenario="Benchmark";s2$chem_scenario="Double LFP";s3$chem_scenario="Double NMC 811";
df_rest <- rbind(s1,s2,s3);rm(s1,s2,s3)

# Join them
df <- rbind(df_ldv,df_rest); rm(df_ldv,df_rest);

## kWH ----
df <- df %>% 
  mutate(kwh_required=Sales*kwh_veh)

# Add stationary power storage
stationary <- stationary %>% 
  rename(kwh_required=stationaryPower) %>% # in Mwh 
  mutate(kwh_required=kwh_required*1e3)

# add dummies to join
stationary <- stationary %>% 
  mutate(Powertrain="SPS",
         Sales=0,kwh_veh=0) 
df$Scenario %>% unique()
s1 <- s2 <- s3 <- stationary;
s1$Scenario="Baseline";s2$Scenario="Momentum";s3$Scenario="Ambitious";
stationary <- rbind(s1,s2,s3);rm(s1,s2,s3)
# No chem scenario for SPS
s1 <- s2 <- s3 <- stationary;
s1$chem_scenario="Benchmark";s2$chem_scenario="Double LFP";s3$chem_scenario="Double NMC 811";
stationary <- rbind(s1,s2,s3);rm(s1,s2,s3)

df <- rbind(df,stationary) # 1 M rows for now


## Add Mineral Intensity --------

# use same chemistries for now
df$chemistry %>% unique()
mineral$chemistry %>% unique()
df <- df %>% mutate(chemistry=case_when(
  chemistry %in% c("NMC 89:4:4:3","NMC 721") ~ "NMC 811",
  chemistry %in% c("LCO","LMNO") ~ "LMO",
  chemistry=="Other" ~ "LFP",
  T ~ chemistry))

mineral <- mineral %>% filter(Mineral %in% min_interest)

df <- df %>% left_join(mineral)
nrow(df)/1e6 # 4.9 MILLION

####################
# CALCULATIONS ---------
####################

## Minerals ----
df <- df %>% mutate(tons_mineral=kwh_required*kg_per_kwh/1e3)


## Save results -----

write.csv(df,"Results/MineralDemand.csv",row.names = F)

# Save results at Region level
names(df)
df_region <- df %>% 
  group_by(Scenario,chem_scenario,Year,Region,Powertrain,Vehicle,chemistry,Mineral) %>% 
  reframe(tons_mineral=sum(tons_mineral)) %>% ungroup()
nrow(df_region) # 516K records

write.csv(df_region,"Results/MineralDemandRegion.csv",row.names = F)

# EoF