# Load and pre-process ICCT Data
# Mineral Demand Module
# PBH August 2023

source("Scripts/01-ParametersDemand.R", encoding = "UTF-8")


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


# share of 2-3 wheelers
share_2_3_wheelers <- read_excel("Data/Disaggregated 2-3 wheeler sales.xlsx",
                                 sheet="2_3_WheelerShare",range = "A27:D31")

bat_wheelers <- bat_others %>% filter(str_detect(Vehicle,"wheeler"))
bat_others <- bat_others %>% filter(!str_detect(Vehicle,"wheeler"))

bat_wheelers <- share_2_3_wheelers %>% 
  dplyr::select(Country,Share_2Wheeler) %>% mutate(Year=2022) %>% 
  left_join(bat_wheelers)
bat_wheelers <- bat_wheelers %>% pivot_wider(names_from = Vehicle, values_from = kwh_veh) %>% 
  mutate(kwh_veh=`2 wheeler`*Share_2Wheeler+`3 wheeler`*(1-Share_2Wheeler))
bat_wheelers$Share_2Wheeler <- bat_wheelers$`2 wheeler` <- bat_wheelers$`3 wheeler` <- NULL

countries_wheelers <- bat_wheelers %>% filter(Country!="World") %>% pull(Country) %>% unique()

bat_wheelers$Vehicle <- "Two/Three Wheelers"
bat_others$Country <- "World"

bat_others <- rbind(bat_others,bat_wheelers)
rm(share_2_3_wheelers,bat_wheelers)

## Transport chemistry share ------------

# Use regional chemistry share forecasts, BY 3 scenarios
chem <- read.csv("Results/Battery/Chemistry_Scenarios/bat_share_2050_region.csv")
chem$chem_scenario <- "Baseline"
chem_LFP <- read.csv("Results/Battery/Chemistry_Scenarios/LFP_scen_region.csv")
chem_LFP$chem_scenario <- "Double LFP"
chem_NMC <- read.csv("Results/Battery/Chemistry_Scenarios/NMC811_scen_region.csv")
chem_NMC$chem_scenario <- "Double NMC 811"
chem <- rbind(chem,chem_LFP,chem_NMC);rm(chem_LFP,chem_NMC)
chem <- chem %>% rename(Year=year)

## Battery for stationary power storage -----
stationary <- read.csv("Results/stationaryPower.csv")
stationary <- stationary %>% rename(Country=ICCT_Country,Region=ICCT_Region)
stationary$Vehicle <- "Stationary Power Storage"

## Mineral intensity -----
mineral <- read_excel("Data/Mineral_Intensity.xlsx",sheet = "BatPac")

## Reuse statistics from Survival Model -----
reuse <- read.csv("Data/Survival Curves/world_outflows_LIB.csv")

## Other sector demand -----
otherSectors <- read.csv("Results/Intermediate Results/otherSector_demand.csv")


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


# other vehicles
bat_others$Year <- NULL # for all years use the same battery
df_rest <- df %>% filter(Vehicle!="Car")
df_rest <- df_rest %>% 
  mutate(country_bat=if_else(Vehicle=="Two/Three Wheelers" & # add country to join to world or others
                             Country %in% countries_wheelers,Country,"World"))
bat_others <- bat_others %>% rename(country_bat=Country)
df_rest <- df_rest %>% left_join(bat_others)
df_rest$country_bat <- NULL
rm(countries_wheelers)

# no chem scenario for rest - FOR NOW
# DELETE 
# s1 <- s2 <- s3 <- df_rest;
# s1$chem_scenario="Baseline";s2$chem_scenario="Double LFP";s3$chem_scenario="Double NMC 811";
# df_rest <- rbind(s1,s2,s3);rm(s1,s2,s3)
##
df_rest$chem_scenario <- "No Scenario"

# Join them
df <- rbind(df_ldv,df_rest); rm(df_ldv,df_rest);

## add need for additional batteries due to failures-----

# get battery flow for SSPS and recycling
lib_outflow <- df %>% left_join(reuse) %>% 
  filter(!is.na(perc_lib_ssps)) %>% 
  # Note: kwh_veh is stacked (shares) for the whole battery size
  mutate(lib_ssps_kwh=Sales*perc_lib_ssps*kwh_veh,
         lib_recycling_kwh=Sales*perc_lib_recycling*kwh_veh) %>% 
  group_by(Region,Country,Year,chemistry,Scenario,chem_scenario) %>% 
  summarise(lib_ssps_kwh=sum(lib_ssps_kwh),
            lib_recycling_kwh=sum(lib_recycling_kwh)) %>% ungroup()

df_addLib <- df %>% 
  left_join(dplyr::select(reuse,Year,Scenario,Vehicle,Powertrain,perc_add_lib)) %>% 
  filter(!is.na(perc_add_lib)) %>% 
  mutate(Sales=Sales*perc_add_lib,
         Vehicle="Additional LIB") %>%  # additional new LIB required
  dplyr::select(-perc_add_lib)

df <- rbind(df,df_addLib)

## kWH ----
df <- df %>% 
  mutate(kwh_required=Sales*kwh_veh)

# Add stationary power storage
stationary <- stationary %>% 
  rename(kwh_required=stationaryPower) %>% # in Mwh 
  mutate(kwh_required=kwh_required*1e3)

# reduce demand by using LIB outflow
stationary <- stationary %>% left_join(lib_outflow) %>% 
  mutate(lib_ssps_kwh = if_else(is.na(lib_ssps_kwh), 0, lib_ssps_kwh),
         lib_recycling_kwh = if_else(is.na(lib_recycling_kwh), 0, lib_recycling_kwh)) %>% 
  mutate(allocate=pmin(kwh_required,lib_ssps_kwh)) %>%  # substract only to 0 max
  mutate(kwh_required=kwh_required-allocate,
         lib_ssps_kwh=lib_ssps_kwh-allocate)

# for NAs add scenarios - SCENARIO REQUIRED DUE TO LIB FLOW TO SSPS
stationary_na <- stationary %>% filter(is.na(Scenario))
stationary <- stationary %>% filter(!is.na(Scenario))
stationary_na <- stationary_na %>% dplyr::select(-Scenario,-chem_scenario)
s1 <- s2 <- s3 <- stationary_na;
s1$Scenario="Baseline";s2$Scenario="Momentum";s3$Scenario="Ambitious";
stationary_na <- rbind(s1,s2,s3);rm(s1,s2,s3)
# No chem scenario for SPS
s1 <- s2 <- s3 <- stationary_na;
s1$chem_scenario="Baseline";s2$chem_scenario="Double LFP";s3$chem_scenario="Double NMC 811";
stationary_na <- rbind(s1,s2,s3);rm(s1,s2,s3)
stationary <- rbind(stationary,stationary_na)
rm(stationary_na)

# update lib outflow, other ssps goes to recycling
lib_outflow <- lib_outflow %>% 
  left_join(dplyr::select(stationary,Region,Country,Year,chemistry,Scenario,chem_scenario,
                          allocate)) %>% 
  mutate(allocate = if_else(is.na(allocate), 0, allocate),
         lib_ssps_kwh=lib_ssps_kwh-allocate) %>% 
  mutate(lib_recycling_kwh=lib_recycling_kwh+lib_ssps_kwh) %>% 
  dplyr::select(-lib_ssps_kwh,-allocate)

stationary <- stationary %>% dplyr::select(-lib_ssps_kwh,-lib_recycling_kwh,-allocate)

# add dummies to join
stationary <- stationary %>% 
  mutate(Powertrain="SPS",
         Sales=0,kwh_veh=0) 
df$Scenario %>% unique()

df <- rbind(df,stationary) # 1 M rows for now


## Add Mineral Intensity --------

df <- df %>% filter(kwh_required>0) 

# use same chemistries for now
df$chemistry %>% unique()
df %>% group_by(chemistry) %>% summarise(x=sum(kwh_required)) %>% arrange(x)
mineral$chemistry %>% unique()

mineral <- mineral %>% filter(Mineral %in% min_interest)

df <- df %>% left_join(mineral)
nrow(df)/1e6 # 4.9 MILLION

####################
# CALCULATIONS ---------
####################

## Minerals ----
df <- df %>% mutate(tons_mineral=kwh_required*kg_per_kwh/1e3)

# Substract due to recycling - remaining SSPS goes to recycling as well
# same year for now
lib_outflow <- lib_outflow %>% 
  left_join(mineral) %>% 
  rename(kwh_required=lib_recycling_kwh) %>% 
  mutate(tons_mineral=-kwh_required*kg_per_kwh/1e3,
         Vehicle="Recycling")
# BETTER TO DO IT AS ADDITIONAL VEHICLE: Scenario, and negative

names(df)
names(lib_outflow)
lib_outflow$kwh_veh <- lib_outflow$Sales <- 0
lib_outflow$Powertrain <- "Recycling"

df <- rbind(df,lib_outflow)


## Add Other sector demand -------- 
names(df)
names(otherSectors)

# add region
region_dict <- df %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)
otherSectors <- otherSectors %>% left_join(region_dict) %>% 
  mutate(Scenario="No Scenario",chem_scenario="No Scenario",
         chemistry="Other Sectors",
         Vehicle="Other Sectors",kwh_veh=0,kwh_required=0,kg_per_kwh=0,Sales=0)
rm(region_dict)
df <- rbind(df,otherSectors)


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