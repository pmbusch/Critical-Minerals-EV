# Load and join all avg. battery size data
# Simply gets all pre-process data from other sources.
# PBH February 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")



## ICCT on-road demand projections -----
icct <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")

eq_country_region <- icct %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)


## Load Battery size -----
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
