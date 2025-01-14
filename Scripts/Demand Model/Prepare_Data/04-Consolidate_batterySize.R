# Merge and Consolidate battery size data at country level
# Useful to have clear data on battery size used
# Adds battery chemistry scenarios as well
# PBH February 2024

url_save <- "Parameters/Demand Intermediate Results/%s.csv"

# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# dictionary or equivalency for ICCT
eq_country_region <- read_excel("Data/Joins/Eq_Countries_ICCT_EVV.xlsx",
                                sheet = "Eq_Country", range = "A1:B188")
names(eq_country_region) <- names(eq_country_region) %>% str_remove("ICCT_")

# for LDV by country 
bat_ldv <- read.csv("Parameters/Battery/battery_size_country.csv")
# By region
bat_region <- read.csv("Parameters/Battery/battery_size_region.csv")
bat_region <- bat_region %>% filter(Year==2022)
bat_ldv$Vehicle <- "Car";bat_region$Vehicle <- "Car";

# Battery for other on-road transport 
bat_others_orig <- read_excel("Data/Demand Model/Battery_Size.xlsx",sheet = "Battery_Size")
bat_others_orig <- bat_others_orig %>% 
  pivot_longer(c(kwh_veh,low_capacity,high_capacity), 
               names_to = "scen", values_to = "kwh_veh")
  
# share of 2-3 wheelers
share_2_3_wheelers <- read_excel("Data/Demand Model/Disaggregated 2-3 wheeler sales.xlsx",
                                 sheet="2_3_WheelerShare",range = "A27:D31")

bat_wheelers <- bat_others_orig %>% filter(str_detect(Vehicle,"wheeler"))
bat_others <- bat_others_orig %>% filter(!str_detect(Vehicle,"wheeler"))

bat_wheelers <- share_2_3_wheelers %>% 
  dplyr::select(Country,Share_2Wheeler) %>% mutate(Year=2022) %>% 
  left_join(bat_wheelers)
bat_wheelers <- bat_wheelers %>%
  pivot_wider(names_from = Vehicle, values_from = kwh_veh) %>% 
  mutate(kwh_veh=`2 wheeler`*Share_2Wheeler+`3 wheeler`*(1-Share_2Wheeler))
bat_wheelers$Share_2Wheeler <- bat_wheelers$`2 wheeler` <- bat_wheelers$`3 wheeler` <- NULL

bat_wheelers$Vehicle <- "Two/Three Wheelers"
bat_others$Country <- "World"

bat_others <- rbind(bat_others,bat_wheelers)
rm(share_2_3_wheelers,bat_wheelers)

# Battery Size  ---------

# expand list of countries to have data on battery size
bat2 <- bat <- eq_country_region
bat$Powertrain="BEV";bat2$Powertrain="PHEV";
bat <- rbind(bat,bat2);rm(bat2)

# For LDV -+ join by country name
bat <- bat %>% 
  left_join(bat_ldv) %>%  # add region
  mutate(Year=NULL) # for all years use the same battery size (FOR NOW)


# get NA terms - countries that we will use  by region
bat_ldv_na <- bat %>% filter(is.na(kwh_veh_total)) %>% 
  dplyr::select(Region,Country,Powertrain)
bat_region <- bat_region %>% mutate(Year=NULL) # no year for now
# add the regional estimates
bat_ldv_na <- bat_ldv_na %>% left_join(bat_region)

bat_ldv <- bat %>% filter(!is.na(kwh_veh_total)) # to join later
# NO MISSING no
bat_ldv <- rbind(bat_ldv,bat_ldv_na); rm(bat_ldv_na);
# Check
length(unique(bat_ldv$Country))*length(unique(bat_ldv$Powertrain)) # 187 * 2 = 374
bat_ldv$MWh <- bat_ldv$unit <- NULL  
bat_ldv <- bat_ldv %>% rename(kwh_veh=kwh_veh_total)

# ratio PHEV to BEV - aprox 20%
bat_ldv %>% group_by(Powertrain,Region,Vehicle) %>% 
  reframe(kwh_veh=mean(kwh_veh)) %>% 
  pivot_wider(names_from = Powertrain, values_from = kwh_veh) %>% 
  mutate(ratio_phev_bev=PHEV/BEV)

# other vehicles - All BEVs
bat_others_scen <- bat_others
bat_others$Year <- NULL # for all years use the same battery
bat_others <- bat_others %>% filter(scen=="kwh_veh") %>% mutate(scen=NULL)

# simply use world average for all other vehicles
bat_rest <- c()
for (veh in unique(bat_others$Vehicle)){
  print(veh)
  aux_bat <- bat_others %>% filter(Vehicle==veh,Country=="World") %>% mutate(Country=NULL)
  bat_aux <- eq_country_region %>% mutate(Vehicle=veh) %>% 
    left_join(aux_bat)
  bat_rest <- rbind(bat_rest,bat_aux)
  rm(aux_bat,bat_aux)
}

# replace 2-3 wheelers battery to specific country data
bat_special <- bat_others %>% filter(Country!="World") %>% rename(kwh_veh_special=kwh_veh)
bat_rest <- bat_rest %>% 
  left_join(bat_special) %>% 
  mutate(kwh_veh=if_else(is.na(kwh_veh_special),kwh_veh,kwh_veh_special),
         kwh_veh_special=NULL)
nrow(bat_rest) # 187 * 5 = 935

# merge both no chem
bat_rest_aux <- mutate(bat_rest,chemistry=NULL)
bat_rest_aux <- rbind(mutate(bat_rest_aux,Powertrain="BEV"),
                      # 20% of battery size assumed for PHEV
                      mutate(bat_rest_aux,Powertrain="PHEV",kwh_veh=kwh_veh*0.2))

bat <- rbind(bat_ldv,bat_rest_aux)


# Save results
write.csv(bat,sprintf(url_save,"bat_size"),row.names = F)

# Bat Size Over time ------------

# Scenarios to 45 and 90 kwh for cars
# Scenarios for all
years <- tibble(Year=2022:2070,dummy=1)
batSize_scen <- tibble(dummy=1,capacity_scenario=c("Baseline","Low Capacity","High Capacity"))
# scenario constant
bat_2050 <- bat_ldv %>% mutate(dummy=1) %>% 
  left_join(years) %>% 
  left_join(batSize_scen) %>% 
  mutate(dummy=NULL)

# Do Scenarios for BEV  based on desired kWh
goal_year <- 2035 # year that range/cap is achieved
cap_goals <- tibble(dummy=1,
                      capacity_scenario=c("Low Capacity","High Capacity"),
                      cap_goal=c(45,90))
                      
# slope
slope_bat <- bat_ldv %>% mutate(dummy=1) %>% 
  left_join(cap_goals) %>% 
  mutate(slope=(cap_goal-kwh_veh)/(goal_year-2022)) %>% 
  dplyr::select(Country,Powertrain,capacity_scenario,slope,cap_goal)

# linearly until 2035
bat_2050 <- bat_2050 %>% 
  left_join(slope_bat) %>% 
  rename(kwh_veh_2022=kwh_veh) %>% 
  mutate(kwh_veh=if_else(Year>goal_year,
                       cap_goal,
                       kwh_veh_2022+(Year-2022)*slope)) %>%
  mutate(kwh_veh=if_else(is.na(kwh_veh)|Powertrain=="PHEV",kwh_veh_2022,kwh_veh)) %>% 
  dplyr::select(-slope,-kwh_veh_2022,-cap_goal)

# Figure at region level
bat_2050 %>% 
  filter(Powertrain=="BEV") %>% 
  group_by(Region,Year,capacity_scenario) %>% 
  reframe(kwh_veh=mean(kwh_veh)) %>% ungroup() %>% # note that unit are valid for 2022
  filter(Year<2036) %>% 
  ggplot(aes(Year,kwh_veh,col=Region))+
  geom_line()+
  facet_wrap(~capacity_scenario,ncol=1)+
  scale_color_manual(values = region_colors) +
  labs(x="",y="Battery \n Capacity \n [kWh]")+
  coord_cartesian(expand = F)+
  theme(legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))+
  scale_x_continuous(breaks = seq(2023,2035,2))+
  scale_y_continuous(breaks=c(20,40,60,80),limits = c(0,95))
f.fig.save("Figures/batSize_scenarios.png")

## For other vehicles ------

# add time and expand linearly to 2035, based on goals
bat_2050_others <- bat_rest_aux %>% mutate(dummy=1) %>% 
  left_join(years) %>% 
  left_join(batSize_scen) %>% 
  mutate(dummy=NULL)

# join based on average original kWh value (to avoid left join to countries again)
aux_join <- bat_others_scen %>% 
  dplyr::select(-Year,-chemistry) %>% 
  pivot_wider(names_from = scen, values_from = kwh_veh) %>% 
  dplyr::select(-Country,-Vehicle) %>% 
  mutate(Baseline=kwh_veh) %>% 
  pivot_longer(c(-kwh_veh), names_to = "capacity_scenario", values_to = "goal") %>% 
  mutate(capacity_scenario=str_replace(capacity_scenario,"_"," ") %>% 
           str_to_title())

# Linear to 2035
aux_join <- aux_join %>% mutate(dummy=1) %>% 
  left_join(years,relationship = "many-to-many") %>% 
  mutate(slope=(goal-kwh_veh)/(goal_year-2022)) %>% 
  mutate(new_kwh_veh=if_else(Year>2035,goal,kwh_veh+(Year-2022)*slope)) %>% 
  dplyr::select(-goal,-slope,-dummy) 
  
bat_2050_others <- bat_2050_others %>% 
  left_join(aux_join) %>% 
  mutate(kwh_veh=if_else(is.na(new_kwh_veh),kwh_veh,new_kwh_veh),
         new_kwh_veh=NULL)

bat_2050_all <- rbind(bat_2050,bat_2050_others)

write.csv(bat_2050_all,sprintf(url_save,"bat_size_forecast"),row.names = F)

# Chemistry ------------

# Use regional chemistry share forecasts, BY 3 scenarios
chem <- read.csv("Parameters/Battery/Chemistry_Scenarios/bat_share_2050_region.csv")
chem$chem_scenario <- "Baseline"
chem_LFP <- read.csv("Parameters/Battery/Chemistry_Scenarios/LFP_scen_region.csv")
chem_LFP$chem_scenario <- "Double LFP"
chem_NMC <- read.csv("Parameters/Battery/Chemistry_Scenarios/NMC811_scen_region.csv")
chem_NMC$chem_scenario <- "Double NMC 811"
chem_SS <- read.csv("Parameters/Battery/Chemistry_Scenarios/SolidState_scen_region.csv")
chem_SS$chem_scenario <- "Solid State adoption"
chem_SIB <- read.csv("Parameters/Battery/Chemistry_Scenarios/Sodium_scen_region.csv")
chem_SIB$chem_scenario <- "Sodium Battery adoption"


chem <- rbind(chem,chem_LFP,chem_NMC,chem_SS,chem_SIB);rm(chem_LFP,chem_NMC,chem_SS,chem_SIB)
chem <- chem %>% rename(Year=year)

# Use battery size forecasts

# Add chemistry scenarios for LDV
bat_ldv <- bat_2050 %>%
  filter(Vehicle=="Car") %>% 
  left_join(chem) %>% 
  mutate(kwh_veh=kwh_veh*share_units,
         share_units=NULL)

# add chem to others
chem_rest <- bat_rest %>% group_by(Vehicle,chemistry) %>% 
  tally() %>% mutate(n=NULL)
bat_rest2 <- bat_2050_others %>% 
  left_join(chem_rest)

# Save ldv and rest separately
write.csv(bat_ldv,sprintf(url_save,"bat_size_chem_ldv"),row.names = F)
write.csv(bat_rest2,sprintf(url_save,"bat_size_chem_rest"),row.names = F)


# EoF