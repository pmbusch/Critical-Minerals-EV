# Compile world Mineral Demand from results of the Demand Module
# Nickel Demand Results - Also Cobalt demand
# Load results from main demand model and create simplified inputs for the supply model
# PBH and PO November 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load demand results -----
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster


# Filter
df <- df %>% filter(Mineral %in% c("Nickel","Cobalt","Copper","Battery_MWh"))

# Combine Scenarios
df <- df %>% mutate(Scenario=paste(Scenario,
                                   chem_scenario,
                                   capacity_scenario,
                                   lifetime_scenario,
                                   recycling_scenario,sep="-"))
df$Scenario %>% unique()

# Scenarios -----

## Recycling at country level, to get concentration index after -----
df_recyc <- df %>% rename(t=Year) %>% 
  filter(Vehicle=="Recycling") %>% 
  group_by(Mineral,Scenario,Region,t) %>% 
  reframe(Recycling=-sum(tons_mineral)/1e3)

unique(df$Vehicle)
df_sector <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Vehicle,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

# Highlight stainless steel
unique(df$Powertrain)
df_sector2 <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Vehicle,Powertrain,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df_region <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Region,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))


## Aggregate at world level - Primary mineral demand --------
df <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df$Demand %>% range()

# significant digits
df <- df %>% mutate(Demand=Demand/1e3) # in ktons

df$t %>% range()


# save results

# Nickel
df %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Nickel_Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Nickel_Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Nickel_Demand_Detail.csv",row.names = F)
df_sector2 %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Nickel_Demand_GreaterDetail.csv",row.names = F)
df_region %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Nickel_Demand_Region.csv",row.names = F)

# Cobalt
df %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand_Detail.csv",row.names = F)
df_sector2 %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand_GreaterDetail.csv",row.names = F)
df_region %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand_Region.csv",row.names = F)

# Copper
df %>% filter(Mineral=="Copper") %>% 
  write.csv("Nickel/Parameters/Copper_Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Copper") %>% 
  write.csv("Nickel/Parameters/Copper_Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Copper") %>% 
  write.csv("Nickel/Parameters/Copper_Demand_Detail.csv",row.names = F)
df_sector2 %>% filter(Mineral=="Copper") %>% 
  write.csv("Nickel/Parameters/Copper_Demand_GreaterDetail.csv",row.names = F)
df_region %>% filter(Mineral=="Copper") %>% 
  write.csv("Nickel/Parameters/Copper_Demand_Region.csv",row.names = F)

# Battery
df %>% filter(Mineral=="Battery_MWh") %>% 
  write.csv("Nickel/Parameters/BatteryGWh_Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Battery_MWh") %>% 
  write.csv("Nickel/Parameters/BatteryGWh_Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Battery_MWh") %>% 
  write.csv("Nickel/Parameters/BatteryMWh_Demand_Detail.csv",row.names = F)
df_sector2 %>% filter(Mineral=="Battery_MWh") %>% 
  write.csv("Nickel/Parameters/Battery_MWh_Demand_GreaterDetail.csv",row.names = F)
df_region %>% filter(Mineral=="Battery_MWh") %>% 
  write.csv("Nickel/Parameters/BatteryMWh_Demand_Region.csv",row.names = F)


# EoF