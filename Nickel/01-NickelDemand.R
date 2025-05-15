# Compile world Mineral Demand from results of the Demand Module
# Nickel Demand Results - Also Cobalt demand
# Load results from main demand model and create simplified inputs for the supply model
# PBH and PO November 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R")

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
df$chem_scenario <- df$capacity_scenario <- df$lifetime_scenario <- df$recycling_scenario <- df$scen_all <- NULL

df$Scenario %>% unique()

# Nickel Stainless steel scenarios ------

demand_ss <- read.csv("Parameters/Demand Intermediate Results/Stainless_Steel_Scen.csv") 
icct <- read.csv("Parameters/Demand Intermediate Results/ICCT_demand.csv")
icct <- icct %>% filter(Sales>0) # reduce computational time
dict_region <- icct %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)
demand_ss <- demand_ss %>% 
  left_join(dict_region) %>% 
  group_by(Year,ss_scen,Region) %>% 
  reframe(tons_mineral=sum(tons_mineral)) %>% ungroup()

# nickel demand no stainless steel
dummy_demand <- df %>% filter(Mineral=="Nickel",Powertrain!="Stainless Steel")

names(dummy_demand)
demand_ni <- tibble(Scenario=unique(df$Scenario)) %>% 
  cross_join(demand_ss) %>% 
  mutate(Powertrain="Stainless Steel",Vehicle="Other Sectors",chemistry="Other Sectors",
         Mineral="Nickel") 
  
unique(demand_ni$ss_scen)
dummy_demand <- rbind(mutate(dummy_demand,ss_scen="GDP Elasticity 0.5"),
                      mutate(dummy_demand,ss_scen="GDP Elasticity 1.2"),
                      demand_ni)
dummy_demand <- dummy_demand %>% 
  mutate(Scenario=paste0(Scenario,"-Steel",ss_scen),ss_scen=NULL)

df <- rbind(df,dummy_demand)

# Scenarios -----

# Filter Scenarios to include
unique(df$Scenario)
df <- df %>% filter(str_detect(Scenario,"Baseline-Baseline-Baseline-Baseline|Enhanced recycling|High|Low Capacity-Baseline-Baseline|NMC 811|LFP"))


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


# Exploratory figures
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))
df %>% 
  filter(Mineral=="Nickel") %>% 
  # filter(str_detect(Scenario,"High")) %>% 
  ggplot(aes(t,Demand,col=Scenario))+geom_line()



# EoF