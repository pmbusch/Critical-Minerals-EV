# Compile world Mineral Demand from results of the Demand Module
# Nickel Demand Results - Also Cobalt demand
# Load results from main demand model and create simplified inputs for the supply model
# PBH and PO November 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load demand results -----
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster


# Filter
df <- df %>% filter(Mineral %in% c("Nickel","Cobalt"))

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
  write.csv("Nickel/Parameters/Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Demand_Detail.csv",row.names = F)
df_region %>% filter(Mineral=="Nickel") %>% 
  write.csv("Nickel/Parameters/Demand_Region.csv",row.names = F)

# Cobalt
df %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand.csv",row.names = F)
df_recyc %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Recycling.csv",row.names = F)
df_sector %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand_Detail.csv",row.names = F)
df_region %>% filter(Mineral=="Cobalt") %>% 
  write.csv("Nickel/Parameters/Cobalt_Demand_Region.csv",row.names = F)

# EoF