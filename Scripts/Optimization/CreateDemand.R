# Compile world Mineral Demand from results of the Demand Module
# PBH February 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load demand results -----
# df <- read.csv("Results/MineralDemandRegion.csv")
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster


# Filter - Lithium for now
df <- df %>% filter(Mineral=="Lithium")

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
  group_by(Scenario,Region,t) %>% 
  reframe(Recycling=-sum(tons_mineral)/1e3)

unique(df$Vehicle)
df_sector <- df %>% rename(t=Year) %>% 
  group_by(t,Vehicle,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df_region <- df %>% rename(t=Year) %>% 
  group_by(t,Region,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))


## Aggregate at world level - Primary mineral demand --------
df <- df %>% rename(t=Year) %>% 
  group_by(t,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df$Demand %>% range()

# significant digits
df <- df %>% mutate(Demand=Demand/1e3) # in ktons

df$t %>% range()


# save results
write.csv(df,"Parameters/Demand.csv",row.names = F)
write.csv(df_recyc,"Parameters/Recycling.csv",row.names = F)
write.csv(df_sector,"Parameters/Demand_Detail.csv",row.names = F)
write.csv(df_region,"Parameters/Demand_Region.csv",row.names = F)


# Figure
ggplot(df,aes(t,Demand,group=Scenario))+
  # geom_line(alpha=.5,col="darkgrey")+
  geom_line(alpha=.5,aes(col=Scenario))+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022,seq(2030,2070,10)))+
  labs(x="",y="Lithium \n Demand \n [ktons]")+
  theme(axis.text.x = element_text(hjust=0.8),
        legend.position = "bottom")

# Recycling  
df_recyc %>% 
  group_by(t,Scenario) %>% 
  reframe(Demand=sum(Recycling)) %>% 
  ggplot(aes(t,Demand,group=Scenario))+
  # geom_line(alpha=.5,col="darkgrey")+
  geom_line(alpha=.5,aes(col=Scenario))+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022,seq(2030,2070,10)))+
  labs(x="",y="Lithium \n Recycled \n [ktons]")+
  theme(axis.text.x = element_text(hjust=0.8),
        legend.position = "bottom")



# EoF