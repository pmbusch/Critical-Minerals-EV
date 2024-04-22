# Compile world Mineral Demand from results of the Demand Module
# PBH February 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load demand results
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

# Aggregate at world level
df <- df %>% rename(t=Year) %>% 
  group_by(t,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df$Demand %>% range()

# significant digits
df <- df %>% mutate(Demand=Demand/1e3) # in ktons

df$t %>% range()

# save results
write.csv(df,"Parameters/Demand.csv",row.names = F)

# Figure
ggplot(df,aes(t,Demand,group=Scenario))+
  # geom_line(alpha=.5,col="darkgrey")+
  geom_line(alpha=.5,aes(col=Scenario))+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022,seq(2030,2070,10)))+
  labs(x="",y="Lithium \n Demand \n [ktons]")+
  theme(axis.text.x = element_text(hjust=0.8),
        legend.position = "bottom")
  

# EoF