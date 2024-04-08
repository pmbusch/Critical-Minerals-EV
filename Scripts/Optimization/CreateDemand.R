# Compile world Mineral Demand from results of the Demand Module
# PBH February 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load demand results
df <- read.csv("Results/MineralDemandRegion.csv")

# Filter - Lithium for now
df <- df %>% filter(Mineral=="Lithium")

# Combine Scenarios
df <- df %>% mutate(Scenario=paste(Scenario,chem_scenario,capacity_scenario,sep="-"))
df$Scenario %>% unique()

# Aggregate at world level
df <- df %>% rename(t=Year) %>% 
  group_by(t,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df$Demand %>% range()

# significant digits
df <- df %>% mutate(Demand=Demand/1e3) # in ktons

# Expand to 2070 base on last 5 years growth

# avg last 5 years
avg_5 <- df %>% 
  filter(t>2045) %>% rename(x=Demand) %>% 
  group_by(Scenario) %>% 
  summarise(growth = mean((x / lag(x) - 1) * 100,na.rm=T))

# Fill in the dataframe with the projected values
df_fill <- expand.grid(Scenario=unique(df$Scenario),t=2050:2070) %>% 
  left_join(df)

df_fill <- df_fill %>% left_join(avg_5) %>% 
  group_by(Scenario) %>% 
  mutate(Growth_Factor = if_else(t==2050,1,1 + growth/100),
    Demand=first(Demand) * cumprod(Growth_Factor)) %>% 
  filter(t>2050) # remove initial
df_fill$growth <- df_fill$Growth_Factor <- NULL

df <- rbind(df,df_fill)

# save results
write.csv(df,"Parameters/Demand.csv",row.names = F)

# Figure
ggplot(df,aes(t,Demand,group=Scenario))+
  geom_line(alpha=.5,col="darkgrey")+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022,seq(2030,2070,10)))+
  labs(x="",y="Lithium \n Demand \n [ktons]")+
  theme(axis.text.x = element_text(hjust=0.8))
  

# EoF