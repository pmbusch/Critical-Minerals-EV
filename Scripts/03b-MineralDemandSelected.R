# Analyze Mineral Demand Results for Selected Scenarios
# Mineral Demand Module
# PBH April 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

fig_name <- "Figures/MineralDemand/Scenarios/%s.png"


# load pre-computed results
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster

df <- df %>% filter(Year<2051)
nrow(df)/1e6


# Combine Scenarios
df <- df %>% mutate(scen_all=paste(Scenario,chem_scenario,
                                   capacity_scenario,
                                   lifetime_scenario,recycling_scenario,sep="-"))
df$scen_all %>% unique()

scens_selected
df <- df %>% left_join(tibble(scen_all=scens_selected,name=scens_names))

# Save Cumulative Demand -----------
df_save <- df %>% group_by(name,Mineral) %>% 
  reframe(ktons_cumDemand=sum(tons_mineral)/1e3)
write.csv(df_save,"Results/MineralCumulativeDemand.csv",row.names = F)

# TABLES ------------

# USGS Data ---------

# USGS 2024 reserves and demonstratred resources
usgs <- read_excel("Data/USGS2024.xlsx",sheet="Data")

# to M ton
usgs <- usgs %>% 
  mutate(Reserves=Reserves/1e6,Resources_Demonstrated=Resources_Demonstrated/1e6)

df %>% group_by(name,Mineral) %>% 
  reframe(cumDemand=sum(tons_mineral)/1e6) %>% 
  left_join(usgs) %>% 
  mutate(share_reserve=cumDemand/Reserves,
         share_resource=cumDemand/Resources_Demonstrated) 
# # spread
#   dplyr::select(-Reserves,-Resources_Demonstrated) %>% 
#   pivot_wider(names_from = Mineral, values_from = c(cumDemand,share_reserve,share_resource))

.Last.value %>% write.table("clipboard", sep="\t",row.names = F)

  

# Figures ---------

data_fig <- df %>% 
  group_by(name,Year,Mineral) %>%
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() 

order_scen <- data_fig %>% filter(Mineral=="Lithium",Year==2050) %>% 
  arrange(desc(kton)) %>% pull(name)

data_fig <- data_fig %>% mutate(name=factor(name,levels=order_scen))

data_fig %>% 
  filter(Mineral!="Lithium") %>% 
  ggplot()+
  geom_line(aes(Year,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  coord_cartesian(expand=F)+
  facet_wrap(~Mineral,scales = "free_y")+
  labs(x="",y="",title="Mineral Demand [ktons]",col="Demand \nScenario")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,"AllMinerals"))

# For Lithium
data_fig <- data_fig %>% 
  filter(Mineral=="Lithium")

ggplot(data_fig)+
  geom_line(aes(Year,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  coord_cartesian(expand=F)+
  labs(x="",y="",title="Lithium Demand [ktons]",col="Demand \nScenario")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,"Lithium"))


# EoF