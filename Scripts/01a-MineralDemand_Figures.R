# Analyze Mineral Demand Results
# Mineral Demand Module
# PBH August 2023


source("Scripts/00-Libraries.R", encoding = "UTF-8")


# load pre-computed results
# df <- read.csv("Results/MineralDemand.csv")
df <- read.csv("Results/MineralDemandRegion.csv") # faster

# summary
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario=="Baseline") %>%
  filter(chem_scenario=="Benchmark") %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,chem_scenario,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton)

# summary by type
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario=="Baseline") %>%
  filter(chem_scenario=="Benchmark") %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Vehicle,Year,Mineral) %>% summarise(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(type=case_when(Vehicle=="Car" ~ "EV",
                        Vehicle=="Stationary Power Storage"~"SPS",
                        T ~ "Other on-road transport")) %>%
  group_by(type,Year,Mineral) %>% summarise(kton=sum(kton)) %>%
  pivot_wider(names_from = Year, values_from = kton)



# cumulative demand
df %>% 
  filter(Scenario=="Baseline") %>%
  filter(chem_scenario=="Benchmark") %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3)

# by scenario
df %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,chem_scenario,Mineral) %>% 
  summarise(Mton=sum(tons_mineral)/1e6) %>% 
  pivot_wider(names_from = Mineral, values_from = Mton)


# Figures -----
fig_name <- "Figures/MineralDemand/%s.png"

## By Scenario -----------
df %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Year,Scenario,chem_scenario,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(Scenario=factor(Scenario,levels=scen_level)) %>%
  ggplot(aes(Year,kton,col=Scenario,group=Scenario))+
  geom_line(linewidth=0.5)+
  # facet_wrap(~Mineral,ncol=1,scales = "free_y")+
  facet_grid(Mineral~chem_scenario,scales = "free_y")+
  labs(y="Mineral \n Demand \n [ktons]",x="")+
  coord_cartesian(expand=F)+
  ylim(0,NA)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,"scenarios"))

# range of scenarios
df %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Year,Scenario,chem_scenario,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(comb=paste0(Scenario,"-",chem_scenario)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(Scenario=factor(Scenario,levels=scen_level)) %>%
  ggplot(aes(Year,kton,group=comb,col=Scenario,linetype=chem_scenario))+
  geom_line(linewidth=0.5,alpha=.7)+
  facet_wrap(~Mineral,ncol=2,scales = "free_y")+
  labs(y="Mineral \n Demand \n [ktons]",x="",
       linetype="Chemistry \ Scenario",col="ICCT Demand \n Scenario")+
  coord_cartesian(expand=F)+
  ylim(0,NA)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))
  # theme(legend.position = "none")
f.fig.save(sprintf(fig_name,"Rangescenarios"))

## By powertrain -----------

scen="Baseline"
chem_scen="Benchmark"
mine="Lithium"

df %>% 
  filter(Mineral %in% min_interest) %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Powertrain) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Powertrain))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales = "free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),
       x="",fill="Powertrain"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "E")+
  ggtitle("B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("powertrain_",scen)))


## Vehicle Type ------

pt="BEV"

df %>% 
  filter(Mineral %in% min_interest) %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  # filter(Powertrain==pt) %>%
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Vehicle=factor(Vehicle,levels=vehicle_level)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",
       fill="Vehicle"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  ggtitle("A")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("vehicle_",scen)))

## region ----
df %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  # filter(Mineral==mine) %>%
  mutate(Region=if_else(Region=="World","Rest of the World",Region)) %>% # for stationary, for now, erase latter 
  filter(Mineral %in% min_interest) %>%
  group_by(Year,Region,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Region"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  ggtitle("C")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(legend.text = element_text(size=10),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("region",scen,sep="_")))

## Vehicle by region of interest -----

# version 1

regs <- c("United States","European Union","China","India","ASEAN")

df %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  filter(Mineral==mine) %>% 
  filter(Powertrain!="SPS") %>% 
  mutate(Region=if_else(Region %in% regs,Region,"Rest of the World")) %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Mineral,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Region)+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Vehicle type",
       caption = paste0(scen," scenario."))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste("veh",scen,mine,sep="_")))

# version 2
df %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  filter(Mineral==mine) %>% 
  filter(Powertrain!="SPS") %>% 
  group_by(Year,Mineral,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Vehicle,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Region",
       caption = paste0(scen," scenario."))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"),
        legend.text = element_text(size=10),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("veh",scen,mine,"2",sep="_")))

# For SPS
df %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  # filter(Mineral==mine) %>% 
  filter(Mineral %in% min_interest) %>%
  filter(Powertrain=="SPS") %>% 
  group_by(Year,Mineral,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Mineral,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Region",
       caption = paste0(scen," scenario."))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  # scale_fill_viridis_d()+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"),
        legend.text = element_text(size=10),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("sps",scen,mine,sep="_")))

## By Chemistry AND MINERAL---------

df %>% 
  filter(Scenario==scen) %>% 
  filter(chem_scenario==chem_scen) %>% 
  # filter(Powertrain==pt) %>% 
  # filter(Mineral==mine) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Year,chemistry,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(chemistry))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Chemistry"
       # caption = paste0(scen," scenario.")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  ggtitle("D")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste("chemistry",scen,sep="_")))

# EoF