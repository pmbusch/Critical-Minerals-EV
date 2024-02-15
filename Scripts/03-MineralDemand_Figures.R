# Analyze Mineral Demand Results
# Mineral Demand Module
# PBH August 2023

source("Scripts/00-Libraries.R", encoding = "UTF-8")


# load pre-computed results
# df <- read.csv("Results/MineralDemand.csv")
df <- read.csv("Results/MineralDemandRegion.csv") # faster

scens <- c("Ambitious","Baseline","Momentum")
chems_scen <- c("Baseline","Double LFP","Double NMC 811")

# summary
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario %in% c("Momentum","No Scenario")) %>%
  filter(chem_scenario %in% c("Baseline","No Scenario")) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,chem_scenario,Year,Mineral) %>% 
  f.duplicateScenarios(c("Baseline"),scenario_col = "chem_scenario") %>%
  f.duplicateScenarios(c("Momentum"),scenario_col = "Scenario") %>%
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton)

# by scenario
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Year==2050) %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Scenario,chem_scenario,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = c(Mineral), values_from = kton)

# ratio 2050 to 2022 - ranges
df %>% 
  filter(Year %in% c(2022,2050)) %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Scenario,chem_scenario,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton) %>% 
  mutate(ratio=`2050`/`2022`) %>% dplyr::select(-`2022`,-`2050`) %>% 
  group_by(Mineral) %>% summarise(min_ratio=min(ratio),max_ratio=max(ratio))

# summary by type
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario %in% c("Momentum","No Scenario")) %>%
  filter(chem_scenario %in% c("Baseline","No Scenario")) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Vehicle,Year,Mineral) %>% summarise(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(type=case_when(Vehicle=="Car" ~ "EV",
                        Vehicle=="Stationary Power Storage"~"SPS",
                        T ~ "Other on-road transport")) %>%
  group_by(type,Year,Mineral) %>% summarise(kton=sum(kton)) %>%
  pivot_wider(names_from = Year, values_from = kton)

# cumulative demand
df %>% 
  filter(Scenario %in% c("Momentum","No Scenario")) %>%
  filter(chem_scenario %in% c("Baseline","No Scenario")) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,Mineral) %>% 
  f.duplicateScenarios("Momentum",scenario_col = "Scenario") %>% 
  summarise(kton=sum(tons_mineral)/1e3)

# by scenario, million tons
df %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Scenario,chem_scenario,Mineral) %>% 
  summarise(Mton=sum(tons_mineral)/1e6) %>% 
  pivot_wider(names_from = Mineral, values_from = Mton)

# ranges
df %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Scenario,chem_scenario,Mineral) %>% 
  summarise(Mton=sum(tons_mineral)/1e6) %>% ungroup() %>% 
  group_by(Mineral) %>% summarise(min_Mton=min(Mton),max_Mton=max(Mton))


# Figures -----
fig_name <- "Figures/MineralDemand/%s.png"

## By Scenario -----------
df %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
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
depletion_horizon <- 50 # years, or 2% depletion rate
dep2 <- 20 # 5% depletion rate
reserves <- tibble(Mineral=factor(min_interest,levels = min_interest),
                   reserve=c(26,100,8.3)*1e3,  # USGS 2022, 1.7 Mn
                   prod=c(0.13,3.3,0.19)*1e3) %>%  # 0.02 Mn
  mutate(dep50=reserve/depletion_horizon,
         dep20=reserve/dep2) %>% 
  mutate(label_pos=c(1400,-1000,-1100),
         label_prod=c(190,-1000,-1000))  

# required scaling of production - 2022 to 2050
df %>% filter(Year==2050) %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Scenario,chem_scenario,Mineral) %>% reframe(ktons=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  left_join(reserves) %>% 
  mutate(upscale=ktons/prod) %>% 
  dplyr::select(Scenario,chem_scenario,Mineral,upscale) %>% 
  pivot_wider(names_from = Mineral, values_from = upscale)

p1 <- df %>% 
  filter(Mineral %in% min_interest) %>% 
  f.duplicateScenarios(chems_scen,scenario_col = "chem_scenario") %>% 
  f.duplicateScenarios(scens,scenario_col = "Scenario") %>% 
  group_by(Year,Scenario,chem_scenario,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(comb=paste0(Scenario,"-",chem_scenario)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(Scenario=factor(Scenario,levels=scen_level)) %>%
  ggplot(aes(Year,kton))+
  geom_line(aes(group=comb,col=Scenario,linetype=chem_scenario),
            linewidth=0.5,alpha=.7)+
  facet_wrap(~Mineral,ncol=3,scales = "free_y")+
  labs(linetype="Chemistry \ Scenario",x="",
       # y="Mineral \n Demand \n [ktons]",
       title="Mineral Demand [ktons]",y="",
       col="ICCT Demand \n Scenario")+
  coord_cartesian(expand=F)+
  ylim(0,NA)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
  theme(panel.grid.minor = element_blank())
  # theme(legend.position = "none")
p1
f.fig.save(sprintf(fig_name,"Rangescenarios"),h=6)

# with production rates

p1+
  # geom_hline(data=reserves,aes(yintercept=dep50),col="brown")+
  geom_hline(data=reserves,aes(yintercept=dep20),col="brown")+
  geom_hline(data=reserves,aes(yintercept=prod))+
  geom_line(aes(group=comb,col=Scenario,linetype=chem_scenario),
            linewidth=0.5,alpha=.7)+
  # geom_text(data=reserves,aes(y=label_pos),x=2042,size=8*5/14 * 0.8,
  #           label="2% depletion rate",col="brown")+
  geom_text(data=reserves,aes(y=label_pos),x=2030.5,size=8*5/14 * 0.8,
            label="5% depletion rate",col="brown")+
  geom_text(data=reserves,aes(y=label_prod),x=2042,size=8*5/14 * 0.8,
          label="2022 Production")

f.fig.save(sprintf(fig_name,"Rangescenarios_Prod"),h=6)

## By powertrain -----------

scen="Ambitious"
chem_scen="Baseline"
mine="Lithium"

df %>% 
  filter(Mineral %in% min_interest) %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Powertrain) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest),
         Powertrain=factor(Powertrain,power_level)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Powertrain))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=3,scales = "free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),
       x="",fill="Powertrain"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "E")+
  ggtitle("B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("powertrain_",scen)),h=6)


## Vehicle Type ------

pt="BEV"

df %>% 
  filter(Mineral %in% min_interest) %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  # filter(Powertrain==pt) %>%
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Vehicle=factor(Vehicle,levels=vehicle_level)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=3,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",
       fill="Vehicle"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  ggtitle("A")+
  theme(legend.text = element_text(size=6),
        legend.key.height= unit(0.4, 'cm'),
        legend.key.width= unit(0.4, 'cm'))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("vehicle_",scen)),h=6)

## region ----
df %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  # filter(Mineral==mine) %>%
  mutate(Region=if_else(Region=="World","Rest of the World",Region)) %>% # for stationary, for now, erase latter 
  filter(Mineral %in% min_interest) %>%
  group_by(Year,Region,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=3,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Region"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  ggtitle("C")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("region",scen,sep="_")),h=6)

## Vehicle by region of interest -----

# version 1

regs <- c("United States","European Union","China","India","ASEAN")

df %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  filter(Mineral==mine) %>% 
  filter(Powertrain!="SPS") %>% 
  mutate(Region=if_else(Region %in% regs,Region,"Rest of the World")) %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  mutate(Vehicle=factor(Vehicle,levels=vehicle_level)) %>% 
  group_by(Year,Mineral,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Region)+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Vehicle type",
       caption = paste0(scen," scenario."))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022,2030, 2040, 2050))+
  theme(panel.spacing.x = unit(0.7, "cm"))

f.fig.save(sprintf(fig_name,paste("veh",scen,mine,sep="_")))

# version 2
df %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  filter(Mineral==mine) %>% 
  filter(Powertrain!="SPS") %>% 
  group_by(Year,Mineral,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Vehicle=factor(Vehicle,levels=vehicle_level)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Vehicle,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Region",
       caption = paste0(scen," scenario."))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(0.5, "cm"),
        strip.text = element_text(size=7),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("veh",scen,mine,"2",sep="_")))

# For SPS
df %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
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
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("sps",scen,mine,sep="_")))

## By Chemistry AND MINERAL---------
df %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  # filter(Powertrain==pt) %>% 
  # filter(Mineral==mine) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(Year,chemistry,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(chemistry))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=3,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Chemistry"
       # caption = paste0(scen," scenario.")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  ggtitle("D")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("chemistry",scen,sep="_")),h=6)

# EoF