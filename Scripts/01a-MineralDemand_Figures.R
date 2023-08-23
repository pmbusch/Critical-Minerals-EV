# Analyze Mineral Demand Results
# Mineral Demand Module
# PBH August 2023


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# load pre-computed results
df <- read.csv("Results/MineralDemand.csv")


# summary
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario=="Baseline") %>%
  filter(Mineral %in% c("Lithium","Nickel","Cobalt")) %>% 
  group_by(Scenario,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton)

# cumulative demand
df %>% 
  filter(Scenario=="Baseline") %>%
  filter(Mineral %in% c("Lithium","Nickel","Cobalt")) %>% 
  group_by(Scenario,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3)


# Figures -----
fig_name <- "Figures/MineralDemand/%s.png"

## By Scenario -----------
df %>% 
  filter(Mineral %in% c("Lithium","Nickel","Cobalt")) %>% 
  group_by(Year,Scenario,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year,kton,col=Scenario,group=Scenario))+
  geom_line(linewidth=1)+
  facet_wrap(~Mineral,ncol=1,scales = "free_y")+
  labs(y="Mineral \n Demand \n [ktons]",x="")+
  coord_cartesian(expand=F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,"scenarios"))

## By powertrain -----------

scen="Baseline"
mine="Lithium"

df %>% 
  filter(Scenario==scen) %>% 
  filter(Mineral==mine) %>% 
  group_by(Year,Powertrain) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Powertrain))) +
  geom_area() +
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Powertrain",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "E")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("powertrain_",scen,"_",mine)))


## Vehicle Type ------

pt="BEV"

df %>% 
  filter(Scenario==scen) %>% 
  filter(Powertrain==pt) %>% 
  filter(Mineral==mine) %>% 
  group_by(Year,Powertrain,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Powertrain,ncol=1,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Vehicle",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("vehicle_",scen,"_",mine)))

## region ----

df %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  filter(Mineral==mine) %>% 
  group_by(Year,Powertrain,Region) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Powertrain,ncol=1,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Region",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste("region",scen,pt,mine,sep="_")))

## Vehicle by region of interest -----

# version 1

regs <- c("United States","European Union","China","India","ASEAN")

df %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
    filter(Mineral==mine) %>% 
  mutate(Region=if_else(Region %in% regs,Region,"Rest of the World")) %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Powertrain,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%  
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Region)+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Vehicle type",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste("veh",scen,pt,mine,sep="_")))

# version 2
df %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  filter(Mineral==mine) %>% 
  group_by(Year,Powertrain,Region,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Vehicle,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Region",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste("veh",scen,pt,mine,"2",sep="_")))

## By Chemistry AND MINERAL---------

df %>% 
  filter(Scenario==scen) %>% 
  filter(Powertrain==pt) %>% 
  # filter(Mineral==mine) %>% 
  filter(Mineral %in% c("Lithium","Nickel","Cobalt")) %>% 
  group_by(Year,Powertrain,chemistry,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  ggplot(aes(Year, kton, fill = fct_rev(chemistry))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0(mine," \n Mineral \n Demand \n [ktons]"),x="",fill="Chemistry",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste("chemistry",scen,pt,sep="_")))

# EoF