# Analyze Mineral Demand Results
# Mineral Demand Module - Generate Figures
# PBH August 2023

# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
fig_name <- "Figures/MineralDemand/%s.png"


# load pre-computed results
# df <- read.csv("Results/MineralDemand.csv")
# df <- read.csv("Results/MineralDemandRegion.csv") # faster
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster

df <- df %>% filter(Year<2051)

nrow(df)/1e6

scens <- c("Ambitious","Baseline","Momentum")
chems_scen <- c("Baseline","Double LFP","Double NMC 811",
                "Solid State adoption","Sodium Battery adoption")
capacity_scen <- c("Baseline","Low Range","High Range")
lifetime_scen <- c("Baseline","Long duration")
recycling_scen <- c("Baseline","Enhanced recycling","Enhanced SSPS")


# Combine Scenarios
df <- df %>% mutate(scen_all=paste(Scenario,chem_scenario,
                                   capacity_scenario,
                                   lifetime_scenario,recycling_scenario,sep="-"))
df$scen_all %>% unique()



# summary
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario %in% c("Momentum")) %>%
  filter(chem_scenario %in% c("Baseline")) %>% 
  filter(capacity_scenario=="Baseline") %>%
  filter(lifetime_scenario=="Baseline",recycling_scenario=="Baseline") %>%
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,chem_scenario,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton)

# by scenario
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Year==2050) %>% 
  filter(Mineral %in% min_interest) %>% 
  group_by(scen_all,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = c(Mineral), values_from = kton)

# ratio 2050 to 2022 - ranges
df %>% 
  filter(Year %in% c(2022,2050)) %>% 
  filter(Mineral %in% min_interest3) %>% 
  group_by(scen_all,Year,Mineral) %>% 
  summarise(kton=sum(tons_mineral)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton) %>% 
  mutate(ratio=`2050`/`2022`) %>% dplyr::select(-`2022`,-`2050`) %>% 
  group_by(Mineral) %>% summarise(min_ratio=min(ratio),max_ratio=max(ratio))

# summary by type
df %>% 
  # filter(Vehicle=="Stationary Power Storage") %>%
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario %in% c("Momentum")) %>%
  filter(chem_scenario %in% c("Baseline")) %>%
  filter(capacity_scenario=="Baseline") %>%
  filter(lifetime_scenario=="Baseline",recycling_scenario=="Baseline") %>%
  filter(Mineral %in% min_interest) %>% 
  group_by(Vehicle,Year,Mineral) %>% summarise(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(type=case_when(Vehicle=="Car" ~ "EV",
                        Vehicle=="Stationary Power Storage"~"SPS",
                        T ~ "Other on-road transport")) %>%
  group_by(type,Year,Mineral) %>% summarise(kton=sum(kton)) %>%
  pivot_wider(names_from = Year, values_from = kton)

# cumulative demand
df %>% 
  filter(Scenario %in% c("Momentum")) %>%
  filter(chem_scenario %in% c("Baseline")) %>% 
  filter(capacity_scenario=="Baseline") %>% 
  filter(lifetime_scenario=="Baseline",recycling_scenario=="Baseline") %>%
  filter(Mineral %in% min_interest) %>% 
  group_by(Scenario,Mineral) %>% 
  f.duplicateScenarios("Momentum",scenario_col = "Scenario") %>% 
  summarise(kton=sum(tons_mineral)/1e3)

# by scenario, million tons
df %>% 
  filter(Mineral %in% min_interest2) %>% 
  group_by(scen_all,Mineral) %>% 
  summarise(Mton=sum(tons_mineral)/1e6) %>% 
  pivot_wider(names_from = Mineral, values_from = Mton)

# ranges
df %>% 
  filter(Mineral %in% min_interest3) %>% 
  group_by(scen_all,Mineral) %>% 
  summarise(Mton=sum(tons_mineral)/1e6) %>% ungroup() %>% 
  group_by(Mineral) %>% summarise(min_Mton=min(Mton),max_Mton=max(Mton))

## Recycling --------

# Recycling cumulative demand
df %>% 
  filter(Mineral=="Lithium") %>% 
  filter(Vehicle=="Recycling") %>% 
  group_by(scen_all) %>% 
  reframe(million_tons=-sum(tons_mineral)/1e6)

# Recycling "Capacity" at 2050 
df %>% 
  filter(Vehicle=="Recycling") %>%
  filter(Year==2050) %>% 
  filter(Mineral=="Lithium") %>% 
  group_by(scen_all) %>% 
  reframe(ktons=-sum(tons_mineral)/1e3)

df %>% 
  filter(Mineral=="Lithium") %>% 
  filter(Vehicle=="Recycling") %>% 
  group_by(scen_all,Year) %>% 
  reframe(ktons=-sum(tons_mineral)/1e3) %>% 
  ggplot(aes(Year,ktons,col=scen_all))+geom_line()



# Figures -----

## By Scenario -----------
df %>% 
  filter(Mineral %in% min_interest) %>% 
  filter(capacity_scenario=="Baseline") %>%
  filter(lifetime_scenario=="Baseline",recycling_scenario=="Baseline") %>%
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
  filter(capacity_scenario=="Baseline") %>% 
  group_by(Scenario,chem_scenario,Mineral) %>% reframe(ktons=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  left_join(reserves) %>% 
  mutate(upscale=ktons/prod) %>% 
  dplyr::select(Scenario,chem_scenario,Mineral,upscale) %>% 
  pivot_wider(names_from = Mineral, values_from = upscale)

data_fig <- df %>% 
  filter(Mineral %in% min_interest) %>% 
  # filter(Mineral=="Lithium") %>% 
  # filter(capacity_scenario=="Baseline") %>% 
  # mutate(chem_scenario=paste0(chem_scenario,"-",capacity_scenario)) %>% 
  group_by(Year,scen_all,Scenario,chem_scenario,capacity_scenario,
           lifetime_scenario,recycling_scenario,
           Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(comb=paste0(Scenario,"-",chem_scenario,"-",capacity_scenario)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>%
  mutate(Scenario=factor(Scenario,levels=scen_level)) %>% 
  mutate(capacity_scenario=factor(capacity_scenario,capacity_scen))
p1 <- ggplot(data_fig,aes(Year,kton))+
  geom_line(aes(group=scen_all,col=Scenario,
                linetype=chem_scenario,linewidth=capacity_scenario),alpha=.7)+
  facet_wrap(~Mineral,ncol=3,scales = "free_y")+
  labs(linetype="Chemistry \n Scenario",x="",
       # y="Mineral \n Demand \n [ktons]",
       title="Mineral Demand [ktons]",y="",linewidth="Bat. Size \n Scenario",
       col="ICCT Demand \n Scenario")+
  coord_cartesian(expand=F)+
  ylim(0,NA)+
  scale_linetype_manual(values=c("Baseline"="solid","Double LFP"="dashed",
                                   "Double NMC 811"="dotted","Solid State adoption"="dotdash",
                                 "Sodium Battery adoption"="twodash"))+
  scale_linewidth_manual(values = c("Low Range" = 0.4, "Baseline" = 0.7, "High Range" = 1))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  # geom_label_repel(aes(label=chem_scenario),
  #                  data=filter(data_fig,Year==2050,str_detect(comb,"Sodium")))+ #debug
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
  theme(panel.grid.minor = element_blank(),
        legend.text = element_text(size=12),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"))
  # theme(legend.position = "none")
p1
f.fig.save(sprintf(fig_name,"Rangescenarios"),w=8.7*3)

# with production rates
# reserves <- reserves %>% filter(Mineral=="Lithium")

p1+
  # geom_hline(data=reserves,aes(yintercept=dep50),col="brown")+
  geom_hline(data=reserves,aes(yintercept=dep20),col="brown")+
  geom_hline(data=reserves,aes(yintercept=prod))+
  # geom_line(aes(group=comb,col=Scenario,linetype=chem_scenario),
  #           linewidth=0.5,alpha=.7)+
  # geom_text(data=reserves,aes(y=label_pos),x=2042,size=8*5/14 * 0.8,
  #           label="2% depletion rate",col="brown")+
  geom_text(data=reserves,aes(y=label_pos),x=2027.5,size=8*5/14 * 0.8,
            label="5% depletion rate",col="brown")+
  geom_text(data=reserves,aes(y=label_prod),x=2042,size=8*5/14 * 0.8,
          label="2022 Production")

f.fig.save(sprintf(fig_name,"Rangescenarios_Prod"),w=8.7*3)



## Scenario facet ------

df %>% 
  filter(Mineral=="Lithium") %>% 
  group_by(Year,Scenario,chem_scenario,capacity_scenario,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(comb=paste0(Scenario,chem_scenario,capacity_scenario)) %>% 
  pivot_longer(c(Scenario,chem_scenario,capacity_scenario), 
               names_to = "key", values_to = "value") %>% 
  # mutate(comb=paste0(value,key)) %>% 
  ggplot(aes(Year,kton))+
  geom_line(aes(group=comb,col=value),
            linewidth=1,alpha=.7)+
  facet_wrap(~key)+
  guides(color = guide_legend(override.aes = list(shape = 15)))
# HOW TO DO DIFFERENT LEGENDS by facet??

library(cowplot)

f.fig.backPlot <- function(data.fig,colScenario, mineral="Lithium",title=""){
  colScenario <- enquo(colScenario)
  # get data to group
  data_fig <- data.fig %>% 
      filter(Mineral==mineral) %>% 
      group_by(Year,Scenario,chem_scenario,capacity_scenario,Mineral) %>% 
      reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
      mutate(comb=paste0(Scenario,chem_scenario,capacity_scenario)) %>% 
    mutate(chem_scenario=factor(chem_scenario,levels=chems_scen)) %>%
    mutate(Scenario=factor(Scenario,levels=scen_level)) %>%
    mutate(capacity_scenario=factor(capacity_scenario,levels=capacity_scen))
    
    
  # background data
  data_fig2 <- data_fig %>% dplyr::select(-{{colScenario}})
  # plot
  p <- ggplot(data_fig,aes(Year,kton))+
    geom_line(data=data_fig2,aes(group=comb),col="grey",
                linewidth=1,alpha=.7)+
    geom_line(aes(group=comb,col=!!colScenario),
              linewidth=1,alpha=.7)+  
    facet_wrap(formula(paste("~", colScenario,collapse = " "))) +
      guides(color = guide_legend(override.aes = list(shape = 15)))+
    theme(legend.position = "none")+
    labs(x="",y=paste0(mineral," \n[ktons]"),title = title)
  
  return(p)
}
df$Mineral %>% unique()
mine <- "Lithium"
p1 <- f.fig.backPlot(df,Scenario,mineral = mine,title = "ICCT Demand Scenario")
p2 <- f.fig.backPlot(df,chem_scenario,mineral = mine,title = "Battery Chemistry Scenario")
p3 <- f.fig.backPlot(df,capacity_scenario, mineral = mine,title="Battery Capacity Scenario")

plot_grid(p1,p2,p3,nrow=3)



## By powertrain -----------

scen="Ambitious"
chem_scen="Baseline"
cap_scen <- "Baseline"
life_scen <- "Baseline"
recyc_scen <- "Baseline"
# recyc_scen <- "Enhanced recycling"
mine="Lithium"


df %>% 
  filter(Mineral %in% min_interest2) %>% 
  filter(Scenario %in% c(scen)) %>%
  filter(chem_scenario %in% c(chem_scen)) %>% 
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Powertrain) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest2),
         Powertrain=factor(Powertrain,power_level)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Powertrain))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales = "free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),
       x="",fill="Sector"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "E")+
  # ggtitle("B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("powertrain_",scen)),h=6)


## Vehicle Type ------

pt="BEV"

df %>% 
  filter(Mineral %in% min_interest2) %>% 
  filter(Scenario %in% c(scen,"No Scenario")) %>%
  filter(chem_scenario %in% c(chem_scen,"No Scenario")) %>% 
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
  # filter(Powertrain==pt) %>%
  # filter(Mineral==mine) %>% 
  group_by(Year,Mineral,Vehicle) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  mutate(Vehicle=factor(Vehicle,levels=vehicle_level)) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest2)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",
       fill="Vehicle/Sector"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  # ggtitle("A")+
  theme(legend.text = element_text(size=6),
        legend.key.height= unit(0.4, 'cm'),
        legend.key.width= unit(0.4, 'cm'))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("vehicle_",scen)),h=6)

## region ----
df %>% 
  filter(Scenario %in% c(scen)) %>%
  filter(chem_scenario %in% c(chem_scen)) %>% 
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
  # filter(Mineral==mine) %>%
  mutate(Region=if_else(Region=="World","Rest of the World",Region)) %>% # for stationary, for now, erase latter 
  filter(Mineral %in% min_interest2) %>%
  group_by(Year,Region,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest2)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y=paste0("Mineral \n Demand \n [ktons]"),x="",fill="Region"
       # caption = paste0(scen," scenario")
       )+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  # ggtitle("C")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save(sprintf(fig_name,paste("region",scen,sep="_")),h=6)

## Vehicle by region of interest -----

# version 1

regs <- c("United States","European Union","China","India","ASEAN")

df %>% 
  filter(Scenario %in% c(scen)) %>%
  filter(chem_scenario %in% c(chem_scen)) %>% 
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
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
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
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
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
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
  filter(chem_scenario %in% c(chem_scen)) %>%
  filter(capacity_scenario==cap_scen) %>% 
  filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>%
  # filter(Powertrain==pt) %>% 
  # filter(Mineral==mine) %>% 
  filter(Mineral %in% min_interest2) %>% 
  group_by(Year,chemistry,Mineral) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>%
  mutate(Mineral=factor(Mineral,levels=min_interest2)) %>% 
  ggplot(aes(Year, kton, fill = fct_rev(chemistry))) +
  geom_area() +
  facet_wrap(~Mineral,ncol=2,scales="free_y")+
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


# Demand by scenario ----------


data_fig <- df %>% 
  group_by(scen_all,Scenario,lifetime_scenario,recycling_scenario,Year,Mineral) %>%
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() 

data_fig <- data_fig %>% 
  filter(Mineral=="Lithium")

main_plot <- ggplot(data_fig)+
  geom_line(aes(Year,kton,group=scen_all),col="darkred",alpha=.5,linewidth=.5)+
  coord_cartesian(expand=F)+
  labs(x="",y="",title="Mineral Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
main_plot

main_plot+
  geom_hline(data=filter(reserves,Mineral=="Lithium"),aes(yintercept=dep20),col="brown")+
  geom_hline(data=filter(reserves,Mineral=="Lithium"),aes(yintercept=prod))+
  geom_text(data=filter(reserves,Mineral=="Lithium"),aes(y=label_pos),x=2027.5,size=8*5/14 * 0.8,
            label="5% depletion rate",col="brown")+
  geom_text(data=filter(reserves,Mineral=="Lithium"),aes(y=label_prod),x=2042,size=8*5/14 * 0.8,
            label="2022 Production")

main_plot+
  geom_histogram(aes(y=kton,x=after_stat(density)*2000), 
                 position = position_nudge(x=2050.2),
                 data=filter(data_fig,Year==2050),bins=60,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#A80000",col="white")
f.fig.save(sprintf(fig_name,"LithiumDemandAll"))


# Circularity Index ---------

# Percentage of demand that is met by recycling only
data_fig <- df %>% 
  # filter(Scenario %in% c(scen)) %>%
  # filter(chem_scenario %in% c(chem_scen)) %>%
  # filter(capacity_scenario==cap_scen) %>%
  # filter(lifetime_scenario==life_scen,recycling_scenario==recyc_scen) %>% 
  mutate(group2=if_else(Vehicle=="Recycling","Recycling","Other")) %>% 
  group_by(scen_all,Scenario,lifetime_scenario,recycling_scenario,Year,Mineral,group2) %>%
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() %>% 
  pivot_wider(names_from = group2, values_from = kton) %>% 
  mutate(circularity_index=-Recycling/Other) 


data_fig <- data_fig %>% 
  filter(Mineral=="Lithium")

ggplot(data_fig,aes(Year,circularity_index,
                    col=recycling_scenario,
                    group=scen_all))+
  geom_line(alpha=.5,linewidth=.5)+
  # facet_grid(Scenario~lifetime_scenario)+
  coord_cartesian(expand=F)+
  labs(x="",y="",title="Potential Circularity",col="Recycling \nScenario")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels=scales::percent)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
f.fig.save(sprintf(fig_name,"CircularityIndex"))


# Analyzing of factors ----------
# Analysis of factors contributing to mineral demand
cum_demand <- df %>% 
  group_by(Scenario,chem_scenario,capacity_scenario,lifetime_scenario,recycling_scenario,Mineral) %>% 
  reframe(cum_tons_mineral=sum(tons_mineral)/1e6) %>% ungroup() %>% # million tons
  mutate(Scenario=factor(Scenario,levels=rev(scen_level))) %>% 
  mutate(chem_scenario=factor(chem_scenario,levels=rev(chems_scen))) %>%
  mutate(capacity_scenario=factor(capacity_scenario,capacity_scen))


cum_demand %>% 
  filter(Mineral=="Lithium") %>% 
  # mutate(Scenario=recycling_scenario) %>% 
  # group_by(Mineral) %>% mutate(max_d=max(cum_tons_mineral)) %>% ungroup() %>% 
  ggplot(aes(cum_tons_mineral))+
  geom_histogram(aes(fill=Scenario),binwidth = 2)+
  geom_hline(yintercept = 1:50,col="white")+
  geom_vline(xintercept=9:43,col="white")+ # only for lithium for now
  facet_wrap(~Mineral,scales = "free")+
  labs(y="Count",x="Mineral Cumulative Demand 2022-2050")


# Figure scatter
(nudge_y_fig <- tibble(
  capacity_scenario=capacity_scen,
  nudge=c(0.2,-0.2,0)))

data_fig <- cum_demand %>%
  filter(Mineral=="Lithium") %>%
  # filter(Mineral=="Nickel") %>% 
  # filter(Mineral=="Phosphorus") %>%
  left_join(nudge_y_fig) %>% 
  mutate(cum_tons_mineral=cum_tons_mineral)

ggplot(data_fig,aes(Scenario,chem_scenario,size=cum_tons_mineral,col=capacity_scenario))+
  geom_point(position = position_nudge(y=data_fig$nudge))+
  labs(y="",size="Mineral Cumulative \nDemand 2022-2050")


# Bar plot
ggplot(data_fig,aes(Scenario,cum_tons_mineral,fill=chem_scenario))+
  geom_col(position = "dodge")+
  coord_flip()+
  facet_grid(capacity_scenario~.)+
  ggforce::facet_col(facets = vars(capacity_scenario), 
                     scales = "free_y", 
                     space = "free") +
  guides(fill = guide_legend(reverse=TRUE))+
  labs(x="Demand \nScenario",y="Mineral Cumulative Demand 2022-2050",
       fill="Battery Chemistry \nScenario")

# Regression for Cumulative Demand -------

# get cumulative demand for each scenario
data_reg <- df %>% 
  filter(Mineral=="Lithium") %>% 
  group_by(Scenario,chem_scenario,capacity_scenario,lifetime_scenario,recycling_scenario) %>% 
  reframe(kton=sum(tons_mineral)/1e3) %>% ungroup() 

data_reg %>% arrange(desc(kton)) %>% head(12)

ggplot(data_reg,aes(kton))+stat_ecdf()

## Linear model -----
mod <- lm(kton~Scenario+chem_scenario+capacity_scenario+lifetime_scenario+recycling_scenario,
          data=data_reg)
nobs(mod)
summary(mod)

## Decision Tree --------
library(rpart)
library(rpart.plot)

tree_mod <- rpart(kton ~ Scenario + chem_scenario + capacity_scenario + lifetime_scenario + recycling_scenario,
                  data = data_reg)
summary(tree_mod)
plot(tree_mod)
text(tree_mod)
rpart::plotcp(tree_mod)

rpart.plot(tree_mod, uniform=TRUE, main="Regression Tree")

# Decision tree for binary variable, above 25M
data_reg <- data_reg %>% 
  mutate(bin_kton=kton>25e3)

tree_mod2 <- rpart(bin_kton ~ Scenario + chem_scenario + capacity_scenario + lifetime_scenario + recycling_scenario,
                  data = data_reg)
rpart.plot(tree_mod2, uniform=TRUE, main="Regression Tree")


# EoF