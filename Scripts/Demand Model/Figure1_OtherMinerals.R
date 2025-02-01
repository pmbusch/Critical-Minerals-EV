# Fig. 1 Demand Results
# ALL MINERALS, not just lithium
# PBH June 2024
# Fig widths note: 1 column=5.7cm, 2 col=12.1cm or 3 col=18.4cm


source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# load and prepare data -----
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster


# Combine Scenarios
df <- df %>% mutate(Scenario=paste(Scenario,
                                   chem_scenario,
                                   capacity_scenario,
                                   lifetime_scenario,
                                   recycling_scenario,sep="-"))
df$Scenario %>% unique()



unique(df$Vehicle)
df_sector <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Vehicle,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

unique(df$Powertrain)
df_sector2 <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Vehicle,Powertrain,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))


df_region <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Region,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))


## Aggregate at world level - Primary mineral demand 
df <- df %>% rename(t=Year) %>% 
  group_by(Mineral,t,Scenario) %>% 
  reframe(Demand=sum(tons_mineral))

df$Demand %>% range()

# significant digits
df <- df %>% mutate(Demand=Demand/1e3) # in ktons

df$t %>% range()


# FIGURES --------------
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

# 2050 demand
df %>% 
  filter(t==2050) %>% 
  group_by(Mineral,Scenario) %>% 
  reframe(x=sum(Demand)) %>%  # in ktons
  ungroup() %>% group_by(Mineral) %>% 
  reframe(min=min(x),max=max(x))

# upscale with respect to 2022
df %>% 
  filter(t %in% c(2022,2050)) %>% 
  group_by(Mineral,Scenario,t) %>%
  reframe(x=sum(Demand)) %>%  # in ktons
  pivot_wider(names_from = t, values_from = x) %>% 
  mutate(ratio=`2050`/`2022`) %>% 
  ungroup() %>% group_by(Mineral) %>% 
  reframe(min=min(ratio),max=max(ratio))


# cumulative demand
df %>% 
  filter(t<2051) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  group_by(Mineral,name) %>% 
  reframe(x=sum(Demand)/1e3) %>% 
  pivot_wider(names_from = Mineral, values_from = x)
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)

  
# Change with respect to reference scenario
# cumulative demand
df %>% 
  filter(t<2051) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  group_by(Mineral,name) %>% 
  reframe(x=sum(Demand)) %>% 
  mutate(name=str_remove_all(name,"\\(\\d{1,2}\\) ")) %>% 
  pivot_wider(names_from = name, values_from = x) %>% 
  pivot_longer(c(-Mineral,-Reference), 
               names_to = "name", values_to = "x") %>% 
  mutate(rel_change=(x-Reference)/Reference) %>% 
  dplyr::select(-Reference,-x) %>% 
  pivot_wider(names_from = Mineral, values_from = rel_change)
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)
  
# peak demand
df %>% 
  filter(t<2051) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  group_by(Mineral,name) %>% 
  reframe(x=max(Demand)) %>%  # change to max
  mutate(name=str_remove_all(name,"\\(\\d{1,2}\\) ")) %>% 
  pivot_wider(names_from = name, values_from = x) %>% 
  pivot_longer(c(-Mineral,-Reference), 
               names_to = "name", values_to = "x") %>% 
  mutate(rel_change=(x-Reference)/Reference) %>% 
  dplyr::select(-Reference,-x) %>% 
  pivot_wider(names_from = Mineral, values_from = rel_change)
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)




## Lithium ------------
demand <- df %>% filter(Mineral=="Lithium")
demandSector <- df_sector %>% filter(Mineral=="Lithium")
demandRegion <- df_region %>% filter(Mineral=="Lithium")

data_fig1 <- demand %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(scen_num=substr(name,1,3))

cum_demand <- data_fig1 %>% group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))

p1 <- ggplot(data_fig1)+
  geom_line(aes(t,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  # highlight ref scenario
  geom_line(data=filter(data_fig1,str_detect(name,"Refere")),
            aes(t,kton,group=name,col=name),alpha=.7,linewidth=.8)+
  geom_text(data=filter(data_fig1,t==2050),show.legend = F,
            aes(label=scen_num,y=kton,col=name),x=2050.5,
            size=6*5/14 * 0.8,
            # order: 1,8,9,2,3,5,4,7,6
            nudge_y = c(0,1.7,0,-0.5,0,-1,1,-1.2,0)*5e1)+
  coord_cartesian(expand=F,xlim=c(2022,2051))+
  labs(x="",y="",col="Demand Scenario",
       title="(A) Lithium Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_color_manual(values = scen_colors)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        axis.text.x = element_text(hjust = 1,size=9),
        axis.text.y = element_text(size=9),
        # legend.position = "none",
        legend.position = c(0.18,0.69),
        legend.text = element_text(size=5.5),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p1

# cumulative demand
unique(demandSector$Vehicle)
data_fig1a <- demandSector %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "LIB Replacement\nfor EVs",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    T ~ Vehicle) %>% 
      factor(levels = c("Other Sectors","SSPS","2-3 Wheelers",
                        "Heavy-duty","LIB Replacement\nfor EVs","Car",'Recycling'))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Sector) %>% 
  reframe(Demand=sum(Demand)/1e6) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=substr(name,1,3))

p1a <- ggplot(data_fig1a,aes(name,Demand,fill=Sector))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_viridis_d(option = 7)+
  labs(x="Scenario",y="",title = "(B) Li Cumulative Demand [Mtons]",
       fill="")+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=8.5),
        legend.text = element_text(size=6))
p1a


cowplot::plot_grid(p1,p1a,ncol=2,rel_widths = c(0.63,0.37))


# Cumulative demand by region
data_fig <- demandRegion %>% 
  mutate(Region=case_when(
    Region %in% c("China") ~ "China",
    Region %in% c("Australia/NZ","ASEAN",
                  "Other Asia Pacific","Japan",
                  "South Korea") ~ "Asia Pacific/Oceania",
    Region %in% c("Middle East","India","Africa") ~"Middle East/Africa",
    Region %in% c("Brazil","Other Latin America and Caribbean") ~ "Latin America",
    Region %in% c("United States","Mexico","Canada") ~ "North America",
    Region %in% c("EFTA","European Union","Other Europe",
                  "United Kingdom") ~ "Europe") %>% 
      factor(levels=c("Middle East/Africa","Asia Pacific/Oceania","China",
                      "Latin America","North America","Europe"))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Region) %>%
  reframe(Demand=sum(Demand)/1e6) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


p1_r <- ggplot(data_fig,aes(name,Demand,fill=Region))+
  geom_col(col="black",linewidth=0.1)+
  coord_flip(ylim=c(0,38),expand = F)+
  guides(fill= guide_legend(reverse = T,byrow=T))+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values=c("Middle East/Africa"="#D16D6F","Asia Pacific/Oceania"="#cab2d6",
                             "China"="#ff0000","Latin America"="#d95f02",
                             "North America"="#1f78b4","Europe"="#a6cee3"))+
  labs(x="",y="",title = "Cumulative Lithium Demand [2022-2050] [Mtons]",fill="Region")+
  theme_bw(8)+ 
  theme(legend.text = element_text(size=6),
        axis.text.y = element_text(hjust=0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
p1_r
ggsave("Figures/MineralDemand/LiDemandRegion.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12,height=8.7)

## Nickel ------------
demand <- df %>% filter(Mineral=="Nickel")
# detail of stainless steel
demandSector <- df_sector2 %>% filter(Mineral=="Nickel")
demandRegion <- df_region %>% filter(Mineral=="Nickel")

data_fig1 <- demand %>% 
  filter(t<2051) %>%
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(scen_num=substr(name,1,3))

cum_demand <- data_fig1 %>% group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))

p2 <- ggplot(data_fig1)+
  geom_line(aes(t,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  # highlight ref scenario
  geom_line(data=filter(data_fig1,str_detect(name,"Refere")),
            aes(t,kton,group=name,col=name),alpha=.7,linewidth=.8)+
  geom_text(data=filter(data_fig1,t==2050),show.legend = F,
            aes(label=scen_num,y=kton,col=name),x=2050.5,
            size=6*5/14 * 0.8,
            # order: 1,8,9,2,3,5,4,7,6
            nudge_y = c(0,2,0,-1,0,-1,1,2,-1)*100)+
  coord_cartesian(expand=F,xlim=c(2022,2051))+
  labs(x="",y="",col="Demand Scenario",
       title="(C) Nickel Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_color_manual(values = scen_colors)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        axis.text.x = element_text(hjust = 1,size=9),
        axis.text.y = element_text(size=9),
        legend.position = "none",
        # legend.position = c(0.18,0.69),
        legend.text = element_text(size=5.5),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p2

# cumulative demand
demandSector %>% group_by(Vehicle,Powertrain) %>% tally()
data_fig1a <- demandSector %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "LIB Replacement\nfor EVs",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    Vehicle=="Other Sectors" & Powertrain=="Stainless Steel" ~ "Stainless Steel",
    T ~ Vehicle) %>% 
      factor(levels = c("Stainless Steel","Other Sectors","SSPS","2-3 Wheelers",
                        "Heavy-duty","LIB Replacement\nfor EVs","Car",'Recycling'))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Sector) %>% 
  reframe(Demand=sum(Demand)/1e6) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=substr(name,1,3))

p2a <- ggplot(data_fig1a,aes(name,Demand,fill=Sector))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_viridis_d(option = 7)+
  labs(x="Scenario",y="",title = "(D) Ni Cumulative Demand [Mtons]",
       fill="")+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=8.5),
        legend.text = element_text(size=6))
p2a


# Cumulative demand by region
data_fig <- demandRegion %>% 
  mutate(Region=case_when(
    Region %in% c("China") ~ "China",
    Region %in% c("Australia/NZ","ASEAN",
                  "Other Asia Pacific","Japan",
                  "South Korea") ~ "Asia Pacific/Oceania",
    Region %in% c("Middle East","India","Africa") ~"Middle East/Africa",
    Region %in% c("Brazil","Other Latin America and Caribbean") ~ "Latin America",
    Region %in% c("United States","Mexico","Canada") ~ "North America",
    Region %in% c("EFTA","European Union","Other Europe",
                  "United Kingdom") ~ "Europe") %>% 
      factor(levels=c("Middle East/Africa","Asia Pacific/Oceania","China",
                      "Latin America","North America","Europe"))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Region) %>%
  reframe(Demand=sum(Demand)/1e6) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


p2_r <- ggplot(data_fig,aes(name,Demand,fill=Region))+
  geom_col(col="black",linewidth=0.1)+
  coord_flip(ylim=c(0,220),expand = F)+
  guides(fill= guide_legend(reverse = T,byrow=T))+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values=c("Middle East/Africa"="#D16D6F","Asia Pacific/Oceania"="#cab2d6",
                             "China"="#ff0000","Latin America"="#d95f02",
                             "North America"="#1f78b4","Europe"="#a6cee3"))+
  labs(x="",y="",title = "Cumulative Nickel Demand [2022-2050] [Mtons]",fill="Region")+
  theme_bw(8)+ 
  theme(legend.text = element_text(size=6),
        axis.text.y = element_text(hjust=0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
p2_r
ggsave("Figures/MineralDemand/NiDemandRegion.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12,height=8.7)

## Cobalt ------------
demand <- df %>% filter(Mineral=="Cobalt")
demandSector <- df_sector %>% filter(Mineral=="Cobalt")
demandRegion <- df_region %>% filter(Mineral=="Cobalt")

data_fig1 <- demand %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(scen_num=substr(name,1,3))

cum_demand <- data_fig1 %>% group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))

p3 <- ggplot(data_fig1)+
  geom_line(aes(t,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  # highlight ref scenario
  geom_line(data=filter(data_fig1,str_detect(name,"Refere")),
            aes(t,kton,group=name,col=name),alpha=.7,linewidth=.8)+
  geom_text(data=filter(data_fig1,t==2050),show.legend = F,
            aes(label=scen_num,y=kton,col=name),x=2050.5,
            size=6*5/14 * 0.8,
            # order: 1,8,9,2,3,5,4,7,6
            nudge_y = c(-0.5,0.3,0,-0.5,0,0,0,0.5,0)*3e1)+
  coord_cartesian(expand=F,xlim=c(2022,2051))+
  labs(x="",y="",col="Demand Scenario",
       title="(E) Cobalt Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_color_manual(values = scen_colors)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        axis.text.x = element_text(hjust = 1,size=9),
        axis.text.y = element_text(size=9),
        legend.position = "none",
        # legend.position = c(0.18,0.69),
        legend.text = element_text(size=5.5),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p3

# cumulative demand
demandSector %>% group_by(Vehicle) %>% tally()
data_fig1a <- demandSector %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "LIB Replacement\nfor EVs",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    T ~ Vehicle) %>% 
      factor(levels = c("Stainless Steel","Other Sectors","SSPS","2-3 Wheelers",
                        "Heavy-duty","LIB Replacement\nfor EVs","Car",'Recycling'))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Sector) %>% 
  reframe(Demand=sum(Demand)/1e6) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=substr(name,1,3))

p3a <- ggplot(data_fig1a,aes(name,Demand,fill=Sector))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_viridis_d(option = 7)+
  labs(x="Scenario",y="",title = "(F) Co Cumulative Demand [Mtons]",
       fill="")+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=8.5),
        legend.text = element_text(size=6))
p3a


# Cumulative demand by region
data_fig <- demandRegion %>% 
  mutate(Region=case_when(
    Region %in% c("China") ~ "China",
    Region %in% c("Australia/NZ","ASEAN",
                  "Other Asia Pacific","Japan",
                  "South Korea") ~ "Asia Pacific/Oceania",
    Region %in% c("Middle East","India","Africa") ~"Middle East/Africa",
    Region %in% c("Brazil","Other Latin America and Caribbean") ~ "Latin America",
    Region %in% c("United States","Mexico","Canada") ~ "North America",
    Region %in% c("EFTA","European Union","Other Europe",
                  "United Kingdom") ~ "Europe") %>% 
      factor(levels=c("Middle East/Africa","Asia Pacific/Oceania","China",
                      "Latin America","North America","Europe"))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Region) %>%
  reframe(Demand=sum(Demand)/1e6) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


p3_r <- ggplot(data_fig,aes(name,Demand,fill=Region))+
  geom_col(col="black",linewidth=0.1)+
  coord_flip(ylim=c(0,22),expand = F)+
  guides(fill= guide_legend(reverse = T,byrow=T))+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values=c("Middle East/Africa"="#D16D6F","Asia Pacific/Oceania"="#cab2d6",
                             "China"="#ff0000","Latin America"="#d95f02",
                             "North America"="#1f78b4","Europe"="#a6cee3"))+
  labs(x="",y="",title = "Cumulative Cobalt Demand [2022-2050] [Mtons]",fill="Region")+
  theme_bw(8)+ 
  theme(legend.text = element_text(size=6),
        axis.text.y = element_text(hjust=0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
p3_r
ggsave("Figures/MineralDemand/CoDemandRegion.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12,height=8.7)

## Join all ------

cowplot::plot_grid(
  cowplot::plot_grid(p1,p1a,ncol=2,rel_widths = c(0.63,0.37)),
  cowplot::plot_grid(p2,p2a,ncol=2,rel_widths = c(0.63,0.37)),
  cowplot::plot_grid(p3,p3a,ncol=2,rel_widths = c(0.63,0.37)),
  nrow = 3)

ggsave("Figures/Article/Fig1_AllMinerals.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=6.4*3)


# EoF