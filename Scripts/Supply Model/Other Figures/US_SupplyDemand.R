# USA Supply and Demand Figures
# PBH May 2025, from previous analysis 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Demand for US by Type -----
(dict_scen <- tibble(Scenario=scens_selected,scen_name=scens_names))
demandRegion <- read.csv("Results/MineralDemand_FewScenarios.csv")
demandUSA <- demandRegion %>% 
  rename(t=Year,Demand=tons_mineral) %>% 
  filter(t<2051) %>% 
  filter(Region=="United States") %>%
  filter(Mineral=="Lithium") %>% 
  dplyr::select(scen_all,t,Powertrain,Vehicle,Demand) %>% 
  rename(Scenario=scen_all) %>% 
  left_join(dict_scen) %>% 
  mutate(Demand=Demand/1e3) # to ktons
demandUSA %>% group_by(scen_name) %>% reframe(x=sum(Demand)/1e3) # million tons

demandUSA <- demandUSA %>% 
  group_by(scen_name,t,Vehicle) %>% 
  reframe(Demand=sum(Demand)) %>% ungroup

# Supply ----
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
deposit <- read.csv("Parameters/Deposit.csv")
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = basename(folder_path)))) %>% 
  rename(Deposit_Name=d)

supplyUSA <- df_results %>%
  filter(t<2051) %>% 
  left_join(deposit,by="Deposit_Name") %>% 
  filter(Country=="United States") %>% 
  left_join(dict_scen) %>% 
  group_by(scen_name,t,Resource_Type) %>% 
  reframe(Supply=sum(tons_extracted1+tons_extracted2+tons_extracted3)) # in ktons
supplyUSA %>% group_by(scen_name) %>% reframe(x=sum(Supply)/1e3) # million tons

# Figures ------------

# join
names(demandUSA);names(supplyUSA)
demandUSA <- demandUSA %>% rename(value=Demand)
supplyUSA <- supplyUSA %>% rename(value=Supply,Vehicle=Resource_Type) %>% mutate(value=-value)


sect <- c("Recycling","Volcano-Sedimentary","Hard Rock","Brine",
          "Other Sectors","SSPS","2-3 Wheelers",
          "Heavy-duty","LIB Replacement\nfor EVs","Car")
data_fig <- rbind(demandUSA,supplyUSA) %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "LIB Replacement\nfor EVs",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    T ~ Vehicle)) %>% 
  group_by(scen_name,t,Sector) %>% 
  reframe(value=-sum(value)) %>% ungroup() %>% # Demand is negative
  mutate(Sector=factor(Sector,levels = sect)) %>% 
  arrange(t,Sector)

## Reference ---------
data_fig_ref <- data_fig %>% 
  filter(str_detect(scen_name,"Reference"))

cols <- viridis::viridis(7)
cols <- c(cols[7],rev(resource_colors),cols[1:6])
names(cols) <- sect

surplus <- data_fig_ref %>% group_by(t) %>% reframe(value=sum(value))

text_d <- tibble(t=c(2032,2032,2042),
                 value=c(250,-200,380),
                 text1=c("Supply","Demand","Surplus"))

ggplot(data_fig_ref,aes(t,value))+
  geom_area(aes(fill=Sector))+
  geom_line(data=surplus,linewidth=0.5)+
  geom_text(data=text_d,aes(label=text1),
            size=10*5/14 * 0.8)+
  geom_hline(yintercept = 0,col="black",linewidth=0.1,linetype="dashed")+
  coord_cartesian(expand = F)+
  scale_y_continuous(breaks = c(-250,0,250,500,750),
                     labels = c(250,0,250,500,750))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_fill_manual(values=cols)+
  labs(x="",y="Li ktons",title="USA Lithium Demand and Supply. Reference scenario.")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.title.y=element_text(angle=0,margin=margin(r=0)))


ggsave("Figures/USA/ts_ref.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*1.5,height=8.7)

## Scenarios - Table -----

data_fig %>% 
  mutate(type=if_else(value<0,"Demand","Supply")) %>% 
  mutate(scen_name=factor(scen_name,levels=scens_names)) %>% 
  group_by(scen_name,type) %>%
  reframe(value=sum(value)/1e3) %>% ungroup() %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(Demand=-Demand) %>% 
  mutate(Surplus=Supply-Demand)
.Last.value %>% write.table("clipboard-16384", sep="\t",row.names = F)

## Scenarios - Time series

data_fig2 <- data_fig %>% 
  mutate(scen_name=factor(scen_name,levels=scens_names)) %>% 
  group_by(scen_name,t) %>% 
  reframe(value=sum(value)) %>% ungroup()

ggplot(data_fig2,aes(t,value,col=scen_name))+
  geom_line(alpha=.7,linewidth=.5)+
  geom_line(data=filter(data_fig2,str_detect(scen_name,"Reference")),
            alpha=.8,linewidth=.8)+
  geom_hline(yintercept = 0,col="black",linewidth=0.1,linetype="dashed")+
  coord_cartesian(expand = F)+
  scale_y_continuous(breaks = seq(-150,600,150),
                     limits = c(-150,700))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_color_manual(values=scen_colors)+
  labs(x="",y="Li ktons",title="Lithium Surplus or Deficit by Demand Scenario",
       col="Scenario")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.25,0.75),
        legend.text = element_text(size=7),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        axis.title.y=element_text(angle=0,margin=margin(r=0)))

ggsave("Figures/USA/ts_scen.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*1.5,height=8.7)



# EoF