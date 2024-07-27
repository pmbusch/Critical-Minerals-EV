# Fig. 2 Demand Results and Supply Dispatch curve
# PBH June 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")

demand <- read.csv("Parameters/Demand.csv")
demandSector <- read.csv("Parameters/Demand_Detail.csv")
deposit <- read.csv("Parameters/Deposit.csv")
demandRegion <- read.csv("Parameters/Demand_Region.csv")


theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

# Demand -----
data_fig1 <- demand %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) 

cum_demand <- data_fig1 %>% group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))

p0 <- cum_demand %>%
  mutate(name=factor(name,levels=cum_demand$name)) %>%
  ggplot(aes(name,Mton,fill=name))+geom_col()+
  coord_flip(expand = F)+labs(x="",y="Cumulative Demand [million tons Li metal")+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = scen_colors)+
  theme(legend.position = "none")
p0

# data_fig1 <- data_fig1 %>% 
#   mutate(name=factor(name,levels=cum_demand$name))

p1 <- ggplot(data_fig1)+
  geom_line(aes(t,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  coord_cartesian(expand=F)+
  labs(x="",y="",col="Demand Scenario",
       title="(A) Lithium Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_color_manual(values = scen_colors)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        axis.text.x = element_text(hjust = 1),
        # legend.position = "none",
        legend.position = c(0.14,0.67),
        legend.text = element_text(size=5.2),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p1
# cowplot::plot_grid(p1, p0, rel_widths = c(0.7, 0.3))

# cumulative demand
unique(demandSector$Vehicle)
data_fig1a <- demandSector %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "Add. LIB",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    T ~ Vehicle) %>% 
      factor(levels = c("Other Sectors","SSPS","2-3 Wheelers",
                        "Heavy-duty","Add. LIB","Car",'Recycling'))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Sector) %>% 
  reframe(Demand=sum(Demand)/1e6) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=substr(name,0,3))

p1a <- ggplot(data_fig1a,aes(name,Demand,fill=Sector))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_viridis_d(option = 7)+
  labs(x="Scenario",y="",title = "(B) Cumulative Demand [Mtons]",
       fill="")+
  theme(legend.text = element_text(size=6))
p1a


# save as panel

cowplot::plot_grid(p1,p1a,ncol=2,rel_widths = c(0.65,0.35))

# Save with widht size of letter
ggsave("Figures/Article/Fig1.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=6)


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


ggplot(data_fig,aes(name,Demand,fill=Region))+
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

ggsave("Figures/Article/FigDemandRegion.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12,height=8.7)


# Supply Curve ---------

# weighted cost avg
data_fig <- deposit %>% 
  dplyr::select(Deposit_Name,Resource_Type,Status,
                reserve,resource_demostrated,resource_inferred,
                cost1,cost2,cost3) %>% 
  mutate(cost=(cost1*reserve+cost2*resource_demostrated+cost3*resource_inferred)/
           (reserve+resource_demostrated+resource_inferred),
         li_size=(reserve+resource_demostrated+resource_inferred)/1e3) %>% 
  mutate(cost=cost/5.323) %>%  # to USD per ton LCE
  filter(li_size>0) %>% 
  filter(cost>0) %>% 
  arrange(cost) %>% 
  mutate(reserve_cum_end=cumsum(li_size),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(li_size>1.1,Deposit_Name,"")) %>% 
  mutate(lab_pos=reserve_cum_start+li_size/2) %>% 
  mutate(Status=case_when(
    Status %in% c("Producing","Construction","Suspended",
                  "Producing & suspended","Restarting") ~ "Open or under Construction",
    Status %in% c("Permitting","Design","Feasibility","Approved permits",
                  "Pilot plant","Economic assessment") ~ "Feasibility or Permitting",
    T ~ "Exploration") %>% 
      factor(levels=c("Open or under Construction","Feasibility or Permitting","Exploration"))) %>%
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
nrow(data_fig) #160
last_row <- data_fig[nrow(data_fig),]
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/0.5)*0.5 # upper by 500
max(data_fig$cost)
lim_y <- ceiling(max(data_fig$cost)/500)*500

even_row <- data_fig$even_row

p2 <- ggplot(data_fig,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv",
            aes(col=Resource_Type))+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 1000*even_row,
                  size=7*5/14 * 0.8)+
  labs(x="Cumulative Resources [ktons Li]",y="USD/ton LCE",
       title="(B) Lithium Cumulative Availability Curve",
       col="Resource type",alpha="Stage")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(NA,lim_y))+
  scale_color_manual(values=resource_colors)+
  # scale_alpha_manual(values = c("Feasibility or Permitting" = 0.6, 
                                # "Exploration"=0.3,
                                # "Open or under Construction" = 1)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.9,0.2),
        legend.box = "horizontal",
        axis.text.x = element_text(hjust = 1),
        axis.title.y=element_text(angle=0,margin=margin(r = -80,l=30),vjust = 0.95),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=6))
p2

# with cumulative demand
# p2+geom_vline(data=cum_demand,aes(xintercept=Mton))+
#   geom_text(data=cum_demand,aes(x=Mton,y=15000,label=name),angle=90,
#             nudge_x = c(1,1,1,-1,1,-1,-1)*0.5)

# For loop to be able to use a second color scale
p2a <- p2
nudges_aux <- c(1.1,1,1,1.2,1.3,1,-1,1,-1)
adj_y <- c(0,0,0,400,800,0,0,1200,0)
for (i in 1:length(scens_names)){
  data_aux <- cum_demand %>% filter(name==scens_names[i])
  p2a <- p2a+
    geom_vline(data=data_aux,aes(xintercept=Mton),col=scen_colors[i],alpha=.5)+
    geom_text(data=data_aux,aes(x=Mton),y=14000-adj_y[i],
              label=paste0("(",i,")"),
              angle=0,size=6*5/14 * 0.8,
              col=scen_colors[i],nudge_x =nudges_aux[i]*1)
}
p2a

# Save with widht size of letter
ggsave("Figures/Article/Fig2b.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=6)


# EoF