# Analysis Reserves & Resources by Country
# March 2024 PBH

# Load Data ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Deposit data
deposit <- read.csv("Parameters/Deposit.csv")

# Cumulative demand
df_demand <- read.csv("Results/MineralCumulativeDemand.csv")

url_fig <- "Figures/Deposit/%s.png"


# By Country -----
df_country <- deposit %>% 
  mutate(Resource_Type=Resource_Type_orig) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(reserve=sum(reserve)/1e3,
         resource_demonstrated=sum(resource_demostrated)/1e3,
         resource_inferred=sum(resource_inferred)/1e3) %>% 
  mutate(resource_all=reserve+resource_demonstrated+resource_inferred) %>% 
  arrange(desc(resource_all)) %>% ungroup()
df_country

# By Type ----------
df_country %>% dplyr::select(-Country,-resource_all) %>% 
  pivot_longer(c(-Resource_Type), names_to = "key", values_to = "value") %>% 
  group_by(Resource_Type,key) %>% reframe(value=sum(value)) %>% ungroup() %>% 
  mutate(key=key %>% str_replace("_"," ") %>% str_to_title()) %>% 
  mutate(key=factor(key,levels=c("Resource Inferred","Resource Demonstrated","Reserve"))) %>%
  group_by(Resource_Type) %>% mutate(value_sum=sum(value)) %>% ungroup() %>% 
  ggplot(aes(reorder(Resource_Type,value_sum),value,fill=key))+
  geom_col()+
  geom_hline(yintercept = seq(10,60,10),col="white")+
  coord_flip(expand = F,ylim = c(NA,65))+
  scale_fill_manual(values =  c("#6b6b6b", "#ff7f0e", "#1f77b4"))+
  labs(x="",y="Lithium Million tons",fill="")+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "top")
  
f.fig.save(sprintf(url_fig,"Resource_type"))


# Mosaic Figure --------
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
# create stats for display
data_fig <- df_country %>% 
  # mutate(resource_all=reserve) %>%  # for Reserve analysis
  filter(resource_all>0) %>% 
  mutate(Resource_Type=if_else(str_detect(Resource_Type,"Other"),"Other",Resource_Type)) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(resource_all=sum(resource_all)) %>% ungroup() %>% 
  group_by(Resource_Type) %>%
  mutate(total_resources=sum(resource_all),
         share_resource=resource_all/sum(resource_all)) %>% 
  mutate(r_label=if_else(resource_all>0.5,
                         paste0(Country,": ",format(round(resource_all,1),big.mark=","),"M"),"")) %>% 
  mutate(r_label=if_else(resource_all<1 & Resource_Type=="Other","",r_label)) %>% 
  ungroup() %>% mutate(share_type=total_resources/sum(resource_all)) %>% 
  mutate(Resource_Type=paste0(Resource_Type,"\n(",round(share_type*100,0),"%)"))
  

r_order <- data_fig %>% group_by(Resource_Type) %>% summarise(p=sum(resource_all)) %>%
  arrange(desc(p)) %>% pull(Resource_Type)
data_fig <- data_fig %>% mutate(Resource_Type=factor(Resource_Type,levels=r_order))
c_order <- data_fig %>% group_by(r_label) %>% summarise(p=sum(resource_all)) %>%
  mutate(p=if_else(r_label=="",0,p)) %>% # no label at end
  arrange(desc(p)) %>% pull(r_label)
data_fig <- data_fig %>% mutate(r_label=factor(r_label,levels=c_order))

sum(data_fig$resource_all)

ggplot(data_fig,aes(x = Resource_Type, y = share_resource, 
           width = total_resources,group=r_label,
           fill = Resource_Type)) +
  geom_bar(stat = "identity", position = "fill", colour = "black")+
  geom_text(data=filter(data_fig,!str_detect(Resource_Type,"Other")),
            aes(label = r_label), position = position_stack(vjust = 0.5),size=11*5/14 * 0.8) + 
  geom_text(data=filter(data_fig,str_detect(Resource_Type,"Other")),
            aes(label = r_label), position = position_stack(vjust = 0.5),angle=90,size=11*5/14 * 0.8) + 
  facet_grid(~Resource_Type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values=c(unname(resource_colors),"darkgrey"))+
  theme_void(14)+
  theme(legend.position="none",
        panel.spacing.x = unit(0, "npc")) # if no spacing preferred between bars
f.fig.save(sprintf(url_fig,"Mosaic_Resource"),h=20,w=28)
# f.fig.save(sprintf(url_fig,"Mosaic_Reserve"),h=20,w=28)

# Curve Function -------

# Need to have an indexCol to do ordering
f.curve <- function(data_curve,type="Reserves",index_title=""){
  
  data_curve <-  data_curve %>% 
    filter(res>0) %>% 
    arrange(desc(indexCol)) %>% 
    mutate(reserve_cum_end=cumsum(res),
           reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
    mutate(lab_dep=if_else(res>200,Country,"")) %>% 
    mutate(lab_pos=reserve_cum_start+res/2) %>% 
    mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling
  
  
  # duplicate last row
  last_row <- data_curve[nrow(data_curve),]
  last_row$reserve_cum_start <- last_row$reserve_cum_end
  last_row$lab_dep <- ""
  data_curve <- rbind(data_curve,last_row)
  even_row <- data_curve$even_row
  
  # Limits
  lim_x <- ceiling(max(data_curve$reserve_cum_end)/500)*500 # upper by 500
  
  p1 <- ggplot(data_curve,aes(reserve_cum_start,indexCol,group=1))+
    geom_step(linewidth=0.75,direction = "hv")+
    geom_text_repel(aes(x=lab_pos,label=lab_dep),col="brown",nudge_y = 0.1*even_row,
                    size=6*5/14 * 0.8)+
    labs(x=paste0("Cumulative ",type," [ktons Li]"),
         y=index_title)+
    coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,1))+
    scale_y_continuous(labels = scales::comma_format(big.mark = ' '))+
    scale_x_continuous(labels = scales::comma_format(big.mark = ' '))
  
  return(p1)
  
}

# Human Rights Index ----
# Source: https://ourworldindata.org/human-rights
# V-Dem (2023) – with major processing by Our World in Data

hr <- read.csv("Data/Indices/human-rights-index-vdem.csv")
hr <- hr %>% filter(Year==2022) %>% 
  rename(Country=Entity,humanRight_index=civ_libs_vdem_owid) %>% 
  mutate(Country=if_else(Country=="Democratic Republic of Congo","DR Congo",Country),
         Country=if_else(Country=="Czechia","Czech Republic",Country))

## Curve
data_fig <- df_country %>% 
  mutate(res=resource_all*1e3) %>%
  # mutate(res=reserve*1e3) %>% 
  group_by(Country) %>% reframe(res=sum(res)) %>% ungroup() %>% 
  left_join(hr) %>% 
  rename(indexCol=humanRight_index)

p1 <- f.curve(data_fig,
              type = "Resources",
              index_title = "Human Rights \nIndex [0-1]")
p1

# f.fig.save(sprintf(url_fig,"Curve_HumanRight_reserve"),w = 18)
f.fig.save(sprintf(url_fig,"Curve_HumanRight_resource"),w = 18)


p1+geom_vline(data=filter(df_demand,Mineral=="Lithium"),
              aes(xintercept=ktons_cumDemand),col="darkgrey")+
  geom_text(data=filter(df_demand,Mineral=="Lithium"),col="darkgrey",size=6*5/14 * 0.8,
            aes(label=name,x=ktons_cumDemand),y=0.2,nudge_x = 150*3,angle=90)+
  labs(caption="Vertical lines show cumulative mineral demand.")

# f.fig.save(sprintf(url_fig,"Curve_HumanRight_reserve_demand"),w = 18)
f.fig.save(sprintf(url_fig,"Curve_HumanRight_resource_demand"),w = 18)

## Ease of Doing Business ----------
url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Data/Mine Characterization/%s"

edb <- read_excel(sprintf(url_file,"Yunzhu_Policy Datasets/Workingdataset.xlsx"),
                          sheet="Easy of Doing Business",range="A7:B219")
names(edb) <- c("Country","edb")

edb <- edb %>% 
  mutate(Country=if_else(Country=="Congo, Dem. Rep.","DR Congo",Country))

## Curve
data_fig <- df_country %>% 
  mutate(res=resource_all*1e3) %>%
  # mutate(res=reserve*1e3) %>%
  group_by(Country) %>% reframe(res=sum(res)) %>% ungroup() %>% 
  left_join(edb) %>% 
  mutate(indexCol=edb/100)

p1 <- f.curve(data_fig,
              type = "Resources",
              index_title = "Ease of Doing \nBussiness \n[0-1]")
p1

# f.fig.save(sprintf(url_fig,"Curve_EaseBusiness_reserve"),w = 18)
f.fig.save(sprintf(url_fig,"Curve_EaseBusiness_resource"),w = 18)

p1+geom_vline(data=filter(df_demand,Mineral=="Lithium"),
              aes(xintercept=ktons_cumDemand),col="darkgrey")+
  geom_text(data=filter(df_demand,Mineral=="Lithium"),col="darkgrey",size=6*5/14 * 0.8,
            aes(label=name,x=ktons_cumDemand),y=0.2,nudge_x = 150*3,angle=90)+
  labs(caption="Vertical lines show cumulative mineral demand.")

# f.fig.save(sprintf(url_fig,"Curve_EaseBusiness_reserve_demand"),w = 18)
f.fig.save(sprintf(url_fig,"Curve_EaseBusiness_resource_demand"),w = 18)

## USGS vs Li Database -----

usgs_country <- read_excel("Data/USGS2024.xlsx",sheet="Lithium") %>% 
  mutate(Reserves=Reserves/1e6,Resources_Demonstrated=Resources_Demonstrated/1e6)

deposit_country <- deposit %>% 
  mutate(Country=if_else(Country %in% usgs_country$Country,Country,"Other countries")) %>%
  group_by(Country) %>% 
  reframe(reserve=sum(reserve)/1e3,
          resource_demostrated=sum(resource_demostrated+resource_inferred)/1e3) %>% ungroup()

usgs_reserve <- usgs_country %>% filter(Reserves>0) %>% pull(Country)

# Comparison
deposit_country <- deposit_country %>% left_join(usgs_country)
names(deposit_country) <- c("Country","Reserve","Resource","USGS_Reserve","USGS_Resource")
deposit_country %>% 
  mutate(Country=factor(Country,levels=usgs_country$Country)) %>% 
  pivot_longer(c(-Country), names_to = "key", values_to = "value") %>% 
  mutate(type=str_extract(key,"Reserve|Resource")) %>% 
  mutate(Source=if_else(type=="Resource","Demonstrated Resource",type)) %>% 
  mutate(Source=if_else(str_detect(key,"USGS"),"USGS","Database"),key=NULL) %>% 
  mutate(label_text=paste0(round(value,1),""),lab_pos=value*1.1+0.5) %>% 
  # filter(type=="Reserve",Country %in% usgs_reserve) %>% 
  filter(type=="Resource") %>% 
  ggplot(aes(Country,value,fill=Source))+
  geom_col(position="dodge")+
  geom_text(aes(y=lab_pos,label=label_text), size=8*5/14 * 0.8,
            position = position_dodge(width = 1))+
  facet_wrap(~type,scales = "free_x")+
  coord_flip()+
  scale_x_discrete(limits=rev)+
  guides(fill= guide_legend(reverse = TRUE))+
  labs(x="",y="Lithium Million tons")

# f.fig.save(sprintf(url_fig,"USGS_comparison"))
f.fig.save(sprintf(url_fig,"USGS_comparison_resource"))



# Others -------

deposit %>% 
  dplyr::select(cost1,cost2,cost3,Resource_Type) %>% 
  pivot_longer(c(-Resource_Type), names_to = "key", values_to = "value") %>% 
  # filter(key=="cost1") %>% 
  ggplot(aes(value,fill=Resource_Type))+geom_density(alpha=.5)+
  facet_wrap(~key,scales = "free_y",ncol=1)

deposit %>% 
  dplyr::select(cost1,cost2,cost3,Resource_Type) %>% 
  group_by(Resource_Type) %>% 
  skimr::skim_without_charts()

