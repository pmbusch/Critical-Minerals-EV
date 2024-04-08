# Analysis Reserves & Resources by Country
# March 2024 PBH

# Load Data ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Data/Mine Characterization/%s"

# Collected Lithium data
df <- read_excel(sprintf(url_file,"Prosper_Lithium.xlsx"),
                 sheet="Lithium_Database")
(colnames(df) <- colnames(df) %>% str_replace(" ","_") %>% 
    str_replace("%","perc") %>% str_replace("/","_per"))
df_all <- df
df <- df %>% dplyr::select(Country,Deposit_Name,Resource_Type,Latitude,Longitude,Mt_Li,Status,
                           Production_Li_ktons,Reserve_Li_ktons,Resource_Li_ktons,
                           Grade_percLi_Reserve,Grade_percLi_Resource)

df <- df %>% filter(!is.na(Mt_Li) & Mt_Li>0)
# use resource of Benson if NA
df <- df %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Resource_Li_ktons),Mt_Li*1e3,Resource_Li_ktons))
sum(df$Resource_Li_ktons)/1e3 # 94 Mt

# By Country -----
df_country <- df %>% group_by(Country) %>% 
  reframe(Reserve_Li_ktons=sum(Reserve_Li_ktons,na.rm = T),
          Resource_Li_ktons=sum(Resource_Li_ktons,na.rm = T)) %>% ungroup() %>% 
  mutate(share_reserve=Reserve_Li_ktons/sum(Reserve_Li_ktons),
         share_resource=Resource_Li_ktons/sum(Resource_Li_ktons)) %>% 
  # arrange(desc(Resource_Li_ktons))
  arrange(desc(Reserve_Li_ktons))
df_country

url_fig <- "Figures/Deposit/%s.png"
# Mosaic Figure --------
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
# create stats for display
data_fig <- df %>% 
  # mutate(Resource_Li_ktons=Reserve_Li_ktons) %>%  # for Reserve analysis
  filter(Resource_Li_ktons>0) %>% 
  mutate(Resource_Type=if_else(str_detect(Resource_Type,"Other"),"Other",Resource_Type)) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(Resource_Li_ktons=sum(Resource_Li_ktons,na.rm = T)) %>% 
  ungroup() %>%
  group_by(Resource_Type) %>%
  mutate(total_resources=sum(Resource_Li_ktons),
         share_resource=Resource_Li_ktons/sum(Resource_Li_ktons)) %>% 
  mutate(r_label=if_else(Resource_Li_ktons>5e2,
                         paste0(Country,": ",format(round(Resource_Li_ktons/1e3,1),big.mark=","),"M"),"")) %>% 
  ungroup() %>% mutate(share_type=total_resources/sum(Resource_Li_ktons)) %>% 
  mutate(Resource_Type=paste0(Resource_Type,"\n(",round(share_type*100,0),"%)"))
  

r_order <- data_fig %>% group_by(Resource_Type) %>% summarise(p=sum(Resource_Li_ktons)) %>%
  arrange(desc(p)) %>% pull(Resource_Type)
data_fig <- data_fig %>% mutate(Resource_Type=factor(Resource_Type,levels=r_order))
c_order <- data_fig %>% group_by(r_label) %>% summarise(p=sum(Resource_Li_ktons)) %>%
  mutate(p=if_else(r_label=="",0,p)) %>% # no label at end
  arrange(desc(p)) %>% pull(r_label)
data_fig <- data_fig %>% mutate(r_label=factor(r_label,levels=c_order))

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
# f.fig.save(sprintf(url_fig,"Mosaic_Resource"),h=20,w=28)
f.fig.save(sprintf(url_fig,"Mosaic_Reserve"),h=20,w=28)


# Human Rights Index ----
# Source: https://ourworldindata.org/human-rights
# V-Dem (2023) – with major processing by Our World in Data

hr <- read.csv("Data/Indices/human-rights-index-vdem.csv")
hr <- hr %>% filter(Year==2022) %>% 
  rename(Country=Entity,humanRight_index=civ_libs_vdem_owid) %>% 
  mutate(Country=if_else(Country=="Democratic Republic of Congo","DR Congo",Country),
         Country=if_else(Country=="Czechia","Czech Republic",Country))



## Curve
data_fig <- df_country %>% left_join(hr) %>% 
  mutate(res=Resource_Li_ktons) %>% 
  mutate(res=Reserve_Li_ktons) %>% 
  filter(res>0) %>% 
  arrange(desc(humanRight_index)) %>% 
  mutate(reserve_cum_end=cumsum(res),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(res>200,Country,"")) %>% 
  mutate(lab_pos=reserve_cum_start+res/2) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

nrow(data_fig)  # 25

# duplicate last row
last_row <- data_fig[nrow(data_fig),]
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)
even_row <- data_fig$even_row

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/500)*500 # upper by 500
max(data_fig$humanRight_index)


ggplot(data_fig,aes(reserve_cum_start,humanRight_index,group=1))+
  geom_step(linewidth=0.75,direction = "hv")+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 0.1*even_row,
                  size=6*5/14 * 0.8)+
  labs(x="Cumulative Reserves [ktons Li]",y="Human Rights \nIndex [0-1]")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,1))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' '))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))


f.fig.save(sprintf(url_fig,"Curve_HumanRight_reserve"),w = 18)








