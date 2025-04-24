# Ratio reserves and resources over production, historical

library(tidyverse)

df <- read.csv("Data/Supply Model/USGS_Li_Historical.csv")
head(df)

# calculate ratios
df <- df %>% 
  pivot_longer(c(Reserves,Resources), names_to = "key", values_to = "value") %>% 
  mutate(ratio=value/Mine.Production)

# min points
min_df <- df %>% group_by(key) %>% 
  mutate(min_ratio=min(ratio)) %>% ungroup() %>% 
  filter(ratio==min_ratio) %>% 
  mutate(ratio_label=paste0("",round(ratio,0)))
  

ggplot(df,aes(Year,ratio,col=key))+
  geom_line()+
  geom_point(data=min_df)+
  geom_text(data=filter(df,Year==2023),aes(label=key),nudge_x = 1.5,
            size=9*5/14 * 0.8)+
  geom_text(data=min_df,aes(label=ratio_label),nudge_y = c(100,100),
            size=8*5/14 * 0.8)+
  annotate(geom="text",x=2009,y=1600,label="Discovery of multiple resources",
           size=9*5/14 * 0.8)+
  annotate(geom="text",x=2017,y=600,label="Expansion of production - EVs",
           size=9*5/14 * 0.8)+
  annotate(geom="text",x=1998,y=700,label="Early expansion of production",
           size=9*5/14 * 0.8)+
  annotate(geom="text",x=2017,y=80,label="Minimum Years left (max depletion rate)",
           size=9*5/14 * 0.8)+
  labs(col="",x="",y="Years",title="Remaining Lithium Reserve/Resource, based on yearly production")+
  theme_bw(10)+
  coord_cartesian(expand = F)+
  scale_x_continuous(limits=c(1994,2026),breaks = c(1994, 2000,2005,2010,2015,2020,2023))+
  scale_y_continuous(limits = c(0,2150),labels = scales::comma)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y=element_text(angle=0,margin=margin(r=-10)))

ggsave("Figures/RatiosLithium.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


# EoF