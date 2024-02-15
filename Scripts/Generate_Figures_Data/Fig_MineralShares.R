# Figure on reserves and production share
# PBH Nov 2023

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

df <- rbind(
  tibble(mineral=c("Lithium"),
         country=c("Chile","Australia","China"),
         key=c("Production"),
         value=c(39000,61000,19000),
         total=130000),
  tibble(mineral=c("Lithium"),
         country=c("Chile","Australia","Argentina"),
         key=c("Reserves"),
         value=c(9.3*1e6,6.2*1e6,2.7*1e6),
         total=26*1e6),
  tibble(mineral=c("Nickel"),
         country=c("Indonesia","Philippines","Russia"),
         key=c("Production"),
         value=c(1.6,0.16,0.22)*1e6,
         total=3280000),
  tibble(mineral=c("Nickel"),
         country=c("Australia","Indonesia","Brazil"),
         key=c("Reserves"),
         value=c(21,21,16)*1e6,
         total=102*1e6),
  tibble(mineral=c("Cobalt"),
         country=c("DRC","Indonesia","Russia"),
         key=c("Production"),
         value=c(130,10,8.9)*1e3,
         total=190*1e3),
  tibble(mineral=c("Cobalt"),
         country=c("DRC","Indonesia","Australia"),
         key=c("Reserves"),
         value=c(4000,600,1500)*1e3,
         total=8.3*1e6)
  )

codes <- tibble(country=unique(df$country)) %>% 
  mutate(iso3=countrycode::countrycode(unique(df$country),"country.name","iso3c"))



df %>% 
  mutate(mineral=factor(mineral,levels=c("Lithium","Nickel","Cobalt"))) %>% 
  mutate(key=factor(key,levels=c("Reserves","Production"))) %>% 
  mutate(share=value/total*100) %>% 
  left_join(codes) %>% 
  mutate(label_perc=paste0(iso3," ",round(share,0),"%")) %>% 
  ggplot(aes(key,share,fill=country))+
  geom_col()+
  geom_text(aes(label=label_perc), position = position_stack(vjust = .5))+
  facet_wrap(~mineral,ncol=1)+
  coord_flip(expand = F)+
  scale_y_continuous(labels = scales::percent_format(scale = 1),limits = c(0,100))+
  labs(x="",y="",fill="")+
  theme_minimal(12)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "bottom")

f.fig.save("Figures/share3.png")



# EOF