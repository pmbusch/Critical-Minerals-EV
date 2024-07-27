# Analysis Cost vs EDB
# March 2024 PBH

# Load Data ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Deposit data
deposit <- read.csv("Parameters/Deposit.csv")


## Cost vs Country of deposit ranking ----

data_fig <- deposit %>% 
  mutate(res=(reserve+resource_demostrated+resource_inferred)/1e3) %>%
  mutate(edb=100-edb) %>%
  mutate(cost1=cost1/5.323)

# for label
countries <- data_fig %>% filter(res>0.5) %>% 
  group_by(Country) %>% reframe(edb=mean(edb)) %>% ungroup() %>% 
  mutate(Country=case_when(
    Country=="Germany" ~"",
    Country=="Canada" ~"Canada-Germany",
    Country=="Serbia" ~"",
    Country=="Czech Republic" ~"Czechia-Serbia",
    Country=="Mexico" ~"",
    Country=="Chile" ~"Chile-Mexico",
    Country=="France"~"",
    T ~ Country))

ggplot(data_fig,aes(cost1,edb,size=res))+
  geom_point(alpha=.5,aes(col=Resource_Type))+
  geom_text(data=countries,aes(label=Country),x=6000,
            # direction = "x",
                  fontface = "italic",
                  size=6*5/14 * 0.8,
                  hjust=0,min.segment.length = unit(0,"lines"))+
  scale_x_continuous(limits = c(6.2,13.5)*1e3,labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_color_manual(values=resource_colors)+
  labs(y="",title="Ease of Doing Business [0-100]",x="Extraction Costs [USD/ton LCE]",
       size="Total Extraction \n[M tons Li]",col="Resource \ntype")+
  theme_bw(8)+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave("Figures/Article/Tradeoff.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)
