# Figure for Mineral Intensity
# Source of data: Default values of Battery 1. BatPac 5.1
# PBH September 2023

source("Scripts/00-Libraries.R", encoding = "UTF-8")
mineral <- read_excel("Data/Mineral_Intensity.xlsx",sheet = "BatPac")

# Factor
mineral <- mineral %>% 
  mutate(chemistry=factor(chemistry,levels=rev(unique(mineral$chemistry)))) %>% 
  filter(Mineral %in% min_interest) %>% 
  mutate(Mineral=factor(Mineral,min_interest))

# Figure
mineral %>% 
  # filter(chemistry!="LMO-LTO") %>% 
  ggplot(aes(chemistry,kg_per_kwh,fill=chemistry))+
  geom_col()+
  facet_wrap(~Mineral,scales = "free_x")+
  coord_flip()+
  scale_fill_viridis_d(option = "B")+
  labs(x="Chemistry",y="Mineral Intensity [kg per kWh]")+
  theme(legend.position = "none")

f.fig.save("Figures/MinIntensity.png",h=6)


# Swap

# Figure
mineral %>% 
  filter(chemistry!="LMO-LTO") %>%
  filter(chemistry!="NMC 95") %>% 
  ggplot(aes(chemistry,kg_per_kwh,fill=Mineral))+
  geom_col(position = "dodge")+
  coord_flip(expand = F)+
  scale_fill_manual(values=c("Lithium" = "darkblue", "Nickel" = "darkgreen", "Cobalt" = "darkred"))+
  labs(x="",y="Mineral Intensity [kg per kWh]",fill="")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.position = c(0.9,0.8))

f.fig.save("Figures/MinIntensity2.png")

# EoF