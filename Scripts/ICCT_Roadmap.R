# Load and pre-process ICCT Data
# Data comes from Roadmap model v2.2
# https://theicct.github.io/roadmap-doc/versions/v2.2/
# Data requested on August 2023
# PBH August 2023

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data --------------
icct <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>% 
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")


# Dimensions
icct$Region %>% unique() # 18 unique
icct$Country %>% unique() # 187 Unique
icct$CY %>% unique() # 2022 to 2050
icct$Powertrain %>% unique() # ICE, BEV, PHEV
icct$Scenario %>% unique() # Baseline, Momentum, Ambitious
icct$Vehicle %>% unique() # 2-3 Wheelers, Car, Van, Bus, Medium Truck, Heavy Truck


# sales EV in 2035
icct %>% filter(CY==2035) %>% filter(Powertrain!="ICE") %>%
  filter(Vehicle=="Car") %>% pull(Sales) %>% sum()/1e6 # 204M, way more than MONET

# Levels
scen_levels <- c("Baseline","Momentum","Ambitious")
veh_levels <- c("Two/Three Wheelers","Car","Van","Bus","Medium truck","Heavy truck")
icct <- icct %>% 
  mutate(Scenario=factor(Scenario,levels=scen_levels),
         Vehicle=factor(Vehicle,levels=veh_levels),
         Powertrain=factor(Powertrain,levels=power_level),
         Region=factor(Region,levels=region_level))


# Group data -----

# group data at regional level to make it more digestible
icct_reg <- icct %>% group_by(Region,CY,Powertrain,Scenario,Vehicle) %>% 
  reframe(Sales=sum(Sales)) %>% ungroup()

# Figures to Show data ------

# Optinal to run
fig_name <- "Figures/ICCT/%s.png"

theme_set(theme_bw(20)+ theme(panel.grid.major = element_blank(),axis.title.y=element_text(angle=0)))


## Sales by scenario   -----------
icct %>% group_by(CY,Scenario,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY,Sales,col=Scenario,group=Scenario))+
  geom_line(linewidth=1)+
  facet_wrap(~Powertrain,ncol=1,scales = "free_y")+
  labs(y="Sales \n [millions]",x="")+
  coord_cartesian(expand=F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,"scenarios"))


## By powertrain -----------

scen="Baseline"

icct %>% 
  filter(Scenario==scen) %>% 
  group_by(CY,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, fill = fct_rev(Powertrain))) +
  geom_area() +
  labs(y="Sales \n [millions]",x="",fill="Powertrain",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "E")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("powertrain_",scen)))


## Vehicle Type

icct %>% 
  filter(Scenario==scen) %>% 
  group_by(CY,Powertrain,Vehicle) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Powertrain,ncol=1,scales="free_y")+
  labs(y="Sales \n [millions]",x="",fill="Vehicle",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("vehicle_",scen)))

## Just BEV ------

pt="BEV"

icct %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  group_by(CY,Powertrain,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Powertrain,ncol=1,scales="free_y")+
  labs(y="Sales \n [millions]",x="",fill="Region",
       caption = paste0(scen," scenario"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,paste0("region_",scen,"_",pt)))

## Vehicle by region of interest -----

# version 1

regs <- c("United States","European Union","China","India","ASEAN")

icct %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  mutate(Region=if_else(Region %in% regs,Region,"Rest of the World")) %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(CY,Powertrain,Region,Vehicle) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, fill = fct_rev(Vehicle))) +
  geom_area() +
  facet_wrap(~Region)+
  labs(y="Sales \n [millions]",x="",fill="Vehicle type",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_fill_viridis_d(option = "B")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste0("veh_",scen,"_",pt)))

# version 2
icct %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  group_by(CY,Powertrain,Region,Vehicle) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Vehicle,scales="free_y")+
  labs(y="Sales \n [millions]",x="",fill="Region",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste0("veh_",scen,"_",pt,"2")))

# Spagheti plot by country-----

icct %>% 
  filter(Scenario==scen,Powertrain==pt) %>% 
  filter(Vehicle %in% c("Two/Three Wheelers","Car")) %>% 
  group_by(CY,Powertrain,Region,Country,Vehicle) %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  reframe(Sales=sum(Sales)) %>% ungroup() %>% 
  ggplot(aes(CY, Sales, col = fct_rev(Region),group=Country)) +
  geom_line(linewidth=0.5) +
  facet_wrap(~Vehicle)+
  labs(y="Sales \n [millions]",x="",col="Region",
       caption = paste0(scen," scenario. Only ",pt))+  
  coord_cartesian(expand=F)+
  scale_colour_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_log10(labels = c("1K","10K", "100K", "1M","10M"),
                breaks = c(1e3,1e4, 1e5, 1e6,1e7))+
  theme(panel.spacing.x = unit(2, "cm"))

f.fig.save(sprintf(fig_name,paste0("veh_",scen,"_",pt,"_country")))


# EoF