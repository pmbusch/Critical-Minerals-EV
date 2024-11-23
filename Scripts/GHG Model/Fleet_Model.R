# GHG Calculation
# EV Fleet Model
# PBH Nov 2024


# Load Data
source("Scripts/00-Libraries.R", encoding = "UTF-8")


## Reuse statistics from Survival Model -----
reuse <- read.csv("Parameters/Demand Intermediate Results/region_outflows_LIB.csv",
                  stringsAsFactors = FALSE)
# Convert strings to vectors by year - Process the list column back into a list
reuse <- reuse %>%
  mutate(LIB_recycling_vector = str_split(LIB_recycling_vector,"\\|") %>% lapply(as.numeric),
         LIB_Available_vector = str_split(LIB_Available_vector,"\\|") %>% lapply(as.numeric),
         add_LIB_vector = str_split(add_LIB_vector,"\\|") %>% lapply(as.numeric))
names(reuse)

# Only for USA
df <- reuse %>% filter(Region=="United States")

# Fleet at 2050
(x <- df %>% filter(Powertrain=="BEV",Vehicle=="Car",Year==2050) %>% 
    pull(EV_Stock)/1e6)
x/335 # car ownership 0.70


# Select only relevant columns
# add recycling flows later
df <- df %>% dplyr::select(Vehicle,Powertrain,Year,Scenario,Sales,EV_Stock,
                           add_LIB)

head(df)

# Save
write.csv(df,"Results/GHG/USA_fleet.csv",row.names = F)


# Figures ------
names(df)
url_fig <- "Figures/GHG/%s.png"

# Sales
df %>% 
  filter(Year<2051) %>% 
  filter(Powertrain=="BEV") %>% 
  mutate(Sales=Sales/1e6) %>% 
  ggplot(aes(Year,Sales,col=Vehicle))+
  geom_line()+
  facet_wrap(~Vehicle,scales = "free_y")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title="USA BEV Sales [million units]")+
  theme(legend.position = "none")

ggsave(sprintf(url_fig,"sales"),dpi=600,units = "cm",
       width = 12,height=8.7)

# Fleet
df %>% 
  filter(Year<2051) %>% 
  filter(Powertrain=="BEV") %>% 
  mutate(EV_Stock=EV_Stock/1e6) %>% 
  ggplot(aes(Year,EV_Stock,col=Vehicle))+
  geom_line()+
  facet_wrap(~Vehicle,scales = "free_y")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title="USA BEV Fleet [million units]")+
  theme(legend.position = "none")

ggsave(sprintf(url_fig,"fleet"),dpi=600,units = "cm",
       width = 12,height=8.7)



# Battery
df %>% 
  filter(Year<2051) %>% 
  filter(Powertrain=="BEV") %>% 
  mutate(`Replacement LIB`=add_LIB/1e6) %>% 
  mutate(`EV Sales`=Sales/1e6) %>% 
  dplyr::select(Year,`Replacement LIB`,`EV Sales`,Vehicle) %>% 
  pivot_longer(c(-Year,-Vehicle), names_to = "key", values_to = "value") %>% 
  ggplot(aes(Year,value,fill=key))+
  geom_area()+
  facet_wrap(~Vehicle,scales = "free_y")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",fill="",title="USA LIB Need [million units]")+
  theme(legend.position = "bottom")

ggsave(sprintf(url_fig,"LIB"),dpi=600,units = "cm",
       width = 12,height=8.7)


# EoF