# Extend the ICCT Forecast up to 2070
# Extension purpose is to avoid horizon effect in supply model
# PBH April 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Data from ICCT
icct <- readxl::read_excel("Data/Demand Model/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
(names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year"))

# Expand to 2070 base on last 5 years growth -----------

# total sales in 2050
icct %>% 
  filter(Year==2050) %>% 
  filter(Powertrain!="ICE") %>% 
  group_by(Scenario,Vehicle,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% 
  pivot_wider(names_from = Powertrain, values_from = Sales)

icct %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV",Vehicle=="Car") %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  labs(y="Sales \n [millions]",x="",fill="Vehicle type",
       caption = paste0("Ambitious"," scenario. Only ","BEV"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))



# avg last 5 years
avg_5 <- icct %>% 
  filter(Powertrain!="ICE") %>% 
  filter(Year>2045) %>% 
  rename(x=Sales) %>% 
  group_by(Scenario,Country,Vehicle,Powertrain) %>% 
  summarise(growth = mean((x / lag(x) - 1) * 100,na.rm=T)) %>% 
  filter(!is.nan(growth),!is.infinite(growth)) %>% ungroup()

# Limit growth factors - max 1% per year
avg_5 <- avg_5 %>% mutate(growth=case_when(
  growth < -1 ~ -1,
  growth>-1 & growth<1 ~ 0,
  growth > 1 ~ 1))


# Fill in the dataframe with the projected values
df_fill <- expand.grid(Scenario=unique(icct$Scenario),
                       Country = unique(icct$Country),
                       Vehicle = unique(icct$Vehicle),
                       Powertrain = c("BEV","PHEV"),
                       Year=2050:2070) %>% 
  left_join(icct)

df_fill <- df_fill %>% left_join(avg_5) %>% 
  filter(!is.na(growth)) %>% 
  mutate(Sales=if_else(is.na(Sales),0,Sales)) %>% 
  group_by(Scenario,Vehicle,Powertrain,Country) %>% 
  mutate(Growth_Factor = if_else(Year==2050,1,1 + growth/100),
         Sales=first(Sales) * cumprod(Growth_Factor)) %>% ungroup() %>% 
  filter(Year>2050) # remove initial
df_fill$growth <- df_fill$Growth_Factor <- NULL

# add Region for years after 2050
df_fill$Region <- NULL
reg_dict <- icct %>% group_by(Region,Country) %>% tally() %>% dplyr::select(-n)
df_fill <- df_fill %>% left_join(reg_dict)

# total sales in 2070
df_fill %>% 
  filter(Year==2070) %>% 
  group_by(Scenario,Vehicle,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% 
  pivot_wider(names_from = Powertrain, values_from = Sales)

df <- rbind(icct,df_fill)

# Make it comprehensive by adding 0
df$Region <- NULL
df <- df %>% complete(Country,Year,Powertrain,Vehicle,Scenario,fill = list(Sales=0))
nrow(df)
length(unique(df$Vehicle))*length(unique(df$Powertrain))*length(unique(df$Year))*
  length(unique(df$Country))*length(unique(df$Scenario))
df <- df %>% left_join(reg_dict)


df %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV",Vehicle=="Car") %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  geom_vline(xintercept = 2050)+
  labs(y="Sales \n [millions]",x="",fill="Vehicle type",
       caption = paste0("Ambitious"," scenario. Only ","BEV"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save("Figures/ICCT/Demand_extension.png")


# SAVE RESULTS
write.csv(df,"Parameters/Demand Intermediate Results/ICCT_demand.csv",row.names = F)



# EoF