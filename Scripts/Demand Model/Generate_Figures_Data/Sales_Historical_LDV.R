# Figure to show historical sales using InfluenceMap data


source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Load data ------

sales <- read_excel("Data/Demand Model/LDV Sales_InfluenceMap.xlsx",sheet="Data",skip = 4)

names(sales)
sales <- sales %>% 
  pivot_longer(c(31:60), names_to = "year", values_to = "s") %>% 
  mutate(year=year %>% str_remove("CY "))
sales <- sales %>% 
  rename(region=`VS: Sales Region`,
         country=`VS: Country/Territory`,
         body_type=`VS: Bodytype`) %>% 
  # mutate(body_type=Segment) %>% 
  group_by(region,country,year,body_type) %>%
  # group_by(region,country,year,body_type,Segment) %>% 
  summarise(s=sum(s,na.rm=T)) %>% ungroup()

## need to have segment in group _by
sales %>% group_by(body_type,Segment) %>% reframe(x=sum(s,na.rm=T)/1e6) %>% 
  ungroup() %>% group_by(Segment) %>% mutate(x=x/sum(x)*100) %>% 
  pivot_wider(names_from = Segment, values_from = x)


#  Conversion --------
sales$region %>% unique()
sales <- sales %>% 
  filter(s>0) %>% 
  filter(!is.na(region)) %>% 
  mutate(macro_region=case_when(
    region %in% c("South America","North America") ~ "Americas",
    region %in% c("Middle East/Africa") ~ "Africa & ME",
    region %in% c("ASEAN","Greater China","Indian Subcontinent",
                  "Japan/Korea","Oceania") ~ "Asia-Pacific",
    region %in% c("Central Europe","East Europe","West Europe") ~ "Europe",
    T ~ region))
sales$macro_region %>% unique()

sales %>% group_by(body_type) %>% reframe(x=sum(s)/1e6) %>% arrange(desc(x))
sales <- sales %>% 
  mutate(body_type2=case_when(
    body_type %in% c("Wagon","Pickup","Chassis-Cab") ~ "Wagon/\nPick-up/\nChassis-Cab",
    body_type %in% c("Sedan","Roadster","Convertible") ~ "Sedan+\nConvertible+\nRoadster",
    body_type %in% c("Van") ~ "LCV (Van)",
    body_type %in% c("Coupe") ~ "Mini (Coupe)",
    T ~ body_type))
sales$body_type2 %>% unique()

sales <- sales %>% mutate(year=as.numeric(year))

sales <- sales %>% filter(year<2021)


# Figures -----------
fig_name <- "Figures/IHS_%s.png"

df_fig <- sales %>% 
  group_by(macro_region,body_type2,year) %>% 
  reframe(units=sum(s)) %>% ungroup() %>% 
  group_by(macro_region,year) %>% 
  mutate(percentage = units / sum(units)) %>% ungroup()

p1 <- df_fig %>%
  # filter(year>2017) %>% 
  ggplot(aes(year,percentage,fill=factor(body_type2)))+
  geom_area()+
  facet_wrap(~macro_region,nrow = 2)+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales [%]",x="",
       fill="Vehicle type"
       # fill="Segment"
       )+
  # scale_x_continuous(breaks = seq(2018,2022,2))+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"areaSales_bodyType"))
# f.fig.save(sprintf(fig_name,"areaSales_Segment"))


p1 <- df_fig %>%
  ggplot(aes(year,units,fill=factor(body_type2)))+
  geom_area()+
  facet_wrap(~macro_region,nrow = 2,scales = "free_y")+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales",x="",
       # fill="Segment"
       fill="Vehicle type"
       )+
  # scale_x_continuous(breaks = seq(2018,2022,2))+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"Sales_bodyType"))
# f.fig.save(sprintf(fig_name,"Sales_Segment"))




# EoF