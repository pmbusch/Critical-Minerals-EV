# How to forecast battery size for LDV
# Creates model for regression and ranges by country
# PBH October 2023


# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load Data - EV Volumes -----

vehSpec <- read_excel("Data/Vehicle-Specification-Database-19-Dec-2022-agn.xlsx",
                      sheet="Index",range="B3:CD548")
(names(vehSpec) <- names(vehSpec) %>% str_remove("&") %>% str_replace_all(" |-","_") %>% 
    str_replace_all("__","_") %>% str_replace_all("\r\n","_"))
bat <- read_excel("Data/EV Volumes_DB Battery.xlsx",sheet="Database")
(names(bat) <- names(bat) %>% str_remove("&") %>% str_replace_all(" |-","_") %>% 
    str_replace_all("__","_") %>% str_remove("Delivered_"))
bat <- bat %>% filter(Propulsion!="FCEV")

# Join Databases ------------
# notes
# CN NEDC =  New European Driving Cycle Fuel Economy Standard
# EU WLTP =  Worldwide Harmonised Light Vehicles Test Procedure

vehSpec <- vehSpec %>% 
  mutate(range=if_else(is.na(CN_NEDC_km)|CN_NEDC_km==0|CN_NEDC_km=="tba", # use range for km available
                       `EU_km_(WLTP)`,CN_NEDC_km)) %>% 
  mutate(x=`CN_NEDC_kWh/100km_(+ltr/100km)_`,
         efficiency_orig=if_else(is.na(x)|x==0|str_detect(x,"tba"), # choose economy
                                 `kWh/100km_(+ltr/100km)_`,x)) %>% 
  dplyr::select(OEM,Brand,Model,Segment,Propulsion_Type,Cathode_Chemistry,Cathode_mix,
                Battery_Capacity,`Top_Speed_km/h`,efficiency_orig,Modelyear,
                range,Wheelbase,Height,Length,Width,US_Miles,
                Body_style,`#_of_Seats`,`#_of_Doors`,Curbweight) %>% 
  rename(OEM_Group=OEM,ModelSpec=Model,Global_Segment=Segment,Propulsion=Propulsion_Type) %>% 
  rename(top_speed=`Top_Speed_km/h`,n_seats=`#_of_Seats`,
         n_doors=`#_of_Doors`,Battery_Capacity_orig=Battery_Capacity)
head(vehSpec)

vehSpec <- vehSpec %>% 
  mutate(Curbweight=Curbweight %>% str_remove_all("tba|kg| ") %>% as.numeric(),
         top_speed=top_speed %>% str_remove_all("km/h") %>% as.numeric(),
         range=range %>% str_remove_all("km") %>% as.numeric(),
         range_fromMiles=US_Miles %>% str_remove_all("tba|mi") %>% str_replace(",",".") %>% as.numeric(),
         range=case_when(range>0 ~ range,
                         range==0 & range_fromMiles>0 ~ range_fromMiles*1.60934,
                         T ~ NA),
         efficiency=efficiency_orig %>% str_remove_all("\\([^)]*\\)") %>% str_trim() %>% 
           str_replace(",",".") %>% as.numeric(),
         efficiency=if_else(efficiency==0,NA,efficiency),
         # battery capacity requires more effort
         Battery_Capacity=Battery_Capacity_orig %>% str_remove_all("kWh") %>% str_trim() %>% 
           str_replace_all(",",".") %>% str_remove_all(" ")) %>% 
  rowwise() %>%
  # take mean of the listed capacities!
  mutate(Battery_Capacity=mean(as.numeric(unlist(str_split(Battery_Capacity, "/")), na.rm = TRUE))) %>%
  ungroup() %>%
  relocate(efficiency,.after = efficiency_orig) %>% 
  relocate(Battery_Capacity,.after=Battery_Capacity_orig)

# numeric
vehSpec <- vehSpec %>% mutate(Wheelbase=as.numeric(Wheelbase),
                              Height=as.numeric(Height),
                              Width=as.numeric(Width),
                              Length=as.numeric(Length),
                              n_seats=as.numeric(n_seats))

# body style
vehSpec <- vehSpec %>% mutate(Body_style2=case_when(
  Body_style %in% c("Sedan","Coupe","") ~ "Sedan",
  Body_style %in% c("Sedan / Wagon","Wagon","Van","Pick-up","GT") ~ "Wagon / Pick-up",
  T ~ Body_style) %>% as.factor())


sum(is.na(vehSpec$Curbweight))/nrow(vehSpec) # 15% NA
sum(is.na(vehSpec$range))/nrow(vehSpec) # 4% NA
sum(is.na(vehSpec$efficiency))/nrow(vehSpec) # 26% NA

# calculate efficiency based on range and battery capacity
vehSpec <- vehSpec %>% mutate(efficiency_calc=Battery_Capacity/range*100)
sum(is.na(vehSpec$efficiency_calc))/nrow(vehSpec) # 4% NA -  improvement


bat <- bat %>% 
  group_by(Sales_Region,Sales_Sub_Region,Sales_Country,Vehicle_Production_Region,
           Vehicle_Production_Country,OEM_Group,Brand,Make_Model,
           Global_Segment,Propulsion,Cathode_Chemistry) %>% 
  reframe(u2010=sum(`2010`,na.rm=T),u2011=sum(`2011`,na.rm=T),
          u2012=sum(`2012`,na.rm=T),u2013=sum(`2013`,na.rm=T),
          u2014=sum(`2014`,na.rm=T),u2015=sum(`2015`,na.rm=T),
          u2016=sum(`2016`,na.rm=T),u2017=sum(`2017`,na.rm=T),
          u2018=sum(`2018`,na.rm=T),u2019=sum(`2019`,na.rm=T),
          u2020=sum(`2020`,na.rm=T),u2021=sum(`2021`,na.rm=T),
          u2022=sum(`2022`,na.rm=T),
          MWh_2010=sum(MWh_2010_earlier,na.rm = T),
          MWh_2011=sum(MWh_2011,na.rm=T),
          MWh_2012=sum(MWh_2012,na.rm=T),
          MWh_2013=sum(MWh_2013,na.rm=T),
          MWh_2014=sum(MWh_2014,na.rm=T),
          MWh_2015=sum(MWh_2015,na.rm=T),
          MWh_2016=sum(MWh_2016,na.rm=T),
          MWh_2017=sum(MWh_2017,na.rm=T),
          MWh_2018=sum(MWh_2018,na.rm=T),
          MWh_2019=sum(MWh_2019,na.rm=T),
          MWh_2020=sum(MWh_2020,na.rm=T),
          MWh_2021=sum(MWh_2021,na.rm=T),
          MWh_2022=sum(MWh_2022,na.rm=T)) %>% ungroup() %>% 
  pivot_longer(c(u2010,u2011,u2012,u2013,u2014,u2015,u2016,
                 u2017,u2018,u2019,u2020,u2021,u2022,
                 MWh_2010,MWh_2011,MWh_2012,MWh_2013,MWh_2014,MWh_2015,
                 MWh_2016,MWh_2017,MWh_2018,MWh_2019,MWh_2020,MWh_2021,MWh_2022), 
               names_to = "year", values_to = "value") %>% 
  mutate(unit=if_else(str_detect(year,"MWh"),"MWh","unit")) %>% 
  mutate(year=str_remove(year,"u|MWh_") %>% as.numeric()) %>% 
  pivot_wider(names_from = unit, values_from = value,values_fill = 0) %>% 
  mutate(unit = if_else(is.na(unit), 0, unit))


head(bat)
# sales
bat %>% group_by(year,Propulsion) %>% 
  reframe(x=sum(unit)/1e6) %>% spread(Propulsion,x) # 10.56M units in 2022

table(vehSpec$Modelyear)


# join dictionary
join_dict <- read_excel("Data/Joins/Join_EVVolumes.xlsx",sheet="Model name comparison")[,1:2] %>% 
  filter(!is.na(MATCH_Spec))
names(join_dict) <- c("Make_Model","ModelSpec")
head(join_dict)

# remove chem
bat$Cathode_Chemistry <- NULL
# remove others
bat$OEM_Group <- bat$Brand <- NULL
bat$Global_Segment <- NULL

# Add characteristics to sales data
df <- bat %>% left_join(join_dict) %>% 
  filter(!is.na(ModelSpec)) %>%
  left_join(vehSpec)

df %>% mutate(bin=!is.na(Curbweight)) %>% 
  group_by(bin) %>% reframe(qty=sum(unit)/1e6)

# FIGURES ------------
fig_name <- "Figures/Battery Size/%s.png"

## Vehicle specifications ----------


### Calc Efficiency -------
# is range and capacity a proxy for efficiency?
ggplot(vehSpec,aes(efficiency,efficiency_calc))+geom_point()+
  geom_abline(slope=1,col="grey")+xlim(0,60)+ylim(0,60)+
  geom_smooth(method="lm")

vehSpec$after2019 <- vehSpec$Modelyear>=2019

head(vehSpec)


### Eff vs weight -------
p <- vehSpec %>% 
  filter(Propulsion=="BEV") %>% 
  filter(!is.na(efficiency)) %>% 
  filter(after2019) %>% 
  ggplot(aes(Curbweight,efficiency,col=factor(Modelyear)))+
  geom_point()+
  facet_wrap(~Propulsion)+
  labs(x="Curbweight [kg]",y="Energy \n Consumption \n [kWh/100km]",
       col="Model Year")
p
f.fig.save(sprintf(fig_name,"Cons_Weight"))
p+aes(col=factor(Body_style2))+labs(col="Body Style")
f.fig.save(sprintf(fig_name,"Cons_Weight2"))
p+aes(col=factor(Cathode_Chemistry))+labs(col="Battery \nChemistry")
f.fig.save(sprintf(fig_name,"Cons_Weight3"))
p+aes(col=factor(OEM_Group))

p+aes(x=Battery_Capacity)+labs(x="Battery Capacity [kWh]")
f.fig.save(sprintf(fig_name,"Cons_BatSize"))
p+aes(x=Battery_Capacity,col=factor(Body_style2))+
  labs(x="Battery Capacity [kWh]",col="Body Style")
f.fig.save(sprintf(fig_name,"Cons_BatSize2"))
p+aes(x=Battery_Capacity,col=factor(Cathode_Chemistry))+
  labs(x="Battery Capacity [kWh]",col="Battery \nChemistry")
f.fig.save(sprintf(fig_name,"Cons_BatSize3"))

### Capacity vs others ------
p1 <- vehSpec %>% 
  filter(Propulsion=="BEV") %>% 
  filter(after2019) %>% 
  ggplot(aes(Curbweight,Battery_Capacity,col=factor(Modelyear)))+
  geom_point()+
  facet_wrap(~Propulsion)+
  labs(x="Curbweight [kg]",y="Battery \n Capacity \n [kWh]",col="Model Year")
p1
f.fig.save(sprintf(fig_name,"Weight_BatSize"))
p1+aes(col=factor(Body_style2))+labs(col="Body Style")
f.fig.save(sprintf(fig_name,"Weight_BatSize2"))
p1+aes(col=factor(Cathode_Chemistry))+labs(col="Battery \nChemistry")
f.fig.save(sprintf(fig_name,"Weight_BatSize3"))
p1+aes(col=factor(OEM_Group))

p1+aes(x=range)+labs(x="Range [km]")
f.fig.save(sprintf(fig_name,"range_BatSize"))
p1+aes(x=range,col=factor(Body_style2))+labs(x="Range [km]",col="Body Style")
f.fig.save(sprintf(fig_name,"range_BatSize2"))
p1+aes(x=range,col=factor(Cathode_Chemistry))+labs(x="Range [km]",col="Battery \nChemistry")
f.fig.save(sprintf(fig_name,"range_BatSize3"))


p1+aes(x=range,y=Curbweight)+labs(x="Range [km]",y="Curbweight [kg]")
f.fig.save(sprintf(fig_name,"range_weight"))
p1+aes(x=range,y=Curbweight,col=factor(Body_style2))+
  labs(x="Range [km]",y="Curbweight [kg]",col="Body Style")
f.fig.save(sprintf(fig_name,"range_weight2"))
p1+aes(x=range,y=Curbweight,col=factor(Cathode_Chemistry))+
  labs(x="Range [km]",y="Curbweight [kg]",col="Battery \nChemistry")
  # facet_wrap(~Body_style2)
f.fig.save(sprintf(fig_name,"range_weight3"))


### GOOD Range and weight -----
data_fig <- vehSpec %>% 
  filter(Propulsion=="BEV") %>% 
  filter(after2019) %>% 
  filter(!is.na(range)) %>% 
  mutate(range_d=case_when(
    range<200 ~ "< 200 km",
    range<300 ~ "200-300 km",
    range<400 ~ "300-400 km",
    range<500 ~ "400-500 km",
    range<600 ~ "500-600 km",
    T ~ "> 700 km") %>% factor(levels=c("< 200 km","200-300 km","300-400 km",
                                        "400-500 km","500-600 km","> 700 km")))

  # group_by(range_d) %>% tally() # count
ggplot(data_fig,aes(Curbweight,Battery_Capacity,col=range_d))+
  geom_point()+
  geom_smooth(method="lm",se = F)+
  facet_wrap(~Propulsion)+
  labs(x="Curbweight [kg]",y="Battery \n Capacity \n [kWh]",col="Range")

f.fig.save(sprintf(fig_name,"range_weight_size"))


# boxplot chemsitry
data_fig %>% 
  filter(Cathode_Chemistry %in% c("LFP","NMC","NCA")) %>% 
  # mutate(range_d=OEM_Group) %>% # Which chemistries is using each OEM
  # mutate(range_d=Brand) %>% 
  # mutate(range_d=Global_Segment) %>% 
  ggplot(aes(range_d,Battery_Capacity,fill=Cathode_Chemistry,col=Cathode_Chemistry))+
  geom_boxplot(alpha=.5,outlier.shape=NA)+
  geom_jitter(position=position_jitterdodge())+
  facet_wrap(~Propulsion)+
  # coord_flip()+
  guides(col="none")+
  labs(x="Range [km]",y="Battery \n Capacity \n [kWh]",fill="Battery \n Chemistry")

data_fig %>% group_by(Cathode_Chemistry,Cathode_mix) %>% tally() %>% arrange(desc(n))
f.fig.save(sprintf(fig_name,"range_bat_chem"))

# Share of models of chemistry by range
data_fig %>% 
  filter(Cathode_Chemistry %in% c("LFP","NMC","NCA")) %>% 
  group_by(range_d,Cathode_Chemistry) %>% tally() %>% ungroup() %>% 
  # group_by(range_d) %>% mutate(n=n/sum(n)) %>% # in relative terms
  group_by(range_d) %>% mutate(n_sum=sum(n),
                               label_n=paste0("",round(n_sum,0))) %>% 
  ggplot(aes(range_d,n,fill=Cathode_Chemistry)) +
  geom_col(position = "stack")+
  geom_text(aes(y=n_sum,label=label_n),nudge_y = 5)+
  labs(x="Range [km]",y="# of Models",fill="Battery \n Chemistry")
f.fig.save(sprintf(fig_name,"range_bat_chem_count"))



### GGRIDGES ----
library(ggridges)
# distribution by model year
vehSpec %>% 
  filter(Propulsion=="BEV") %>% 
  filter(Modelyear>2019) %>%
  dplyr::select(Battery_Capacity,Modelyear,Curbweight,efficiency,range) %>%
  rename(Enegy_Consumption=efficiency) %>% 
  pivot_longer(c(-Modelyear), names_to = "key", values_to = "value") %>% 
  ggplot(aes(value,factor(Modelyear),fill=Modelyear))+
  facet_wrap(~key,scales = "free")+
  # geom_density_ridges(fill = "lightblue", alpha = 0.5)+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                     quantiles = c(0.5))+
  # geom_density(alpha=.8)+
  # scale_y_reverse()+
  labs(x="",y="",fill="Model Year")
f.fig.save(sprintf(fig_name,"model_density"))


## SALES Battery ------

### Sales per region -----------
bat %>% 
  filter(Propulsion=="BEV") %>% 
  group_by(year,Sales_Region) %>% reframe(unit=sum(unit)/1e6) %>% ungroup() %>% 
  ggplot(aes(year, unit, fill = fct_rev(Sales_Region))) +
  geom_area() +
  # facet_wrap(~Mineral,ncol=2,scales="free_y")+
  labs(y="BEV \n Sold \n [million units]",x="",fill="Region")+  
  coord_cartesian(expand=F)
  # scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))
f.fig.save(sprintf(fig_name,"BEV_sold"))

# Same figures as before, but with total sales per model
# to do weights, need to repeat ROWS!!!
# 
# df_fig <- df %>% 
#   filter(Propulsion=="BEV") %>% 
#   filter(Modelyear>2019) %>%
#   filter(unit>0) %>% 
#   dplyr::select(Battery_Capacity,Curbweight,efficiency,Modelyear,units) %>%
#   rename(Enegy_Consumption=efficiency) %>% 
#   pivot_longer(c(Battery_Capacity,Curbweight,Enegy_Consumption), 
#                names_to = "key", values_to = "value") %>% 
#   filter(key=="Battery_Capacity")
# 
# df_fig <- df_fig[rep(seq_along(df_fig$unit), df_fig$units), ]
# 
# df_fig %>% 
#   ggplot(aes(value,factor(Modelyear),fill=Modelyear))+
# # facet_wrap(~key,scales = "free")+
#   geom_density_ridges(fill = "lightblue", alpha = 0.5,
#                       aes(height=..density..,weight=unit), stat="density")+
#   # stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
#   #                     quantiles = c(0.5))+
#   labs(x="",y="",fill="Model Year")


### World Battery size evolution -----
df %>% 
  filter(Propulsion=="BEV") %>% 
  filter(Modelyear>2019) %>%
  group_by(Modelyear) %>%
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          Energy_Consumption=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T)) %>% ungroup() %>% 
  pivot_longer(c(-Modelyear), names_to = "key", values_to = "value") %>% 
  ggplot(aes(Modelyear,value))+
  geom_col(fill="brown")+
  # facet_grid(key~Sales_Region,scales = "free_y")+
  facet_wrap(~key,scales = "free")+
  labs(x="",y="",fill="")

### Weight by country and years --------
# Only EVs

df_fig <- df %>% 
  filter(unit>0) %>% 
  group_by(Sales_Region,Sales_Sub_Region,year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit)) %>% ungroup()
  
regs <- df_fig %>% filter(year==2022) %>% filter(unit>4e4) %>% 
  pull(Sales_Sub_Region) %>% unique()

p1 <- df_fig %>%
  filter(Sales_Sub_Region %in% regs) %>% 
  ggplot(aes(year,Curbweight,group=Sales_Sub_Region,col=Sales_Sub_Region))+
  geom_line()+
  facet_wrap(~Propulsion)+
  labs(y="Curbweight [kg]",x="",col="Region",caption="Weighted by sales")+
  scale_x_continuous(breaks = seq(2010,2022,2))
p1
f.fig.save(sprintf(fig_name,"Weight_region"))

p1+aes(y=range)+labs(y="Range [km]")
f.fig.save(sprintf(fig_name,"range_region"))
  
p1+aes(y=Battery_Capacity)+labs(y="Battery \n Capacity \n [kWh]")
f.fig.save(sprintf(fig_name,"batsize_region"))

p1+aes(y=efficiency)+labs(y="Energy \n Consumption \n [kWh/100km]")
f.fig.save(sprintf(fig_name,"eff_region"))

#### USA stats--------------
df_fig <- df %>% 
  filter(unit>0) %>% 
  filter(Sales_Country=="USA") %>% 
  group_by(Sales_Country,year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit)) %>% ungroup()

df_fig <- df_fig %>% filter(Propulsion=="BEV") %>% 
  filter(year>2014)

p1 <- df_fig %>% 
  ggplot(aes(year,Curbweight,group=Sales_Country))+
  geom_line(col=colors_cat[10],linewidth=1)+
  facet_wrap(~Propulsion,nrow=1)+
  labs(title="Curbweight [kg]",y="",x="",col="",caption="Weighted by sales")+
  scale_x_continuous(breaks = seq(2015,2022,1))+
  # ylim(0,NA)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p1
f.fig.save(sprintf(fig_name,"Weight_USA"))

p1+aes(y=range)+labs(title="Range [km]",y="")
f.fig.save(sprintf(fig_name,"range_USA"))

p1+aes(y=Battery_Capacity)+labs(title="Battery Capacity [kWh]",y="")
f.fig.save(sprintf(fig_name,"batsize_USA"))

p1+aes(y=efficiency)+labs(title="Energy Consumption [kWh/100km]",y="")
f.fig.save(sprintf(fig_name,"eff_USA"))

# Model sales
df %>% 
  filter(unit>0, year>2014,Propulsion=="BEV") %>% 
  filter(Sales_Country=="USA") %>% 
  group_by(Sales_Country,year,Make_Model) %>% 
  reframe(unit=sum(unit,na.rm=T)) %>% ungroup() %>% 
  group_by(Sales_Country,year) %>% 
  mutate(share_sales=unit/sum(unit)) %>% ungroup() %>% 
  arrange(desc(share_sales)) %>%
  mutate(Make_Model=if_else(unit>2e4,Make_Model,"Other")) %>% 
  ggplot(aes(year,unit,fill=Make_Model))+
  geom_col()+
  scale_x_continuous(breaks = seq(2015,2022,1))+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  labs(x="",y="BEV \n sales",fill="Model")
f.fig.save(sprintf(fig_name,"sales_USA"))


#### Spaghetti plot by country --------------
df_fig <- df %>% 
  filter(unit>0) %>% 
  group_by(Sales_Country,year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit)) %>% ungroup()

# last 4 years, at least 1000 unit
(conts <- df_fig %>% filter(year==2022) %>% filter(unit>1e3) %>% 
    pull(Sales_Country) %>% unique())
df_fig <- df_fig %>%
  filter(Sales_Country %in% conts) %>% 
  filter(year>2017)

cons_interest <- c("USA","UAE","South Korea","UK","Germany",
                   "Thailand","France","India","Japan","China")

colors_cat <- viridis::turbo(10)
category_colors <- c("Rest of World" = "#B0B0B0",
                     "World" = "#1A1A1A",
                     "UAE" = colors_cat[1],
                     "France" = colors_cat[2],
                     "Japan" = colors_cat[3],
                     "South Korea" = colors_cat[4],
                     "Thailand"=colors_cat[5],
                     "India" = colors_cat[6],
                     "Germany" = colors_cat[7],
                     "UK" = colors_cat[8],
                     "China" = colors_cat[9], 
                     "USA" = colors_cat[10])

df_fig <- df_fig %>%  mutate(c_int=if_else(Sales_Country %in% cons_interest,Sales_Country,"Rest of World"))

df_fig <- df_fig %>% filter(Propulsion=="BEV")

p1 <- df_fig %>% 
  ggplot(aes(year,Curbweight,group=Sales_Country))+
  geom_line(col="grey",linewidth=0.5)+
  geom_line(aes(col=c_int),linewidth=1,
            data=filter(df_fig,Sales_Country %in% cons_interest))+
  facet_wrap(~Propulsion,nrow=1)+
  labs(title="Curbweight [kg]",y="",x="",col="",caption="Weighted by sales")+
  scale_x_continuous(breaks = seq(2018,2022,1))+
  scale_color_manual(values = category_colors, breaks = cons_interest,labels = cons_interest)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p1
f.fig.save(sprintf(fig_name,"Weight_country"))

p1+aes(y=range)+labs(title="Range [km]",y="")
f.fig.save(sprintf(fig_name,"range_country"))

p1+aes(y=Battery_Capacity)+labs(title="Battery Capacity [kWh]",y="")
f.fig.save(sprintf(fig_name,"batsize_country"))

p1+aes(y=efficiency)+labs(title="Energy Consumption [kWh/100km]",y="")
f.fig.save(sprintf(fig_name,"eff_country"))


### Range, weight and others by vehicle type ------
df_fig <- df %>% 
  filter(unit>0) %>% 
  filter(!is.na(Body_style2)) %>% 
  group_by(Body_style2,year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit)) %>% ungroup() %>% 
  filter(Propulsion=="BEV")

df_fig %>% group_by(Body_style2) %>% reframe(sum(unit)/1e6)

p1 <- df_fig %>%
  ggplot(aes(year,Curbweight,group=Body_style2,col=Body_style2))+
  geom_line(linewidth=1)+
  facet_wrap(~Propulsion)+
  labs(y="Curbweight [kg]",x="",col="Vehicle type",caption="Weighted by sales")+
  scale_x_continuous(breaks = seq(2010,2022,2))
p1
f.fig.save(sprintf(fig_name,"Weight_bodyType"))

p1+aes(y=range)+labs(y="Range [km]")
f.fig.save(sprintf(fig_name,"range_bodyType"))

p1+aes(y=Battery_Capacity)+labs(y="Battery \n Capacity \n [kWh]")
f.fig.save(sprintf(fig_name,"batsize_bodyType"))

p1+aes(y=efficiency)+labs(y="Energy \n Consumption \n [kWh/100km]")
f.fig.save(sprintf(fig_name,"eff_bodyType"))

### Bars to compare variables by Body type ------
df_fig <- df %>% 
  filter(unit>0) %>% 
  filter(!is.na(Body_style2)) %>% 
  filter(year==2022) %>% filter(Propulsion=="BEV") %>% 
  group_by(Sales_Region,Body_style2,year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit)) %>% ungroup()

p1 <- df_fig %>%
  ggplot(aes(Sales_Region,Curbweight,fill=Body_style2))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  guides(fill = guide_legend(reverse=T))+
  labs(y="Curbweight [kg]",x="",fill="Vehicle type",caption="Weighted by sales. Year 2022")
p1
f.fig.save(sprintf(fig_name,"bar_Weight_bodyType"))

p1+aes(y=range)+labs(y="Range [km]")
f.fig.save(sprintf(fig_name,"bar_range_bodyType"))

p1+aes(y=Battery_Capacity)+labs(y="Battery Capacity [kWh]")
f.fig.save(sprintf(fig_name,"bar_batsize_bodyType"))

p1+aes(y=efficiency)+labs(y="Energy Consumption [kWh/100km]")
f.fig.save(sprintf(fig_name,"bar_eff_bodyType"))

# Same bar but reversed
p1 <- p1+aes(fill=Sales_Region,x=Body_style2)+labs(fill="Region",x="")
p1
f.fig.save(sprintf(fig_name,"bar_We_bodyTypeight"))

p1+aes(y=range)+labs(y="Range [km]")
f.fig.save(sprintf(fig_name,"bar_r_bodyTypeange"))

p1+aes(y=Battery_Capacity)+labs(y="Battery Capacity [kWh]")
f.fig.save(sprintf(fig_name,"bar_bat_bodyTypesize"))

p1+aes(y=efficiency)+labs(y="Energy Consumption [kWh/100km]")
f.fig.save(sprintf(fig_name,"bar_bodyType_eff"))


### Stacked area of body type sales BY REGION --------

df_fig <- df %>% 
  filter(unit>0) %>% 
  filter(!is.na(Body_style2)) %>% 
  group_by(Sales_Region,Body_style2,year) %>% 
  reframe(unit=sum(unit)) %>% ungroup() %>% 
  group_by(Sales_Region,year) %>% 
  mutate(percentage = unit / sum(unit)) %>% ungroup()


p1 <- df_fig %>%
  # filter(Sales_Region!="Africa & ME") %>% 
  filter(year>2017) %>% 
  ggplot(aes(year,percentage,fill=factor(Body_style2)))+
  geom_area()+
  facet_wrap(~Sales_Region,nrow = 2)+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales [%]",x="",fill="Vehicle type")+
  scale_x_continuous(breaks = seq(2018,2022,2))+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"areaSales_bodyType"))

p1 <- df_fig %>%
  # filter(Sales_Region!="Africa & ME") %>% 
  filter(year>2017) %>% 
  ggplot(aes(year,unit,fill=factor(Body_style2)))+
  geom_area()+
  facet_wrap(~Sales_Region,nrow = 2,scales = "free_y")+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales",x="",fill="Vehicle type")+
  scale_x_continuous(breaks = seq(2018,2022,2))+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"Sales_bodyType"))


#### by country of interest -----
df_fig <- df %>% 
  filter(unit>0) %>% 
  filter(year>2018) %>% 
  filter(!is.na(Body_style2)) %>% 
  filter(Sales_Country %in% cons_interest) %>% 
  group_by(Sales_Country,Body_style2,year) %>% 
  reframe(unit=sum(unit)) %>% ungroup() %>% 
  group_by(Sales_Country,year) %>% 
  mutate(percentage = unit / sum(unit)) %>% ungroup()


p1 <- df_fig %>%
  ggplot(aes(year,percentage,fill=factor(Body_style2)))+
  geom_area()+
  facet_wrap(~Sales_Country,nrow = 2)+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales [%]",x="",fill="Vehicle type")+
  scale_x_continuous(breaks = seq(2018,2022,2))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = category_colors, breaks = cons_interest,labels = cons_interest)+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"areaSales_bodyType_country"))


p1 <- df_fig %>%
  ggplot(aes(year,unit,fill=factor(Body_style2)))+
  geom_area()+
  facet_wrap(~Sales_Country,nrow = 2,scales = "free_y")+
  # guides(fill = guide_legend(reverse=T))+
  labs(y="Sales",x="",fill="Vehicle type")+
  scale_x_continuous(breaks = seq(2018,2022,2))+
  scale_color_manual(values = category_colors, breaks = cons_interest,labels = cons_interest)+
  coord_cartesian(expand = F)
p1
f.fig.save(sprintf(fig_name,"Sales_bodyType_country"))


## Size by chemistry ---------
# most common chems
vehSpec %>% group_by(Cathode_Chemistry) %>% tally()
vehSpec %>% group_by(Cathode_Chemistry,Cathode_mix) %>% tally() %>% 
  arrange(desc(n))

chems <- c("NCA","NMC","LFP")
chems_mix <- c("NMC-tba","NMC-NMC 532","NMC-NMC 622","LFP-tba","NMC-NMC 111",
               "NMC-NMC 811","NMC-NMC 111 + NCA","LFP-LFP","NMC-NMC 721",
               "NCA-NCA","NCA-tba")

p2 <- vehSpec %>% 
  # filter(Cathode_Chemistry %in% chems) %>% 
  mutate(Cathode_Chemistry=paste0(Cathode_Chemistry,"-",Cathode_mix)) %>% 
  filter(Cathode_Chemistry %in% chems_mix) %>%
  mutate(Cathode_Chemistry=Cathode_Chemistry %>% str_remove_all("-LFP|-NCA|-NMC|tba|-")) %>% 
  ggplot(aes(Cathode_Chemistry,Battery_Capacity))+
  geom_jitter(alpha=.5)+
  geom_boxplot(alpha=.7)+
  coord_flip()+
  labs(x="Cathode Chemistry",y="Battery \n Capacity \n [kWh]")
p2
f.fig.save(sprintf(fig_name,"Chem_BatSize2"))

p2+aes(y=Curbweight)+labs(y="Curbweight [kg]")
f.fig.save(sprintf(fig_name,"Chem_weight2"))

vehSpec %>% 
  filter(Cathode_Chemistry %in% chems) %>% 
  dplyr::select(Cathode_Chemistry,Curbweight,Battery_Capacity,range) %>% 
  group_by(Cathode_Chemistry) %>% 
  skimr::skim_without_charts()
# NCA seems to be used for heavier vehicles
# LFP seems to be used for smaller vehicles

p3 <- vehSpec %>% 
  filter(Cathode_Chemistry %in% chems) %>% 
  ggplot(aes(Battery_Capacity,fill=Cathode_Chemistry))+
  geom_density(alpha=.8)+
  labs(fill="Cathode Chemistry",y="Battery \n Capacity \n [kWh]")
p3

p3+aes(x=Curbweight)





# Regression Efficiency -------------
library(broom)
f.CIplot <- function(MODEL){
  coefficients_summary <- tidy(MODEL)
  
  # Step 3: Create a data frame
  coefficients_data <- data.frame(
    Variable = coefficients_summary$term,
    Estimate = coefficients_summary$estimate,
    Lower_CI = confint(MODEL)[,1],
    Upper_CI = confint(MODEL)[,2])
  
  # Step 4: Create the coefficient plot
  coefficients_data %>% 
    mutate(signif=sign(Lower_CI)==sign(Upper_CI)) %>% 
    ggplot( aes(x = Variable, y = Estimate)) +
    geom_linerange(aes(ymin = Lower_CI, ymax = Upper_CI)) +
    geom_point(aes(col=signif),size=3) +
    geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
    labs(x = "",
         y = "Coefficient Estimate") +
    scale_color_manual(values = c("black", "red"), labels = c(F, T))+
    coord_flip()+
    theme_bw(20)+
    theme(legend.position = "none")
  
}

# NA analysis - almost all are in the Efficiency column!
vehSpec %>% 
  filter(!is.na(efficiency)) %>% 
  summarize_all(~ mean(is.na(.))) %>%
  gather(key = "Column", value = "NA_Proportion") %>% arrange(desc(NA_Proportion))
sum(!is.na(vehSpec$efficiency))


## Correlations -------
vehSpec %>% dplyr::select(efficiency,Battery_Capacity,range,Curbweight,
                          efficiency_calc) %>% 
  # na.omit() %>% 
  # nrow()
  cor(use = "pairwise.complete.obs",method = "pearson")



## Simple Model ----------
df_veh <- vehSpec %>% filter(Modelyear>=2019) %>% 
  mutate(Modelyear=factor(Modelyear)) %>% 
  filter(Propulsion=="BEV") %>% 
  mutate(Curbweight_ton=Curbweight/1e3,
         Curbweight_100kg=Curbweight/1e2,
         Battery_Capacity_10kWh=Battery_Capacity/10,
         range_100km=range/100,
         Wheelbase_m=Wheelbase/1e3) %>% 
  mutate(footprint_m2=Length/1e3*Width/1e3) %>% 
  rename(energy_consumption=efficiency)


df_veh %>% filter(!is.na(energy_consumption)) %>% 
  filter(!is.na(Curbweight)) %>% filter(!is.na(range)) %>% 
  skimr::skim_without_charts()

# by body style
df_veh %>% filter(!is.na(energy_consumption)) %>% 
  filter(!is.na(Curbweight)) %>% filter(!is.na(range)) %>% 
  dplyr::select(range,Curbweight,Body_style2,Battery_Capacity,energy_consumption) %>% 
  group_by(Body_style2) %>% 
  skimr::skim_without_charts()

data_lm <-df_veh %>% filter(!is.na(energy_consumption)) %>% 
  filter(!is.na(Curbweight))
  # filter(!is.na(range))

data_lm %>% group_by(Cathode_Chemistry) %>% tally() %>% ungroup() %>% mutate(perc=n/sum(n))

mod_base <- lm(
  energy_consumption~
  # efficiency_calc~
                 # Battery_Capacity_10kWh+
    # Curbweight_100kg-1
                 Curbweight_100kg:Body_style2
                 # I(Curbweight_100kg^2)
                 # range_100km
                 # range_100km:Curbweight_100kg
                 # Body_style2
                 # footprint_m2
               , 
               data=df_veh)
nobs(mod_base)
summary(mod_base)
summary(mod_base)$coefficients
f.CIplot(mod_base)+ggtitle("Y: Energy consumption - kWh per 100km")
plot(mod_base)

# rapid ggplot
data_lm %>% 
  filter(Body_style2!="Mini"|energy_consumption<15) %>% 
  ggplot(aes(Curbweight,energy_consumption,col=Body_style2))+
  geom_point()+
  geom_smooth(method="lm",se=F,formula = "y~x")+
  labs(x="Curbweight [kg]",y="Energy \n Consumption \n [kWh/100km]",
       col="Body Style")
plot(data_lm$Curbweight, residuals(mod_base), xlab = "Curbweight", ylab = "Fitted Residuals",
     main = "Fitted Residuals vs Explanatory Variable")
abline(h = 0, col = "red", lty = 2)


# Boxcox procedure

library(MASS)

# Use the boxcox function to find the optimal lambda for transformation

boxcox_result <- boxcox(mod_base, lambda = seq(-2, 2, 0.1))
(optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)])
# -0.5: 1/sqrt(x)

# Apply the Box-Cox transformation to the response variable
data_lm$energy_consumption_transformed <- (data_lm$energy_consumption^optimal_lambda-1)/optimal_lambda
                                                
# Fit the transformed model
mod_transformed <- lm(
  energy_consumption_transformed ~ Curbweight_100kg,
  data = data_lm)
nobs(mod_transformed)
summary(mod_transformed)
plot(mod_transformed)

coefficients(mod_base)
# coef back to bla bla
(y=coefficients(mod_transformed))
exp(log(y*optimal_lambda+1)/optimal_lambda)

y=predict(mod_transformed,tibble(Curbweight_100kg=17.85))
y=exp(log(y*optimal_lambda+1)/optimal_lambda)
y2=predict(mod_transformed,tibble(Curbweight_100kg=17.85+1))
y2=exp(log(y2*optimal_lambda+1)/optimal_lambda)
y2-y



# Model for Battery Capacity
mod_cap <- lm(Battery_Capacity~+Curbweight_ton+footprint_m2+
                # Body_style2+
                range_100km
                # energy_consumption
              , 
               data=df_veh)
nobs(mod_cap) 
summary(mod_cap)
summary(mod_cap)$adj.r.squared
f.CIplot(mod_cap)+ggtitle("Y: Battery Capacity kWh")


mod1 <- lm(efficiency~Battery_Capacity+Cathode_Chemistry+Curbweight+top_speed+Body_style2+
             Wheelbase, 
           data=filter(vehSpec,Propulsion=="BEV"))
nobs(mod1) # 121, due to efficiency
summary(mod1)
f.CIplot(mod1)


mod2 <- lm(efficiency~Curbweight_kg+Wheelbase+range, 
           data=filter(vehSpec,Propulsion=="BEV"))
nobs(mod2) # 121, due to efficiency
summary(mod2)
f.CIplot(mod2)



# Regression  Capacity -------------

df_sales <- df %>% filter(Propulsion=="BEV") %>% 
  filter(Modelyear>2019) %>% 
  mutate(Curbweight_ton=Curbweight/1e3,
         range_100km=range/100) %>% 
  mutate(footprint_m2=Length/1e3*Width/1e3)

mod_cap <- lm(Battery_Capacity~range_100km+Curbweight_ton,
                # efficiency,
              weights = unit,
              data=df_sales)
nobs(mod_cap)
summary(mod_cap)
summary(mod_cap)$adj.r.squared
f.CIplot(mod_cap)+ggtitle("Y: Battery Capacity kWh. Weighted by sales")


mod1 <- lm(Battery_Capacity~
             Sales_Region+
             # Vehicle_Production_Region+
             # Global_Segment+
             range+
             Cathode_Chemistry+Curbweight_kg,
           weights = unit,
           data=df)
nobs(mod1)
mod1$weights %>% sum()/1e6

summary(mod1)

f.CIplot(mod1)

df$Wheelbase <- df$Wheelbase/1e3
mod2 <- lm(Battery_Capacity~
             range+Curbweight_kg+Wheelbase,
           weights = unit,
           data=df)
nobs(mod2)
summary(mod2)
f.CIplot(mod2)

# Model simple and forecast ------------

# Vehicle specifications
df_veh <- vehSpec %>% 
  filter(Modelyear>=2019) %>%
  mutate(Modelyear=factor(Modelyear)) %>% 
  filter(Propulsion=="BEV") %>% 
  mutate(Curbweight_ton=Curbweight/1e3,
         Curbweight_100kg=Curbweight/1e2,
         Battery_Capacity_10kWh=Battery_Capacity/10,
         range_100km=range/100,
         Wheelbase_m=Wheelbase/1e3) %>% 
  mutate(footprint_m2=Length/1e3*Width/1e3) %>% 
  rename(energy_consumption=efficiency)

mod_base <- lm(Battery_Capacity~Curbweight_100kg+range_100km+Cathode_Chemistry,
  # log(Battery_Capacity)~log(Curbweight_100kg)+log(range_100km),
               # +energy_consumption, #not for now
               # data=df_veh)
              data=filter(df_veh,Cathode_Chemistry %in% c("LFP","NMC","NCA")))

nobs(mod_base)
summary(mod_base)
summary(mod_base)$adj.r.squared #0.887
f.CIplot(mod_base)+ggtitle("Y: Battery Capacity")
coef(mod_base)


# Weighted by sales
df_sales <- df %>% filter(Propulsion=="BEV") %>% 
  filter(Modelyear>2019) %>% 
  mutate(Curbweight_ton=Curbweight/1e3,
         Curbweight_100kg=Curbweight/1e2,
         range_100km=range/100) %>% 
  mutate(footprint_m2=Length/1e3*Width/1e3) %>% 
  rename(energy_consumption=efficiency)

df_sales$energy_consumption %>% summary()
mod_cap <- lm(Battery_Capacity~Curbweight_100kg+range_100km+
                energy_consumption,
              weights = unit,
              data=df_sales)
nobs(mod_cap)
summary(mod_cap)
summary(mod_cap)$adj.r.squared # 0.959
f.CIplot(mod_cap)+ggtitle("Y: Battery Capacity kWh. Weighted by sales")
coef(mod_cap)


mod2<- lm(Battery_Capacity~Curbweight_100kg*range_100km,
              weights = unit,data=df_sales)
summary(mod2)
anova(mod_cap,mod2) # different, so interaction exists

## Figure of ranges ------------

# Create data
range(df_sales$Curbweight,na.rm = T)
range(df_sales$range,na.rm = T)

mod_cap <- mod2 # to include interaction
mod_cap <- mod_base # regression with veh specification
coefs <- cbind(coef(mod_cap),confint(mod_cap,level = 0.95))

intercept <- coef(mod_cap)["(Intercept)"]
slope_curbweight <- coef(mod_cap)["Curbweight_100kg"]
slope_range <- coef(mod_cap)["range_100km"]
slope_int <- coef(mod_cap)["Curbweight_100kg:range_100km"]

# Create a dataframe with ranges
new_data <- expand.grid(Curbweight = seq(500, 5000, by = 50),
                        Range = seq(100, 800, by = 100))

# Calculate predictions based on the provided slopes
# predictions <- intercept + slope_curbweight * new_data$Curbweight_100kg + 
#   slope_range * new_data$range_100km

#different slope based on curbweight

#calculate weighted mean for base rate
bc <- weighted.mean(df_sales$Battery_Capacity,df_sales$unit,na.rm=T)  
cw <- weighted.mean(df_sales$Curbweight,df_sales$unit,na.rm = T) 
rn <- weighted.mean(df_sales$range,df_sales$unit,na.rm = T) 

# for veh spec
vehSpec_aux <- df_veh %>% dplyr::select(Battery_Capacity,Curbweight,range) %>% 
  na.omit()
bc <- mean(vehSpec_aux$Battery_Capacity,na.rm=T)  
cw <- mean(vehSpec_aux$Curbweight,na.rm = T) 
rn <- mean(vehSpec_aux$range,na.rm = T) 

# slope assumed linear (constant)
var_int <- "Curbweight_100kg"
slope <- coefs[var_int,]

# create dataframe with points for lines
new_data <- new_data %>% 
  mutate(non_random_part=bc+
           (Range-rn)/100*coefs["range_100km",1],
           # (Curbweight-cw)/100*(Range-rn)/100*coefs["Curbweight_100kg:range_100km",1], #interaction
         y=non_random_part+slope[1]*(Curbweight-cw)/100, # Check: Simple linear slope starting from means
         y_low=non_random_part+slope[2]*(Curbweight-cw)/100,
         y_high=non_random_part+slope[3]*(Curbweight-cw)/100)

# Make predictions for the new data
# predictions <- predict(mod_cap, newdata = new_data, interval = "confidence", level = 0.95)
# Combine the predictions with the new_data dataframe
# result_df <- cbind(new_data, predictions)
# Rename the columns for clarity
# colnames(result_df) <- c("Curbweight_100kg", "range_100km", "Mean_Prediction", "Low_CI", "High_CI")

new_data %>%
  # mutate(Curbweight=Curbweight_100kg*100,Range=range_100km*100) %>% 
  filter(Range<700) %>% 
  filter(Curbweight>=1000 & Curbweight<=3000) %>% 
  mutate(Range=as.factor(Range)) %>% 
  ggplot(aes(Curbweight,y))+
  geom_line(aes(col=Range)) +
  geom_ribbon(aes(ymin = y_low, ymax = y_high,group=Range,fill=Range),alpha = 0.3) +
  coord_cartesian(expand = F)+
  labs(x = "Curbweight [kg]",y = "Battery \n Capacity \n [kWh]",
       fill="Range [km]",col="Range [km]",
       caption="Battery Capacity ~ Curbweight + Range. n=258. R2=0.89")

f.fig.save(sprintf(fig_name,"Predictions_Batsize"))

# COUNTRY LEVEL REGRESSION -------------

# Battery capacity declared vs sales
# Note that they may be a mistmacht between sales and catalogue
df %>% 
  filter(unit>50) %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% 
  # mutate(diff=kWh_veh-Battery_Capacity,
  #        abs_diff=abs(diff)) %>% arrange(desc(abs_diff)) %>% view()
  ggplot(aes(Battery_Capacity,kWh_veh))+
  geom_point()

df %>% 
  filter(unit>0,!is.na(range)) %>% 
  # filter(year>2018) %>% 
  mutate(Region=factor(Sales_Region)) %>% 
  ggplot(aes(range,fill=Region,group=Region,weights=unit))+
  geom_density(alpha=.5)+
  facet_wrap(~Propulsion,ncol=1,scales = "free")


library(ggridges)
prop="BEV"
prop="PHEV"
df %>% 
  filter(Propulsion==prop) %>%
  filter(Modelyear>2010) %>%
  filter(unit>0,!is.na(range)) %>% 
  mutate(Region=factor(Sales_Region)) %>%
  ggplot(aes(range,Region,fill=Region))+
  # facet_wrap(~key,scales = "free")+
  # geom_density_ridges(fill = "lightblue", alpha = 0.5)+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = c(0.5))+
  # geom_density(alpha=.8)+
  # scale_y_reverse()+
  # xlim(0,150)+
  labs(x="Range [km]",y="",fill="Region", 
       caption=paste0(prop," Sales 2021-2022"))
f.fig.save(sprintf(fig_name,paste0("Density_Range_",prop)))

segment_share <- df %>% 
  # filter(year==2022) %>% 
  filter(unit>0) %>% 
  filter(!is.na(Global_Segment)) %>% 
  mutate(Global_Segment=Global_Segment %>% 
           str_remove_all("-A|-B|-C|-D|-E|-F")) %>% 
  group_by(year,Sales_Region,Sales_Country,Global_Segment,Propulsion) %>% 
  reframe(x=sum(unit,na.rm=T)) %>% ungroup() %>% 
  group_by(year,Sales_Country) %>% 
  mutate(share_seg=x/sum(x)) %>% ungroup() %>%
  mutate(x=NULL) %>% 
  pivot_wider(names_from = Global_Segment, values_from = share_seg,
              values_fill = 0)

skimr::skim_without_charts(segment_share)

df_country <- df %>% 
  filter(unit>0) %>% 
  filter(!is.na(Global_Segment)) %>% 
  group_by(Sales_Region,Sales_Sub_Region,Sales_Country,
           year,Propulsion) %>% 
  reframe(Battery_Capacity=weighted.mean(Battery_Capacity,unit,na.rm = T),
          Curbweight=weighted.mean(Curbweight,unit,na.rm = T),
          efficiency=weighted.mean(efficiency,unit,na.rm = T),
          range=weighted.mean(range,unit,na.rm=T),
          unit=sum(unit),
          MWh=sum(MWh)) %>% ungroup() %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% 
  left_join(segment_share) %>% 
  mutate(t=as.numeric(year)-2009)

bat_country <- df_country %>% filter(unit>50)

# global average:
bat_country %>% filter(year==2022) %>% 
  group_by(Propulsion) %>% reframe(x=weighted.mean(kWh_veh,unit))
bat_country %>% group_by(Propulsion) %>% reframe(x=sum(unit)/1e6)

# average by category
df %>% 
  filter(year==2022) %>%
  mutate(Global_Segment=Global_Segment %>% 
           str_remove_all("-A|-B|-C|-D|-E|-F")) %>% 
  group_by(Global_Segment,Propulsion) %>% summarise(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% arrange(desc(unit))

# range
bat_country %>% group_by(Propulsion) %>% reframe(x=weighted.mean(range,unit))

mod <- lm(kWh_veh~
            # t+I(t^2)+
            # `SUV`+`LCV`+
            # `LSV`+# `PUP`+`QC`+# `SS`+
            # `MPV`+`Car`+
            # Curbweight,
            range,
            # Sales_Country,
          # Sales_Region,
          data=filter(bat_country,Propulsion=="BEV"),
          weights = unit)
# mod <- lm(kWh_veh~Sales_Country+t, 
#           data=bat_country)
nobs(mod)
summary(mod)
summary(mod)$adj.r.squared

mod_phev <- lm(kWh_veh~range,weights = unit,
               data=filter(bat_country,Propulsion=="PHEV"))
summary(mod_phev)

# SAVE DATA
write.csv(tibble(Propulsion=c("BEV","PHEV"),
                 Intercept=c(coefficients(mod)[1],coefficients(mod_phev)[1]),
                 Range_Slope=c(coefficients(mod)[2],coefficients(mod_phev)[2])),
          sprintf(url_save,"BatSizeLinearModel"),row.names = F)
write.csv(bat_country,sprintf(url_save,"Data_LinearModel"),row.names = F)


# scenarios
predict(mod,newdata = tibble(range=424))
predict(mod,newdata = tibble(range=600))
predict(mod,newdata = tibble(range=300))

# Predictions comparison
bat_country$pred <- predict(mod)
bat_country %>% filter(year==2022) %>% pull(pred) %>% range()
bat_country %>% filter(year==2022,Sales_Country=="USA") %>% pull(pred)

data_fig <- bat_country %>% 
  mutate(unit=unit/1e6) %>% 
  filter(year>2017)

data_label <- data_fig %>% filter(year==2022) %>% 
  filter(Sales_Country %in% c("China","USA","India","Germany"))

ggplot(data_fig,aes(range,kWh_veh,size=unit,col=Sales_Region))+
  geom_point(alpha=.5)+
  geom_point(data=data_label,col="darkgrey",shape=1)+
  geom_text_repel(data=data_label,aes(label=Sales_Country),show.legend = F,size=4)+
  # theme(legend.position = c(0.8,0.2))+
  labs(x="Range [km]",y="Battery \n Capacity \n [kWh]",
       col="Region",size="Sales \n [million units]",
       caption = paste0("Each dot is a country avg ",prop," fleet for a whole year (2018-2022)."))
f.fig.save(sprintf(fig_name,paste0("Country_Bat_Range_",prop)))


# Using USA stats
loc_names <- 13:19# columns where Car to SS goes
names(bat_country)[loc_names]
(usa <- bat_country %>% filter(Sales_Country=="USA",year==2022) %>% .[,loc_names])
bat_country2 <- bat_country
bat_country2[,loc_names] <- usa
bat_country$pred1 <- predict(mod,newdata = bat_country2)

# China
(china <- bat_country %>% filter(Sales_Country=="China",year==2022) %>% .[,loc_names])
bat_country3 <- bat_country
bat_country3[,loc_names] <- china
bat_country$pred2 <- predict(mod,newdata = bat_country3)

#High SUV - 70%
bat_country3 <- bat_country
bat_country3[,loc_names] <- list(0.2,0.05,0.02,0.7,0.03,0,0)
bat_country$pred3 <- predict(mod,newdata = bat_country3)

#High CAR - 70%
bat_country3 <- bat_country
bat_country3[,loc_names] <- list(0.7,0.05,0.02,0.2,0.03,0,0)
bat_country$pred4 <- predict(mod,newdata = bat_country3)

# Low Range 200 miles 
bat_country3 <- bat_country
bat_country3[,9] <- 200*1.6
bat_country$pred5 <- predict(mod,newdata = bat_country3)

# Mid Range 350 miles 
bat_country3 <- bat_country
bat_country3[,9] <- 350*1.6
bat_country$pred6 <- predict(mod,newdata = bat_country3)

# High Range 500 miles 
bat_country3 <- bat_country
bat_country3[,9] <- 500*1.6
bat_country$pred7 <- predict(mod,newdata = bat_country3)


bat_country %>% 
  filter(Sales_Country %in% c("USA","China","India","Germany")) %>% 
  dplyr::select(year,Sales_Country,kWh_veh,pred) %>% 
  pivot_longer(c(kWh_veh,pred), names_to = "key", values_to = "value") %>%
  # ggplot(aes(kWh_veh,pred))+
  # geom_point()+geom_abline(slope=1)
  ggplot(aes(year,value,fill=key)) +
  geom_col(position = "dodge")+
  facet_wrap(~Sales_Country)

# 2050 base on stats
c_special <-  c("USA","China","India","Germany","France","UK","South Korea","Japan",
                "Brazil")
bat_country %>% group_by(Sales_Country) %>% reframe(x=sum(unit)) %>% arrange(desc(x))

levels_scenario <- c("2022 Predictions",
  "Based on USA Fleet Share","Based on China Fleet Share",
  "70% SUV Fleet","70% Car Fleet",
  "200mi range","350mi range","500mi range")

bat_country %>% 
  filter(year==2022) %>% 
  # mutate(special=Sales_Country %in% c_special) %>% 
  filter(Sales_Country %in% c_special) %>% 
  pivot_longer(c(pred,pred1,pred2,pred3,pred4,pred5,pred6,pred7), 
               names_to = "key", values_to = "value") %>% 
  mutate(key=case_when(
    key=="pred1" ~ "Based on USA Fleet Share",
    key=="pred2" ~ "Based on China Fleet Share",
    key=="pred3" ~ "70% SUV Fleet",
    key=="pred4" ~ "70% Car Fleet",
    key=="pred5" ~ "200mi range",
    key=="pred6" ~ "350mi range",
    key=="pred7" ~ "500mi range",
    T ~"2022 Predictions") %>% factor(level=rev(levels_scenario))) %>% 
  ggplot(aes(Sales_Country,value,fill=key))+
  geom_col(position = "dodge")+
  coord_flip(expand = F)+
  guides(fill= guide_legend(reverse = TRUE))+
  labs(x="",y="Predicted Battery Capacity [kWh]",fill="")
# theme(legend.position = "none")

df %>% 
  # filter(year==2022) %>% 
  filter(Propulsion==prop) %>%
  filter(!is.na(Global_Segment)) %>% 
  mutate(Global_Segment=Global_Segment %>% 
           str_remove_all("-A|-B|-C|-D|-E|-F")) %>% 
  group_by(year,Global_Segment) %>% 
  reframe(x=sum(unit)) %>% ungroup() %>% 
  group_by(year) %>% 
  mutate(x=x/sum(x)) %>% ungroup() %>% 
  ggplot(aes(year,x,fill=Global_Segment,group=Global_Segment))+
  geom_area()+
  scale_y_continuous(labels=percent)+
  labs(x="",y="",fill="Segment",title="Global Share BEV")+
  coord_cartesian(expand = F)


df %>% 
  filter(Propulsion==prop) %>% 
  filter(!is.na(Global_Segment)) %>% 
  mutate(Global_Segment=Global_Segment %>% 
           str_remove_all("-A|-B|-C|-D|-E|-F")) %>% 
  group_by(year,Global_Segment,Sales_Region) %>% 
  reframe(x=sum(unit)) %>% ungroup() %>% 
  group_by(year,Sales_Region) %>% 
  mutate(x=x/sum(x)) %>% ungroup() %>% 
  ggplot(aes(year,x,fill=Global_Segment,group=Global_Segment))+
  geom_area()+
  facet_wrap(~Sales_Region)+
  scale_y_continuous(labels=percent)+
  labs(x="",y="",fill="Segment",title="Global Share BEV")+
  coord_cartesian(expand = F)


# EoF