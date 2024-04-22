# Battery Capacity Analysis
# Source of Data: EV Volumes
# Battery-Installation-Tracker-December-2022-tge-1.xlsx
# PBH August 2023
# Similar to MONET, but with different objectives

# 2 parts separated: Battery Size and Chemistry Share

# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

fig_name <- "Figures/Battery/%s.png"

bat <- read_excel("Data/EV Volumes_DB Battery.xlsx",sheet="Database")
(names(bat) <- names(bat) %>% str_remove("&") %>% str_replace_all(" |-","_") %>% 
    str_replace_all("__","_") %>% str_remove("Delivered_"))
# check
sum(bat$MWh_2018) # 74674

# select column of interest
bat <- bat %>% 
  dplyr::select(Sales_Region,Sales_Sub_Region,Sales_Country, Global_Segment,
                Vehicle_Production_Region,Vehicle_Production_Country,
                Propulsion,Cathode_Chemistry,Cathode_Mix,
                `2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,
                `2019`,`2020`,`2021`,`2022`,
                MWh_2010_earlier,MWh_2011,MWh_2012,MWh_2013,MWh_2014,MWh_2015,
                MWh_2016,MWh_2017,MWh_2018,MWh_2019,MWh_2020,MWh_2021,MWh_2022) %>% 
  rownames_to_column()

# totals by year
bat %>% mutate(x=1) %>% dplyr::select(x,MWh_2010_earlier,MWh_2011,MWh_2012,MWh_2013,MWh_2014,MWh_2015,
                                      MWh_2016,MWh_2017,MWh_2018,MWh_2019,MWh_2020,MWh_2021,MWh_2022) %>% 
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  group_by(key) %>% summarise(MWh=sum(value))

# totals EV by year
bat %>% mutate(x=1) %>% dplyr::select(x,`2011`,`2012`,`2013`,`2014`,`2015`,
                                      `2016`,`2017`,`2018`,`2019`,`2020`,`2021`,`2022`) %>% 
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  group_by(key) %>% summarise(MWh=sum(value,na.rm=T)/1e6)
  
# DATA WRANGLING -----

## Fix error of regions - Some countries are wrongly labelled
# which countries are labelled twice?
bat %>% group_by(Sales_Region,Sales_Sub_Region,Sales_Country) %>% tally() %>% 
  group_by(Sales_Country) %>% tally() %>% arrange(desc(n))
# Only Colombia it seems
bat %>% filter(Sales_Country=="Colombia") %>% pull(Sales_Region) %>% table()
bat %>% filter(Sales_Country=="Colombia") %>% pull(Sales_Sub_Region) %>% table()
bat <- bat %>% 
  mutate(Sales_Region=if_else(Sales_Country=="Colombia","Americas",Sales_Region))


## Aggregate Chemistry 2022-----
bat %>% group_by(Cathode_Chemistry) %>% summarise(x=sum(MWh_2022,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
bat %>% group_by(Cathode_Mix) %>% summarise(x=sum(MWh_2022,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
# NMC ratios: 721, 622, 811, 532, 111
# Others NMCA 89-4-4-3 (used by Tesla only) https://evreporter.com/nmca-cathode-for-lithium-ion-batteries/

# Mix aggregation
bat$Cathode_Mix %>% unique()
bat <- bat %>% 
  mutate(mix=case_when(
    str_detect(Cathode_Mix,"NMC 111") & Cathode_Chemistry!="LMO" ~ " 111",
    str_detect(Cathode_Mix,"NMC 721") ~ " 721",
    str_detect(Cathode_Mix,"NMC 532|NMC532|NMC 523") ~ " 532", # I assume 523 is a typo error
    str_detect(Cathode_Mix,"NMC 622|NMC622") ~ " 622",
    str_detect(Cathode_Mix,"NMC 442") ~ " 442",
    str_detect(Cathode_Mix,"NMC 811") ~ " 811",
    str_detect(Cathode_Mix,"NMCA 89:04:04:03") ~ "A 89:4:4:3",
    T ~ ""))
bat %>% group_by(Cathode_Chemistry,Cathode_Mix,mix) %>% tally() %>% arrange(desc(n))

bat <- bat %>% 
  mutate(chemistry=paste0(Cathode_Chemistry,mix))

# share of ratios of NMC
bat %>% 
  filter(chemistry %in% c("NMC","NMC 111","NMC 811","NMC 622","NMC 532")) %>% 
                          # "NMC 721","NMCA 89:4:4:3","NMC 442")) %>% 
  filter(Propulsion!="FCEV") %>% 
  # mutate(Sales_Region="World") %>%
  group_by(Propulsion,Sales_Region,chemistry) %>% 
  summarise(MWh_2021=sum(MWh_2021)) %>% ungroup() %>% 
  group_by(Propulsion,Sales_Region) %>% 
  mutate(perc=MWh_2021/sum(MWh_2021)) %>% 
  ggplot(aes(Sales_Region,perc,fill=chemistry))+
  geom_col()+
  coord_flip()+
  facet_wrap(~Propulsion,nrow=1)

# Do it by world shares of NMC, easier to explain. Only major difference will be Japan
# Do not distinguish by propulsion, BEV is dominant
nmc_share <- bat %>% 
  filter(chemistry %in% c("NMC 111","NMC 811","NMC 622","NMC 532")) %>%
  #   "NMC 721","NMCA 89:4:4:3" not considered to go towards NMC with no detail
  filter(Propulsion!="FCEV") %>% 
  group_by(chemistry) %>% 
  summarise(MWh_2021=sum(MWh_2021)) %>% ungroup() %>% 
  # group_by(Propulsion) %>% 
  mutate(perc=MWh_2021/sum(MWh_2021)) 


# aggregate chemistry - above 2% (for figure)
chem_selected <- bat %>% group_by(chemistry) %>% summarise(x=sum(MWh_2022,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100) %>% 
  filter(perc>2) %>% pull(chemistry)
chem_selected <- c(chem_selected,"NMC 111","LMO")
bat <- bat %>% mutate(chemistry=if_else(chemistry %in% chem_selected,chemistry,"Other"))
rm(chem_selected)

# other share
other_share <- bat %>% 
  filter(!(chemistry %in% c("NMC","Other"))) %>% 
  filter(Propulsion!="FCEV") %>% 
  group_by(chemistry) %>% 
  summarise(MWh_2021=sum(MWh_2021)) %>% ungroup() %>% 
  # group_by(Propulsion) %>% 
  mutate(perc=MWh_2021/sum(MWh_2021)) 

write.csv(nmc_share,"Results/Battery/NMC_share.csv",row.names = F)
write.csv(other_share,"Results/Battery/OtherBattery_share.csv",row.names = F)


## Flat years -----
bat <- bat %>% 
  pivot_longer(c(`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,
                 `2019`,`2020`,`2021`,`2022`,
                 MWh_2010_earlier,MWh_2011,MWh_2012,MWh_2013,MWh_2014,MWh_2015,
                 MWh_2016,MWh_2017,MWh_2018,MWh_2019,MWh_2020,MWh_2021,MWh_2022), 
               names_to = "year", values_to = "value") %>% 
  mutate(unit=if_else(str_detect(year,"MWh"),"MWh","unit")) %>% 
  mutate(year=year %>% str_remove_all("MWh_|_earlier")) %>% 
  pivot_wider(names_from = unit, values_from = value,values_fill = 0) %>% 
  mutate(unit = if_else(is.na(unit), 0, unit))
# Checks
bat %>% group_by(year) %>% summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit)

## orders -----

bat$Sales_Region %>% table()
region_order <- c("Americas","Europe","Asia-Pacific","Africa & ME")
bat <- bat %>% mutate(Sales_Region=factor(Sales_Region,levels=region_order))

chem_order <- bat %>% group_by(chemistry) %>% 
  summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% arrange(kWh_veh) %>% pull(chemistry)
bat <- bat %>% mutate(chemistry=factor(chemistry,levels=chem_order))

## Dimensions -----
bat$Sales_Region %>% unique()
bat$Sales_Sub_Region %>% unique() # 13
bat$Sales_Country %>% unique() # 116
bat$year %>% unique()
bat$Propulsion %>% unique() # "PHEV" "BEV"  "FCEV"
bat$chemistry %>% unique()


# Distribute NMC and Other as averages
nmc_share
bat_nmc <- bat %>% filter(chemistry=="NMC") %>% rename(x=chemistry)
# bat_nmc %>% filter(year==2022) %>%  group_by(Propulsion,year) %>% summarise(MWh=sum(MWh,na.rm=T))
bat_nmc <- bat_nmc %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="NMC",MWh_2021=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)
# bat_nmc %>% filter(year==2022) %>%  group_by(Propulsion,year) %>% summarise(MWh=sum(MWh,na.rm=T))
# add back to database
bat <- bat %>% filter(chemistry!="NMC") %>% rbind(bat_nmc)
rm(bat_nmc)

# others share - same method
other_share
bat_other <- bat %>% filter(chemistry=="Other") %>% rename(x=chemistry)
bat_other <- bat_other %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="Other",MWh_2021=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)
bat <- bat %>% filter(chemistry!="Other") %>% rbind(bat_other)
rm(bat_other)

bat$chemistry %>% unique()

# Aggregate data -----

# First we need to match country and regions names
# Key idea: generate output files using the ICCT country and regions names

## World level ----------
bat_world <- bat %>% 
  filter(year==2022) %>% 
  group_by(Propulsion,year,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_world <- bat_world %>% 
  filter(Propulsion!="FCEV") %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(Propulsion,year) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(Propulsion,year) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()

bat_world <- bat_world %>% rename(Powertrain=Propulsion,Year=year)

## Region level -----------

eq_region <- read_excel("Data/Eq_Countries_ICCT_EVV.xlsx",sheet="Eq_Region")

## Region -----
eq_region$EVV_Region <- NULL
bat_region <- bat %>% left_join(eq_region,by=c("Sales_Sub_Region"="EVV_SubRegion"))

bat_region <- bat_region %>% 
  # filter(year==2022) %>% 
  filter(!is.na(ICCT_Region)) %>% 
  group_by(Propulsion,year,ICCT_Region,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()
bat_region$ICCT_Region %>% unique() # 18, Rest of the World uses world avg.

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_region <- bat_region %>% 
  filter(Propulsion!="FCEV") %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(Propulsion,year,ICCT_Region) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(Propulsion,year,ICCT_Region) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()
#check
bat_region %>% group_by(Propulsion,year,ICCT_Region) %>% 
  summarise(sum(share_units))

bat_region <- bat_region %>% rename(Region=ICCT_Region,
                                    Powertrain=Propulsion,
                                    Year=year)

# Country level ----------
# load dictionary
eq <- read_excel("Data/Eq_Countries_ICCT_EVV.xlsx",sheet="Eq_Country2")

bat_country <- bat %>% left_join(eq,by=c("Sales_Country"="EVV_Country")) %>% 
  filter(!is.na(ICCT_Country))

# historical EV sales for stock model
ev_historical <- bat_country %>% 
  filter(year>2014) %>% 
  group_by(Propulsion,year,ICCT_Country,chemistry) %>% 
  summarise(unit=sum(unit,na.rm=T)) %>% ungroup()
ev_historical %>% group_by(year) %>% reframe(unit=sum(unit)/1e6)
write.csv(ev_historical,"Results/Intermediate Results/historicalEV_sales.csv",
          row.names = F)

bat_country <- bat_country %>% 
  filter(year==2022) %>% 
  group_by(Propulsion,year,ICCT_Country,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_country <- bat_country %>% 
  filter(Propulsion!="FCEV") %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(Propulsion,year,ICCT_Country) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(Propulsion,year,ICCT_Country) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()

bat_country <- bat_country %>% rename(Country=ICCT_Country,
                                      Powertrain=Propulsion,
                                      Year=year)

# SAVE DATA -----------

bat_region2 <- bat_region %>% filter(Year==2022)

write.csv(bat_world,"Results/Battery/battery_world.csv",row.names = F)
write.csv(bat_region2,"Results/Battery/battery_region.csv",row.names = F)
write.csv(bat_country,"Results/Battery/battery_country.csv",row.names = F)

## Save battery size -----
bat_world_size <- bat_world %>% group_by(Powertrain,Year) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_world_size,"Results/Battery/battery_size_world.csv",row.names = F)

bat_region_size <- bat_region %>% group_by(Powertrain,Year,Region) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_region_size,"Results/Battery/battery_size_region.csv",row.names = F)

bat_country_size <- bat_country %>% group_by(Powertrain,Year,Country) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_country_size,"Results/Battery/battery_size_country.csv",row.names = F)

## Save Chem Share ----------
bat_world_chem <- bat_world %>% group_by(Powertrain,Year,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>% group_by(Powertrain,Year) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_world_chem,"Results/Battery/battery_chem_world.csv",row.names = F)

bat_region_chem <- bat_region %>% group_by(Powertrain,Year,Region,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>%  group_by(Powertrain,Year,Region) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_region_chem,"Results/Battery/battery_chem_region.csv",row.names = F)

bat_country_chem <- bat_country %>% group_by(Powertrain,Year,Country,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>%  group_by(Powertrain,Year,Country) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_country_chem,"Results/Battery/battery_chem_country.csv",row.names = F)


# FIGURES ----

# Bat size
bat_region %>% 
  filter(Powertrain=="BEV",Year==2022) %>% 
  mutate(lab_kwh=paste0(round(kwh_veh_total,0),"")) %>% 
  ggplot(aes(reorder(Region,kwh_veh_total),kwh_veh_total))+
  geom_col(fill="brown")+
  geom_text(aes(label=lab_kwh),nudge_y = 3,size=20*5/14 * 0.8)+
  coord_flip(expand = T)+
  labs(x="",y="Average Battery Size per BEV [kWh]")
f.fig.save("Figures/BatterySize2022.png")


chem_levels <- c("NMC 111","NMC 532","NMC 622","NMC 721","NMC 811","NMCA 89:4:4:3",
                 "NCA","LFP","LMO")
bat_region <- bat_region %>% mutate(chemistry=factor(chemistry,chem_levels))


bat_region %>% 
  filter(Year==2022) %>% 
  mutate(kwh_veh=MWh/unit*1e3) %>% # calculate again as it is dodge 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  mutate(chemistry=factor(chemistry,levels=chem_order)) %>% 
  ggplot(aes(Region,kwh_veh,fill=chemistry))+
  geom_col(position = "dodge")+
  facet_grid(chemistry~Powertrain)+
  coord_flip(expand = F)+
  labs(x="",y="kWh Battery Capacity per Light Duty Vehicle in 2022",fill="")+
  scale_fill_viridis_d()+
  theme_bw(14)+ 
  theme(panel.grid.major = element_blank(),
        legend.position = "none")

f.fig.save(sprintf(fig_name,"Bat_Overall"))

## Size over time
bat_region %>% 
  filter(Powertrain=="BEV") %>% 
  mutate(Year=as.Date(paste0(Year,"-01-01"))) %>% 
  mutate(kwh_veh=MWh/unit*1e3) %>% # calculate again as it is dodge 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  mutate(chemistry=factor(chemistry,levels=chem_order)) %>% 
  ggplot(aes(Year,kwh_veh,col=chemistry,group=chemistry))+
  geom_line(linewidth=0.4)+
  facet_wrap(~Region)+
  labs(x="",y="kWh Battery Capacity per Light Duty Vehicle in 2022",col="Chemistry")+
  scale_fill_viridis_d()+
  theme_bw(20)+ 
  theme(panel.grid.major = element_blank(),
        legend.position = "right")

f.fig.save(sprintf(fig_name,"Bat_Time"))

## Chemistry stacked -----
prop="BEV"

chem_level <- c("LFP","LMO","NCA","NMC 111","NMC 532",
                "NMC 622","NMC 721","NMC 811","NMCA 89:4:4:3")

bat_region %>% 
  filter(Powertrain==prop) %>% 
  filter(Year=="2022") %>% 
  # Show EV Volumnes regions
  left_join(eq_region,by=c("Region"="ICCT_Region")) %>% mutate(Region=EVV_SubRegion) %>% 
  distinct(Region,chemistry,.keep_all = T) %>% 
# end show
  mutate(chemistry=factor(chemistry,levels=chem_level)) %>% 
  ggplot(aes(reorder(Region,kwh_veh_total),kwh_veh,fill=fct_rev(chemistry)))+
  geom_col(position = "stack")+
  # facet_wrap(~Propulsion,scales = "free_y",dir = "v")+
  coord_flip(expand = F)+
  labs(x="",y=paste0("kWh Battery Capacity per ",prop," Vehicle"),fill="Battery \nChemistry \nShare")+
  scale_fill_viridis_d(option="turbo",direction = -1)+
  # tidytext::scale_x_reordered()+
  theme_bw(10)+ 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=8))+
  guides(fill = guide_legend(reverse = T,byrow = T))

f.fig.save(sprintf(fig_name,paste0(prop,"_stackChem")))

# Wolrd averages by chem
bat_world %>% 
  filter(Powertrain=="BEV") %>% 
  mutate(perc=MWh/sum(MWh)) %>% 
  dplyr::select(chemistry,perc) %>% arrange(desc(perc))


bat_country %>% 
  filter(Powertrain==prop) %>% 
  filter(Year=="2022") %>% 
  filter(Country %in% c(region_level)) %>% 
  ggplot(aes(reorder(Country,kwh_veh_total),kwh_veh,fill=fct_rev(chemistry)))+
  geom_col(position = "stack")+
  # facet_wrap(~Propulsion,scales = "free_y",dir = "v")+
  coord_flip(expand = F)+
  labs(x="",y=paste0("kWh Battery Capacity per ",prop," Vehicle"),fill="Battery Chemistry Share")+
  scale_fill_viridis_d(option="turbo")+
  # tidytext::scale_x_reordered()+
  theme_bw(20)+ 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(reverse = T,byrow = T))


# EoF