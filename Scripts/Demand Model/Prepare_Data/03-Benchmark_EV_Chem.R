# Load and pre-process EV Chemistry demand forecast
# Data comes from Benchmark Mineral Intelligence
# PBH September 2023

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data --------------
df <- read_excel("Data/Demand Model/Lithium-ion-Battery-Database-Q2-2023-1.xlsx",
                  sheet="EV - Demand",range="C46:AE57")
names(df) <- c("chemistry","type","Unit",2015:2040)
head(df)

# Data Manipulation ----

# flat
df <- df %>% dplyr::select(-Unit,-type) %>% 
  pivot_longer(c(-chemistry), names_to = "year", values_to = "share") %>% 
  mutate(year=as.numeric(year))

df <- df %>% filter(chemistry!="NCM") %>%  #NMC is repeating (or sum over) all the different ratios, like NMC 811 
  mutate(chemistry=chemistry %>% 
           str_replace("NCM 523","NMC 532") %>%
           str_replace_all("NCM","NMC"))

# filter chemistry
chems <- df %>% group_by(chemistry) %>% 
  reframe(share=sum(share)) %>% 
  filter(share>0.1) %>% pull(chemistry)


# figure
df %>% 
  filter(year>2021) %>% 
  filter(chemistry %in% chems) %>% 
  ggplot(aes(year,share,fill=chemistry))+
  geom_area()+
  scale_fill_viridis_d(option = "turbo")+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry",
       caption="Source: Benchmark Mineral Intelligence")

## NEED TO MAKE COMMON CHEM SCALE, SEE BATTERY PLOT

f.fig.save("Figures/Battery/EV_Bench_Battery.png")

# Compare to EV Volumes 2022
evVol <- read.csv("Parameters/Battery/battery_world.csv")

evVol %>% filter(Powertrain=="BEV") %>% 
  mutate(x=1) %>% 
  # filter(chemistry %in% chems) %>% 
  ggplot(aes(x,y=share_units,fill=chemistry))+
  geom_col()+
  scale_fill_viridis_d(option = "turbo")+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x="EV Volumes 2022",y="")+
  theme(axis.text.x = element_blank())

# Forecasts -----
# Based on principle of proportional growth among countries

# equivalency of chemistry categories
(x=unique(df$chemistry))
(y=unique(evVol$chemistry))

match(x,y) # missing: LCO, LMNO, Other
match(y,x) # missing: NMC 721, NMCA 89:4:4:3 - add to NMC 811 growth

## extend to Benchmark to 2050 ------ 

# avg growth of last 5 years
avg5 <- df %>% 
  group_by(chemistry) %>% 
  filter(year >= 2035 & year <= 2040) %>%
  summarise(x = mean((share / lag(share) - 1) * 100,na.rm=T)) 
avg5[avg5$chemistry=="LCO",2] <- 0 # LCO is 0
avg5

# Fill in the dataframe with the projected values
df_aux <- df %>% filter(year==2040) # keeps last values
df_ext <- df
for (y in 2041:2050) {
  df_aux <- df_aux %>% mutate(year=y)
  df_ext <- rbind(df_ext,df_aux)
}
df_ext <- df_ext %>% filter(year>2040) %>% 
  left_join(avg5) %>% 
  mutate(share=share*(1 + x / 100)^(year-2040)) %>% 
  mutate(x=NULL)
# re-normalize to 1
df_ext <- df_ext %>% group_by(year) %>% mutate(share=share/sum(share)) %>% ungroup()
df_ext %>% group_by(year) %>% reframe(sum(share)) # check

df_ext <- rbind(df,df_ext)

#figure again
df_fig <- df_ext %>% 
  filter(year>2021) %>% 
  filter(chemistry %in% chems) %>%
  mutate(proj=year>2040)
ggplot(df_fig,aes(year,share,fill=chemistry))+
  geom_area(aes(alpha=proj))+
  geom_area(data=filter(df_fig,year>2039,year<2042),alpha=0.7,linewidth=0.01)+ # fill missing gap behind
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_alpha_manual(values=c(1,0.7))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry",
       caption = "Source: Benchmark Mineral Intelligence. \n 2040-2050 projection based on 2035-2040 avg. growth")
f.fig.save("Figures/Battery/EV_Bench_Battery_Ext.png")

# Project share up to 2070 by repeating 2050
range(df_ext$year)
df_aux <- df_ext %>% filter(year==2050)
for (y in 2051:2070) {
  df_aux <- df_aux %>% mutate(year=y)
  df_ext <- rbind(df_ext,df_aux)
}



## Project EV volumes 2022 using benchmark forecast -----
# get proportional increase relative to 2022 for Benchmark 
bench2022 <- df_ext %>% filter(year==2022) %>% mutate(year=NULL) %>% rename(b2022=share)
bench2022[bench2022$chemistry=="LMNO",2] <- df_ext %>% filter(year==2023,chemistry=="LMNO") %>% pull(share)

bench <- df_ext %>% filter(year>2021) %>%
  left_join(bench2022) %>% 
  mutate(ratio2022=if_else(b2022==0,0,share/b2022)) %>% 
  rename(chem_eq=chemistry) %>% 
  mutate(share=NULL,b2022=NULL)

# PROJECT EV volumes share
evVol <- read.csv("Parameters/Battery/battery_world.csv")
evVol <- evVol %>% 
  mutate(chem_eq=if_else(chemistry %in% c("NMC 721","NMCA 89:4:4:3"),"NMC 811",chemistry)) %>% 
  left_join(bench,relationship = "many-to-many") %>% 
  mutate(share_units=share_units*ratio2022,Year=NULL,chem_eq=NULL,ratio2022=NULL,
         MWh=NULL,unit=NULL,kwh_veh=NULL,kwh_veh_total=NULL)

# normalize to 1
evVol <- evVol %>% group_by(Powertrain,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol,"Parameters/Battery/Chemistry_Scenarios/bat_share_2050_world.csv",row.names = F)

evVol %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")
f.fig.save("Figures/Battery/EV_Bench_Battery_Adjusted.png")

## Region wise
evVol_region <- read.csv("Parameters/Battery/battery_region.csv")
evVol_region <- evVol_region %>% 
  mutate(chem_eq=if_else(chemistry %in% c("NMC 721","NMCA 89:4:4:3"),"NMC 811",chemistry)) %>% 
  left_join(bench,relationship = "many-to-many") %>% 
  mutate(share_units=share_units*ratio2022,Year=NULL,chem_eq=NULL,ratio2022=NULL,
         MWh=NULL,unit=NULL,kwh_veh=NULL,kwh_veh_total=NULL) %>%
  group_by(Powertrain,Region,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_region,"Parameters/Battery/Chemistry_Scenarios/bat_share_2050_region.csv",row.names = F)

evVol_region %>% 
  filter(year<2051) %>% 
  filter(Powertrain=="BEV") %>% 
  filter(Region %in% c("United States","China","Brazil","European Union",
                       "India","Japan")) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")+
  theme(panel.spacing.x = unit(0.75, "cm"))

f.fig.save("Figures/Battery/EV_Bench_Battery_AdjustedRegion.png")

## Country Wise
evVol_Country <- read.csv("Parameters/Battery/battery_Country.csv")
evVol_Country <- evVol_Country %>% 
  mutate(chem_eq=if_else(chemistry %in% c("NMC 721","NMCA 89:4:4:3"),"NMC 811",chemistry)) %>% 
  left_join(bench,relationship = "many-to-many") %>% 
  mutate(share_units=share_units*ratio2022,Year=NULL,chem_eq=NULL,ratio2022=NULL,
         MWh=NULL,unit=NULL,kwh_veh=NULL,kwh_veh_total=NULL) %>%
  group_by(Powertrain,Country,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_Country,"Parameters/Battery/Chemistry_Scenarios/bat_share_2050_Country.csv",row.names = F)

evVol_Country %>% 
  filter(year<2051) %>% 
  filter(Powertrain=="BEV") %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Country)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

# better to use regionwise projections
# f.fig.save("Figures/EV_Bench_Battery_AdjustedCountry.png")

# Battery Chemistry Scenarios -----------
# Key idea: set target goals and achieve them proportionally through time

## LFP doubles ------------

# Make LFP twice as big by 2050, then maintain
exp_fact <- evVol %>% filter(year==2050,chemistry=="LFP") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
expansion_df <- expand.grid(Powertrain=c("BEV","PHEV"),year=2022:2070) %>% 
  left_join(exp_fact) %>% group_by(Powertrain) %>% 
  mutate(exp_factor=if_else(year<2051,
                            1+(exp_factor-1)/28*(year-2022),
                            1+(exp_factor-1)/28*(2050-2022))) %>% # 28 years between 2022 to 2050
  ungroup() 

# add to original
evVol_LFP <- evVol %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="LFP",exp_factor,1)) %>% # expand only LFP
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  group_by(Powertrain,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_LFP %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_LFP,"Parameters/Battery/Chemistry_Scenarios/LFP_scen_world.csv",row.names = F)

evVol_LFP %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_AdjustedLFP.png")

## regional
# Make LFP twice as big by 2050
exp_fact <- evVol_region %>% filter(year==2050,chemistry=="LFP") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
expansion_df <- expand.grid(Powertrain=c("BEV","PHEV"),year=2022:2070,
                            Region=unique(exp_fact$Region)) %>% 
  left_join(exp_fact) %>% group_by(Powertrain,Region) %>% 
  mutate(exp_factor=if_else(year<2051,
                            1+(exp_factor-1)/28*(year-2022),
                            1+(exp_factor-1)/28*(2050-2022))) %>% # 28 years between 2022 to 2050
  ungroup()

# add to original
evVol_LFP <- evVol_region %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="LFP",exp_factor,1)) %>% # expand only LFP
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  group_by(Powertrain,Region,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_LFP %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_LFP,"Parameters/Battery/Chemistry_Scenarios/LFP_scen_region.csv",row.names = F)

evVol_LFP %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  filter(Region %in% c("United States","China","Brazil","European Union",
                       "India","Japan")) %>%
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_AdjustedLFPRegion.png")

## NMC 811 Doubles --------------
# Make NMC twice as big by 2050
exp_fact <- evVol %>% filter(year==2050,chemistry=="NMC 811") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
expansion_df <- expand.grid(Powertrain=c("BEV","PHEV"),year=2022:2070) %>% 
  left_join(exp_fact) %>% group_by(Powertrain) %>% 
  mutate(exp_factor=if_else(year<2051,
                            1+(exp_factor-1)/28*(year-2022),
                            1+(exp_factor-1)/28*(2050-2022))) %>% # 28 years between 2022 to 2050
  ungroup() 

# add to original
evVol_NMC <- evVol %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="NMC 811",exp_factor,1)) %>% # expand only LFP
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  group_by(Powertrain,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_LFP %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/NMC811_scen_world.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_AdjustedNMC811.png")

## regional
# Make LFP twice as big by 2050
exp_fact <- evVol_region %>% filter(year==2050,chemistry=="NMC 811") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
expansion_df <- expand.grid(Powertrain=c("BEV","PHEV"),year=2022:2070,
                            Region=unique(exp_fact$Region)) %>% 
  left_join(exp_fact) %>% group_by(Powertrain,Region) %>% 
  mutate(exp_factor=if_else(year<2051,
                            1+(exp_factor-1)/28*(year-2022),
                            1+(exp_factor-1)/28*(2050-2022))) %>% # 28 years between 2022 to 2050
  ungroup()

# add to original
evVol_NMC <- evVol_region %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="NMC 811",exp_factor,1)) %>% # expand only LFP
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  group_by(Powertrain,Region,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_LFP %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/NMC811_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  filter(Region %in% c("United States","China","Brazil","European Union",
                       "India","Japan")) %>%
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_AdjustedNMC811Region.png")

## Solid State Adoption --------------
# Half of NMC goes towards solid state, same cathodes
# Electrolyte LPS: Li3PS4
# Anode: Li metal

# Start at 2030, reach half of share by 2040, then half for rest of period
share_difussion <- tibble(year=2022:2070,
       share_multiplier=c(rep(0,7),
                          seq(0,1/2,length.out=12),
                          rep(0.5,10),
                          rep(0.5,20)))
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol %>% filter(str_detect(chemistry,"NMC|NCA")) %>% 
  mutate(chemistry=paste0("SS ",chemistry)) %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol %>%
  left_join(share_difussion) %>% 
  mutate(share_units=if_else(str_detect(chemistry,"NMC|NCA"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(Powertrain,year) %>% mutate(share_units=share_units/sum(share_units))

evVol_NMC %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/SolidState_scen_world.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_SolidState.png")

## regional
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol_region %>% filter(str_detect(chemistry,"NMC|NCA")) %>% 
  mutate(chemistry=paste0("SS ",chemistry)) %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol_region %>% 
  left_join(share_difussion) %>%
  mutate(share_units=if_else(str_detect(chemistry,"NMC|NCA"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(Powertrain,Region,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/SolidState_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  filter(Region %in% c("United States","China","Brazil","European Union",
                       "India","Japan")) %>%
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_SolidStateRegion.png")

## Sodium Batteries Adoption --------------
# Half of LFP goes towards Sodium based battery
# Electrolyte based on sodium: NaPF6
# Cathode active material: NaCu(1/3)Fe(1/3)Mn(1/3)O2
# Anode:	Hard Carbon

# Start at 2030, reach half of share by 2040, then half for rest of period
share_difussion <- tibble(year=2022:2070,
                          share_multiplier=c(rep(0,7),
                                             seq(0,1/2,length.out=12),
                                             rep(0.5,10),
                                             rep(0.5,20)))
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol %>% filter(str_detect(chemistry,"LFP")) %>% 
  mutate(chemistry="SIB") %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol %>%
  left_join(share_difussion) %>% 
  mutate(share_units=if_else(str_detect(chemistry,"LFP"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(Powertrain,year) %>% mutate(share_units=share_units/sum(share_units))

evVol_NMC %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/Sodium_scen_world.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_Sodium.png")

## regional
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol_region %>% filter(str_detect(chemistry,"LFP")) %>% 
  mutate(chemistry="SIB") %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol_region %>% 
  left_join(share_difussion) %>%
  mutate(share_units=if_else(str_detect(chemistry,"LFP"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(Powertrain,Region,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_NMC,"Parameters/Battery/Chemistry_Scenarios/Sodium_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(Powertrain=="BEV") %>% 
  filter(year<2051) %>% 
  filter(Region %in% c("United States","China","Brazil","European Union",
                       "India","Japan")) %>%
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

f.fig.save("Figures/Battery/EV_Bench_Battery_SodiumRegion.png")


# EoF 