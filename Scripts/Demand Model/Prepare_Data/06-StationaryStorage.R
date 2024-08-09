# Load and pre-process Stationary Storage Data
# Data comes from Benchmark Mineral Intelligence
# PBH September 2023

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data --------------
# in MWh
# Both: Grid Level Storage and Behind the Meter Storage included
ess <- read_excel("Data/Demand Model/Lithium-ion-Battery-Database-Q2-2023-1.xlsx",
                  sheet="ESS - Demand",range="C17:AD28")
names(ess) <- c("chemistry","Unit",2015:2040)
head(ess)

region_share <- read_excel("Data/Demand Model/Lithium-ion-Battery-Database-Q2-2023-1.xlsx",
                           sheet="ESS - Demand",range="C64:AD70")
names(region_share) <- c("Region","share",2015:2040)
region_share <- region_share %>% mutate(Region=str_replace(Region,"Oceania","Other"))

# Data Manipulation ----

# flat
ess <- ess %>% dplyr::select(-Unit) %>% 
  pivot_longer(c(-chemistry), names_to = "year", values_to = "stationaryPower") %>% 
  mutate(year=as.numeric(year))

ess <- ess %>% filter(chemistry!="NCM") %>%  #NMC is repeating (or sum over) all the different ratios, like NMC 811 
  mutate(chemistry=chemistry %>% 
           str_replace("NCM 523","NMC 532") %>%
           str_replace_all("NCM","NMC"))

# add Benchmark regional share
region_share <- region_share %>% dplyr::select(-share) %>% 
  pivot_longer(c(-Region), names_to = "year", values_to = "share") %>% 
  mutate(year=as.numeric(year))
region_share %>% group_by(year) %>% reframe(sum(share))
# region names

ess <- ess %>% left_join(region_share,relationship = "many-to-many")
ess <- ess %>% mutate(stationaryPower=stationaryPower*share,share=NULL)

# Extend towards 2050 ----

# get last 10 years average growth total
ess_total <- ess %>% group_by(year) %>% reframe(x=sum(stationaryPower))
# avg last 10 years
avg_10 <- ess_total %>% 
  filter(year >= 2030 & year <= 2040) %>%
  summarise(x = mean((x / lag(x) - 1) * 100,na.rm=T)) %>% 
  pull(x)
# around 6 %

# Fill in the dataframe with the projected values
ess_aux <- ess %>% filter(year==2040) # keeps last values
for (y in 2041:2050) {
  # Calculate the projected value based on the average relative growth
  ess_aux$stationaryPower <- ess_aux$stationaryPower * (1 + avg_10 / 100)
  ess_aux <- ess_aux %>% mutate(year=y)
  ess <- rbind(ess,ess_aux)
}

# Extend to 2070 by 1%
ess_aux <- ess %>% filter(year==2050)
for (y in 2051:2070) {
  ess_aux$stationaryPower <- ess_aux$stationaryPower * (1 + 1 / 100)
  ess_aux <- ess_aux %>% mutate(year=y)
  ess <- rbind(ess,ess_aux)
}


ess %>% group_by(year) %>% reframe(x=sum(stationaryPower)) %>% arrange(desc(x))

ess <- ess %>% rename(Year=year) %>% 
  filter(Year>2021)
ess$Year %>% range()





 
# Dissagregate based on Generation Share of 2022 ----
source("Scripts/Demand Model/Prepare_Data/05-ElectGeneration.R", encoding = "UTF-8")

# match totals
ess %>% filter(Year %in% c(2022,2050)) %>% group_by(Year) %>% reframe(sum(stationaryPower))
gen$electGen_TWh <- NULL
#join
ess <- ess %>% 
  left_join(gen,relationship = "many-to-many",
            by=c("Region"="Benchmark_Region")) %>% 
  mutate(stationaryPower=stationaryPower*share_gen,
         share_gen=NULL,Region=NULL)
ess %>% filter(Year %in% c(2022,2040,2050)) %>% group_by(Year) %>% reframe(sum(stationaryPower))

# Save data ----
write.csv(ess,"Results/stationaryPower.csv",row.names = F)

# Figure -----

ess_fig <- ess %>% 
  group_by(Year,chemistry) %>% 
  reframe(stationaryPower=sum(stationaryPower)) %>% ungroup() %>% 
  filter(stationaryPower>0) %>% 
  filter(Year<2051) %>%
  mutate(proj=Year>2040)

ess_fig %>% 
  ggplot(aes(Year,stationaryPower,fill=chemistry))+
  geom_area(aes(alpha=proj))+
  geom_area(data=filter(ess_fig,Year>2039,Year<2042),alpha=0.7,linewidth=0.01)+ # fill missing gap behind
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_alpha_manual(values=c(1,0.7))+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  labs(x="",y="MWh",fill="Battery Chemistry",caption = "2040-2050 projection based on 2030-2040 avg. growth")+
  guides(alpha = FALSE)

f.fig.save("Figures/SPS/SPS.png")
# f.fig.save("Figures/SPS/SPS2070.png")


# by region
ess_fig <- ess %>% 
  group_by(Year,ICCT_Region) %>% 
  reframe(stationaryPower=sum(stationaryPower)) %>% ungroup() %>% 
  filter(stationaryPower>0) %>% 
  mutate(proj=Year>2040) %>% 
  filter(Year<2051) %>% 
  mutate(ICCT_Region=factor(ICCT_Region,levels=region_level))
  

ess_fig %>% 
  ggplot(aes(Year,stationaryPower,fill=ICCT_Region))+
  geom_area(aes(alpha=proj))+
  geom_area(data=filter(ess_fig,Year>2039,Year<2042),alpha=0.7,linewidth=0.01)+ # fill missing gap behind
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_alpha_manual(values=c(1,0.7))+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = region_colors) +
  labs(x="",y="MWh",fill="Battery Chemistry",caption = "2040-2050 projection based on 2030-2040 avg. growth")+
  guides(alpha = FALSE)

f.fig.save("Figures/SPS/SPS_region.png")

# Scenario Sodium Battery adoption -----
# Half of LFP goes towards Sodium based battery
# Electrolyte based on sodium: NaPF6
# Cathode active material: NaCu(1/3)Fe(1/3)Mn(1/3)O2
# Anode:	Hard Carbon


# Start at 2030, reach half of share by 2040, then half for rest of period
share_difussion <- tibble(Year=2022:2070,
                          share_multiplier=c(rep(0,7),
                                             seq(0,1/2,length.out=12),
                                             rep(0.5,10),
                                             rep(0.5,20)))
# Create new rows duplicate based on NMC - with only half of share
ess_SIB <- ess %>% filter(str_detect(chemistry,"LFP")) %>% 
  mutate(chemistry="SIB") %>% 
  left_join(share_difussion) %>% 
  mutate(stationaryPower=stationaryPower*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
ess_SIB <- ess %>%
  left_join(share_difussion) %>% 
  mutate(stationaryPower=if_else(str_detect(chemistry,"LFP"),
                             stationaryPower*(1-share_multiplier),
                             stationaryPower),share_multiplier=NULL) %>% 
  rbind(ess_SIB) 

ess_SIB %>% group_by(Year) %>% reframe(sum(stationaryPower))

write.csv(ess_SIB,"Results/Sodium_stationaryPower.csv",row.names = F)


ess_fig <- ess_SIB %>% 
  group_by(Year,chemistry) %>% 
  reframe(stationaryPower=sum(stationaryPower)) %>% ungroup() %>% 
  filter(stationaryPower>0 | chemistry=="SIB") %>%
  filter(Year<2051) %>% 
  mutate(proj=Year>2040)

ess_fig %>% 
  ggplot(aes(Year,stationaryPower,fill=chemistry))+
  geom_area(aes(alpha=proj))+
  geom_area(data=filter(ess_fig,Year>2039,Year<2042),alpha=0.7,linewidth=0.01)+ # fill missing gap behind
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_alpha_manual(values=c(1,0.7))+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  labs(x="",y="MWh",fill="Battery Chemistry",caption = "2040-2050 projection based on 2030-2040 avg. growth")+
  guides(alpha = FALSE)

f.fig.save("Figures/SPS/Sodium_SPS.png")


# EoF