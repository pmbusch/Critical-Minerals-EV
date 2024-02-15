# PBH January 2024
# Compute mineral demand from other sectors

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")


url_file <- "Data/OtherSectors_Demand.xlsx"


other_sector_demand <- tibble(year=2022:2050)

# NICKEL --------
sheet <- "Nickel"

## Stainless steel --------

# historical data
ss <- read_excel(url_file,sheet,"B30:E47")
(names(ss) <- c("year","a","ss_ton","gdp"))
ss$a <- NULL

# fit regression
mod1 <- lm(ss_ton~gdp-1,data=ss) # no intercept, to track growth based on GDP
summary(mod1)

ggplot(ss,aes(gdp,ss_ton))+geom_point()+geom_smooth(method="lm")
cor(ss$ss_ton,ss$gdp)

# gdp forecast
gdp <- read_excel(url_file,"GDP","A1:D72")
names(gdp)
gdp <- gdp %>% filter(year>2021,year<2051)

# stainless steel growth
gdp$ss <- predict(mod1,tibble(gdp=gdp$gdp))

ggplot(gdp,aes(year,ss))+geom_line()

# Nickel
# USGS wolrd primary nickel requirement
(ss_nickel_intensity <- 2.33*0.66/50.7*1e3) # kg Ni per ton Stainless Steel

gdp$Ni_stainlessSteel <- gdp$ss*ss_nickel_intensity/1e3 # tons Nickel

# add to maindatabase
# other_sector_demand <- other_sector_demand %>% 
#   left_join(dplyr::select(gdp,year,Ni_stainlessSteel))

## GDP driver ------
gdp <- read_excel(url_file,"GDP","A1:D72")

gdp_2018 <- gdp %>% filter(year==2018) %>% pull(gdp)
gdp <- gdp %>% filter(year>2021,year<2051) %>% mutate(ratio_2018=gdp/gdp_2018)

# use ratio to expand
ni_2018 <- 2.33*1e6 # consumption
other_sector_demand$Ni_nonferrousAlloys <- gdp$ratio_2018*ni_2018*0.1 
other_sector_demand$Ni_electroplating <- gdp$ratio_2018*ni_2018*0.09
other_sector_demand$Ni_alloySteels <- gdp$ratio_2018*ni_2018*0.08
other_sector_demand$Ni_foundry <- gdp$ratio_2018*ni_2018*0.03
# better to use GDP growth directly
other_sector_demand$Ni_stainlessSteel <- gdp$ratio_2018*ni_2018*0.66

# LITHIUM -----------
sheet <- "Lithium"
li_cons <- 49000 # Li 2018 consumption

## Ceramics & Glass ----------

# historical data
cg <- read_excel(url_file,sheet,"A48:G57")
(names(cg) <- c("year","a","a1","cer","glass","cg","gdp"))

# fit regression
mod2 <- lm(cg~gdp-1,data=cg) # no intercept
summary(mod2)
cor(cg$cg,cg$gdp)

ggplot(cg,aes(gdp,cg))+geom_point()+geom_smooth(method="lm")

# gdp forecast
gdp <- read_excel(url_file,"GDP","A1:D72")
gdp <- gdp %>% filter(year>2021,year<2051)

# stainless steel growth
gdp$cg <- predict(mod2,tibble(gdp=gdp$gdp))

ggplot(gdp,aes(year,cg))+geom_line()

gdp$Li_ceramics <- gdp$cg # in metric tons

# add to maindatabase
# other_sector_demand <- other_sector_demand %>% 
#   left_join(dplyr::select(gdp,year,Li_ceramics))

## Lubricant Grease -------

# use constant data for 2018 

# tons
other_sector_demand$Li_lubricant <- li_cons*0.06 # 6% of consumption


## GDP driver ------
gdp <- read_excel(url_file,"GDP","A1:D72")

gdp_2018 <- gdp %>% filter(year==2018) %>% pull(gdp)
gdp <- gdp %>% filter(year>2021,year<2051) %>% mutate(ratio_2018=gdp/gdp_2018)


# use ratio to expand
other_sector_demand$Li_polymer <- gdp$ratio_2018*li_cons*0.04 # 4% of 2018 consumption
other_sector_demand$Li_castingMold <- gdp$ratio_2018*li_cons*0.03 # 3% of 2018 consumption
# expand based on GDP
other_sector_demand$Li_ceramics <- gdp$ratio_2018*li_cons*0.22


## Population driver -----------
pop <- read_excel(url_file,"Population","A1:C152")
pop_2018 <- pop %>% filter(year==2018) %>% pull(pop_thousand)
pop <- pop %>% filter(year>2021,year<2051) %>% mutate(ratio_2018=pop_thousand/pop_2018)

# use ratio to expand
other_sector_demand$Li_AirTreatment <- pop$ratio_2018*li_cons*0.02 # 2% of 2018 consumption
other_sector_demand$Li_OtherUses <- pop$ratio_2018*li_cons*0.06 # 6% of 2018 consumption
other_sector_demand$Li_PortableElectronics <- pop$ratio_2018*li_cons*0.57*0.25 

# COBALT ----------

co_cons <- 187000 # Cobalt consumption 2022

## GDP driver ------
gdp <- read_excel(url_file,"GDP","A1:D72")

gdp_2022 <- gdp %>% filter(year==2022) %>% pull(gdp)
gdp <- gdp %>% filter(year>2021,year<2051) %>% mutate(ratio_2022=gdp/gdp_2022)

# use ratio to expand
other_sector_demand$Co_superalloys <- gdp$ratio_2022*co_cons*0.09
other_sector_demand$Co_HardMetals <- gdp$ratio_2022*co_cons*0.05
other_sector_demand$Co_Catalyst <- gdp$ratio_2022*co_cons*0.03
other_sector_demand$Co_Ceramic <- gdp$ratio_2022*co_cons*0.03
other_sector_demand$Co_Othes <- gdp$ratio_2022*co_cons*0.1

## Population driver -----------
pop <- read_excel(url_file,"Population","A1:C152")
pop_2022 <- pop %>% filter(year==2022) %>% pull(pop_thousand)
pop <- pop %>% filter(year>2021,year<2051) %>% mutate(ratio_2022=pop_thousand/pop_2022)

# use ratio to expand
other_sector_demand$Co_PortableElectronics <- pop$ratio_2022*co_cons*0.3 


# Data manipulation --------
names(other_sector_demand)
other_sector_demand <- other_sector_demand %>% 
  rename(Year=year) %>% 
  pivot_longer(c(-Year), names_to = "key", values_to = "tons_mineral") %>% 
  mutate(Mineral=str_extract(key,"Li|Co|Ni")) %>% 
  mutate(Mineral=Mineral %>% str_replace("Li","Lithium") %>% 
           str_replace("Ni","Nickel") %>% 
           str_replace("Co","Cobalt"),
         Powertrain=str_remove(key,"Li_|Co_|Ni_"),
         key=NULL) 
# Aggregate to sectors
other_sector_demand %>% group_by(Powertrain,Mineral) %>%
  reframe(tons=sum(tons_mineral)) %>% 
  arrange(desc(tons)) %>% arrange(Mineral)
  
other_sector_demand <- other_sector_demand %>% 
  mutate(Powertrain=case_when(
    Powertrain=="stainlessSteel" ~ "Stainless Steel",
    Powertrain %in% c("ceramics","Ceramic") ~ "Ceramics",
    Powertrain=="PortableElectronics" ~ "Portable Electronics",
    T ~ "Non-battery sectors")) %>% 
  group_by(Year,Powertrain,Mineral) %>%
  reframe(tons_mineral=sum(tons_mineral)) %>% 
  arrange(desc(tons_mineral)) %>% arrange(Mineral)


# Regional dissagregation ----
# Dissagreggate to Country level using GDP or population

## Population based ---------
pop <- read_excel("Data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
                  sheet = "Estimates",range="A17:L20613")
names(pop) <- c("Index","Variant","country","Notes","loc_code","iso3","iso2","sdmx",
                "Type","Parent_code","Year","pop")
pop <- pop %>% filter(Type=="Country/Area")
pop <- pop %>% mutate(pop=as.numeric(pop)*1e3) # original comes in thousand
head(pop)

pop %>% filter(Year=="2021") %>% pull(pop) %>% sum()/1e9 # 7.88 billions

pop <- pop %>% filter(Year=="2021")

# dictionary
dict_pop <- read_excel("Data/Eq_Countries_ICCT_Pop_GDP.xlsx",
                       sheet = "Pop_Eq_Country2",range = "B1:C238")
names(dict_pop)[1] <- "country" 
pop <- pop %>% left_join(dict_pop) %>% group_by(ICCT_Country) %>% 
  reframe(pop=sum(pop)) %>% ungroup() %>% mutate(perc_pop=pop/sum(pop))
sum(pop$perc_pop)  
sum(pop$pop)/1e9 # 7.88 billions

# Portable electronics
nrow(pop)*2*29 # number of rows
demand <- pop %>% rename(Country=ICCT_Country) %>% 
  cross_join(filter(other_sector_demand,Powertrain=="Portable Electronics")) %>% 
  mutate(pop=NULL,
         tons_mineral=tons_mineral*perc_pop,perc_pop=NULL)
demand %>% filter(Year==2050) %>% group_by(Mineral) %>% reframe(ton=sum(tons_mineral))

## GDP based -----------
gdp <- read_excel("Data/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_6508392.xlsx",
                  sheet = "Data",range="A4:BP270")
names(gdp)[6:68] <- paste0("y",1960:2022)
gdp$gdp <- gdp$y2021

gdp <- gdp %>% filter(Include!="0",Include!="") %>% 
  rename(Country=`Country Name`) %>% 
  dplyr::select(Country,gdp) %>% 
  filter(!is.na(gdp))
sum(gdp$gdp,na.rm=T)/1e9


# dictionary
dict_gdp <- read_excel("Data/Eq_Countries_ICCT_Pop_GDP.xlsx",
                       sheet = "GDP_Eq_Country2",range = "A1:B219")
names(dict_gdp)[1] <- "Country" 
gdp <- gdp %>% left_join(dict_gdp) %>% group_by(ICCT_Country) %>% 
  reframe(gdp=sum(gdp)) %>% ungroup() %>% mutate(perc_gdp=gdp/sum(gdp))
sum(gdp$perc_gdp)  
sum(gdp$gdp)/1e9 

# All except portable electronics
demand2 <- gdp %>% rename(Country=ICCT_Country) %>% 
  cross_join(filter(other_sector_demand,Powertrain!="Portable Electronics")) %>% 
  mutate(gdp=NULL,
         tons_mineral=tons_mineral*perc_gdp,perc_gdp=NULL)
demand2 %>% filter(Year==2050) %>% group_by(Powertrain,Mineral) %>% 
  reframe(ton=sum(tons_mineral))

# Save ----------
demand <- rbind(demand,demand2)
write.csv(demand,"Results/Intermediate Results/otherSector_demand.csv",row.names = F)

# EoF