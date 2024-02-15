# ICCT Survival Curves Analysis
# Data provided by ICCT Roadmap Model
# PBH Dec 2023

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load Data -------
# Avg retirement age, used when no survival curve is available
retAge <- read.csv("Data/Survival Curves/survival_curve_params.csv")
sc <- read.csv("Data/Survival Curves/survival_curves.csv")

# Covert to vehicle categories
retAge$Vehicle %>% unique()
retAge <- retAge %>% 
  mutate(Vehicle=case_when(
    Vehicle=="MC" ~ "Two/Three Wheelers",
    Vehicle=="PC" ~ "Car",
    Vehicle=="LCV" ~ "Van",
    Vehicle=="MDT" ~ "Medium truck",
    Vehicle=="HDT" ~ "Heavy truck",
    T ~ Vehicle))
sc <- sc %>% 
  mutate(Vehicle=case_when(
    Vehicle=="MC" ~ "Two/Three Wheelers",
    Vehicle=="PC" ~ "Car",
    Vehicle=="LCV" ~ "Van",
    Vehicle=="MDT" ~ "Medium Truck",
    Vehicle=="HDT" ~ "Heavy Truck",
    T ~ Vehicle))

## Country name -----
library(countrycode)
iso <- unique(retAge$ISO)
iso <- tibble(ISO=iso,Country=countrycode(iso, "iso3c", "country.name"))

# Add ISO to ICCT dictionary
dict_iso <- read_excel("Data/Eq_Countries_ICCT_Pop_GDP.xlsx",
                       sheet = "Pop_Eq_Country2",range = "A1:C238")
dict_iso$UN_Country <- NULL

# remove duplicates from ISO
iso_duplicates <- c("HKG","MAC","TWN","FRO","REU","BES","CUW","COK","XKX",
                    "AIA","VGB","FLK","GUM","PRI","VIR")
iso <- iso %>% left_join(dict_iso,by=c("ISO"="ISO3")) %>% 
  mutate(Country=NULL) %>% rename(Country=ICCT_Country) %>% 
  filter(!(ISO %in% iso_duplicates))

retAge <- retAge %>% left_join(iso)
sc <- sc %>% left_join(iso)
rm(iso)

c_interest <- c("China","United States","India",
                "Japan","Mexico","Germany","France","United Kingdom",
                "Indonesia","Spain","South Korea","Italy","Canada","Russia",
                "Brazil","Australia")


# Retirement Age -----------
head(retAge)

c_order <- retAge %>% filter(Vehicle=="Car") %>% arrange(AvgRetAge) %>% pull(Country)

# retAge %>% 
#   filter(Country %in% c_interest) %>% 
#   mutate(Country=factor(Country,c_order)) %>% 
#   ggplot(aes(Country,AvgRetAge))+
#   geom_col(fill="brown")+
#   coord_flip()+
#   facet_wrap(~Vehicle)+
#   labs(x="",y="Average Retirement Age")
# by region
icct <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")
eq_country_region <- icct %>% 
  filter(Year==2022) %>% 
  # filter(Vehicle=="Car") %>% 
  group_by(Region,Country,Vehicle) %>% reframe(n=sum(Sales))


data_fig <- retAge %>% 
  filter(!is.na(Country)) %>% 
  # filter(Vehicle=="Car") %>%
  left_join(eq_country_region) %>% 
  group_by(Region,Vehicle) %>% 
  reframe(AvgRetAge=weighted.mean(AvgRetAge,n,na.rm=T)) %>% ungroup() %>% 
  mutate(age_lab=paste0("",round(AvgRetAge,1))) %>% 
  filter(!is.na(Region)) %>% 
  mutate(Region=Region %>% str_remove(" and Caribbean"))

order_reg <- data_fig %>% filter(Vehicle=="Car") %>% arrange((AvgRetAge)) %>% 
  pull(Region) %>% unique()

data_fig %>% 
  mutate(Region=factor(Region,levels=order_reg)) %>% 
  ggplot(aes(Region,AvgRetAge))+
  geom_col(fill="brown")+
  geom_text(aes(label=age_lab),nudge_y = 2,size=9*5/14 * 0.8)+
  coord_flip()+
  facet_wrap(~Vehicle)+
  theme_bw(9)+
  labs(x="",y="Average Retirement Age")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
f.fig.save("Figures/avgRetirementAge.png")

## Comparisons of retirement ages ---------

macroReg <- read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="MacroRegion")


data_fig <- retAge %>% 
  filter(!is.na(Country)) %>% 
  # filter(Vehicle=="Car") %>%
  left_join(eq_country_region) %>% 
  filter(!is.na(Region)) %>%
  left_join(macroReg) %>% 
  mutate(n=NULL) %>% 
  pivot_wider(names_from = Vehicle, values_from = AvgRetAge) %>% 
  dplyr::select(-ISO,-Country,-Region)

ggplot(data_fig,aes(Car,Bus,col=MacroRegion))+
  geom_jitter(width = 0.2,height = 0.2,alpha=.8)

library(GGally)
ggpairs(data_fig)
ggpairs(data_fig, aes(colour = MacroRegion, alpha = 0.4))


# Survival Curves -----------

# Eg for USA and Cars

sc %>% 
  filter(Country=="United States",Vehicle=="Car") %>%
  # filter(Country=="India",Vehicle=="Two/Three Wheelers") %>% # shortes life span
  # filter(Country=="Greece",Vehicle=="Heavy Truck") %>%  # longest
  arrange(Age) %>% 
  # mutate(cumulative_survival = if_else(Age == 0, PctSurv, lag(cumulative_survival) * PctSurv))
  mutate(cumulative_survival = purrr::accumulate(PctSurv, `*`)) %>% 
  ggplot(aes(Age,cumulative_survival))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  labs(x="Age [years]",y="% Surviving",title = "USA Car Survival Curve")+
  coord_cartesian(expand = F)
f.fig.save("Figures/USA_survivalCurve.png")

# Avg retirement age
sc_retAge <- sc %>% 
  # filter(Country=="United States",Vehicle=="Car") %>% 
  group_by(Country,Vehicle) %>% 
  arrange(Age,.by_group = T) %>% 
  mutate(cumulative_survival = purrr::accumulate(PctSurv, `*`)) %>% 
  # retired
  mutate(retired=if_else(Age == 0, 1-cumulative_survival, # retired: past period - new period
                         lag(cumulative_survival)-cumulative_survival)) %>% 
  # average age
  mutate(ageMult=Age*retired) %>% 
  reframe(ageMult=sum(ageMult)+min(cumulative_survival)*max(Age),# remaining car assumed to retired at max age
          sumRet=sum(retired)+min(cumulative_survival)) %>%  
  mutate(Avg_retAge=ageMult/sumRet)
sc_retAge

sc_retAge %>% 
  ggplot(aes(Country,Avg_retAge))+
  geom_col(fill="brown")+
  coord_flip()+
  facet_wrap(~Vehicle)+
  labs(x="",y="Average Retirement Age")

# Other ideas: calculate shape and steepness parameters


# EoF