# Comparison of Mineral demand with other estimates
# Comparison to IEA and ICCT
# Overall a good match
# PBH March 2024
# Updated October 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

fig_name <- "Figures/MineralDemand/Comparison/%s.png"

# load pre-computed results
# df <- read.csv("Results/MineralDemandRegion.csv") # faster
df <- read.csv("Results/MineralDemand_FewScenarios.csv") # much faster

df <- df %>% 
  filter(chem_scenario=="Baseline",  
         capacity_scenario=="Baseline",
         lifetime_scenario=="Baseline",
         recycling_scenario=="Baseline")

df <- df %>% filter(Year<2051)

# IEA Critical Minerals Review 2024 ---------
# https://www.iea.org/data-and-statistics/data-tools/critical-minerals-data-explorer

# load data - 2024
iea_url <- "Data/MineralDemand_Comparison/CM_Data_Explorer 2024.xlsx"
years_iea <- seq(2030,2050,5)

f.read.iea <- function(range_iea,mineral,totals=T){
  iea <- read_excel(iea_url,sheet = "1 Total demand for key minerals",
                    range = range_iea)
  # remove missing cols
  iea <- iea[,c(-3,-9,-15)]
  
  names(iea) <- c("Sector","sps_2023",paste0("sps_",years_iea),
                     paste0("aps_",years_iea),paste0("nze_",years_iea))
  # filter totals
  if(totals==F){
  iea <- iea %>% filter(!str_detect(Sector,"Total"))
  }
  
  # format to long table
  iea <- iea %>% 
    mutate(aps_2023=sps_2023,nze_2023=sps_2023) %>%  # repeat baseline year
    pivot_longer(c(-Sector), names_to = "key", values_to = "ktons") %>% 
    mutate(Year=str_remove_all(key,"sps_|aps_|nze_") %>% as.numeric(),
           IEA_Scenario=substring(key,1,3),key=NULL,
           IEA_Scenario=case_when(
             IEA_Scenario=="sps" ~ "Stated policies",
             IEA_Scenario=="aps" ~ "Announced pledges",
             IEA_Scenario=="nze" ~ "Net Zero 2050"))
  
  # add mineral
  iea$Mineral <- mineral
  
  return(iea)
}

iea <- rbind(f.read.iea("A32:T37","Lithium"),
             f.read.iea("A41:T50","Nickel"),
             f.read.iea("A21:T28","Cobalt"))

# big summary for IEA
iea %>% group_by(IEA_Scenario,Mineral,Sector,Year) %>% 
  reframe(ktons=sum(ktons)) %>% 
  pivot_wider(names_from = c(IEA_Scenario,Year), values_from = ktons)

# figure
iea %>% 
  filter(Sector=="Total demand") %>% 
  ggplot(aes(Year,ktons,col=IEA_Scenario))+
  geom_point()+geom_line()+
  facet_wrap(~Mineral,nrow=1,scales = "free_y")

## Upscale 2023 to 2050 -----

iea %>% 
  filter(Year %in% c(2023,2050)) %>% 
  group_by(IEA_Scenario,Year,Mineral) %>% 
  summarise(kton=sum(ktons)/1e3) %>% 
  pivot_wider(names_from = Year, values_from = kton) %>% 
  mutate(ratio=`2050`/`2023`) %>% dplyr::select(-`2023`,-`2050`) %>% 
  pivot_wider(names_from = IEA_Scenario, values_from = ratio)

## Comparison ours vs IEA ------

# normalize scenarios
scens_combined <- c("ICCT Baseline \n IEA SPS","ICCT Momentum \n IEA APS","ICCT Ambitious \n IEA Net Zero")
df$Scenario %>% unique()

# All scenarios
(norm_scen <- tibble(
  Scenario=unique(df$Scenario),
  IEA_Scenario=unique(iea$IEA_Scenario),
  scen=scens_combined))
# Only ambitious
norm_scen <- norm_scen[3,]

df$Vehicle %>% unique()
df$Powertrain %>% unique()

iea_aux <- iea %>% left_join(norm_scen) %>% 
  filter(!is.na(scen)) %>% 
  mutate(IEA_Scenario=NULL,Scenario=NULL)

# join to df
df_iea <- df %>% 
  filter(chem_scenario=="Baseline",capacity_scenario=="Baseline") %>% 
  filter(lifetime_scenario=="Baseline",recycling_scenario=="Baseline") %>%
  filter(Mineral %in% min_interest) %>% 
  # group to sectors
  mutate(Sector=case_when(
    # recycling assign to EV for comparison
    Powertrain %in% c("BEV","PHEV","Recycling") ~ "Electric vehicles",
    Powertrain=="SPS" ~ "Grid battery storage",
    T ~ "Other uses")) %>% 
  left_join(norm_scen) %>% 
  group_by(Year,Sector,Mineral,scen) %>% 
  reframe(ktons=sum(tons_mineral)/1e3) %>% ungroup()
# add total demand
df_iea_total <- df_iea %>% group_by(Year,Mineral,scen) %>% 
  reframe(ktons=sum(ktons)) %>% ungroup() %>% 
  mutate(Sector="Total demand")
df_iea <- rbind(df_iea,df_iea_total)
df_iea$Source <- "Own Demand Model"
iea_aux$Source <- "IEA Critical \nMineral Explorer"

df_iea <- rbind(df_iea,iea_aux)

df_iea <- df_iea %>% 
  filter(Sector %in% c("Total demand","Electric vehicles","Grid battery storage")) %>% 
  mutate(scen=factor(scen,scens_combined)) %>% 
  mutate(Mineral=factor(Mineral,min_interest))

# Figure
df_iea %>% 
  # filter(Sector=="Total demand") %>% 
  ggplot(aes(Year,ktons,
             # linetype=scen,
             col=Source))+
  geom_point(data=filter(df_iea,Source=="IEA"),size=0.5)+
  geom_line(linewidth=0.5)+
  facet_wrap(Mineral~Sector,scales = "free")+
  labs(y="",title="Mineral Demand [ktons]",x="",linetype="Scenario")+
  coord_cartesian(expand=F)+
  ylim(0,NA)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))

f.fig.save(sprintf(fig_name,"IEA"),w=8.7*3,h=8.7*2)

# EoF