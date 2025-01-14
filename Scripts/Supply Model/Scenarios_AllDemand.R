# All Demand Scenario runs
# PBH August 2024

# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenarioAll",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/02-LoadOptimizationResults.R", encoding = "UTF-8")
demand <- read.csv("Parameters/Demand_AllScenarios.csv")

nrow(df_results)/1e6
df_results$Scenario %>% unique()



# Number of mines opened
df_results %>% 
  filter(t<2051) %>%
  group_by(Scenario) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  arrange(desc(mines_open))
# best: low range + recycling = 8
# worst: solid state, high range, no recyc = 88


# get metrics
data_fig <- df_results %>%
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  # sum over deposits
  reframe(mines_open=sum(new_mine_open),
          tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  group_by(Scenario) %>%
  # sum over time
  reframe(mines_open=sum(mines_open),
          Total_Demand=sum(tons_extracted),
          Peak_Demand=max(tons_extracted)) %>% ungroup() %>% 
  mutate(recycling=str_detect(Scenario,"recycling")) %>% 
  mutate(high_cap=str_detect(Scenario,"High"))
  

data_fig %>% 
  pivot_longer(c(Total_Demand,Peak_Demand), names_to = "key", values_to = "value") %>% 
  mutate(value=value/1e3) %>% # million tons
  mutate(key=str_replace(key,"_"," ")) %>% 
  ggplot(aes(value,mines_open))+
  # geom_point(aes(col=recycling))+
  geom_point(aes(col=high_cap))+
  # geom_smooth(method = "lm",formula = y ~ x ,aes(col=recycling))+
  facet_wrap(~key,scales = "free_x")+
  coord_cartesian(xlim=c(0,NA),ylim=c(0,NA))+
  labs(x="",y="Mines \nopen")


summary(lm(mines_open~Total_Demand+Peak_Demand,data=data_fig))
summary(lm(mines_open~Total_Demand,data=data_fig))
summary(lm(mines_open~Peak_Demand,data=data_fig))
summary(lm(mines_open~Peak_Demand+recycling,data=data_fig))
summary(lm(mines_open~I(Peak_Demand^2),data=data_fig))


