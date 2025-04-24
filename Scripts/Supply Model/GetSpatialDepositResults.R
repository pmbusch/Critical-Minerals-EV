# Create .csv with each deposit yyear of opening based on model results
# 
# PBH February 2025

# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/01-LoadOptimizationResults.R", encoding = "UTF-8")

# Select first year of production
df <- df_results %>% 
  # filter(mine_opened==1)
  filter(tons_extracted>0) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  filter(t==min(t)) %>% ungroup()

# Add coordinates
df <- df %>% left_join(deposit)

# only variables of interest
names(df)
df <- df %>% dplyr::select(Country,Deposit_Name,Latitude,Longitude,t,
                           Resource_Type_orig,Resource_Type,name)

# long Format by scenario# long Format by scenario
df <- df %>% 
  pivot_wider(names_from = name, values_from = t)

head(df)

write.csv(df,"Results/MineYearOpening.csv",row.names = F)

# Cumulative lithium extraction, in million tons
prod <- df_results %>% 
  filter(tons_extracted>0) %>% 
  filter(t<2051) %>% 
  group_by(name,Deposit_Name) %>% 
  reframe(tons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  pivot_wider(names_from = name, values_from = tons)
sum(prod$`(1) Reference`,na.rm=T)

write.csv(prod,"Results/LiExtraction_Mtons.csv",row.names = F)


# EoF