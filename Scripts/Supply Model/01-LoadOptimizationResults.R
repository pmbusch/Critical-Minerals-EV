# Analysis Results of Optimization
# Common script to load optimization results from Julia in a fast way, and process the results accordingly
# Need to specify the runs variable beforehand
# PBH March 2024

# Input Parameters
demand <- read.csv("Parameters/Demand.csv")
recycling <- read.csv("Parameters/Recycling.csv")
deposit <- read.csv("Parameters/Deposit.csv")

(d_size <- nrow(deposit))
(t_size <- nrow(filter(demand,str_detect(Scenario,"Enhanced rec"))))

prod_rate <- expand.grid(Deposit_Name=unique(deposit$Deposit_Name),
                         t=unique(demand$t)) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,prod_rate2022,prod_rate2023,
                          prod_rate2025,prod_rate2030)) %>% 
  mutate(prod_rate=case_when(
    t == 2022 ~ prod_rate2022,
    t == 2023 ~ prod_rate2023,
    t == 2024 ~ (prod_rate2023+prod_rate2025)/2, # interpolation
    t == 2025 ~ prod_rate2025,
    t >= 2026 & t <= 2029 ~ (1-(t-2025)/5)*prod_rate2025+((t-2025)/5)*prod_rate2030,
    T ~ prod_rate2030)) %>% 
  dplyr::select(Deposit_Name,t,prod_rate)


opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])
(discount_rate <- opt_param[1,2] )

# Load Results --------

# runs is specified outside this folder, and give all folders to load as scenarios

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = basename(folder_path)))) %>% 
  rename(Deposit_Name=d)
df_results$Scenario %>% unique()


slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = basename(folder_path))))

# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)
sum(ald_opens$already_open) # 52

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  left_join(prod_rate) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1), # Near() instead of a==1 avoids rounding error mistakes!
         cap_total=cumsum(capacity_added)+prod_rate,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

# subset of opened deposits
open_deposits <- df_results %>% group_by(Deposit_Name,Scenario) %>% 
  reframe(mine_open=sum(mine_opened)) %>% filter(mine_open>0) %>% 
  mutate(d=paste0(Deposit_Name,Scenario)) %>% 
  pull(d) %>% unique()

# dict of scenarios - specified beforehand
df_results <- df_results %>% left_join(dict_scen) %>% mutate(name=factor(name,levels=scens_names))
slack <- slack %>% left_join(dict_scen) %>% mutate(name=factor(name,levels=scens_names))
demand <- demand %>% left_join(dict_scen) %>% mutate(name=factor(name,levels=scens_names))
recycling <- recycling %>% left_join(dict_scen) %>% mutate(name=factor(name,levels=scens_names))


# EoF