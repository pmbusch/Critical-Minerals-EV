# Analysis Results of Optimization for different Parameters
# Optimization is run in Julia
# PBH July 2024


# Load Data -----------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))


demand <- read.csv("Parameters/Demand.csv")
recycling <- read.csv("Parameters/Recycling.csv")

# note that deposits parameters are different for each run
deposit <- read.csv("Parameters/Deposit.csv")

# prod rate over time
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


(d_size <- nrow(deposit))
(t_size <- length(unique(demand$t)))

bigM_cost <- 100000*5.323/1e3 # historic high was 68K for LCE
discount_rate <- 0.07 # same as Julia

# load results from Julia
## Deposits Scenarios -------------
(runs <- list.dirs("Results/Optimization/ParameterCheck",recursive = T))
runs <- runs[str_count(runs,"/")==3] # keep only the final folders

ref <- list.dirs("Results/Optimization/DemandScenario",recursive = F)
runs <- c(ref,runs)

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# get name
df_results <- df_results %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  # separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
  separate(path, into = c("Aux","Scen_Deposit"), sep = "/") %>%
  mutate(count_aux=NULL,Aux=NULL)

slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = (folder_path))))

slack <- slack %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL)

# add scen name
df_results <- df_results %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# add deposits name
unique(df_results$Scen_Deposit)
dep_scen <- c("Base","Deposit_SCCost_2k","Deposit_SCCost_3k",
              "Deposit_LCEPrice_15k","Deposit_LCEPrice_30k",
              "Deposit_LCEPrice_40k")
dep_scen_name <- c("Reference",
                   "Conversion cost SC6 to LCE $2,000",
                   "Conversion cost SC6 to LCE $3,000",
                   "LCE Price $15,000","LCE Price $30,000",
                   "LCE Price $40,000")
df_results <- df_results %>% 
  left_join(tibble(Scen_Deposit=dep_scen,
                   dep_scen=dep_scen_name)) %>% 
  mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name))) %>% 
  filter(!is.na(dep_scen))
  

# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,dep_scen,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & mine_opened==1,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results %>% filter(t<2051) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,dep_scen,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Scen_Deposit) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))

# Mines open
df_results %>%
  filter(t<2051) %>%
  group_by(name,dep_scen) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  ungroup()


