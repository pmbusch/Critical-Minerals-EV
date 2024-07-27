# Analysis Results of Optimization of Scenarios deposits
# Optimization is run in Julia
# Files are run by demand scenario and deposit scenario, so multiple files
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
(runs <- list.dirs("Results/Optimization/Scenarios_Deposit",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders

ref <- list.dirs("Results/Optimization/DemandScenario",recursive = F)
runs <- c(ref,runs)

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# separate demand and deposit scenario
df_results <- df_results %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
        path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
        separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
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
dep_scen <- c("Base","noDLE_prodRate","AllDLE_prodRate",
              "2_prodRate","5_prodRate",
              "NoRampUp","2yRampUp","8yRampUp",
              "No Clay","NoInferredResources")
dep_scen_name <- c("Reference",
                   "No DLE Brine","All Brine DLE",
                   "2% Max prod. rate","5% Max prod. rate",
                   "1-year ramp up","2-year ramp-up","8-year ramp-up",
                   "No clay resources","No inferred resources")
df_results <- df_results %>% 
  left_join(tibble(Scen_Deposit=dep_scen,
                   dep_scen=dep_scen_name)) %>% 
  mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name)))


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


# Deposit Scenarios -------------

## Mines opened ---------
data_fig <- df_results %>%
  filter(t<2051) %>%
  group_by(name,dep_scen) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  ungroup %>% group_by(dep_scen) %>%
  # find duplicate values in same row of table
  mutate(overlap_x = ifelse(duplicated(mines_open) | duplicated(mines_open, fromLast = TRUE), 
                            1, 0)) %>% ungroup() %>% 
  # do the overlap
  group_by(dep_scen,mines_open) %>%
  mutate(sum_X = sum(overlap_x),
         pos_X = ifelse(overlap_x == 1, seq_along(overlap_x) / (sum_X + 1) - 0.5, 0)) %>% ungroup()

# reference values
ref_values <- data_fig %>% filter(dep_scen=="Reference") %>% pull(mines_open)


p1 <- ggplot(data_fig,aes(dep_scen,mines_open))+
  geom_point(aes(col=name),
             position=position_nudge(data_fig$pos_X/3*2))+
  scale_color_manual(values = scen_colors)+
  # rest of scenarios
  geom_hline(yintercept = ref_values[2],linetype="dashed",col="#0072B2",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[3],linetype="dashed",col="#56B4E9",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[4],linetype="dashed",col="#D55E00",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[5],linetype="dashed",col="#CC79A7",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[6],linetype="dashed",col="#E69F00",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[7],linetype="dashed",col="#F0E442",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[8],linetype="dashed",col="#D62728",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[9],linetype="dashed",col="#009E73",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[1],linetype="dashed",col="#808080")+ #ref
  geom_vline(xintercept = c(0.5,2.5,5.5,7.5,9.5),
             col="grey",linewidth=0.15)+
  coord_flip()+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  labs(x="",col="Demand Scenario",y="Number of new opened Deposits",
       title="(A) Deposit Parameters Sensitivity")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(panel.border=element_blank(),
        axis.text.y = element_text(hjust=0),
        legend.text = element_text(size=6),
        legend.position = "bottom",
        legend.spacing.y = unit(0.1, "cm"),
        axis.ticks.y = element_blank())
p1

## Heatmap --------------
# I like the current figure much more
data_fig %>% 
  mutate(name=substr(name,0,3)) %>% 
  ggplot(aes(name,dep_scen))+
  geom_tile(aes(fill=mines_open),col="black",linewidth=0.1)+
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",y="",fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient(low = "white", high = "purple") +  # Change "purple" to "red" if preferred
  theme_minimal()
  


## Total Cost ----

# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))

df_results %>%
  filter(t<2051) %>% 
  left_join(deposit) %>% 
  mutate(total_cost=cost1*tons_extracted1+cost2*tons_extracted2+cost3*tons_extracted3+
           capacity_added*cost_expansion+mine_opened*cost_opening) %>% 
  left_join(discounter) %>% 
  mutate(total_cost=total_cost/r) %>% 
  group_by(name,Scen_Deposit) %>% 
  reframe(total_cost=sum(total_cost)/1e9) %>%  # a chillion USD
  ggplot(aes(Scen_Deposit,total_cost))+
  geom_point(aes(col=name))+
  scale_color_manual(values = scen_colors)+
  geom_hline(yintercept = 4.3,linetype="dashed",col="grey")+ #ref 
  coord_flip()

## Reserve depletion ------
df_results %>%
  filter(t<2051) %>%
  left_join(deposit) %>% 
  mutate(reserve=resource_demostrated) %>%
  group_by(name,Scen_Deposit,Deposit_Name) %>% 
  reframe(tons_extracted1=sum(tons_extracted1),
          reserve=max(reserve)) %>% ungroup() %>% 
  group_by(name,Scen_Deposit) %>% 
  reframe(tons_extracted1=sum(tons_extracted1),
          reserve=sum(reserve)) %>% ungroup() %>% 
  mutate(dep=tons_extracted1/reserve) %>% 
  ggplot(aes(Scen_Deposit,dep))+
  geom_point(aes(col=name))+
  scale_color_manual(values = scen_colors)+
  geom_hline(yintercept = 0.102,linetype="dashed",col="grey")+ #ref 
  coord_flip()

## Slack ------
slack %>%
  filter(t<2051) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,Scen_Deposit) %>% 
  reframe(slack=sum(value)/1e3) %>% 
  ggplot(aes(Scen_Deposit,slack))+
  geom_point(aes(col=name))+
  scale_color_manual(values = scen_colors)+
  geom_hline(yintercept = 0,linetype="dashed",col="grey")+ #ref 
  coord_flip()


# N-1 Country ------------
(runs <- list.dirs("Results/Optimization/N1_Countries_Demand",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders
ref <- list.dirs("Results/Optimization/DemandScenario",recursive = F)
runs <- c(ref,runs)

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# separate demand and deposit scenario
df_results <- df_results %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
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
# country order
count_order <- c("Reference","Canada","United States",
                 "DR Congo","Tanzania","Australia",
                 "Lithium Triangle","Bolivia","Argentina","Chile")
df_results <- df_results %>% 
  mutate(Scen_Deposit=Scen_Deposit %>% 
           str_replace("Base","Reference") %>% 
           factor(levels=rev(count_order)))


# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,Scen_Deposit,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & mine_opened==1,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results %>% filter(t<2051) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,Scen_Deposit,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Scen_Deposit) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))


# Deposit Scenarios -------------

## Mines opened ---------
data_fig2 <- df_results %>%
  filter(t<2051) %>%
  group_by(name,Scen_Deposit) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  ungroup %>% group_by(Scen_Deposit) %>%
  # find duplicate values in same row of table
  mutate(overlap_x = ifelse(duplicated(mines_open) | duplicated(mines_open, fromLast = TRUE), 
                            1, 0)) %>% ungroup() %>% 
  # do the overlap
  group_by(Scen_Deposit,mines_open) %>%
  mutate(sum_X = sum(overlap_x),
         pos_X = ifelse(overlap_x == 1, seq_along(overlap_x) / (sum_X + 1) - 0.5, 0)) %>% ungroup()

p2 <- ggplot(data_fig2,aes(Scen_Deposit,mines_open))+
  geom_point(aes(col=name),
             position=position_nudge(data_fig2$pos_X/3*2))+
  scale_color_manual(values = scen_colors)+
  # rest of scenarios
  geom_hline(yintercept = ref_values[2],linetype="dashed",col="#0072B2",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[3],linetype="dashed",col="#56B4E9",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[4],linetype="dashed",col="#D55E00",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[5],linetype="dashed",col="#CC79A7",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[6],linetype="dashed",col="#E69F00",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[7],linetype="dashed",col="#F0E442",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[8],linetype="dashed",col="#D62728",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[9],linetype="dashed",col="#009E73",linewidth=0.1)+  
  geom_hline(yintercept = ref_values[1],linetype="dashed",col="#808080")+ #ref 
  geom_vline(xintercept = 0.5,col="grey",linewidth=0.15)+
  coord_flip()+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  labs(x="",col="Demand Scenario",y="Number of new opened Deposits",
       title="(B) N-1 Country Analysis")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(panel.border=element_blank(),
        axis.text.y = element_text(hjust=0),
        legend.position = "none",
        axis.ticks.y = element_blank())
p2

# Combined Figure ----------


lege <- cowplot::get_plot_component(p1, 'guide-box-bottom', return_all = TRUE)
p1 <- p1 + theme(legend.position = "none")

cowplot::plot_grid(
  cowplot::plot_grid(p1,p2, ncol = 2),
  lege, ncol = 1, rel_heights = c(0.85, 0.15))

# Save with widht size of letter
ggsave("Figures/Article/Fig4.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)


## Slack in N-1 -----
slack %>%
  filter(t<2051) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,Scen_Deposit) %>% 
  reframe(slack=sum(value)/1e3) %>% 
  ggplot(aes(Scen_Deposit,slack))+
  geom_point(aes(col=name),
             position=position_nudge(data_fig2$pos_X/3*2))+
  scale_color_manual(values = scen_colors)+
  coord_flip()+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(panel.border=element_blank(),
        axis.text.y = element_text(hjust=0),
        legend.position = "bottom",
        axis.ticks.y = element_blank())
  
  




# EoF