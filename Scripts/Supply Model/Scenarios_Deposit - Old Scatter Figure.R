# Analysis Results of Optimization of Scenarios deposits
# Optimization is run in Julia
# Files are run by demand scenario and deposit scenario, so multiple files
# PBH July 2024


# Load Data -----------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))


# just result for 2022-2050
limit_year <- 2051

# note that some deposits parameters are different for each run - so do not use them sparingly
# Input Parameters
demand <- read.csv("Parameters/Demand.csv")
recycling <- read.csv("Parameters/Recycling.csv")
deposit <- read.csv("Parameters/Deposit.csv")

(d_size <- nrow(deposit))
(t_size <- nrow(filter(demand,str_detect(Scenario,"recycling"))))

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


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/Scenarios_Deposit",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders
ref <- list.dirs("Results/Optimization/DemandScenario",recursive = F)
runs <- c(ref,runs)

(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))

opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])
(discount_rate <- opt_param[1,2] )


## Deposits Scenarios -------------

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
  mutate(count_aux=NULL,Aux=NULL) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


# add scen name
df_results <- df_results %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# add deposits name
unique(df_results$Scen_Deposit)
dep_scen <- c("Base","noDLE_prodRate","AllDLE_prodRate",
              "2_prodRate","5_prodRate",
              "NoRampUp","2yRampUp","8yRampUp",
              "No Clay","NoInferredResources",
              "shorter_LeadTime","longer_LeadTime")
dep_scen_name <- c("Reference",
                   "No DLE Brine","All Brine DLE",
                   "2% Max prod. rate","5% Max prod. rate",
                   "1-year ramp up","2-year ramp-up","8-year ramp-up",
                   "No clay resources","No inferred resources",
                   "Shorter Lead Time","Longer Lead Time")
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
  mutate(new_mine_open= !already_open & near(mine_opened,1),
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results %>% filter(t<limit_year) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,dep_scen,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack %>% 
  filter(t<limit_year) %>% 
  group_by(Scenario,Scen_Deposit) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))

# Figure -------------

## Mines opened ---------
data_fig <- df_results %>%
  filter(t<limit_year) %>%
  group_by(name,dep_scen) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% ungroup() 

y_title="Number of new opened Deposits"


# reference values
ref_values <- data_fig %>% filter(dep_scen=="Reference") %>% pull(mines_open)


p1 <- ggplot(data_fig,aes(dep_scen,mines_open))+
  geom_point(aes(col=name),
             # alpha=0.8,
             position=position_nudge(data_fig$pos_X/3*2))+
  scale_color_manual(values = scen_colors)+
  # rest of scenarios
  geom_hline(yintercept = ref_values[2],linetype="dashed",col="#0072B2",linewidth=0.1)+
  geom_hline(yintercept = ref_values[3],linetype="dashed",col="#56B4E9",linewidth=0.1)+
  geom_hline(yintercept = ref_values[4],linetype="dashed",col="#D55E00",linewidth=0.1)+
  geom_hline(yintercept = ref_values[5],linetype="dashed",col="#8B4513",linewidth=0.1)+
  geom_hline(yintercept = ref_values[6],linetype="dashed",col="#E69F00",linewidth=0.1)+
  geom_hline(yintercept = ref_values[7],linetype="dashed",col="#CC79A7",linewidth=0.1)+
  geom_hline(yintercept = ref_values[8],linetype="dashed",col="#D62728",linewidth=0.1)+
  geom_hline(yintercept = ref_values[9],linetype="dashed",col="#009E73",linewidth=0.1)+
  geom_hline(yintercept = ref_values[1],linetype="dashed",col="#000000")+ #ref
  geom_vline(xintercept = c(0.5,2.5,5.5,7.5,9.5),
             col="grey",linewidth=0.15)+
  coord_flip()+
  # scale_y_continuous(breaks = c(0,25,50,75),limits = c(0,97))+
  labs(x="",col="Demand\nScenario",
       y=y_title,
       title="(A) Deposit Parameters Sensitivity")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(panel.border=element_blank(),
        axis.text.y = element_text(hjust=0),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.position = "bottom",
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(0.1,'cm'),
        axis.ticks.y = element_blank())
p1

## Heatmap --------------
# I like the current figure much more
data_fig %>% 
  mutate(name=substr(name,0,3)) %>% 
  ggplot(aes(name,dep_scen))+
  geom_tile(aes(fill=mines_open),col="black",linewidth=0.1)+
  geom_text(aes(label=mines_open))+
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",y="",fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient(low = "white", high = "purple") +  # Change "purple" to "red" if preferred
  theme_minimal()
  





# # uncomment for SLACK
# slack_value <- slack %>%
#   filter(t<2051) %>%
#   left_join(tibble(Scen_Deposit=dep_scen,dep_scen=dep_scen_name)) %>%
#   left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
#   group_by(name,dep_scen) %>%
#   reframe(slack=sum(value))
# data_fig <- data_fig %>%
#   left_join(slack_value) %>%
#   mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name))) %>%
#   mutate(mines_open=slack,pos_X=0)
# y_title="Lithium demand not met [ktons]"


# uncomment for total cost
# # Remove effect of discount rate
# discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))
# cost <- df_results %>%
#   filter(t<limit_year) %>%
#   left_join(deposit) %>%
#   # all costs are converted to million usd, same as julia
#   mutate(total_cost=cost1*tons_extracted1/1e3+
#            cost2*tons_extracted2/1e3+
#            cost3*tons_extracted3/1e3+
#            capacity_added*cost_expansion/1e3+
#            mine_opened*cost_opening/1e6) %>%
#   left_join(discounter) %>%
#   mutate(total_cost=total_cost/r) %>%
#   left_join(tibble(Scen_Deposit=dep_scen,dep_scen=dep_scen_name)) %>%
#   group_by(name,dep_scen) %>%
#   reframe(total_cost=sum(total_cost)/1e3) # to billion
# slack_cost <- slack %>%
#   filter(t<limit_year) %>%
#   left_join(discounter) %>%
#   mutate(cost=value*bigM_cost/r) %>%
#   left_join(tibble(Scen_Deposit=dep_scen,dep_scen=dep_scen_name)) %>%
#   group_by(name,dep_scen) %>%
#   reframe(slack=sum(cost)/1e3)
# data_fig <- data_fig %>%
#   left_join(cost) %>%
#   left_join(slack_cost) %>%
#   mutate(total_cost=total_cost+slack) %>%
#   mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name))) %>%
#   mutate(mines_open=total_cost,pos_X=0)
# y_title="Total Cost [billion USD in 2022]"











# ## Reserve depletion ------
# df_results %>%
#   filter(t<2051) %>%
#   left_join(deposit) %>% 
#   mutate(reserve=resource_demostrated) %>%
#   group_by(name,Scen_Deposit,Deposit_Name) %>% 
#   reframe(tons_extracted1=sum(tons_extracted1),
#           reserve=max(reserve)) %>% ungroup() %>% 
#   group_by(name,Scen_Deposit) %>% 
#   reframe(tons_extracted1=sum(tons_extracted1),
#           reserve=sum(reserve)) %>% ungroup() %>% 
#   mutate(dep=tons_extracted1/reserve) %>% 
#   ggplot(aes(Scen_Deposit,dep))+
#   geom_point(aes(col=name))+
#   scale_color_manual(values = scen_colors)+
#   geom_hline(yintercept = 0.102,linetype="dashed",col="grey")+ #ref 
#   coord_flip()



# N-1 Country ------------
(runs <- list.dirs("Results/Optimization/N1_Countries_Demand",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders
ref <- list.dirs("Results/Optimization/DemandScenario",recursive = F)
runs <- c(ref,runs)

# Read all results and put them in the same dataframe!
df_results2 <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results2 <- df_results2 %>% rename(Deposit_Name=d)
df_results2$Scenario %>% unique()

# separate demand and deposit scenario
df_results2 <- df_results2 %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL)

slack2 <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = (folder_path))))

slack2 <- slack2 %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scenario","Scen_Deposit"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


# add scen name
df_results2 <- df_results2 %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# add deposits name
unique(df_results2$Scen_Deposit)
# country order
count_order <- c("Reference","Canada","United States",
                 "DR Congo","Tanzania","Australia",
                 "Lithium Triangle","Bolivia","Argentina","Chile")
df_results2 <- df_results2 %>% 
  mutate(Scen_Deposit=Scen_Deposit %>% 
           str_replace("Base","Reference") %>% 
           factor(levels=rev(count_order)))


# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results2 <- df_results2 %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,Scen_Deposit,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1),
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results2 %>% filter(t<limit_year) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,Scen_Deposit,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack2 %>% 
  filter(t<limit_year) %>% 
  group_by(Scenario,Scen_Deposit) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))


# Deposit Scenarios -------------

## Mines opened ---------
data_fig2 <- df_results2 %>%
  filter(t<limit_year) %>%
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
y_title="Number of new opened Deposits"


# uncomment for slack
# slack_value2 <- slack2 %>%
#   filter(t<2051) %>%
#   mutate(Scen_Deposit=Scen_Deposit %>% str_replace("Base","Reference")) %>%
#   left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
#   group_by(name,Scen_Deposit) %>%
#   reframe(slack=sum(value))
# data_fig2 <- data_fig2 %>%
#   left_join(slack_value2) %>%
#   mutate(mines_open=slack,pos_X=0)
# y_title="Lithium demand not met [ktons]"


# uncomment for total cost
# Remove effect of discount rate
# discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))
# cost2 <- df_results2 %>%
#   filter(t<limit_year) %>%
#   left_join(deposit) %>%
#   # all costs are converted to million usd, same as julia
#   mutate(total_cost=cost1*tons_extracted1/1e3+
#            cost2*tons_extracted2/1e3+
#            cost3*tons_extracted3/1e3+
#            capacity_added*cost_expansion/1e3+
#            mine_opened*cost_opening/1e6) %>%
#   left_join(discounter) %>%
#   mutate(total_cost=total_cost/r) %>%
#   mutate(Scen_Deposit=Scen_Deposit %>%  str_replace("Base","Reference")) %>%
#   group_by(name,Scen_Deposit) %>%
#   reframe(total_cost=sum(total_cost)/1e3) # to billion
# slack_cost2 <- slack2 %>% 
#   filter(t<limit_year) %>% 
#   left_join(discounter) %>% 
#   mutate(cost=value*bigM_cost/r) %>% 
#   mutate(Scen_Deposit=Scen_Deposit %>%  str_replace("Base","Reference")) %>%
#   group_by(name,Scen_Deposit) %>% 
#   reframe(slack=sum(cost)/1e3)
# data_fig2 <- data_fig2 %>%
#   left_join(cost2) %>%
#   left_join(slack_cost2) %>%
#   mutate(total_cost=total_cost+slack) %>%
#   mutate(mines_open=total_cost,pos_X=0)
# y_title="Total Cost [billion USD in 2022]"

data_fig2 <- data_fig2 %>% mutate(Scen_Deposit=factor(Scen_Deposit,levels=rev(count_order)))

p2 <- ggplot(data_fig2,aes(Scen_Deposit,mines_open))+
  geom_point(aes(col=name),
             # alpha=.8,
             position=position_nudge(data_fig2$pos_X/3*2))+
  scale_color_manual(values = scen_colors)+
  # rest of scenarios
  geom_hline(yintercept = ref_values[2],linetype="dashed",col="#0072B2",linewidth=0.1)+
  geom_hline(yintercept = ref_values[3],linetype="dashed",col="#56B4E9",linewidth=0.1)+
  geom_hline(yintercept = ref_values[4],linetype="dashed",col="#D55E00",linewidth=0.1)+
  geom_hline(yintercept = ref_values[5],linetype="dashed",col="#8B4513",linewidth=0.1)+
  geom_hline(yintercept = ref_values[6],linetype="dashed",col="#E69F00",linewidth=0.1)+
  geom_hline(yintercept = ref_values[7],linetype="dashed",col="#CC79A7",linewidth=0.1)+
  geom_hline(yintercept = ref_values[8],linetype="dashed",col="#D62728",linewidth=0.1)+
  geom_hline(yintercept = ref_values[9],linetype="dashed",col="#009E73",linewidth=0.1)+
  geom_hline(yintercept = ref_values[1],linetype="dashed",col="#000000")+ #ref 
  geom_vline(xintercept = 0.5,col="grey",linewidth=0.15)+
  coord_flip()+
  # scale_y_continuous(breaks = c(0,25,50,75,100))+
  labs(x="",col="Demand Scenario",y=y_title,
       title="(B) N-1 Country Analysis")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(panel.border=element_blank(),
        axis.text.y = element_text(hjust=0),
        legend.position = "none",
        axis.ticks.y = element_blank())
p2



scen_abr <- tibble(
  name=paste0("(",1:9,")"),
  name_abr=name_abbr)

# Heat map
data_fig2 %>% 
  mutate(name=substr(name,0,3)) %>% 
  left_join(scen_abr) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr)) %>% 
  mutate(aux_height=if_else(Scen_Deposit=="Reference",0.7,1)) %>% 
  ggplot(aes(name_abr,Scen_Deposit))+
  geom_tile(aes(fill=mines_open,height=aux_height),
            col="black",linewidth=0.1)+
  # geom_text(aes(label=mines_open),col="#3A3A3A")+
  geom_text(aes(label = mines_open,
                fontface = ifelse(Scen_Deposit == "Reference", "bold", "plain")),
            size=7*5/14 * 0.8) +
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",
       y="N-1",
       fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient(low = "white", high = "purple") +  # Change "purple" to "red" if preferred
  theme_minimal(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.title.y=element_text(angle=0,margin=margin(r=-10)),
        axis.text.y = element_text(face = ifelse(unique(data_fig2$Scen_Deposit) == "Reference", "bold", "plain")))

# save
ggsave("N_analysis.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)


# Combined Figure ----------


lege <- cowplot::get_plot_component(p1, 'guide-box-bottom', return_all = TRUE)
p1 <- p1 + theme(legend.position = "none")

cowplot::plot_grid(
  cowplot::plot_grid(p1,p2, ncol = 2),
  lege, ncol = 1, rel_heights = c(0.85, 0.15))

# Save with width size of letter
fig_name="Figures/Article/DepositSensitivity.png"
# fig_name="Figures/Article/DepositSensitivity_slack.png"
# fig_name="Figures/Article/DepositSensitivity_cost.png"

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=8.7)


# EoF