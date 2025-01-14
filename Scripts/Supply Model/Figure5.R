# Analysis Results of Optimization of Scenarios deposits
# Optimization is run in Julia
# Files are run by demand scenario and deposit scenario, so multiple files
# PBH July 2024


# Load Data -----------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

scen_abr <- tibble(name=scens_names,name_abr=name_abbr)

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


# Deposits Scenarios -------------

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
              # "NoTax","RockTransportCosts")
dep_scen_name <- c("Reference",
                   "No DLE Brine","All Brine DLE",
                   "2% Max prod. rate","5% Max prod. rate",
                   "1-year ramp-up","2-year ramp-up","8-year ramp-up",
                   "No clay resources","No inferred resources",
                   "Shorter Lead Time","Longer Lead Time")
                   # "No Tax or Royalty","20% Hard Rock Transport Costs")
df_results <- df_results %>% 
  left_join(tibble(Scen_Deposit=dep_scen,
                   dep_scen=dep_scen_name)) %>% 
  mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name))) %>% 
  filter(!is.na(dep_scen))
unique(df_results$dep_scen)


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

# categories
cat_levels <- c("Reference Case","Brine Extraction Technology",
                "Max Production Rate","Max Ramp Up",
                "Mine Opening Lead Time","Limited Resources",
                "Other")

df_results <- df_results %>% 
  mutate(category=case_when(
    dep_scen=="Reference" ~ cat_levels[1],
    str_detect(dep_scen,"Brine") ~ cat_levels[2],
    str_detect(dep_scen,"Max prod") ~ cat_levels[3],
    str_detect(dep_scen,"ramp") ~ cat_levels[4],
    str_detect(dep_scen,"Lead Time") ~ cat_levels[5],
    str_detect(dep_scen,"resources") ~ cat_levels[6],
    T ~ cat_levels[7]) %>% factor(levels=cat_levels))


## Mines opened ---------
data_fig <- df_results %>%
  filter(t<limit_year) %>%
  group_by(name,category,dep_scen) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% ungroup() %>% 
  left_join(scen_abr) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr))

# value at reference case
(ref_value <- data_fig %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 

## Heatmap 
p1 <- ggplot(data_fig,aes(name_abr,dep_scen))+
  geom_tile(aes(fill=mines_open,
                linewidth = ifelse(str_detect(dep_scen,"Reference") &name_abr=="Ref.", "thick", "thin")),
            col="black")+
  # geom_text(aes(label=mines_open),col="#3A3A3A",size=7*5/14 * 0.8)+
  geom_text(aes(label = mines_open,
                fontface = ifelse(str_detect(dep_scen,"Reference"), "bold", "plain")
                ),
            size=5*5/14 * 0.8) +
  ggforce::facet_col(facets = vars(category),
                     scales = "free_y",
                     space = "free")+
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",
       y="",
       fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+ 
  scale_linewidth_manual(values = c(thick = 0.5, thin = 0.1)) +
  guides(linewidth="none")+
  theme_minimal(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.text = element_text(size=7),
        legend.text = element_text(size=9),
        axis.title = element_text(size=9),
        axis.text.y = element_text(size=5),
        axis.text.x = element_text(size=5))
p1


## Slack -----

dict <- df_results %>% group_by(Scen_Deposit,dep_scen,category) %>% 
  tally() %>% mutate(n=NULL)

slack_value <- slack %>%
  filter(t<limit_year) %>%
  left_join(dict) %>% filter(!is.na(dep_scen)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,category,dep_scen) %>%
  reframe(slack=sum(value))
data_fig2 <- data_fig %>%
  left_join(slack_value) %>%
  mutate(dep_scen=factor(dep_scen,levels=rev(dep_scen_name))) %>%
  mutate(mines_open=round(slack,0))

(ref_value <- data_fig2 %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 

p_slack <- p1
p_slack$data <- data_fig2
p_slack <- p_slack+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Lithium demand\ncurtailed [ktons]")
p_slack

## Total Cost ----

# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))
cost <- df_results %>%
  filter(t<limit_year) %>%
  left_join(deposit) %>%
  # all costs are converted to million usd, same as julia
  mutate(total_cost=cost1*tons_extracted1/1e3+
           cost2*tons_extracted2/1e3+
           cost3*tons_extracted3/1e3+
           capacity_added*cost_expansion/1e3+
           mine_opened*cost_opening/1e6) %>%
  left_join(discounter) %>%
  mutate(total_cost=total_cost/r) %>%
  group_by(name,category,dep_scen) %>%
  reframe(total_cost=sum(total_cost)/1e3) # to billion
slack_cost <- slack %>%
  filter(t<limit_year) %>%
  left_join(discounter) %>%
  mutate(cost=value*bigM_cost/r) %>%
  left_join(dict) %>% filter(!is.na(dep_scen)) %>% 
  group_by(name,category,dep_scen) %>%
  reframe(slack=sum(cost)/1e3)
data_fig3 <- data_fig %>%
  left_join(cost) %>%
  left_join(slack_cost) %>%
  mutate(total_cost=total_cost+slack) %>%
  mutate(mines_open=round(total_cost,0))

(ref_value <- data_fig3 %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 

p_cost <- p1
p_cost$data <- data_fig3
p_cost <- p_cost+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Total Cost\n[billion USD]")
p_cost

######################
# N-1 Country ------------
####################

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
                 "Tanzania","Australia",
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

## Mines opened ---------
data_fig_N <- df_results2 %>%
  filter(t<limit_year) %>%
  group_by(name,Scen_Deposit) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% ungroup() 

data_fig_N <- data_fig_N %>% mutate(Scen_Deposit=factor(Scen_Deposit,levels=rev(count_order))) %>% 
  left_join(scen_abr) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr)) %>% 
  mutate(aux_height=if_else(Scen_Deposit=="Reference",0.7,1)) 


(ref_value <- data_fig_N %>% filter(str_detect(name,"Ref"),
                                  str_detect(Scen_Deposit,"Ref")) %>% pull(mines_open)) 

# Heat map
p2 <- ggplot(data_fig_N,aes(name_abr,Scen_Deposit))+
  geom_tile(aes(fill=mines_open,
                linewidth = ifelse(str_detect(Scen_Deposit,"Reference") &name_abr=="Ref.", "thick", "thin")),
            col="black")+
  # geom_text(aes(label=mines_open),col="#3A3A3A",size=7*5/14 * 0.8)+
  geom_text(aes(label = mines_open,
                fontface = ifelse(str_detect(Scen_Deposit,"Reference"), "bold", "plain")),
            size=5*5/14 * 0.8) +
  coord_cartesian(expand=F,clip="off")+
  labs(x="Demand Scenario",
       y="",
       fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+ 
  scale_linewidth_manual(values = c(thick = 0.5, thin = 0.1)) +
  guides(linewidth="none")+
  theme_minimal(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.text = element_text(size=9),
        legend.text = element_text(size=9),
        axis.title = element_text(size=9),
        axis.text.x = element_text(size=5))
p2

## Slack ----
slack_value2 <- slack2 %>%
  filter(t<limit_year) %>%
  mutate(Scen_Deposit=Scen_Deposit %>% str_replace("Base","Reference")) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,Scen_Deposit) %>%
  reframe(slack=sum(value))
data_fig_N2 <- data_fig_N %>%
  left_join(slack_value2) %>%
  mutate(Scen_Deposit=factor(Scen_Deposit,levels=rev(count_order))) %>% 
  mutate(mines_open=round(slack,0))

(ref_value <- data_fig_N2 %>% filter(str_detect(name,"Ref"),
                                  str_detect(Scen_Deposit,"Ref")) %>% pull(mines_open)) 

p_slack2 <- p2
p_slack2$data <- data_fig_N2
p_slack2 <- p_slack2+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Lithium demand\ncurtailed [ktons]")
p_slack2

## Total Cost -----
# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))
cost2 <- df_results2 %>%
  filter(t<limit_year) %>%
  left_join(deposit) %>%
  # all costs are converted to million usd, same as julia
  mutate(total_cost=cost1*tons_extracted1/1e3+
           cost2*tons_extracted2/1e3+
           cost3*tons_extracted3/1e3+
           capacity_added*cost_expansion/1e3+
           mine_opened*cost_opening/1e6) %>%
  left_join(discounter) %>%
  mutate(total_cost=total_cost/r) %>%
  mutate(Scen_Deposit=Scen_Deposit %>%  str_replace("Base","Reference")) %>%
  group_by(name,Scen_Deposit) %>%
  reframe(total_cost=sum(total_cost)/1e3) # to billion
slack_cost2 <- slack2 %>%
  filter(t<limit_year) %>%
  left_join(discounter) %>%
  mutate(cost=value*bigM_cost/r) %>%
  mutate(Scen_Deposit=Scen_Deposit %>%  str_replace("Base","Reference")) %>%
  group_by(name,Scen_Deposit) %>%
  reframe(slack=sum(cost)/1e3)
data_fig_N3 <- data_fig_N %>%
  left_join(cost2) %>%
  left_join(slack_cost2) %>%
  mutate(Scen_Deposit=factor(Scen_Deposit,levels=rev(count_order))) %>% 
  mutate(total_cost=total_cost+slack) %>%
  mutate(mines_open=round(total_cost,0))

(ref_value <- data_fig_N3 %>% filter(str_detect(name,"Ref"),
                                  str_detect(Scen_Deposit,"Ref")) %>% pull(mines_open)) 

p_cost2 <- p2
p_cost2$data <- data_fig_N3
p_cost2 <- p_cost2+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Total Cost\n[billion USD]")
p_cost2

# Combined Figure ----------

## Mines Open ----

# Force them to share legend
range(data_fig$mines_open)
range(data_fig_N$mines_open)

# value at reference case
(ref_value <- data_fig %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 


p1_plot <- p1+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,110))+
  labs( title="(A) Deposit Parameters Sensitivity")+
  theme(legend.position = "none",
        plot.margin = margin(5,-3,5,-10))
p2_plot <- p2+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,110))+
  labs(fill="Number of new\nopened deposits",
       title="(B) N-1 Country Analysis")+
  theme(legend.position = "bottom",
        plot.margin = margin(5,5,5,-5))
  
cowplot::plot_grid(p1_plot,p2_plot,ncol=2)

# Save with width size of letter
fig_name="Figures/Article/Figure5.png"

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=600,
       width=20,height=10)
pdf(str_replace(fig_name,"png","pdf"),
    width=18/2.54,height=8.7/2.54)
ggplot2::last_plot()
dev.off()


## Cost -----

# value at reference case
(ref_value <- data_fig3 %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 

# Force them to share legend
range(data_fig3$mines_open)
range(data_fig_N3$mines_open)
p_cost_plot <- p_cost+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,1000))+
  labs( title="(A) Deposit Parameters Sensitivity")+
  theme(legend.position = "none",
        plot.margin = margin(5,-3,5,-10))
p2_cost_plot <- p_cost2+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,1000))+
  labs(fill="Total Cost\n[billion USD]",
       title="(B) N-1 Country Analysis")+
  theme(legend.position = "bottom",
        plot.margin = margin(5,5,5,-5))



cowplot::plot_grid(p_cost_plot,p2_cost_plot, ncol = 2)

# Save with width size of letter
fig_name="Figures/Article/Figure5_cost.png"

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=600,
       width=20,height=10)


## Slack -----

(ref_value <- data_fig2 %>% filter(str_detect(name,"Ref"),
                                  str_detect(dep_scen,"Ref")) %>% pull(mines_open)) 

range(data_fig2$mines_open)
range(data_fig_N2$mines_open)
p_slack_plot <- p_slack+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,400))+
  labs( title="(A) Deposit Parameters Sensitivity")+
  theme(legend.position = "none",
        plot.margin = margin(5,-3,5,-10))
p2_slack_plot <- p_slack2+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value,
                       limits=c(0,400))+
  labs(fill="Lithium demand\ncurtalied [ktons]",
       title="(B) N-1 Country Analysis")+
  theme(legend.position = "bottom",
        plot.margin = margin(5,5,5,-5))


cowplot::plot_grid(p_slack_plot,p2_slack_plot, ncol = 2)

# Save with width size of letter
fig_name="Figures/Article/Figure5_slack.png"

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=600,
       width=20,height=10)


# EoF