# Analysis Results of Optimization of Scenarios for Non Monetary Factors
# Also model with No Cost
# Optimization is run in Julia
# Files are run for each demand scenario
# PBH December 2024


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


# Load Results -------------
# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/NonMonetary",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders

(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))


# Optimization parameters
opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])
(discount_rate <- opt_param[1,2] )

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
  separate(path, into = c("Aux","Scen_Factor","Scenario"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL)
unique(df_results$Scenario)
unique(df_results$Scen_Factor)

slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = (folder_path))))

slack <- slack %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scen_Factor","Scenario"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


# add scen name
df_results <- df_results %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# add scenario names
unique(df_results$Scen_Factor)
df_results <- df_results %>% 
  mutate(NM_Factor=str_extract(Scen_Factor,"OnlyCost|EDB|WGI|WCR"),
         NM_weight=as.numeric(str_remove_all(Scen_Factor,
                                         "OnlyCost|EDB|WGI|WCR| |Weight")),
         NM_weight=if_else(is.na(NM_weight),0,NM_weight),
         NM_weight=paste0("",round(NM_weight*100,0),"%")) 
# Factorize
aux_levels <- c("No Non-Monetary Index",
                "Ease of Doing Business Index",
                "World Governance Index",
                "World Competitiveness Index")
df_results <- df_results %>% 
  mutate(NM_Factor=case_when(
    NM_Factor=="EDB" ~ aux_levels[2],
    NM_Factor=="WGI" ~ aux_levels[3],
    NM_Factor=="WCR" ~ aux_levels[4],
    T ~ aux_levels[1]) %>% 
      factor(levels=c(aux_levels)))
rm(aux_levels)
df_results <- df_results %>% 
  mutate(NM_weight=factor(NM_weight,levels=paste0("",rev(c(0,1,3,5,10,15)),"%")))


# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,NM_Factor,NM_weight,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1),
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results %>% filter(t<limit_year) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,NM_Factor,NM_weight,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack %>% 
  filter(t<limit_year) %>% 
  group_by(Scenario,Scen_Factor) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))

# Figures  -------------

## Mines opened ---------
data_fig <- df_results %>%
  filter(t<limit_year) %>%
  group_by(name,NM_Factor,NM_weight) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  left_join(scen_abr) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr))

# value at refernce case
(ref_value <- data_fig %>% filter(str_detect(name,"Ref"),
                                  str_detect(NM_Factor,"Ease of"),
                                  NM_weight=="10%") %>% pull(mines_open)) 
  

# Heat map
p1 <- ggplot(data_fig,aes(name_abr,NM_weight))+
  geom_tile(aes(fill=mines_open,
                linewidth = ifelse(str_detect(NM_Factor,"Ease of") & NM_weight=="10%"&name_abr=="Ref.", "thick", "thin")),
            col="black")+
  # geom_text(aes(label=mines_open),col="#3A3A3A",size=7*5/14 * 0.8)+
  geom_text(aes(label = mines_open,
                fontface = ifelse(str_detect(NM_Factor,"Ease of") & NM_weight=="10%", "bold", "plain")),
            size=7*5/14 * 0.8) +
  ggforce::facet_col(facets = vars(NM_Factor), 
                     scales = "free_y", 
                     space = "free")+
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",
       y="Cost Degradation",
       fill="Number of \nnew opened \nDeposits")+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+ 
  scale_linewidth_manual(values = c(thick = 0.5, thin = 0.1)) +
  guides(linewidth="none")+
  theme_minimal(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.text = element_text(size=9),
        legend.text = element_text(size=9),
        axis.title = element_text(size=9))
p1
# save
ggsave("Figures/Article/SI_NonMonetaryFactors.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)

## Slack or demand Unmet -----

dict <- df_results %>% group_by(Scen_Factor,NM_Factor,NM_weight) %>% 
  tally() %>% mutate(n=NULL)

slack_value <- slack %>%
  filter(t<limit_year) %>%
  left_join(dict) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,NM_Factor,NM_weight) %>%
  reframe(slack=sum(value))
data_fig2 <- data_fig %>%
  left_join(slack_value) %>% 
  mutate(mines_open=round(slack,0))


(ref_value <- data_fig2 %>% filter(str_detect(name,"Ref"),
                                  str_detect(NM_Factor,"Ease of"),
                                  NM_weight=="10%") %>% pull(mines_open)) 
p_slack <- p1
p_slack$data <- data_fig2
p_slack+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Lithium demand\ncurtailed [ktons]")
  
ggsave("Figures/Article/SI_NonMonetaryFactors_Slack.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)


## Total Cost -----

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
  group_by(name,NM_Factor,NM_weight) %>%
  reframe(total_cost=sum(total_cost)/1e3) # to billion
slack_cost <- slack %>%
  filter(t<limit_year) %>%
  left_join(discounter) %>%
  mutate(cost=value*bigM_cost/r) %>%
  left_join(dict) %>% 
  group_by(name,NM_Factor,NM_weight) %>%
  reframe(slack=sum(cost)/1e3) # to billioon
data_fig3 <- data_fig %>%
  left_join(cost) %>%
  left_join(slack_cost) %>%
  mutate(mines_open=round(total_cost+slack,0)) # same name to recreate figure
         

(ref_value <- data_fig3 %>% filter(str_detect(name,"Ref"),
                                   str_detect(NM_Factor,"Ease of"),
                                   NM_weight=="10%") %>% pull(mines_open)) 
p_cost <- p1
p_cost$data <- data_fig3
p_cost+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Total Cost\n[billion USD]")

ggsave("Figures/Article/SI_NonMonetaryFactors_Cost.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)
         
# EoF