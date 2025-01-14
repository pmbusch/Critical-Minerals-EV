# Analysis Results of Optimization of Scenarios for diffrent discount rates
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
(runs <- list.dirs("Results/Optimization/DiscountRate",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders

(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))


# Optimization parameters
opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])


# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# separate scenario
df_results <- df_results %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scen_r","Scenario"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL)
unique(df_results$Scenario)
unique(df_results$Scen_r)

slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = (folder_path))))

slack <- slack %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scen_r","Scenario"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))


# add scen name
df_results <- df_results %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# add scenario names
unique(df_results$Scen_r)
df_results <- df_results %>%
  mutate(category_r=if_else(str_detect(Scen_r,"Hyperbolic"),"Hyperbolic","Exponential")) %>% 
  mutate(Scen_r=as.numeric(str_remove_all(Scen_r,"Hyperbolic r|r"))) %>% 
  mutate(Scen_r=paste0(round(Scen_r*100,0),"%")) %>% 
  mutate(Scen_r=factor(Scen_r,levels=paste0("",rev(c(3,7,10,15,25)),"%")))
unique(df_results$Scen_r)
unique(df_results$category)

  
# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,category_r,Scen_r,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1),
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

df_results %>% filter(t<limit_year) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  group_by(name,Scen_r,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

# Slack
slack %>% 
  filter(t<limit_year) %>% 
  group_by(Scenario,Scen_r) %>% reframe(x=sum(value)/1e3) %>% arrange(desc(x))

# Figures  -------------

## Mines opened ---------
data_fig <- df_results %>%
  filter(t<limit_year) %>%
  group_by(name,category_r,Scen_r) %>% 
  reframe(mines_open=sum(new_mine_open)) %>% 
  left_join(scen_abr) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr))
 
# value at reference case
(ref_value <- data_fig %>% filter(str_detect(name,"Ref"),
                                  str_detect(category_r,"Expon"),
                                  str_detect(Scen_r,"7")) %>% pull(mines_open)) 


# Heat map
p1 <- ggplot(data_fig,aes(name_abr,Scen_r))+
  geom_tile(aes(fill=mines_open,
                linewidth = ifelse(str_detect(Scen_r,"7") &name_abr=="Ref."&str_detect(category_r,"Expo"), "thick", "thin")),
            col="black")+
  # geom_text(aes(label=mines_open),col="#3A3A3A",size=7*5/14 * 0.8)+
  geom_text(aes(label = mines_open,
                fontface = ifelse(str_detect(Scen_r,"7")&str_detect(category_r,"Expo"), "bold", "plain")),
            size=7*5/14 * 0.8) +
  ggforce::facet_col(facets = vars(category_r),
                     scales = "free_y",
                     space = "free")+
  coord_cartesian(expand=F)+
  labs(x="Demand Scenario",
       y="Discount Rate",
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
ggsave("Figures/Article/SI_DiscountRate.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)

## Slack or demand Unmet -----
slack <- slack %>% 
  mutate(Scen_r=as.numeric(str_remove_all(Scen_r,"r"))) %>% 
  mutate(Scen_r=paste0(round(Scen_r*100,0),"%")) %>% 
  mutate(Scen_r=factor(Scen_r,levels=paste0("",rev(c(3,7,10,15,25)),"%")))
  
slack_value <- slack %>%
  filter(t<limit_year) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,Scen_r) %>%
  reframe(slack=sum(value))

data_fig2 <- data_fig %>%
  left_join(slack_value) %>% 
  mutate(mines_open=round(slack,0))


(ref_value <- data_fig2 %>% filter(str_detect(name,"Ref"),
                                   str_detect(Scen_r,"7")) %>% pull(mines_open)) 
p_slack <- p1
p_slack$data <- data_fig2
p_slack+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Lithium demand\ncurtailed [ktons]")

ggsave("Figures/Article/SI_DiscountRate_Slack.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)

## Total Cost -----

# Remove effect of discount rate
discounter <- expand.grid(t=2022:(t_size+2021),
                          discount_rate=c(0.03,0.07,0.1,0.15,0.25)) %>% 
  mutate(r=(1+discount_rate)^(t-2022),
         Scen_r=paste0(round(discount_rate*100,0),"%"))

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
  group_by(name,Scen_r) %>%
  reframe(total_cost=sum(total_cost)/1e3) # to billion
slack_cost <- slack %>%
  filter(t<limit_year) %>%
  left_join(discounter) %>%
  mutate(cost=value*bigM_cost/r) %>%
  group_by(name,Scen_r) %>%
  reframe(slack=sum(cost)/1e3) # to billioon
data_fig3 <- data_fig %>%
  left_join(cost) %>%
  left_join(slack_cost) %>%
  mutate(Scen_r=factor(Scen_r,levels=paste0("",rev(c(3,7,10,15,25)),"%"))) %>% 
  mutate(mines_open=round(total_cost+slack,0)) # same name to recreate figure


(ref_value <- data_fig3 %>% filter(str_detect(name,"Ref"),
                                   str_detect(Scen_r,"7")) %>% pull(mines_open)) 
p_cost <- p1
p_cost$data <- data_fig3
p_cost+
  scale_fill_gradient2(low = "#0000FFBB",mid = "#FFFFFFBB",
                       high = "#FF0000BB",midpoint = ref_value)+
  labs(fill="Total Cost\n[billion USD]")

ggsave("Figures/Article/SI_DiscountRate_Cost.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=13,height=8.7)

# EoF