# Analysis Results of Optimization of Recycling Loop Demand Scenarios
# Optimization is run in Julia
# Files are run by demand scenario, multiple files
# PBH August 2024


# Load Data -----------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))


# note that some deposits parameters are different for each run - so do not use them sparingly
# Input Parameters
demand <- read.csv("Parameters/DemandRecyclingLoop.csv")
recycling <- read.csv("Parameters/Recycling.csv")
deposit <- read.csv("Parameters/Deposit.csv")

(d_size <- nrow(deposit))
(t_size <- nrow(filter(demand,Scenario=="Ambitious-Baseline-Baseline-Baseline-Recycling percentage 5")))

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

## Recycling Supply Scenarios -------------
# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandRecyclingLoop",recursive = F))
(dict_scen <- tibble(Scenario=runs) %>% 
    mutate(name=case_when(
      str_detect(Scenario,"High") ~ "Large Capacity LIB",
      str_detect(Scenario,"Low") ~ "Small Capacity LIB",
      T ~ "Reference"),
      recyc= as.numeric(str_extract(Scenario, "(?<=percentage )\\d+"))/100))

opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])
(discount_rate <- opt_param[1,2] )

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# separate scenarios
df_results <- df_results %>% left_join(dict_scen)
  
slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = (folder_path))))

slack <- slack %>% left_join(dict_scen)

slack %>% group_by(name,recyc) %>% reframe(x=sum(value)/1e3)


# add deposits name
# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  group_by(name,recyc,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1),
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

# Deposit Scenarios -------------

## Mines opened ---------
data_fig <- df_results %>%
  filter(t<2051) %>%
  group_by(name,recyc) %>% 
  reframe(mines_open=sum(new_mine_open)) %>%  ungroup() %>% 
  mutate(name=factor(name,levels=c("Large Capacity LIB","Reference","Small Capacity LIB")))

ggplot(data_fig,aes(recyc,mines_open,col=name))+
  # geom_line()+
  geom_point()+
  # geom_smooth(se=F)+
  coord_cartesian(expand = F,ylim=c(0,90))+
  scale_x_continuous(labels=scales::percent,limits = c(0,1),breaks = seq(0.05,0.95,0.1))+
  scale_color_manual(values = c( "Reference" = "#000000","Large Capacity LIB"="#8B0000",
                                 "Small Capacity LIB"="#56B4E9"))+
  labs(x="Global Lithium Recovery Scenario (%)",y="",title="New Deposits openings required by 2050",
       col="Battery Capacity Scenario")+
  theme(legend.position = c(0.75,0.9),
        # axis.text.x = element_text(hjust = 1),
        legend.background = element_rect(fill = "transparent", color = "lightgray"),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
  

ggsave("Figures/Article/recyclingLoop.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7,height=8.7)


# EoF