# Cost of dispatch based on otpimization results per year
# PBH June-Oct 2024


# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/01-LoadOptimizationResults.R", encoding = "UTF-8")


# Function to get last cost of dispatch per year
df_results$name %>% unique()
df <- df_results %>% filter(str_detect(name,"Reference"))


df_results %>% 
  filter(t<2051) %>%
  left_join(deposit,by="Deposit_Name") %>% 
  filter(total_extraction>0) %>% 
  mutate(cost=case_when(
    tons_extracted3>tons_extracted2 ~ cost3,
    tons_extracted2>tons_extracted1 ~ cost2,
    T ~ cost1)) %>%
  group_by(name,t) %>% 
  # slice_max(order_by = cost) %>% ungroup() %>% # Max cost
  # reframe(cost = DescTools::Quantile(cost, weights=tons_extracted,probs=0.8)) %>%  # Top quantile cost
  reframe(cost = weighted.mean(cost, tons_extracted)) %>%  # Average cost
  mutate(cost=cost/5.323) %>%  # to LCE
  # mutate(name=factor(name,levels=order_n)) %>%
  ggplot(aes(t,cost))+
  geom_line(aes(col=name))+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_color_manual(values = scen_colors)+
  # ylim(0,max_slack*1.1)+
  theme_bw(8)+
  labs(x="",y="",title="Average Extraction Cost [$USD2022/ton LCE]",
       col="Demand \nScenario")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave("Figures/Article/AverageCost.png",
       ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=8.7)

# By also checking prod and capacity constraints --------

data_fig <- df_results %>% 
  filter(t<2051) %>% 
  filter(mine_open>0) %>% 
  filter(cap_total>0) %>% # can produce something
  # filter(tons_extracted==cap_total) %>%
  left_join(deposit,by="Deposit_Name") %>% 
  # filter deposits at max capacity and ramp up
  filter(tons_extracted+1<cap_total) %>%
  filter(cap_total+1<max_prod_rate) %>%
  filter(capacity_added+1<max_ramp_up) %>% 
  mutate(cost=case_when(
    tons_extracted3>tons_extracted2 ~ cost3,
    tons_extracted2>tons_extracted1 ~ cost2,
    T ~ cost1)) %>%
  mutate(cost_expansion=if_else(tons_extracted<cap_total,0,cost_expansion)) %>% 
  mutate(cost_mg=(cost+cost_expansion)/5.323)

data_fig <- data_fig %>% 
  group_by(name,t) %>%
  slice_min(order_by = cost) %>% ungroup() %>%
  dplyr::select(t,d,name,
                # tons_extracted1,tons_extracted2,tons_extracted3,
                tons_extracted,cap_total,max_prod_rate,
                capacity_added,max_ramp_up,
                # cost1,cost2,cost3,
                cost_mg,cost,cost_expansion) %>% 
  arrange(t) 

data_fig %>% filter(t==2040) %>% dplyr::select(name,cost_mg)


data_fig %>% 
  ggplot(aes(t,cost_mg))+
  geom_line(aes(col=name))+
  # coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  # ylim(0,max_slack*1.1)+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Demand \nScenario")


