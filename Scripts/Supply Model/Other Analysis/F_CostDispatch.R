# Function to get last cost of dispatch per year

df_results$Scenario %>% unique()
df <- df_results %>% filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline")


df_results %>% 
  filter(t<2051) %>% 
  left_join(deposit,by="d") %>% 
  filter(total_extraction>0) %>% 
  mutate(cost=case_when(
    tons_extracted3>tons_extracted2 ~ cost3,
    tons_extracted2>tons_extracted1 ~ cost2,
    T ~ cost1)) %>%
  group_by(Scenario,t) %>% 
  slice_max(order_by = cost) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% #scenario name
  mutate(cost=cost/5.323) %>%  # to LCE
  mutate(name=factor(name,levels=order_n)) %>%
  ggplot(aes(t,cost))+
  geom_line(aes(col=name))+
  # coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  # ylim(0,max_slack*1.1)+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Demand \nScenario")


data_fig <- df_results %>% 
  filter(t<2051) %>% 
  filter(mine_open>0) %>% 
  filter(cap_total>0) %>% # can produce something
  # filter(tons_extracted==cap_total) %>%
  left_join(deposit,by="d") %>% 
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
  group_by(Scenario,t) %>%
  slice_min(order_by = cost) %>% ungroup() %>%
  dplyr::select(t,d,Scenario,
                # tons_extracted1,tons_extracted2,tons_extracted3,
                tons_extracted,cap_total,max_prod_rate,
                capacity_added,max_ramp_up,
                # cost1,cost2,cost3,
                cost_mg,cost,cost_expansion) %>% 
  arrange(t) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% #scenario name
  mutate(name=factor(name,levels=order_n))


data_fig %>% filter(t==2040) %>% dplyr::select(Scenario,cost_mg)


data_fig %>% 
  ggplot(aes(t,cost_mg))+
  geom_line(aes(col=name))+
  # coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  # ylim(0,max_slack*1.1)+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Demand \nScenario")








## Are SP for demand equal to extraction and expansion costs? -----
# SP values, discounted
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))
sp_demand %>% 
  left_join(discounter) %>% 
  mutate(value=value*r) %>% 
  # head(12)
  ggplot(aes(t,value))+geom_line()


df_results %>% 
  filter(mine_open>0) %>% 
  filter(cap_total>0) %>% # can produce something
  # filter(tons_extracted==cap_total) %>%
  left_join(deposit,by="d") %>% 
  # filter deposits at max capacity and ramp up
  filter(tons_extracted+1<cap_total) %>%
  filter(cap_total+1<max_prod_rate) %>%
  filter(capacity_added+1<max_ramp_up) %>% 
  mutate(cost=case_when(
    tons_extracted3>tons_extracted2 ~ cost3,
    tons_extracted2>tons_extracted1 ~ cost2,
    T ~ cost1)) %>%
  mutate(cost_expansion=if_else(tons_extracted<cap_total,0,cost_expansion)) %>% 
  mutate(cost_mg=(cost+cost_expansion)/1e3) %>% 
  # filter(t==2052) %>%
  group_by(t) %>%
  slice_min(order_by = cost) %>% ungroup() %>%
  dplyr::select(t,d,
                # tons_extracted1,tons_extracted2,tons_extracted3,
                tons_extracted,cap_total,max_prod_rate,
                capacity_added,max_ramp_up,
                # cost1,cost2,cost3,
                cost_mg,cost,cost_expansion) %>% 
  arrange(t)
# ggplot(aes(t,cost_mg))+geom_line()
# arrange(cost_mg)