# Table 1 - Model results summary table by demand scenario
# PBH July 2024


# Load Results ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load results with script
# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/02-LoadOptimizationResults.R", encoding = "UTF-8")



# Table Analysis for Scenarios -------

# available Reserves
(total_reserve <- sum(deposit$reserve,na.rm=T)/1e3)
(total_resource_demostrated <- sum(deposit$resource_demostrated,na.rm=T)/1e3)
(total_resource_inferred <- sum(deposit$resource_inferred,na.rm=T)/1e3)


# Cumulative Demand
(table_demand <- demand %>% filter(t<2051) %>% group_by(name) %>% 
    reframe(demand=sum(Demand)/1e3) %>% 
    ungroup() %>% mutate(share_reserves=demand/total_reserve))

# peak demand
(table_Peakdemand <- demand %>% filter(t<2051) %>% 
    group_by(name) %>% reframe(peakDemand=max(Demand)/1e3))

# Ratio demand 2022-2050x
(table_ratio <- demand %>% filter(t %in% c(2022,2050)) %>% 
    group_by(name) %>% pivot_wider(names_from = t, values_from = Demand) %>% 
    mutate(ratio=`2050`/`2022`) %>% dplyr::select(name,ratio))

# recycling capacity (flows) at 2050
(table_recycling <- recycling %>%
    filter(t==2050) %>%
    group_by(name) %>%
    reframe(recyc2050=sum(Recycling)/1e3)) # to Mtons

# Increase in capacity of open mines
(table_capIncrease <- df_results %>% 
    filter(t==2050) %>%
    filter(prod_rate>0) %>% # previous existing prod rate
    group_by(name) %>%
    # group_by(name,Deposit_Name) %>% 
    reframe(capIncrease=sum(cap_total)/sum(prod_rate)-1))


# Number of mines opened
(table_open <- df_results %>% 
    filter(t<2051) %>%
    group_by(name) %>% 
    reframe(mines_open=sum(new_mine_open)))

# Mines opened before 2035
(table_open2035 <- df_results %>% filter(t<2036) %>% group_by(name) %>% 
    reframe(mines_open2035=sum(new_mine_open)))

# Deposits depleted
#only valid for open mines

(table_deplete <- df_results %>%
    filter(t<2051) %>%
    filter(paste0(Deposit_Name,name) %in% open_deposits) %>%
    group_by(Deposit_Name,name) %>% 
    reframe(tons_extracted=sum(tons_extracted)) %>% 
    left_join(deposit) %>% 
    mutate(all_resources=reserve+resource_demostrated+resource_inferred) %>% 
    filter(tons_extracted>=all_resources*0.99) %>%
    group_by(name) %>% 
    reframe(depleted=n()))

# deposits at max capacity
(table_maxCap <- df_results %>% 
    filter(t<2051) %>% 
    group_by(Deposit_Name,name) %>% 
    reframe(cap_total=max(cap_total)) %>% 
    filter(cap_total>0) %>%
    left_join(dplyr::select(deposit,Deposit_Name,max_prod_rate)) %>% 
    filter(cap_total>=max_prod_rate*0.99) %>% 
    # view()
    group_by(name) %>% 
    reframe(maxCap=n()))

# deposits at max ramp up
(table_ramp <- df_results %>% 
    filter(t<2051) %>% 
    group_by(Deposit_Name,name) %>% 
    reframe(capacity_added=max(capacity_added)) %>% 
    filter(capacity_added>0) %>%
    left_join(dplyr::select(deposit,Deposit_Name,max_ramp_up)) %>% 
    filter(capacity_added>=max_ramp_up*0.99) %>% 
    group_by(name) %>% 
    reframe(ramp=n()))

# Depletion rate by stage
(table_depletion <- df_results %>% 
    filter(t<2051) %>% 
    group_by(name) %>% 
    reframe(stage1=sum(tons_extracted1/1e3)/total_reserve,
            # state23=sum((tons_extracted2+tons_extracted3)/1e3)/
            #   (total_resource_demostrated+total_resource_inferred),
            stage2=sum(tons_extracted2/1e3)/total_resource_demostrated,
            stage3=sum(tons_extracted3/1e3)/total_resource_inferred))

# Power or capacity achieved in 2050
demand %>% filter(t==2050)

df_results %>% 
  filter(t==2050) %>% 
  # remove depleted deposits
  left_join(deposit) %>% 
  filter(total_extraction<(reserve+resource_demostrated+resource_inferred)*0.99) %>% 
  group_by(name) %>% 
  reframe(cap_total=sum(cap_total))

# capacity reached
df_results %>% 
  filter(t==2050) %>% 
  filter(near(mine_open,1)) %>% 
  group_by(name) %>% 
  reframe(cap_total=sum(cap_total),
          n=n())

# Cost
# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))

(table_cost <- df_results %>%
    filter(t<2051) %>% 
    left_join(deposit) %>% 
    mutate(total_cost=cost1*tons_extracted1+cost2*tons_extracted2+cost3*tons_extracted3+
             capacity_added*cost_expansion+mine_opened*cost_opening) %>% 
    # slack cost at year level
    group_by(name,t) %>% reframe(total_cost=sum(total_cost)) %>% ungroup() %>% 
    left_join(slack) %>%
    mutate(total_cost=total_cost+value*bigM_cost) %>% 
    left_join(discounter) %>% 
    mutate(total_cost=total_cost/r) %>% 
    group_by(name) %>% 
    reframe(total_cost=sum(total_cost)/1e9))

## EDB Index ------
# Of capacity added
(table_edb <- df_results %>%
   filter(t<2051) %>%
   group_by(name,Deposit_Name) %>% 
   reframe(x=sum(capacity_added)) %>% 
   left_join(dplyr::select(deposit,Deposit_Name,edb)) %>% 
   mutate(edb=100-edb) %>% 
   group_by(name) %>% 
   reframe(edb=weighted.mean(edb,x)) %>% ungroup())

# slack or demand not met
(table_slack <- slack %>% filter(t<2051) %>% 
    group_by(name) %>% reframe(slack=sum(value)))

# as share of extraction - less than 0.5%
df_results %>% filter(t<2051) %>% 
  group_by(name) %>% reframe(tons=sum(tons_extracted)) %>% 
  left_join(table_slack) %>% mutate(share_perc=slack/tons*100)


## HHI Index ---------
# Herfindahl–Hirschman index - MEASURES market concentration
# Get country cumulative production and market share
(table_HHI <- df_results %>%
  filter(t<2051) %>%
  left_join(dplyr::select(deposit,Deposit_Name,Country)) %>% 
  group_by(Country,name) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  # Market Share
  group_by(name) %>% 
  mutate(market_share=tons_extracted/sum(tons_extracted)) %>% 
  # top 3 countries
  mutate(market_share_label=round(market_share*100,0)) %>% 
  arrange(desc(market_share)) %>%
  mutate(top_names = paste(head(Country, 3), "(", head(market_share_label, 3), "%)", collapse = ", ")) %>%
  # Index
  reframe(hhi=sum(market_share^2),
          top_names=first(top_names)) %>% ungroup())
# Note
# <0.15, ok
# 0.15 to 0.25 - moderate concentration
#  >0.25 - highly concentrated


## HHI Index with recycling
## Get demand by country
# SLOW TO LOAD
df_country_final <- read.csv("Results/MineralDemand_FewScenarios_Country.csv")
# get only recycling
country_recycling <- df_country_final %>% rename(t=Year) %>% 
  filter(Mineral=="Lithium") %>% 
  filter(Vehicle=="Recycling") %>% 
  group_by(scen_all,Region,Country,t) %>% 
  reframe(Recycling=-sum(tons_mineral)/1e3)

# recycling supply by country
country_recycling <- country_recycling %>% 
  filter(t<2051) %>% rename(Scenario=scen_all) %>% 
  left_join(dict_scen) %>% 
  group_by(Country,name) %>% 
  reframe(tons_extracted=sum(Recycling))

# Country supply 
country_supply <- df_results %>%
  filter(t<2051) %>%
  left_join(dplyr::select(deposit,Deposit_Name,Country)) %>% 
  group_by(Country,name) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup()

# check country name
country_recycling <- country_recycling %>% 
  mutate(Country=case_when(
    Country=="Congo, Rep." ~ "DR Congo",
    Country=="Czechia" ~ "Czech Republic",
    Country=="Russian Federation" ~ "Russia",
    T ~ Country))
both_supply <- rbind(country_supply,country_recycling) %>% 
  group_by(Country,name) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup()

# NOTE THAT WITH RECYCLING EVERY COUNTRY IS PRODUCER 

(table_HHI_rec <- both_supply %>%
  # Market Share
  group_by(name) %>% 
  mutate(market_share=tons_extracted/sum(tons_extracted)) %>% 
  # top 3 countries
  mutate(market_share_label=round(market_share*100,0)) %>% 
  arrange(desc(market_share)) %>%
  mutate(top_names = paste(head(Country, 3), "(", head(market_share_label, 3), "%)", collapse = ", ")) %>%
  # Index
  reframe(hhi_rec=sum(market_share^2),
          top_names_rec=first(top_names)) %>% ungroup())


# Join all
(table_all <- table_demand %>% 
    left_join(table_Peakdemand) %>% 
    left_join(table_recycling) %>%
    left_join(table_depletion) %>%
    left_join(table_capIncrease) %>% 
    left_join(table_open) %>% left_join(table_open2035) %>% 
    left_join(table_maxCap) %>% left_join(table_ramp) %>% 
    left_join(table_edb) %>% 
    # left_join(table_slack) %>% 
    left_join(table_HHI) %>% 
    left_join(table_HHI_rec) %>% 
    left_join(table_cost) %>% 
    mutate(name=factor(name,levels=scens_names)) %>% arrange(name))

table_all %>% dplyr::select(-share_reserves,-top_names,-top_names_rec) %>% 
  pivot_longer(c(-name), names_to = "key", values_to = "value") %>% 
  pivot_wider(names_from = name, values_from = value)

# copy
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)

# Paste in Excel to better visualize the table

# EoF