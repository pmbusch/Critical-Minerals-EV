# Analysis Results of Optimization
# PBH March 2024

# Run Julia to get all the results much faster
# system("Scripts/Optimization/Julia_Opt.jl") # not working

source("Scripts/00-Libraries.R", encoding = "UTF-8")

demand <- read.csv("Parameters/Demand.csv")
recycling <- read.csv("Parameters/Recycling.csv")
deposit <- read.csv("Parameters/Deposit.csv")

(d_size <- nrow(deposit))
(t_size <- nrow(demand))

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

# bigM_cost <- 1e6 # same as Julia
bigM_cost <- 100000*5.323 # historic high was 68K for LCE
discount_rate <- 0.07 # same as Julia
# discount_rate <- 0.03


# Single Run Analysis ------
folder <- "DemandScenario"
scen <- "Ambitious-Baseline-Baseline-Baseline-Baseline"
demand <- demand %>% filter(Scenario==scen)
# scen <- "EDB"
# folder <- "EDBCurve"
# scen <- "EDBLoop 1"

# load results from Julia
url_result <- paste0("Results/Optimization/Base/%s.csv") # debug
# url_result <- paste0("Results/Optimization/",folder,"/",scen,"/%s.csv")
df_results <- read.csv(sprintf(url_result,"Base_Julia")) %>% 
  rename(Deposit_Name=d)
df_results_LP <- read.csv(sprintf(url_result,"Base_Julia_LP"))
slack <- read.csv(sprintf(url_result,"Slack_Julia"))

# shadow prices
sp_demand <- read.csv(sprintf(url_result,"Julia_sp_demand"))
sp_reserve <- read.csv(sprintf(url_result,"Julia_sp_reserve"))
sp_rest <- read.csv(sprintf(url_result,"Julia_sp_rest"))

# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(prod_rate) %>% 
  group_by(Deposit_Name) %>% mutate(cap_total=cumsum(capacity_added)+prod_rate,
                         mine_open=cumsum(mine_opened),
                         total_extraction=cumsum(tons_extracted),
                         total_extraction1=cumsum(tons_extracted1),
                         total_extraction2=cumsum(tons_extracted2),
                         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

# Report generation Markdown
# Figures are in report
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))
fig_name <- "Figures/Optimization/%s.png"
save_figures <- F # change to T or F

rmarkdown::render("Scripts/Optimization/Run_Report.Rmd",
                  output_file = "../../Optimization Reports/Report.pdf")
                  # output_file = "ReportReserves.pdf")

## Average extraction per deposit in 2050 ---------
df_results %>% filter(t==2050) %>% 
  filter(mine_open==1) %>% 
  filter(tons_extracted>0) %>% 
  arrange(desc(tons_extracted)) %>% 
  reframe(x=mean(tons_extracted),
          y=mean(cap_total))


## Reserves consumption by stage ---------
deposit$reserve %>% sum()/1e3 # million
df_results$tons_extracted %>% sum()/1e3

(size_stage1 <- sum(deposit$reserve))
(size_stage2 <- sum(deposit$resource_demostrated))
(size_stage3 <- sum(deposit$resource_inferred))

df_results %>% 
  # filter(t<2051) %>%
  group_by(t) %>% 
  reframe(total_extraction1=sum(total_extraction1),
          total_extraction2=sum(total_extraction2),
          total_extraction3=sum(total_extraction3)) %>% ungroup() %>% 
  mutate(total_extraction1=total_extraction1/size_stage1,
         total_extraction2=total_extraction2/size_stage2,
         total_extraction3=total_extraction3/size_stage3) %>%   
  pivot_longer(c(total_extraction1,total_extraction2,total_extraction3), 
               names_to = "Stage", values_to = "value") %>% 
  mutate(Stage=str_replace(Stage,"total_extraction","Stage ")) %>% 
  ggplot(aes(t,value,col=Stage))+
  geom_line()+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",title="Share of depleted Reserves")



## Average grade -----
weighted.mean(deposit$Grade_percLi_Reserve,deposit$reserve,na.rm=T)
weighted.mean(deposit$grade_resource,deposit$resource_demostrated,na.rm=T)
weighted.mean(deposit$grade_resource_inferred,deposit$resource_inferred,na.rm=T)

df_grade <- df_results %>% 
  filter(t<2051) %>%
  group_by(d) %>% 
  reframe(total_extraction1=sum(tons_extracted1),
          total_extraction2=sum(tons_extracted2),
          total_extraction3=sum(tons_extracted3)) %>% ungroup() %>% 
  left_join(deposit,by="d") %>% 
  mutate(stage1=reserve-total_extraction1,
         stage2=resource_demostrated-total_extraction2,
         stage3=resource_inferred-total_extraction3)
range(df_grade$stage1)

weighted.mean(df_grade$Grade_percLi_Reserve,df_grade$stage1,na.rm=T)
weighted.mean(df_grade$grade_resource,df_grade$stage2,na.rm=T)
weighted.mean(df_grade$grade_resource_inferred,df_grade$stage3,na.rm=T)

## Extraction by deposit -----
df_results %>% 
  left_join(deposit,by="d") %>% 
  group_by(d,Deposit_Name,Country) %>% 
  reframe(total_extraction=sum(total_extraction),
          open=sum(mine_opened)) %>% 
  arrange(desc(total_extraction))


## Non Monetary Factors ---------

data_fig <- df_results %>% 
  filter(t<2051) %>% 
  group_by(d) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  filter(tons_extracted>0.01) %>% # no extraction
  left_join(deposit,by="d") %>%
  mutate(edb=100-edb) %>% 
  mutate(cost1=cost1/5.323)


# for label
countries <- data_fig %>% filter(tons_extracted>0.2) %>% 
  group_by(Country) %>% reframe(edb=mean(edb)) %>% ungroup()

ggplot(data_fig,aes(cost1,edb))+
  geom_point(alpha=.5,aes(size=tons_extracted,col=Resource_Type))+
  geom_text_repel(data=countries,aes(label=Country),x=100,
                  fontface = "italic",
                  hjust=0,min.segment.length = unit(0,"lines"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_color_manual(values=resource_colors)+
  labs(y="Ease of \nDoing \nBusiness \n[0-100]",x="Extraction Costs [USD/ton LCE]",
       size="Total Extraction \n[M tons Li]",col="Resource \ntype")



  
## Production in opened deposits -----------
data_fig <- df_results %>% 
  # filter(t<2051) %>% 
  left_join(deposit,by="d") %>% 
  filter(cost_opening==0) %>%
  mutate(share=total_extraction/(reserve+resource_demostrated+resource_inferred))

ggplot(data_fig,aes(t,share,col=Resource_Type,group=Deposit_Name))+
  geom_line(alpha=.5)+
  geom_text_repel(data = filter(data_fig,t==2070),aes(label=Deposit_Name),
            nudge_x = 2,max.overlaps = 50)+  
  scale_y_continuous(labels=scales::percent)+
  scale_color_manual(values=resource_colors)+
  labs(x="",y="",title = "Open deposits: Resource depletion",col="Resource Type")
f.fig.save(sprintf(fig_name,"OpenDepositsProduction"),w = 30,h=20)


## Extraction vs capacity ------------
df_results %>% names()

df_results %>% 
  filter(t<2051) %>% 
  group_by(d) %>% 
  reframe(cap_total=max(cap_total),
          total_extraction=max(total_extraction)) %>% ungroup() %>% 
  # filter(cap_total>0) %>% 
  filter(total_extraction>0) %>% 
  left_join(deposit) %>% 
  ggplot(aes(cap_total,total_extraction))+
  geom_point(aes(col=Resource_Type))+
  geom_text_repel(aes(label=Deposit_Name))
  c# labs(x="Total Extraction Capacity [ktons Li/year]",
       # y="Total Li extracted [ktons]")



# Playground
aux <- df_results %>% 
  left_join(deposit,by="d") %>% 
  filter(Resource_Type=="Hard Rock")
ggplot(aux,aes(t,tons_extracted,group=d))+
  geom_text_repel(data=filter(aux,t==2035),aes(label=Deposit_Name))+
  geom_line()

## Demand vs Supply at country level -----

supply_country <- df_results %>% 
  filter(t<2051) %>% 
  left_join(deposit) %>% 
  group_by(Country) %>% 
  reframe(extraction=sum(tons_extracted)/1e3) %>% 
  arrange(desc(extraction))


demandRegion <- read.csv("Results/MineralDemand_FewScenarios.csv")
demandRegion <- demandRegion %>% 
  filter(Mineral=="Lithium") %>% 
  mutate(Scenario=paste(Scenario,chem_scenario,capacity_scenario,
                        lifetime_scenario,recycling_scenario,sep="-"))
demandRegion <- demandRegion %>% 
  filter(scen_all=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>% 
  filter(Year<2051)
demandRegion <- demandRegion %>% 
  group_by(Region) %>% 
  reframe(demand=sum(tons_mineral)/1e6)
demandRegion <- demandRegion %>% arrange(desc(demand))

supply_country
demandRegion


# Multiple Run Analysis -----------
# load results from Julia
# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))

# Get only Scenarios, not Countries
# runs <- runs[str_detect(runs,"Ambitious")] # Scenarios

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
                        transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
                                  Scenario = basename(folder_path)))) %>% 
  rename(Deposit_Name=d)
df_results$Scenario %>% unique()


# df_results_LP <- do.call(rbind, lapply(runs, function(folder_path) 
#   transform(read.csv(file.path(folder_path, "Base_Julia_LP.csv")), 
#             Scenario = basename(folder_path)))) %>% 
#   rename(Deposit_Name=d)

slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = basename(folder_path))))

# shadow prices
# sp_demand <- do.call(rbind, lapply(runs, function(folder_path) 
#   transform(read.csv(file.path(folder_path, "Julia_sp_demand.csv")), 
#             Scenario = basename(folder_path))))
# 
# sp_reserve <- do.call(rbind, lapply(runs, function(folder_path) 
#   transform(read.csv(file.path(folder_path, "Julia_sp_reserve.csv")), 
#             Scenario = basename(folder_path)))) %>% 
#   rename(Deposit_Name=d)
# 
# sp_rest <- do.call(rbind, lapply(runs, function(folder_path) 
#   transform(read.csv(file.path(folder_path, "Julia_sp_rest.csv")), 
#             Scenario = basename(folder_path)))) %>% 
#   rename(Deposit_Name=d)

# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  left_join(prod_rate) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & mine_opened==1,
         cap_total=cumsum(capacity_added)+prod_rate,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()


# subset of opened deposits
open_deposits <- df_results %>% group_by(Deposit_Name,Scenario) %>% 
  reframe(mine_open=sum(mine_opened)) %>% filter(mine_open>0) %>% 
  mutate(d=paste0(Deposit_Name,Scenario)) %>% 
  pull(d) %>% unique()

# dict of scenarios
(dict_scen <- tibble(Scenario=scens_selected,scen_name=scens_names))

## Extraction by type -----
df_results %>% filter(t<2051) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Resource_Type)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
  group_by(name,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)

## Extraction in 2050
df_results %>% filter(t==2050) %>% 
  filter(mine_open==1) %>% 
  filter(tons_extracted>0) %>% 
  arrange(desc(tons_extracted)) %>%
  group_by(Scenario) %>% 
  reframe(avg_exct=mean(tons_extracted),
          avg_cap=mean(cap_total),
          sum_exct=sum(tons_extracted),
          sum_cap=sum(cap_total))

# Slack
slack %>% group_by(Scenario) %>% reframe(x=sum(value))
slack %>% filter(t<2051) %>%  group_by(Scenario) %>% reframe(x=sum(value))


## Table Analysis for Scenarios -------

# available Reserves
(total_reserve <- sum(deposit$reserve,na.rm=T)/1e3)
(total_resource_demostrated <- sum(deposit$resource_demostrated,na.rm=T)/1e3)
(total_resource_inferred <- sum(deposit$resource_inferred,na.rm=T)/1e3)


# Cumulative Demand
(table_demand <- demand %>% filter(t<2051) %>% group_by(Scenario) %>% reframe(demand=sum(Demand)/1e3) %>% 
  ungroup() %>% mutate(share_reserves=demand/total_reserve))

# peak demand
(table_Peakdemand <- demand %>% filter(t<2051) %>% 
    group_by(Scenario) %>% reframe(peakDemand=max(Demand)/1e3))

# Ratio demand 2022-2050x
(table_ratio <- demand %>% filter(t %in% c(2022,2050)) %>% 
  group_by(Scenario) %>% pivot_wider(names_from = t, values_from = Demand) %>% 
  mutate(ratio=`2050`/`2022`) %>% dplyr::select(Scenario,ratio))


# Increase in capacity of open mines
(table_capIncrease <- df_results %>% 
  filter(t==2050) %>%
  filter(prod_rate>0) %>% # previous existing prod rate
  group_by(Scenario) %>%
  # group_by(Scenario,Deposit_Name) %>% 
  reframe(capIncrease=sum(cap_total)/sum(prod_rate)-1))


# Number of mines opened
(table_open <- df_results %>% 
    filter(t<2051) %>%
    group_by(Scenario) %>% 
    reframe(mines_open=sum(new_mine_open)))

# Mines opened before 2035
(table_open2035 <- df_results %>% filter(t<2036) %>% group_by(Scenario) %>% 
    reframe(mines_open2035=sum(new_mine_open)))

# Deposits depleted
#only valid for open mines

(table_deplete <- df_results %>%
    filter(t<2051) %>%
    filter(paste0(Deposit_Name,Scenario) %in% open_deposits) %>%
    group_by(Deposit_Name,Scenario) %>% 
    reframe(tons_extracted=sum(tons_extracted)) %>% 
    left_join(deposit) %>% 
    mutate(all_resources=reserve+resource_demostrated+resource_inferred) %>% 
    filter(tons_extracted>=all_resources*0.99) %>%
    group_by(Scenario) %>% 
    reframe(depleted=n()))

# deposits at max capacity
(table_maxCap <- df_results %>% 
  filter(t<2051) %>% 
  group_by(Deposit_Name,Scenario) %>% 
  reframe(cap_total=max(cap_total)) %>% 
  filter(cap_total>0) %>%
  left_join(dplyr::select(deposit,Deposit_Name,max_prod_rate)) %>% 
  filter(cap_total>=max_prod_rate*0.99) %>% 
  # view()
  group_by(Scenario) %>% 
  reframe(maxCap=n()))

# deposits at max ramp up
(table_ramp <- df_results %>% 
    filter(t<2051) %>% 
    group_by(Deposit_Name,Scenario) %>% 
    reframe(capacity_added=max(capacity_added)) %>% 
    filter(capacity_added>0) %>%
    left_join(dplyr::select(deposit,Deposit_Name,max_ramp_up)) %>% 
    filter(capacity_added>=max_ramp_up*0.99) %>% 
    group_by(Scenario) %>% 
    reframe(ramp=n()))

# Depletion rate by stage
(table_depletion <- df_results %>% 
  filter(t<2051) %>% 
  group_by(Scenario) %>% 
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
  group_by(Scenario) %>% 
  reframe(cap_total=sum(cap_total))

# capacity reached
df_results %>% 
  filter(t==2050) %>% 
  filter(mine_open==1) %>% 
  group_by(Scenario) %>% 
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
    group_by(Scenario,t) %>% reframe(total_cost=sum(total_cost)) %>% ungroup() %>% 
    left_join(slack) %>%
    mutate(total_cost=total_cost+value*bigM_cost) %>% 
    left_join(discounter) %>% 
    mutate(total_cost=total_cost/r) %>% 
    group_by(Scenario) %>% 
    reframe(total_cost=sum(total_cost)/1e9))

## EDB Index ------
# Of capacity added
(table_edb <- df_results %>%
  filter(t<2051) %>%
  group_by(Scenario,Deposit_Name) %>% 
  reframe(x=sum(capacity_added)) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,edb)) %>% 
  mutate(edb=100-edb) %>% 
  group_by(Scenario) %>% 
  reframe(edb=weighted.mean(edb,x)) %>% ungroup())

# slack or demand not met
(table_slack <- slack %>% filter(t<2051) %>% 
    group_by(Scenario) %>% reframe(slack=sum(value)))
# as share of extraction - less than 0.5%
df_results %>% filter(t<2051) %>% 
  group_by(Scenario) %>% reframe(tons=sum(tons_extracted)) %>% 
  left_join(table_slack) %>% mutate(share_perc=slack/tons*100)


# Join all
(table_all <- table_demand %>% 
    left_join(table_Peakdemand) %>% 
    left_join(table_depletion) %>%
    left_join(table_capIncrease) %>% 
    left_join(table_open) %>% left_join(table_open2035) %>% 
  left_join(table_maxCap) %>% left_join(table_ramp) %>% 
    left_join(table_edb) %>% 
    left_join(table_slack) %>% 
    left_join(table_cost) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% arrange(name))
  
table_all %>% dplyr::select(-share_reserves,-Scenario) %>% 
  pivot_longer(c(-name), names_to = "key", values_to = "value") %>% 
  pivot_wider(names_from = name, values_from = value)
  
# copy
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)



## HHI Index ---------
# Herfindahl–Hirschman index - MEASURES market concentration
# Get country cumulative production and market share
df_results %>%
  filter(t<2051) %>%
  left_join(dplyr::select(deposit,Deposit_Name,Country)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
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
          top_names=first(top_names)) %>% ungroup()
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
  filter(t<2051) %>% 
  left_join(tibble(scen_all=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
  group_by(Country,name) %>% 
  reframe(tons_extracted=sum(Recycling))

# Country supply 
country_supply <- df_results %>%
  filter(t<2051) %>%
  left_join(dplyr::select(deposit,Deposit_Name,Country)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
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
  
both_supply %>%
  # Market Share
  group_by(name) %>% 
  mutate(market_share=tons_extracted/sum(tons_extracted)) %>% 
  # top 3 countries
  mutate(market_share_label=round(market_share*100,0)) %>% 
  arrange(desc(market_share)) %>%
  mutate(top_names = paste(head(Country, 3), "(", head(market_share_label, 3), "%)", collapse = ", ")) %>%
  # Index
  reframe(hhi=sum(market_share^2),
          top_names=first(top_names)) %>% ungroup()

## SP demand for scenarios ---------

### For Demand Scenarios ---------
# get max with slack non zero
max_slack <- sp_demand %>% filter(value<bigM_cost*0.9) %>% 
  pull(value) %>% max()

# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))


data_fig <- sp_demand %>% 
  # scenario names
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  filter(t<2051) %>% 
  left_join(discounter) %>% 
  mutate(value=value*r/5.323*1e3) %>%  # to LCE
  filter(value<3e5)

order_n <- data_fig %>% filter(t==2050) %>% arrange(desc(value)) %>% pull(name)

data_fig %>% 
  mutate(name=factor(name,levels=order_n)) %>%
  ggplot(aes(t,value,col=name))+
  geom_line()+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  # ylim(0,max_slack*1.1)+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Demand \nScenario")

# Why the SP could be reduced, due to demand reduction and switching to different deposit
f.fig.save("Figures/Optimization/Scenarios/SP_demand.png")


### For Countries N-1 Scenarios ---------
# load SP data

# NOTE: For countries, the deposit id do not match deposit csv data, 
# so the analysis is not valid without further code. 
# The SP demand is valid as not data on deposits is used

(runs <- list.dirs("Results/Optimization/N1_Countries",recursive = F))
ref <- "Results/Optimization/DemandScenario/Ambitious-Baseline-Baseline-Baseline-Baseline"
runs <- runs[!str_detect(runs,"Triangle")] # no Li triangle
runs <- c(ref,runs)


sp_demand <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Julia_sp_demand.csv")), 
            Scenario = basename(folder_path))))


max_slack <- sp_demand %>% filter(value<bigM_cost*0.9) %>% 
  pull(value) %>% max()

# Remove effect of discount rate
discounter <- tibble(t=2022:(t_size+2021),r=(1+discount_rate)^(0:(t_size-1)))

# for countries only
cats <- sp_demand %>% 
  filter(t==2050,!str_detect(Scenario,"Ambitious")) %>% arrange(desc(value)) %>% 
  pull(Scenario)
cats <- c(cats,"Reference") # N-1
pal <- c(rainbow(8),"Black") #N-1


data_fig <- sp_demand %>% 
  # scenario names
  mutate(Scenario=if_else(str_detect(Scenario,"Ambitious"),"Reference",Scenario)) %>%
  rename(name=Scenario) %>% # N-1 analysis
  mutate(name=factor(name,levels=cats)) %>%
  filter(t<2051) %>% 
  left_join(discounter) %>% 
  mutate(value=value*r/5.323*1e3) %>%  # to LCE
  filter(value<1e6)
  
ggplot(data_fig,aes(t,value,col=name))+
  geom_line()+
  scale_color_manual(values = pal) +
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Country not Available")

f.fig.save("Figures/Optimization/Scenarios/SP_demand_Countries.png")

# Stock analysis ----

stock <- df_results %>% 
  left_join(dict_scen) %>% 
  group_by(scen_name,t) %>% 
  reframe(tons_extracted1=sum(tons_extracted1), # reserve
          tons_extracted2=sum(tons_extracted2)+ # resource
            sum(tons_extracted3)) %>% ungroup() %>% 
  rename(Scenario=scen_name) %>% 
  group_by(Scenario) %>% 
  mutate(cum_extraction1=cumsum(tons_extracted1),
         cum_extraction2=cumsum(tons_extracted2),
         `In Stock`=cum_extraction1+cum_extraction2) %>% ungroup() %>%  
  # add resource and reserve
  mutate(Reserve=sum(deposit$reserve)-cum_extraction1,
         Resource=sum(deposit$resource_demostrated)+
           sum(deposit$resource_inferred)-cum_extraction2)

data_fig <- stock %>% 
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>% 
  filter(t<2051) %>%
  dplyr::select(Scenario,t,`In Stock`,Reserve,Resource) %>% 
  pivot_longer(c(`In Stock`,Reserve,Resource), 
               names_to = "key", values_to = "value") %>%
  mutate(key=factor(key,levels=c("Resource","In Stock","Reserve"))) %>% 
  mutate(Scenario=factor(Scenario,levels=scens_names)) %>% 
  mutate(value=value/1e3) # to million

# level at 2022
data_fig %>% filter(t==2022) # 21.4

ggplot(data_fig,aes(t,value,fill=key))+
  geom_area()+
  geom_hline(yintercept = 21.4,linetype="solid",col="white")+
  facet_wrap(~Scenario)+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d()+
  labs(x="",y="",fill="",title="Li [million tons]")
  

# Scatter plot comparison ----

# get metrics
data_fig <- df_results %>%
  filter(t<2051) %>% 
  left_join(dict_scen) %>% 
  group_by(scen_name,t) %>%
  # sum over deposits
  reframe(mines_open=sum(new_mine_open),
          tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  group_by(scen_name) %>%
  # sum over time
  reframe(mines_open=sum(mines_open),
          Total_Demand=sum(tons_extracted),
          Peak_Demand=max(tons_extracted)) %>% ungroup()
  
data_fig %>% 
  pivot_longer(c(Total_Demand,Peak_Demand), names_to = "key", values_to = "value") %>% 
  mutate(value=value/1e3) %>% # million tons
  mutate(key=str_replace(key,"_"," ")) %>% 
  ggplot(aes(value,mines_open))+
  geom_point()+
  facet_wrap(~key,scales = "free_x")+
  coord_cartesian(xlim=c(0,NA),ylim=c(0,NA))+
  labs(x="",y="Mines \nopen")
  

summary(lm(mines_open~Total_Demand+Peak_Demand,data=data_fig))
summary(lm(mines_open~Total_Demand,data=data_fig))
summary(lm(mines_open~Peak_Demand,data=data_fig))


# Objective value ------
# Note: up to 2070
runs

# Read values
df <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.delim(file.path(folder_path, "OptimalValue.txt"),sep=":",header=F),
            Scenario = basename(folder_path))))

#cumulative demand
cum_demand <- demand %>% 
  group_by(Scenario) %>% 
  reframe(mtons=sum(Demand)/1e3)

cum_extraction <- df_results %>% 
  group_by(Scenario) %>% 
  reframe(mtons=sum(tons_extracted)/1e3)


# total cost
df %>% 
  filter(V1=="Objective 1") %>% 
  left_join(cum_extraction) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(trillion=V2/1e6) %>% 
  dplyr::select(name,trillion,mtons) %>% 
  mutate(cost_perLCE=(trillion*1e12)/(mtons*1e6)/5.323/1e3) # to K

# Surplus --------------

capacity_year <- df_results %>% 
  group_by(Scenario,t) %>% 
  reframe(Capacity=sum(cap_total)) %>% ungroup() %>% 
  left_join(demand) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

head(capacity_year)

capacity_year %>% 
  mutate(surplus=Capacity-Demand) %>% 
  ggplot(aes(t,surplus,col=name,group=name))+
  geom_line()+
  ylim(-10,400)+
  labs(x="",y="Surplus \n[Li ktons]",col="")+
  theme(legend.position = c(0.2,0.8))
  

# Non - Monetary Extraction -----

## Non Monetary Factors ---------

data_fig <- df_results %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  filter(tons_extracted>0.01) %>% # no extraction
  left_join(deposit) %>%
  mutate(edb=100-edb) %>% 
  mutate(cost1=cost1/5.323) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names))

# for label
countries <- data_fig %>% filter(tons_extracted>0.2) %>% 
  group_by(Country) %>% reframe(edb=mean(edb)) %>% ungroup()

ggplot(data_fig,aes(cost1,edb))+
  geom_point(alpha=.5,aes(size=tons_extracted,col=Resource_Type))+
  facet_wrap(~name)+
  geom_text_repel(data=countries,aes(label=Country),x=100,
                  fontface = "italic",
                  size=6*5/14 * 0.8,
                  hjust=0,min.segment.length = unit(0,"lines"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_color_manual(values=resource_colors)+
  labs(y="Ease of \nDoing \nBusiness \n[0-100]",x="Extraction Costs [USD/ton LCE]",
       size="Total Extraction \n[M tons Li]",col="Resource \ntype")



# Capacity expansion ----------

df_results %>% 
  filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>% 
  filter(t==2050) %>% 
  filter(already_open==T) %>% 
  left_join(deposit) %>% 
  dplyr::select(Deposit_Name,Resource_Type,prod_rate,cap_total) %>% 
  mutate(cap_added=cap_total-prod_rate,cap_total=NULL) %>% 
  pivot_longer(c(cap_added,prod_rate), names_to = "key", values_to = "value") %>% 
  ggplot(aes(reorder(Deposit_Name,value),value,fill=key))+
  geom_col()+
  coord_flip()+
  facet_grid(Resource_Type~.,space="free",scale="free")+
  labs(x="",y="Extraction Capacity [Li ktons]",fill="")
  

# Extraction with recycling -------------
recycling_total <- recycling %>%
  group_by(Scenario,t) %>% 
  reframe(tons_extracted=sum(Recycling)) %>% 
  mutate(d="Recycling")

df_results %>% 
  left_join(deposit) %>%
  group_by(Scenario,Country,t) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  rename(d=Country) %>% 
  # aggregate big countries
  group_by(Scenario,d) %>% mutate(total_prod=sum(tons_extracted)) %>% ungroup() %>%
  group_by(Scenario,t) %>% mutate(share_prod=total_prod/sum(total_prod)) %>% ungroup() %>% 
  mutate(d=if_else(share_prod>0.02,d,"Other countries")) %>% 
  group_by(Scenario,d,t) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  # end aggregate other countries
  rbind(recycling_total) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
  filter(t<2051) %>%
  filter(str_detect(name,"Ref|High|Recyc")) %>%
  ggplot(aes(t,tons_extracted,fill=d,group=d))+
  geom_area()+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  # theme(legend.position = "none")+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")

# resource type
df_results %>% 
  left_join(deposit) %>%
  rename(a=d,d=Resource_Type) %>% 
  group_by(Scenario,d,t) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  rbind(recycling_total) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
  filter(t<2051) %>%
  filter(str_detect(name,"Ref|High|Recyc")) %>%
  ggplot(aes(t,tons_extracted,fill=d,group=d))+
  geom_area()+
  facet_wrap(~name)+
  scale_fill_manual(values=resource_colors)+
  coord_cartesian(expand = F)+
  # theme(legend.position = "none")+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")





# Recycling effect ---------
recycling_total <- recycling %>%
  group_by(Scenario,t) %>% 
  reframe(value=sum(Recycling)) %>% 
  mutate(key="Recycling")

# by tons extracted
df_results %>% 
  dplyr::select(Scenario,t,Deposit_Name,prod_rate,cap_total,already_open,tons_extracted) %>% 
  # new cap on opened mines, or new mines open
  mutate(AD=if_else(already_open,cap_total-prod_rate,0),
         ND=if_else(already_open,0,cap_total-prod_rate),
         cap_total=NULL,already_open=NULL) %>% 
  # distribute tons extracted - first existing capacity
  mutate(allocation=pmin(prod_rate,tons_extracted),
         prod_rate=allocation,
         tons_extracted=tons_extracted-allocation,
         # now added capacity
         allocation=pmin(AD,tons_extracted),
         AD=allocation,
         tons_extracted=tons_extracted-allocation,
         # new deposits
         allocation=pmin(ND,tons_extracted),
         ND=allocation,
         tons_extracted=tons_extracted-allocation,
         tons_extracted=NULL,allocation=NULL) %>% 
  rename(`Added Capacity`=AD,`New Deposits`=ND,
         `Planned Capacity`=prod_rate) %>%
    pivot_longer(c(-Scenario,-t,-Deposit_Name), names_to = "key", values_to = "value") %>% 
  group_by(Scenario,t,key) %>% 
  reframe(value=sum(value)) %>% ungroup() %>% 
  rbind(recycling_total) %>% 
  mutate(key=factor(key,levels=c("Recycling","New Deposits",
                                 "Added Capacity","Planned Capacity"))) %>% 
  filter(t<2051) %>%
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>%
  filter(value>0) %>%
  ggplot(aes(t,value)) + 
  geom_area(aes(fill=key,group=key))+
  # geom_col(aes(fill=key))+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  # theme(legend.position = "none")+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")
  
## Same graph but with capacity instead of extraction
df_results %>% 
  mutate(capacity_added=cap_total-prod_rate) %>%
  dplyr::select(Scenario,t,Deposit_Name,prod_rate,capacity_added,already_open) %>% 
  # new cap on opened mines, or new mines open
  mutate(`Added Capacity`=if_else(already_open,capacity_added,0),
         `New Deposits`=if_else(already_open,0,capacity_added),
         capacity_added=NULL,already_open=NULL) %>% 
  rename(`Planned Capacity`=prod_rate) %>% 
  pivot_longer(c(-Scenario,-t,-Deposit_Name), names_to = "key", values_to = "value") %>% 
  group_by(Scenario,t,key) %>% 
  reframe(value=sum(value)) %>% ungroup() %>% 
  rbind(recycling_total) %>% 
  mutate(key=factor(key,levels=c("Recycling","New Deposits",
                                 "Added Capacity","Planned Capacity"))) %>% 
  filter(t<2051) %>%
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>%
  filter(value>0) %>%
  ggplot(aes(t,value)) + 
  geom_area(aes(fill=key,group=key))+
  # geom_col(aes(fill=key))+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  # theme(legend.position = "none")+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")


recycling_total <- recycling_total %>% 
  rename(cap_total=value,Resource_Type=key) %>%
  mutate(Deposit_Name="Recycling")

df_results %>% 
  left_join(deposit) %>% 
  dplyr::select(Scenario,Deposit_Name,t,cap_total,Resource_Type) %>% 
  rbind(recycling_total) %>%
  filter(t<2051) %>%
  mutate(order_plot=case_when(
    Resource_Type=="Brine" ~ 1e3+cap_total,
    Resource_Type=="Hard Rock" ~ 1e4+cap_total,
    Resource_Type=="Volcano-Sedimentary" ~ 1e5+cap_total,
    T ~ -1)) %>% 
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>%
  ggplot(aes(t,cap_total,fill=Resource_Type,group=order_plot)) + 
  # geom_area()+
  geom_col(col="black")+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")

# bar graph by resource type
df_results %>% 
  mutate(capacity_added=cap_total-prod_rate) %>% 
  dplyr::select(Scenario,t,Deposit_Name,prod_rate,capacity_added,already_open) %>% 
  # new cap on opened mines, or new mines open
  mutate(`Added Capacity`=if_else(already_open,capacity_added,0),
         `New Deposits`=if_else(already_open,0,capacity_added),
         capacity_added=NULL,already_open=NULL) %>% 
  rename(`Planned Capacity`=prod_rate) %>% 
  pivot_longer(c(-Scenario,-t,-Deposit_Name), names_to = "key", values_to = "value") %>% 
  group_by(Scenario,t,key) %>% 
  reframe(value=sum(value)) %>% ungroup() %>% 
  rbind(recycling_total) %>% 
  mutate(key=factor(key,levels=c("Recycling","New Deposits",
                                 "Added Capacity","Planned Capacity"))) %>% 
  filter(t<2051) %>%
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  filter(str_detect(name,"Ref|Solid|Recyc")) %>%
  filter(value>0) %>%
  ggplot(aes(t,value)) + 
  # geom_area(aes(fill=key,group=key))+
  geom_col(aes(fill=key))+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  # theme(legend.position = "none")+
  labs(x="",y="",title="Li Extraction [ktons]",fill="")



# Mines opend over time ---------

df_results %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>% 
  reframe(mine_open=sum(mine_open)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  ggplot(aes(t,mine_open,col=name))+
  geom_line()+
  labs(x="",y="",col="",title="Number of Open Deposits")



# Cost curve 2050 ------------

data_fig <- df_results %>%
  # filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>% 
  # filter(t==2050) %>%
  filter(t %in% c(2030,2040,2050)) %>% 
  left_join(deposit) %>% 
  mutate(cost=(cost1*tons_extracted1+cost2*tons_extracted2+cost3*tons_extracted3)/
           (tons_extracted1+tons_extracted2+tons_extracted3),
         li_size=(tons_extracted1+tons_extracted2+tons_extracted3)) %>%
  # based on capacity
  # mutate(cost=cost1,
  #        li_size=cap_total) %>% 
  mutate(cost=cost/5.323) %>%  # to USD per ton LCE
  filter(li_size>0) %>% 
  filter(cost>0) %>% 
  arrange(cost) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(t,name) %>% 
  mutate(reserve_cum_end=cumsum(li_size),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(li_size>1.1,Deposit_Name,"")) %>% 
  mutate(lab_pos=reserve_cum_start+li_size/2) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
nrow(data_fig)
last_row <- data_fig %>% group_by(name,t) %>% 
  filter(cost==max(cost))
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/0.5)*0.5 # upper by 500
max(data_fig$cost)
lim_y <- ceiling(max(data_fig$cost)/500)*500

even_row <- data_fig$even_row

ggplot(data_fig,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv",
            aes(col=name,group=name))+
  # geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 1000*even_row,
  #                 size=7*5/14 * 0.8)+
  facet_grid(t~.)+
  labs(x="Cumulative Extraction [ktons Li]",y="USD/ton LCE",
       title="2050 Lithium Cumulative Availability Curve",
       col="Resource type",alpha="Stage")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(NA,lim_y))+
  scale_color_manual(values=scen_colors)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.9,0.2),
        legend.box = "horizontal",
        axis.text.x = element_text(hjust = 1),
        axis.title.y=element_text(angle=0,margin=margin(r = -75,l=30),vjust = 0.95),
        legend.text = element_text(size=6),
         legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=6))
# p2

## Curve for countries -------------

(runs <- list.dirs("Results/Optimization/N1_Countries",recursive = F))
ref <- "Results/Optimization/DemandScenario/Ambitious-Baseline-Baseline-Baseline-Baseline"
runs <- runs[!str_detect(runs,"Triangle")] # no Li triangle
runs <- c(ref,runs)

df_results <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = basename(folder_path))))
df_results$Scenario %>% unique()

# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(tibble(d=1:d_size,already_open=deposit$open_mine)) %>%
  left_join(prod_rate) %>% 
  group_by(Scenario,d) %>% 
  mutate(new_mine_open= !already_open & mine_opened==1,
         cap_total=cumsum(capacity_added)+prod_rate,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup() %>% 
  mutate(Scenario=if_else(str_detect(Scenario,"Ambitious"),
                          "Reference",Scenario)) 

data_fig <- df_results %>%
  # filter(t==2050) %>%
  filter(t %in% c(2030,2040,2050)) %>%
  left_join(deposit) %>% 
  mutate(cost=(cost1*tons_extracted1+cost2*tons_extracted2+cost3*tons_extracted3)/
           (tons_extracted1+tons_extracted2+tons_extracted3),
         li_size=(tons_extracted1+tons_extracted2+tons_extracted3)) %>%
  mutate(cost=cost/5.323) %>%  # to USD per ton LCE
  filter(li_size>0) %>% 
  filter(cost>0) %>% 
  arrange(cost) %>% 
  group_by(Scenario,t) %>% 
  mutate(reserve_cum_end=cumsum(li_size),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(li_size>1.1,Deposit_Name,"")) %>% 
  mutate(lab_pos=reserve_cum_start+li_size/2) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
nrow(data_fig)
last_row <- data_fig %>% group_by(Scenario,t) %>% 
  filter(cost==max(cost))
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/0.5)*0.5 # upper by 500
max(data_fig$cost)
lim_y <- ceiling(max(data_fig$cost)/500)*500

even_row <- data_fig$even_row

ggplot(data_fig,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv",
            aes(col=Scenario,group=Scenario))+
  # geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 1000*even_row,
  #                 size=7*5/14 * 0.8)+
  facet_grid(t~.)+
  labs(x="Cumulative Extraction [ktons Li]",y="USD/ton LCE",
       title="2050 Lithium Cumulative Availability Curve",
       col="Resource type",alpha="Stage")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(NA,lim_y))+
  # scale_color_manual(values=scen_colors)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.9,0.2),
        legend.box = "horizontal",
        axis.text.x = element_text(hjust = 1),
        axis.title.y=element_text(angle=0,margin=margin(r = -75,l=30),vjust = 0.95),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=6))

# EoF