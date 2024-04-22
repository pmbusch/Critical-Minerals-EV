# Analysis Results of Optimization
# PBH March 2024

# Run Julia to get all the results much faster
# system("Scripts/Optimization/Julia_Opt.jl") # not working

source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Single Run Analysis ------
demand <- read.csv("Parameters/Demand.csv")
deposit <- read.csv("Parameters/Deposit.csv")

scen <- "Ambitious-Baseline-Baseline-Baseline-Baseline"
demand <- demand %>% filter(Scenario==scen)

(d_size <- nrow(deposit))
(t_size <- nrow(demand))

# load results from Julia
url_result <- paste0("Results/Optimization/",scen,"/%s.csv")
df_results <- read.csv(sprintf(url_result,"Base_Julia"))
df_results_LP <- read.csv(sprintf(url_result,"Base_Julia_LP"))
slack <- read.csv(sprintf(url_result,"Slack_Julia"))
# bigM_cost <- 1e6 # same as Julia
bigM_cost <- 100000*5.323 # historic high was 68K for LCE
discount_rate <- 0.07 # same as Julia
# shadow prices
sp_demand <- read.csv(sprintf(url_result,"Julia_sp_demand"))
sp_reserve <- read.csv(sprintf(url_result,"Julia_sp_reserve"))
sp_rest <- read.csv(sprintf(url_result,"Julia_sp_rest"))

# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(tibble(d=1:d_size,prod_rate=deposit$prod_rate)) %>% 
  group_by(d) %>% mutate(cap_total=cumsum(capacity_added)+prod_rate,
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


## Reserves consumption by stage ---------
deposit$reserve %>% sum()/1e3 # million
df_results$tons_extracted %>% sum()/1e3

(reserve_size <- sum(deposit$reserve))

df_results %>% 
  filter(t<2051) %>% 
  pivot_longer(c(total_extraction1,total_extraction2), 
               names_to = "Stage", values_to = "value") %>% 
  group_by(t,Stage) %>% 
  reframe(value=sum(value)) %>% ungroup() %>% 
  mutate(Stage=if_else(Stage=="total_extraction1","Stage 1","Stage 2")) %>% 
  mutate(share_reserves=value/reserve_size*2) %>% 
  ggplot(aes(t,share_reserves,col=Stage))+
  geom_line()+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",title="Share of depleted Reserves")


# Playground
aux <- df_results %>% 
  left_join(deposit,by="d") %>% 
  filter(Resource_Type=="Hard Rock")
ggplot(aux,aes(t,tons_extracted,group=d))+
  geom_text_repel(data=filter(aux,t==2035),aes(label=Deposit_Name))+
  geom_line()

# Multiple Run Analysis -----------

demand <- read.csv("Parameters/Demand.csv")
deposit <- read.csv("Parameters/Deposit.csv")

(d_size <- nrow(deposit))
(t_size <- length(unique(demand$t)))

bigM_cost <- 100000*5.323/1e3 # historic high was 68K for LCE
discount_rate <- 0.07 # same as Julia

# load results from Julia
# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization",recursive = F))

# Choose countries or sccenarios

# runs <- runs[str_detect(runs,"Ambitious")] # Scenarios
ref <- runs[1]
runs <- runs[!str_detect(runs,"Ambitious")] # N-1 countries
runs <- c(ref,runs)

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
                        transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
                                  Scenario = basename(folder_path))))
df_results$Scenario %>% unique()

df_results_LP <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia_LP.csv")), 
            Scenario = basename(folder_path))))

slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = basename(folder_path))))

# shadow prices

sp_demand <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Julia_sp_demand.csv")), 
            Scenario = basename(folder_path))))

sp_reserve <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Julia_sp_reserve.csv")), 
            Scenario = basename(folder_path))))

sp_rest <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Julia_sp_rest.csv")), 
            Scenario = basename(folder_path))))


# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(tibble(d=1:d_size,prod_rate=deposit$prod_rate,
                   already_open=deposit$open_mine)) %>% 
  group_by(Scenario,d) %>% 
  mutate(new_mine_open=!already_open & mine_opened==1,
         cap_total=cumsum(capacity_added)+prod_rate,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()

# subset of opened deposits
open_deposits <- df_results %>% group_by(d,Scenario) %>% 
  reframe(mine_open=sum(mine_opened)) %>% filter(mine_open>0) %>% 
  mutate(d=paste0(d,Scenario)) %>% pull(d) %>% unique()

## Extraction by type -----
df_results %>% filter(t<2051) %>% 
  left_join(dplyr::select(deposit,d,Resource_Type)) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% 
  group_by(name,Resource_Type) %>% 
  reframe(tons_extracted=sum(tons_extracted)/1e3) %>% #million 
  pivot_wider(names_from = Resource_Type, values_from = tons_extracted)


## Table Analysis for Scenarios -------

# available Reserves
(total_reserve <- sum(deposit$reserve,na.rm=T)/1e3)
(total_resource_demostrated <- sum(deposit$resource_demostrated,na.rm=T)/1e3)
(total_resource_inferred <- sum(deposit$resource_inferred,na.rm=T)/1e3)


# Cumulative Demand
(table_demand <- demand %>% filter(t<2051) %>% group_by(Scenario) %>% reframe(demand=sum(Demand)/1e3) %>% 
  ungroup() %>% mutate(share_reserves=demand/total_reserve))

# Ratio demand 2022-2050x
(table_ratio <- demand %>% filter(t %in% c(2022,2050)) %>% 
  group_by(Scenario) %>% pivot_wider(names_from = t, values_from = Demand) %>% 
  mutate(ratio=`2050`/`2022`) %>% dplyr::select(Scenario,ratio))

# Number of mines opened
(table_open <- df_results %>% group_by(Scenario) %>% 
    reframe(mines_open=sum(new_mine_open)))

# Mines opened before 2035
(table_open2035 <- df_results %>% filter(t<2036) %>% group_by(Scenario) %>% 
    reframe(mines_open2035=sum(new_mine_open)))

# Deposits depleted
#only valid for open mines

(table_deplete <- df_results %>%
    # filter(t<2051) %>%
    filter(paste0(d,Scenario) %in% open_deposits) %>%
    group_by(d,Scenario) %>% 
    reframe(tons_extracted=sum(tons_extracted)) %>% 
    left_join(deposit,by="d") %>% 
    mutate(all_resources=reserve+resource_demostrated+resource_inferred) %>% 
    filter(tons_extracted+10>all_resources) %>% 
    group_by(Scenario) %>% 
    reframe(depleted=n()))

# which one is more reliable? shadow price or summation?
sp_reserve %>% 
  filter(paste0(d,Scenario) %in% open_deposits) %>%
  filter(sp_resource_inferred<0) %>% 
  group_by(Scenario) %>% 
  reframe(depleted=n())

# Reserves depleted
sp_reserve %>% 
  filter(paste0(d,Scenario) %in% open_deposits) %>%
  filter(sp_reserve<0) %>% 
  group_by(Scenario) %>% 
  reframe(depleted=n())

sp_reserve %>% 
  filter(paste0(d,Scenario) %in% open_deposits) %>%
  filter(sp_resource_demostrated<0) %>% 
  group_by(Scenario) %>% 
  reframe(depleted=n())


# df_results %>% group_by(Scenario,d) %>% reframe(tons=sum(tons_extracted)) %>% 
#   arrange(desc(tons))

# deposits at max capacity
(table_maxCap <- sp_rest %>% filter(t<2051) %>% 
  # only open mines
    left_join(dplyr::select(df_results,Scenario,d,t,mine_open),by=c("Scenario","d","t")) %>%
    mutate(sp_maxProdRate=mine_open*sp_maxProdRate) %>%
    filter(sp_maxProdRate < 0) %>% group_by(Scenario,d) %>% 
    reframe(sp_maxProdRate=sum(sp_maxProdRate)) %>% group_by(Scenario) %>% 
    reframe(maxCap=n()))

# Why is different?
(table_maxCap <- df_results %>% 
  filter(t<2051) %>% 
  group_by(d,Scenario) %>% 
  reframe(cap_total=max(cap_total)) %>% 
  filter(cap_total>0) %>%
  left_join(dplyr::select(deposit,d,max_prod_rate),by="d") %>% 
  # view()
  filter(cap_total+1>max_prod_rate) %>% 
  group_by(Scenario) %>% 
  reframe(maxCap=n()))

# deposits to ramp up
(table_ramp <- sp_rest %>% 
    filter(t<2051) %>% 
    # only open mines
    left_join(dplyr::select(df_results,Scenario,d,t,mine_open),by=c("Scenario","d","t")) %>%
    mutate(sp_rampUp=mine_open*sp_rampUp) %>%
    filter(sp_rampUp < 0) %>% 
    group_by(Scenario,d) %>% reframe(sp_rampUp=sum(sp_rampUp)) %>% group_by(Scenario) %>% 
    reframe(ramp=n()))

(table_ramp <- df_results %>% 
    filter(t<2051) %>% 
    group_by(d,Scenario) %>% 
    reframe(capacity_added=max(capacity_added)) %>% 
    filter(capacity_added>0) %>%
    left_join(dplyr::select(deposit,d,max_ramp_up),by="d") %>% 
    filter(capacity_added+1>max_ramp_up) %>% 
    group_by(Scenario) %>% 
    reframe(ramp=n()))

# Depletion rate by stage
(table_depletion <- df_results %>% 
  filter(t<2051) %>% 
  group_by(Scenario) %>% 
  reframe(stage1=sum(tons_extracted1/1e3)/total_reserve,
          stage2=sum(tons_extracted2/1e3)/total_resource_demostrated,
          stage3=sum(tons_extracted3/1e3)/total_resource_inferred))

# Power or capacity achieved in 2050
demand %>% filter(t==2050)

df_results %>% 
  filter(t==2050) %>% 
  # remove depleted deposits
  left_join(deposit, by="d") %>% 
  filter(total_extraction+10<reserve+resource_demostrated+resource_inferred) %>% 
  group_by(Scenario) %>% 
  reframe(cap_total=sum(cap_total))


# Join all
table_depletion %>%
  left_join(table_open) %>% left_join(table_open2035) %>% 
  left_join(table_maxCap) %>% left_join(table_ramp) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(name=factor(name,levels=scens_names)) %>% arrange(name)
  

# copy
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)

# slack
slack %>% filter(t<2051) %>% group_by(Scenario) %>% reframe(slack=sum(value))


## HHI Index ---------
# Herfindahl–Hirschman index - MEASURES market concentration
# Get country cumulative production and market share
df_results %>%
  left_join(dplyr::select(deposit,d,Country)) %>% 
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
  mutate(value=value*r/5.323*1e3) # to LCE

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
  mutate(value=value*r/5.323*1e3) # to LCE

ggplot(data_fig,aes(t,value,col=name))+
  geom_line()+
  scale_color_manual(values = pal) +
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  labs(x="",y="",title="Clearing Price [USD/ton LCE]",caption="All costs in 2022 $USD (no discount).",
       col="Country not Available")

f.fig.save("Figures/Optimization/Scenarios/SP_demand_Countries.png")



# EoF