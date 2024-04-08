# Analysis Results of Optimization
# PBH March 2024

# Run Julia to get all the results much faster
# system("Scripts/Optimization/Julia_Opt.jl") # not working

source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Single Run Analysis ------
demand <- read.csv("Parameters/Demand.csv")
deposit <- read.csv("Parameters/Deposit.csv")

demand <- demand %>% filter(Scenario=="Baseline-Baseline-Baseline")

(d_size <- nrow(deposit))
(t_size <- nrow(demand))

# load results from Julia
url_result <- "Results/Optimization/%s.csv"
df_results <- read.csv(sprintf(url_result,"Base_Julia"))
df_results_LP <- read.csv(sprintf(url_result,"Base_Julia_LP"))
slack <- read.csv(sprintf(url_result,"Slack_Julia"))
# bigM_cost <- 1e6 # same as Julia
bigM_cost <- 100000*5.323 # historic high was 68K for LCE
discount_rate <- 0.03 # same as Julia
# shadow prices
sp_demand <- read.csv(sprintf(url_result,"Julia_sp_demand"))
sp_reserve <- read.csv(sprintf(url_result,"Julia_sp_reserve"))
sp_rest <- read.csv(sprintf(url_result,"Julia_sp_rest"))

# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2) %>% 
  left_join(tibble(d=1:d_size,prod_rate=deposit$prod_rate)) %>% 
  group_by(d) %>% mutate(cap_total=cumsum(capacity_added)+prod_rate,
                         mine_open=cumsum(mine_opened),
                         total_extraction=cumsum(tons_extracted)) %>% ungroup()

# Report generation Markdown
# Figures are in report
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))
fig_name <- "Figures/Optimization/%s.png"
save_figures <- F # change to T or F

rmarkdown::render("Scripts/Optimization/Run_Report.Rmd",
                  # output_file = "Report.pdf")
                  output_file = "ReportReserves.pdf")




# Playground
aux <- df_results %>% 
  left_join(deposit,by="d") %>% 
  filter(Resource_Type=="Hard Rock")
ggplot(aux,aes(t,tons_extracted,group=d))+
  geom_text_repel(data=filter(aux,t==2035),aes(label=Deposit_Name))+
  geom_line()




# EoF