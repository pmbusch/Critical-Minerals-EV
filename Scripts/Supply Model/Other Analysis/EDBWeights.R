# Results with different EDB Weights
# PBH June 2024


# Load data -------------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
(runs <- list.dirs("Results/Optimization/EDBCurve",recursive = F))


# Input Parameters
demand <- read.csv("Parameters/Demand.csv") %>% 
  filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline")
recycling <- read.csv("Parameters/Recycling.csv") %>% 
  filter(Scenario=="Ambitious-Baseline-Baseline-Baseline-Baseline")
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

# Load Results --------

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = basename(folder_path)))) %>% 
  rename(Deposit_Name=d)
df_results$Scenario %>% unique()


slack <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Slack_Julia.csv")), 
            Scenario = basename(folder_path))))

# get total capacity and mine opening
ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)
sum(ald_opens$already_open) # 52

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  left_join(prod_rate) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  mutate(new_mine_open= !already_open & near(mine_opened,1), # Near() instead of a==1 avoids rounding error mistakes!
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

# Production by country comparison ------------

levels_edb <- paste0("EDB Weight ",seq(0,20,1),"%")

data_fig <- df_results %>% 
  filter(t<2051) %>% 
  mutate(Scenario=as.numeric(str_remove(Scenario,"EDBLoop "))) %>% 
  arrange(Scenario) %>% 
  filter(Scenario %in% c(0,1,2,3,4,5,8,10,12,15,18,20)) %>% 
  mutate(name=paste0("EDB Weight ",Scenario,"%")) %>%
  left_join(deposit,by="Deposit_Name") %>% 
  group_by(name,t,Country) %>% 
  reframe(tons=sum(tons_extracted)) %>%
  # reframe(tons=sum(capacity_added)) %>% # Capacity added instead of extraction 
  filter(tons>0) %>% 
  mutate(name=factor(name,levels=levels_edb))

# aggregate countries cumulative demand into others
countries <- data_fig %>% group_by(name,Country) %>% 
  reframe(tons=sum(tons)) %>% ungroup() %>% 
  group_by(Country) %>% 
  reframe(tons=max(tons)) %>% 
  arrange(desc(tons)) %>% 
  filter(tons>500) %>%
  # filter(tons>0) %>% # for capacity added
  pull(Country)
countries <- c(countries,"Others")
  
data_fig <- data_fig %>% 
  mutate(Country=if_else(Country %in% countries,Country,"Others"),
         Country=factor(Country,levels=countries)) %>% 
  group_by(name,Country,t) %>% reframe(tons=sum(tons)) %>% ungroup()


label_data <- data_fig %>% 
  filter(t==2050) %>%
  group_by(name) %>% 
  mutate(lab=round(tons/sum(tons),2)*100) %>% 
  mutate(perc=if_else(Country=="Others"|lab<1,"",
                      paste0(lab,"%"))) %>% 
  mutate(t=2051.5)


color_scale <- c("United States"="#1f78b4","Australia"="#cab2d6",
                 "Chile"="#d95f02","Argentina"="#ff7f00",
                 "Canada"="#6A3D9A","China"="#ff0000",
                 "Bolivia"="#33a02c", "Mali"="#b15928",
                 "Czech Republic"="#fb9a99","Serbia"="#1b9e77",
                 "Germany"="#a6cee3","Others"="#808080")

ggplot(data_fig,aes(t,tons,fill=Country,group=Country))+
  geom_area(col="black",linewidth=0.2)+
  facet_wrap(~name)+
  geom_text(data=label_data,aes(label=perc,col=Country),
            position = position_stack(vjust = 0.5),
            size=7*5/14 * 0.8)+
  scale_fill_manual(values=color_scale)+
  scale_color_manual(values=color_scale)+
  scale_x_continuous(breaks = c(2022,2030,2040,2050))+
  labs(x="",y="",title="Lithium ktons extracted")+
  theme_bw(6)+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave("Figures/Validation/TS_Model_NoSlack.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=12.4)


# EoF