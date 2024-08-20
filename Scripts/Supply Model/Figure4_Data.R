# 2050 Comparison Map of Open Deposits by Scenario
# PBH July 2024

# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/02-LoadOptimizationResults.R", encoding = "UTF-8")


# Data to recreate figure is same as Table1 
# (if decide not to run optimization code, then can preload results, along with the first lines of 
# the script 02-LoadOptimizationResults.R (load other data required)
df_results <- read.csv("Results/Data_Table1.csv")


# get difference of opened deposits ------

df <- df_results %>%
  filter(t<2051) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Status)) %>% 
  filter(near(mine_opened,1)) %>% 
  # group_by(name) %>% tally()
  mutate(Status=case_when(
    str_detect(Status,"Producing") ~ "Producing",
    Status=="Construction" ~ "Construction",
    T ~ "Not open")) %>% 
  dplyr::select(Deposit_Name,Status,name) %>%
  mutate(n=1) %>% 
  pivot_wider(names_from = name, values_from = n)
names(df)[3:5] <- c("Ref","Rec","HighLIB")

df <- df %>% 
  mutate(symbol=case_when(
    Status=="Producing" ~ "Producing",
    Status=="Construction" ~ "Construction",
    is.na(Ref) ~ "Needed due to higher capacity LIBs - Scen. (2)",
    is.na(Rec) ~ "Avoided by Recycling - Scen. (9)",
    T ~ "Reference Scenario (1)") %>% 
      factor(levels=c("Producing","Construction","Reference Scenario (1)",
                      "Avoided by Recycling - Scen. (9)",
                      "Needed due to higher capacity LIBs - Scen. (2)")))
table(df$symbol)

total_extraction <- df_results %>%  filter(t<2051) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>% 
  group_by(Deposit_Name,name) %>% 
  reframe(mtons_extraction=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  left_join(dplyr::select(df,Deposit_Name,symbol)) %>% 
  mutate(string_join=paste0(name,"-",symbol)) %>% 
  filter(str_detect(string_join,"Reference-Producing|Reference-Construction|Capacity LIB-Needed due|
                    Reference-Reference|Reference-Avoided by "))

df <- df %>% left_join(deposit,by="Deposit_Name")
df <- df %>% left_join(dplyr::select(total_extraction,Deposit_Name,mtons_extraction))


# Names selection
head(df)
df <- df %>% 
  mutate(label_name=if_else(all_resource>1000,Deposit_Name,"")) %>% 
  mutate(label_name=if_else(all_resource>1000,Deposit_Name,"")) %>% 
  mutate(label_name=if_else(str_detect(Deposit_Name,"Arcadia|Goulamina|Zinnwald|Carolina|LANXESS|Xuxa|Volta|Ostroboth")
                            ,Deposit_Name,label_name)) %>% 
  mutate(label_name=label_name %>% str_remove_all(" \\(All\\)| \\(All Projects\\)| \\(Century\\)"))

unique(df$label_name) %>% sort()

# count by scenario
df %>% group_by(symbol) %>% tally()

# export for QGIS map preparation
write.csv(df,"Results/MapOpenDeposits.csv",row.names = F)

map1 <- map_data('world')
p1 <- ggplot(df) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group),
               col = 'gray', fill="white") +
  coord_fixed(1.4, xlim = c(-140,160), ylim=c(-60,70))+
  # coord_fixed(1.4, xlim = c(-75,-60), ylim=c(-40,-10))+ #Li Triangle
  scale_y_continuous(breaks = NULL,name = "")+
  scale_x_continuous(breaks = NULL,name = "")+
  # data
  geom_point(aes(x = Longitude, y = Latitude,
                 size=mtons_extraction,
                 col=symbol), alpha = 0.7) +
  guides(col = guide_legend(ncol = 2))+
  labs(col="Deposit \nStatus",title="(B) Lithium Deposits")+
  theme(panel.grid = element_blank(),
        # legend.position = c(0.5,0.1),
        legend.position="bottom",
        legend.background = element_rect(fill = "transparent", color = "black"),
        legend.text = element_text(size=5),
        plot.margin = margin(1, 1, 1, 1),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p1
ggsave("Figures/Article/Map.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       bg="white",
       width=8.7,height=8.7)



# EoF