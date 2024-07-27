# 2050 Comparison Map of Open Deposits by Scenario
# PBH July 2024

# Load data -------
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

# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path) 
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = basename(folder_path)))) %>% 
  rename(Deposit_Name=d)
df_results$Scenario %>% unique()

# dict of scenarios
(dict_scen <- tibble(Scenario=scens_selected,scen_name=scens_names))

ald_opens <- deposit %>% dplyr::select(Deposit_Name,open_mine) %>% 
  rename(already_open=open_mine)

df_results <- df_results %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(ald_opens) %>% 
  left_join(prod_rate) %>% 
  group_by(Scenario,Deposit_Name) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(new_mine_open= !already_open & mine_opened==1,
         cap_total=cumsum(capacity_added)+prod_rate,
         mine_open=cumsum(mine_opened),
         total_extraction=cumsum(tons_extracted),
         total_extraction1=cumsum(tons_extracted1),
         total_extraction2=cumsum(tons_extracted2),
         total_extraction3=cumsum(tons_extracted3)) %>% ungroup()


# get difference of opened deposits ------

df <- df_results %>%
  filter(t<2051) %>% 
  filter(str_detect(name,"Ref|High|Recyc")) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Status)) %>% 
  filter(mine_opened==1) %>% 
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

df <- df %>% left_join(deposit,by="Deposit_Name")

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

# cumulative supply --------
# Creates Map showing cumulative (2022-2050) lithium extraction by resource type, and by country
# It shows a 2 barplot merged together

factor=1
# factor=15 # uncomment for 2050 analysis

# By Resource type first, need to add recycling component
recycling_total <- recycling %>%
  filter(t<2051) %>%
  # filter(t==2050) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name) %>% 
  reframe(mtons=sum(Recycling)/1e3) %>% 
  mutate(Resource_Type="Recycling")

# get extraction
data_fig <- df_results %>% 
  left_join(deposit) %>%
  filter(t<2051) %>% 
  # filter(t==2050) %>%  
  group_by(name,Resource_Type) %>% 
  reframe(mtons=sum(tons_extracted)/1e3) %>% 
  rbind(recycling_total) %>% 
  mutate(name=substr(name,2,2) %>% as.factor())

# order of plot Resource - custom with labels for legend
resource_legend <- c("RE","BR","HR","VS")
dict_res <- tibble(Resource_Type=c("Recycling","Brine","Hard Rock","Volcano-Sedimentary"),
                   res_legend=resource_legend)
data_fig <- data_fig %>% 
  left_join(dict_res) %>% 
  mutate(res_legend=factor(res_legend,levels=resource_legend))

# Lithium extraction  - By country
# countries to highlight
key_countries <- c("United States","Australia","Chile","China","EU","Others")
key_countries_legend <- c("USA","AUS","CHL","CHN","EU","Others")

# get recycling at region level
recycling_country <- recycling %>%
  filter(t<2051) %>%
  # filter(t==2050) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  mutate(Country=case_when(
    Region %in% key_countries ~ Region,
    Region=="European Union" ~ "EU",
    T ~ "Others")) %>% 
  group_by(name,Country) %>% 
  reframe(mtons=sum(Recycling)/1e3) %>% 
  mutate(Resource_Type="Recycling")

df_results %>% left_join(deposit) %>% pull(Country) %>% unique()

# get extraction
data2 <- df_results %>% 
  left_join(deposit) %>% 
  filter(t<2051) %>% 
  # filter(t==2050) %>% 
  mutate(Country=case_when(
    Country %in% key_countries ~ Country,
    Country %in% c("Spain","Germany","Finland","Czech Republic",
                   "Portugal","Austria","France") ~ "EU",
    T ~ "Others")) %>% 
  group_by(name,Resource_Type,Country) %>% 
  reframe(mtons=sum(tons_extracted)/1e3) %>% 
  rbind(recycling_country) %>% 
  left_join(tibble(Country=key_countries,country_legend=key_countries_legend)) %>% 
  mutate(group_plot=paste0(Resource_Type,country_legend)) %>% 
  mutate(name=substr(name,1,3) %>% as.factor()) %>% 
  # show only labels for country with a lot of extraction
  mutate(country_label=case_when(
    mtons>4/factor & country_legend=="Others" ~ country_legend,
    mtons>3/factor & country_legend!="Others"~country_legend, 
    mtons>2/factor & country_legend %in% c("EU","CAN")~country_legend,
    mtons>1.5/factor & country_legend %in% c("CAN")~country_legend,
    T ~""))

# order plot - by resource and country size - NEED TO MATCH order of first plot
order_plot_country <- data2 %>% 
  left_join(dict_res) %>% 
  mutate(res_legend=factor(res_legend,levels=resource_legend)) %>% 
  arrange(desc(mtons)) %>% arrange(res_legend) %>% pull(group_plot) %>% unique()
data2 <- data2 %>% mutate(group_plot=factor(group_plot,levels=order_plot_country))

# data_fig %>% group_by(name) %>% reframe(x=sum(mtons))
# data2 %>% group_by(name) %>% reframe(x=sum(mtons))

# Bar plot
# 2 bar plots with nudges
p2 <- ggplot(data_fig,aes(as.numeric(name)-0.15,mtons))+
  # Resource plot
  geom_col(aes(fill=res_legend),col="black",width = 0.3,linewidth=0.1)+
  geom_text(aes(label = res_legend,group=res_legend), position = position_stack(vjust = 0.5),
            size=6*5/14 * 0.8) +
  # Country plot
  geom_col(data=data2,aes(x=as.numeric(name)+0.15,fill=country_legend,group=group_plot),
           col="black",width = 0.3,linewidth=0.1)+
  geom_text(data=data2,position = position_stack(vjust = 0.5),angle=90,size=6*5/14 * 0.8,
            aes(x=as.numeric(name)+0.15,label = country_label,group=group_plot))+
  # Formatting and colors
  labs(x="Lithium Demand Scenario",y="",title="(A) 2022-2050 Li metal Supply [million tons]")+
  scale_x_continuous(breaks=1:9,labels=paste0("(",1:9,")"))+
  scale_fill_manual(values = c("RE"="#009E73","BR"="#0000FF33","HR"="#80008080","VS"="#FF000080",
                               "USA"="#1f78b4","AUS"="#cab2d6","CHL"="#d95f02",
                               "ARG"="#ff7f00","CAN"="#6A3D9A","CHN"="#ff0000",
                               "EU"="#a6cee3","Others"="#808080"))+
  coord_cartesian(expand = F)+
  theme_bw(8)+ 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

p2
# Save with width size of letter
ggsave("Figures/Article/FigSupply.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7,height=8.7)

library(cowplot)
cowplot::plot_grid(p2,p1,nrow=1,align = "v", axis = "tb")
ggsave("Figures/Article/Fig3.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)


# for scenario 4, biggest suppliers by country and type
df_results %>% 
  filter(t<2051) %>% 
  filter(str_detect(Scenario,"NMC")) %>% 
  left_join(deposit) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(tons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  group_by(Resource_Type) %>%  slice_max(order_by = tons, n = 5)


# when does USA becomes dominant
df_results %>% 
  filter(t<2051) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  filter(str_detect(name,"Ref")) %>% 
  left_join(deposit) %>% 
  group_by(Country,t) %>% 
  reframe(tons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  group_by(t) %>% mutate(share=tons/sum(tons)) %>% 
  arrange(desc(tons)) %>% slice_max(order_by = tons,n=1)


# Lithium demadb by time and region -----

data_region <- df_results %>%
  filter(t<2051) %>%
  left_join(deposit) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  group_by(name,Country,t) %>%
  reframe(total_prod=sum(tons_extracted)) %>% ungroup() %>% 
  group_by(name,t) %>% mutate(share_prod=total_prod/sum(total_prod)) %>% ungroup() %>% 
  mutate(d=if_else(share_prod>0.05,Country,"Others"))

countries <- unique(data_region$d) # use it they have surpass the threshold at any year

data_region <- data_region %>% 
  mutate(d=if_else(Country %in% countries,Country,"Others")) %>% 
  group_by(name,d,t) %>% 
  reframe(total_prod=sum(total_prod)) %>% ungroup() %>% 
  complete(name, t, d, fill = list(total_prod = 0)) # all combinatios for plot (avoid discontinuity)
  
# country order
cont_order <- data_region %>% group_by(name,d) %>%
  reframe(total_prod=sum(total_prod)) %>% ungroup() %>% 
  arrange(desc(total_prod)) %>% pull(d) %>% unique()
cont_order <- c(cont_order[-4],cont_order[4]) # move others to the end

data_region %>% 
  mutate(d=factor(d,levels=rev(cont_order))) %>% 
  ggplot(aes(t,total_prod,fill=d,group=d))+
  geom_area(col="black",linewidth=0.1)+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Li Metal Extraction [ktons]",fill="Country")+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_fill_manual(values = c("United States"="#1f78b4","Australia"="#cab2d6","Chile"="#d95f02",
                               "Argentina"="#ff7f00","Canada"="#6A3D9A","China"="#ff0000",
                               "Germany"="#a6cee3","Mali"="#008000","Others"="#808080"))+
  scale_x_continuous(breaks = c(2030, 2040, 2050))+
  theme(axis.text.x = element_text(hjust=1))

ggsave("Figures/Article/Production.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=12)



# EoF