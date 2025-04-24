# Deposit opening and expansion, Map
# PBH February 2025

# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/01-LoadOptimizationResults.R", encoding = "UTF-8")

df_results <- df_results %>% 
  mutate(name=NULL) %>% 
  left_join(tibble(Scenario=scens_selected,name=name_abbr)) %>% 
  mutate(name=factor(name,levels=name_abbr))

# Get status of deposit at specific points in time
df <- df_results %>%
  # filter(str_detect(name,"Ref")) %>% # Filter only for reference scenario
  filter(str_detect(name,"Ref|Small|Large|Recyc|Med")) %>%
  filter(name!="US\nRecyc.") %>% 
  filter(t %in% c(2022,2030,2040,2050)) %>%
  left_join(deposit) %>%  
  filter(near(mine_open,1))

# label first year of opening
df <- df %>%
  group_by(Deposit_Name,name) %>%
  mutate(min_year = t == min(t)) %>%  ungroup() %>% 
  mutate(first_year=if_else(t!=2022&min_year,"Opened","Baseline"))

# Mine at max prod. rate?
df <- df %>% 
  mutate(first_year=if_else(near(prod_rate,max_prod_rate)&first_year!="Opened",
                            "Max Capacity",first_year))
# Mine depleted?
df <- df %>% 
  mutate(first_year=if_else(near(total_extraction,all_resource),
                            "Depleted",first_year)) %>% 
  mutate(first_year=factor(first_year,
                           levels=c("Baseline","Opened","Max Capacity","Depleted")))

table(df$first_year)

# Year summary metric
(year_metric <- df %>% group_by(t,name) %>% 
  reframe(n=sum(mine_open),
          cap=sum(cap_total)) %>% ungroup() %>% 
  mutate(label_metric=paste0("n: ",round(n,0)," ",
                             "Cap: ",round(cap,0))) %>% 
  mutate(long=0,lat=-55))

# remove baseline from figure, show just additionals
# df <- df %>%
#   mutate(remove=first_year=="Baseline"& t!=2022) %>%
#   filter(!remove)

# Map figure# Map figuremine_opened
map1 <- map_data('world')
names(df)
p1 <- ggplot(df) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group),
               col = 'gray', fill="white",linewidth=0.1) +
  coord_fixed(1.4, xlim = c(-140,160), ylim=c(-60,70))+
  # coord_fixed(1.4, xlim = c(-75,-60), ylim=c(-40,-10))+ #Li Triangle
  scale_y_continuous(breaks = NULL,name = "")+
  scale_x_continuous(breaks = NULL,name = "")+
  # data
  geom_point(aes(x = Longitude, y = Latitude,
                 size=cap_total,
                 col=first_year), alpha = 0.6) +
  geom_text(data=year_metric,aes(x=long,y=lat,label=label_metric),col="#454545",
            size=9*5/14 * 0.8,hjust=0)+
  # facet_wrap(~t)+
  facet_grid(name~t)+
  guides(col = guide_legend(ncol = 3))+
  scale_color_manual(values = c("Baseline" = "black", "Opened" = "darkblue",
                                "Max Capacity"="darkorange","Depleted"="darkred"))+
  labs(col="Deposit \nStatus",title="",size="Production Capacity [ktons]")+
  theme(panel.grid = element_blank(),
        # legend.position = c(0.5,0.1),
        strip.text=element_text(size = 14),
        panel.background = element_rect(color = "black"),
        legend.position="bottom",
        legend.background = element_rect(fill = "transparent", color = "black"),
        legend.text = element_text(size=5),
        plot.margin = margin(1, 1, 1, 1),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p1

ggsave("Figures/Article/SupplyMap.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       bg="white",
       width=18,height=18)

# ggsave("Figures/Article/SupplyMapAdditional.png", ggplot2::last_plot(),
#        units="cm",dpi=600,
#        bg="white",
#        width=18,height=18)

# EoF