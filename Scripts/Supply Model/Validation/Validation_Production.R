# Just validate historical production by country using model results
# Reference scenario

deposit <- read.csv("Parameters/deposit.csv")

# historical production - USGS, 2015-2021
prod <- readxl::read_excel("Data/Supply Model/LiProduction_Country.xlsx",
                           sheet="Li_Prod",range="A1:H11")
prod <- prod %>% 
  pivot_longer(c(-Country), names_to = "t", values_to = "tons_extracted")



# read results by different degradation factors
(runs <- list.dirs("Results/Optimization/NonMonetary",recursive = T))
runs <- runs[str_count(runs,"/")==4] # keep only the final folders
runs

# Optimization parameters
opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
(bigM_cost <- opt_param[2,2])
(discount_rate <- opt_param[1,2] )

# Read all results and put them in the same dataframe!
df_results <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.csv(file.path(folder_path, "Base_Julia.csv")), 
            Scenario = folder_path)))
df_results <- df_results %>% rename(Deposit_Name=d)
df_results <- df_results %>% 
  filter(str_detect(Scenario,"Ambitious-Baseline-Baseline-Baseline-Baseline"))
df_results$Scenario %>% unique()
df_results <- df_results %>% 
  mutate(path=Scenario %>% str_remove("Results/Optimization/"),
         count_aux=str_count(path,"/"),
         path=paste0(path,if_else(count_aux==1,"/Base",""))) %>% 
  separate(path, into = c("Aux","Scen_Factor","Scenario"), sep = "/") %>% 
  mutate(count_aux=NULL,Aux=NULL)
unique(df_results$Scen_Factor)

data_fig <- df_results %>% 
  filter(t<2031) %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2+tons_extracted3) %>% 
  left_join(dplyr::select(deposit,Deposit_Name,Country),by=c("Deposit_Name")) %>% 
  group_by(Scen_Factor,Country) %>% reframe(tons_extracted=sum(tons_extracted)) %>% ungroup()

# aggregate 2015-2021
prod <- prod %>% 
  group_by(Country) %>% 
  reframe(tons_extracted=sum(tons_extracted)) %>% ungroup() %>% 
  mutate(share=tons_extracted/sum(tons_extracted)) %>% arrange(desc(share)) %>% 
  mutate(Scen_Factor="Historical 2015-2021")

data_fig2 <- data_fig %>% 
  group_by(Scen_Factor) %>% 
  mutate(share=tons_extracted/sum(tons_extracted)) %>% arrange(desc(share)) %>% 
  ungroup() %>% rbind(prod) %>% 
  filter(!str_detect(Scen_Factor,"WGI|WCR")) %>% 
  mutate(number=as.numeric(str_remove_all(Scen_Factor,"EDB|WGI|WCR| Weight "))) %>% 
  mutate(Scen_Factor=str_replace(Scen_Factor,"OnlyCost","Only Cost"),
         Scen_Factor=if_else(str_detect(Scen_Factor,"EDB|WGI|WCR"),
                             paste0("EDB Weight ",round(number*100,0),"%"),
                             Scen_Factor)) %>% 
  mutate(Scen_Factor=factor(Scen_Factor,
                            levels=c("EDB Weight 15%","EDB Weight 10%",
                                     "EDB Weight 5%","EDB Weight 3%","EDB Weight 1%",
                              "Only Cost","Historical 2015-2021")))

color_scale <- c("United States"="#1f78b4","Australia"="#cab2d6",
                 "Chile"="#d95f02","Argentina"="#ff7f00",
                 "Canada"="#6A3D9A","China"="#ff0000",
                 "Bolivia"="#33a02c", "Mali"="#b15928",
                 "Brazil"="#00755E", 
                 # "Zimbabwe"="#FFD700","Portugal"="#000000","Germany"="#a6cee3",
                 "Others"="#808080")
data_fig3 <- data_fig2 %>% 
  mutate(Country=if_else(Country %in% names(color_scale),Country,"Others")) %>% 
  group_by(Scen_Factor,Country) %>% reframe(share=sum(share)) %>% ungroup() %>% 
  mutate(Country=factor(Country,levels=rev(names(color_scale)))) %>% 
  mutate(label_share=if_else(share>0.05,paste0(round(share*100,0),"%"),""))

ggplot(data_fig3,aes(Scen_Factor,share,fill=Country))+
  geom_col(col="black",linewidth=0.1)+
  geom_text(aes(label=label_share),position = position_stack(vjust = 0.5),
            size=7*5/14 * 0.8)+
  coord_flip(expand = F)+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values=color_scale)+
  labs(x="",y="Lithium extraction share")+
  guides(fill= guide_legend(reverse = TRUE))+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("Figures/Validation/Production.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=14,height=8.7)


# EoF