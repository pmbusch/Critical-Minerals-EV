# EDB Curve Optimization
# PBH June 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")
(runs <- list.dirs("Results/Optimization/EDBCurve",recursive = F))

# Read values
df <- do.call(rbind, lapply(runs, function(folder_path)
  transform(read.delim(file.path(folder_path, "OptimalValue.txt"),sep=":",header=F),
  loop = basename(folder_path))))

df <- df %>% 
  mutate(Degradation=loop %>% str_remove("EDBLoop "),
         Degradation=paste0(Degradation,"%")) %>% 
  pivot_wider(names_from = V1, values_from = V2)

ggplot(df,aes(`Objective 1`,`Objective 2`))+
  geom_point()+
  geom_text(aes(label=Degradation),nudge_x = 3e3,nudge_y = 5e2)+
  labs(x="Min Costs",y="Min Ease of Doing Business Index")+
  theme(axis.title.y=element_text(angle=90,margin=margin(r=0)))

ggsave("Figures/curve.png")
