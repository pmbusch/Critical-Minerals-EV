# Compile Parameters for Deposits based on Database of data
# March 2024 PBH

# A lot of assumptions right now

# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Data/Mine Characterization/%s"

# Collected Lithium data
df <- read_excel(sprintf(url_file,"Prosper_Lithium.xlsx"),
                 sheet="Lithium_Database")
(colnames(df) <- colnames(df) %>% str_replace(" ","_") %>% 
  str_replace("%","perc") %>% str_replace("/","_per"))
df_all <- df
df <- df %>% 
  rename(proven_probable=`Reserve_Proven; Probable`) %>% 
  rename(res_cat=`Resource_Measured, Indicated, Inferred`) %>% 
  rename(grade_cat=`Grade_Measured, Indicated, Inferred`) %>% 
  dplyr::select(Country,Deposit_Name,Resource_Type,Latitude,Longitude,Mt_Li,Status,
                Production_Li_ktons,Reserve_Li_ktons,Resource_Li_ktons,
                Grade_percLi_Reserve,Grade_percLi_Resource,Mg_Li_Ratio,
                proven_probable,res_cat,grade_cat,
                USD_pertonne_Li,USD_pertonne_LCE,Investment_original)


# PROCESS DATA ------------

## Resources -----
# use resource of Benson if NA
df <- df %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Resource_Li_ktons),Mt_Li*1e3,Resource_Li_ktons))
df$Resource_Li_ktons
sum(df$Resource_Li_ktons,na.rm = T)/1e3 # 103 Mt

df <- df %>% filter(!is.na(Resource_Li_ktons) & Resource_Li_ktons>0)
nrow(df) # 129 deposits

names(df)


## Cost difference -----
## Add cost for brines based on price differential 4953USD per ton LCE
# Include diff for LiOH as well
diff_hardrock <- 4953
# missing: volcano-sedimentary
df <- df %>% 
  mutate(USD_pertonne_Li=USD_pertonne_Li+if_else(Resource_Type=="Hard Rock",diff_hardrock*5.323,0),
         USD_pertonne_LCE=USD_pertonne_LCE+if_else(Resource_Type=="Hard Rock",diff_hardrock,0))

## Status ----
table(df$Status)
# In construction: considered open
df <- df %>% 
  mutate(open_mine=Status %in% c("Producing","Producing & suspended","Construction"))
sum(df$open_mine) # 37

## Reserve detail proven and probable --------

# Simple for now, separate by : and then get proportion
df <- df %>% 
  separate(proven_probable, into = c("proven", "probable"), sep = ";") %>% 
  mutate(proven=as.numeric(proven),
         probable=as.numeric(probable)) %>% 
  mutate(share_proven=proven/(proven+probable),
         share_probable=probable/(proven+probable)) %>% 
  mutate(proven=NULL,probable=NULL)

## Resource in detail measured, indicated and inferred -----

# Simple for now, separate by : and then get proportion
df <- df %>% 
  separate(res_cat, into = c("Measured","Indicated","Inferred"), sep = ";") %>% 
  mutate(Measured_tons=as.numeric(Measured),
         Indicated_tons=as.numeric(Indicated),
         Inferred_tons=as.numeric(Inferred)) %>% 
  mutate(share_Measured=Measured_tons/(Measured_tons+Indicated_tons+Inferred_tons),
         share_Indicated=Indicated_tons/(Measured_tons+Indicated_tons+Inferred_tons),
         share_Inferred=Inferred_tons/(Measured_tons+Indicated_tons+Inferred_tons)) %>% 
  mutate(Measured_tons=NULL,Indicated_tons=NULL,Inferred_tons=NULL)

## Grade Resources --------

df <- df %>% 
  separate(grade_cat, into = c("gr_Measured", "gr_Indicated","gr_Inferred"), sep = ";") %>% 
  mutate(gr_Measured=as.numeric(gr_Measured),
         gr_Indicated=as.numeric(gr_Indicated),
         gr_Inferred=as.numeric(gr_Inferred))

  

# REGRESSION MODELS ----------

## Extraction Costs -------

# Brine
# Really bad fit and not so many observations 
mod_brine <- lm(USD_pertonne_Li~
                  Grade_percLi_Reserve+
                  # Mg_Li_Ratio+
                  # I(Grade_percLi_Reserve^2)+
                  Reserve_Li_ktons,
                # Resource_Li_ktons,
                weights = Reserve_Li_ktons,
                data=filter(df,Resource_Type=="Brine",Deposit_Name!="Kachi"))
nobs(mod_brine)
summary(mod_brine)
coef_brine <- coefficients(mod_brine)

# Hard Rock
mod_rock <- lm(USD_pertonne_Li~Grade_percLi_Reserve+Reserve_Li_ktons,
               weights = Reserve_Li_ktons,
               data=filter(df,Resource_Type=="Hard Rock"))
nobs(mod_rock)
summary(mod_rock)
coef_rock <- coefficients(mod_rock)

# Volcano Sedimentary
mod_volcano <- lm(USD_pertonne_Li~Grade_percLi_Reserve,
                  data=filter(df,Resource_Type=="Volcano-Sedimentary"))
nobs(mod_volcano)
summary(mod_volcano)
coef_volcano <- coefficients(mod_volcano)


## Expansion and Opening Costs --------

# cost per tpa 
df %>% 
  filter(Production_Li_ktons>0, Investment_original>0) %>% 
  mutate(cost_tpa=Investment_original/Production_Li_ktons*1e3) %>%  # USD per ton per year Li in capacity
  group_by(Resource_Type) %>% 
  dplyr::select(cost_tpa,Resource_Type) %>% 
  skimr::skim_without_charts()

# capacity expansion models
# need to be non linear or favor economies of scale
# Intercept favor economies of scale

mod_cap_brine <- lm(Investment_original~Production_Li_ktons,
                    data=filter(df,Resource_Type=="Brine"))
nobs(mod_cap_brine)
summary(mod_cap_brine) # Coefficients are in Million USD per kton, or multiply by 1000 to get USD per ton

mod_cap_rock <- lm(Investment_original~Production_Li_ktons,
                   data=filter(df,Resource_Type=="Hard Rock"))
nobs(mod_cap_rock)
summary(mod_cap_rock)


# DELETE
# mod_investment <- lm(Investment_original~Resource_Li_ktons,
#                      data=df)
# nobs(mod_investment)
# summary(mod_investment)
# coef_invest <- coefficients(mod_investment)



# FIGURES -----------
url_fig <- "Figures/Deposit/%s.png"

## Resources and reserves -----
data_fig <- df %>% 
  filter(Reserve_Li_ktons>0)
ggplot(data_fig,aes(reorder(Deposit_Name,Resource_Li_ktons),Resource_Li_ktons,
             fill=Resource_Type))+
  geom_col()+
  geom_col(aes(y=Reserve_Li_ktons),fill="black",alpha=.3)+
  coord_flip(ylim = c(NA,11e3),expand = F)+
  labs(x="",y="Resources [ktons Li]",fill="Resource \ntype",
       caption = "Shaded area: Reserves.")+
  scale_fill_manual(values=resource_colors)+
  # scale_y_log10()+
  theme(legend.position = c(0.8,0.3))+
  scale_y_continuous(breaks = seq(2,10,2)*1e3,
                     labels = scales::comma_format(big.mark = ' '))+
  geom_hline(yintercept = seq(2,10,2)*1e3,col="white") # lines for the bar plot reading

# f.fig.save(sprintf(url_fig,"Resources_log"))
f.fig.save(sprintf(url_fig,"Resources"),h = 12)

# all resources
ggplot(df,aes(reorder(Deposit_Name,Resource_Li_ktons),Resource_Li_ktons,
                    fill=Resource_Type))+
  geom_col()+
  coord_flip(ylim = c(NA,11e3),expand = F)+
  labs(x="",y="Resources [ktons Li]",fill="Resource \ntype")+
  scale_fill_manual(values=resource_colors)+
  scale_y_continuous(breaks = seq(2,10,2)*1e3,
                     labels = scales::comma_format(big.mark = ' '))+
  geom_hline(yintercept = seq(2,10,2)*1e3,col="white")+ # lines for the bar plot reading
  theme(axis.text.y = element_text(size=6))

f.fig.save(sprintf(url_fig,"Resources_all"),h = 20)

## Reserves in detail ----------
# reserve proven and probable
df_res <- df %>% 
  filter(!is.na(share_proven)) %>%
  mutate(share_proven2=share_proven) %>% 
  dplyr::select(Deposit_Name,Resource_Type,share_proven,share_probable,share_proven2) %>%
  pivot_longer(c(share_proven,share_probable), names_to = "Reserve", values_to = "Li_kton") %>% 
  mutate(Reserve=str_replace(Reserve,"share_p","P"))

ggplot(df_res,aes(reorder(Deposit_Name,share_proven2),Li_kton,fill=Reserve))+
  geom_col()+
  facet_grid(Resource_Type~.,space = "free",scales = "free")+
  ggforce::facet_col(facets = vars(Resource_Type), 
                     scales = "free_y", 
                     space = "free") +
  coord_flip(expand = F)+
  scale_y_continuous(labels = scales::percent)+
  # geom_hline(yintercept = c(2.5,5,7.5,10)/10,col="white")+ # lines for the bar plot reading
  scale_fill_manual(values = c("#696969","#A52A2A"))+
  labs(x="",y="Share Proven-Probable Reserve [%]",fill="")+
  theme(strip.text.y = element_text(angle=0),
        legend.position = "bottom")+
  guides(fill=guide_legend(reverse = TRUE))
f.fig.save(sprintf(url_fig,"Reserve_ProvenProb"),h=14)

## Resources in detail ----------
# resources: measured, indicated, inferred
df_res <- df %>% 
  filter(!is.na(share_Measured)) %>% 
  mutate(share_Measured2=share_Measured) %>% 
  dplyr::select(Deposit_Name,Resource_Type,
                share_Measured,share_Indicated,share_Inferred,share_Measured2) %>% 
  pivot_longer(c(share_Measured,share_Indicated,share_Inferred), 
               names_to = "Resource", values_to = "Li_kton") %>%
  mutate(Resource=str_remove(Resource,"share_")) %>% 
  mutate(Resource=factor(Resource,
                         levels=rev(c("Measured","Indicated","Inferred")))) %>% 
  filter(!is.na(Li_kton))

ggplot(df_res,aes(reorder(Deposit_Name,share_Measured2),Li_kton,fill=Resource))+
  geom_col()+
  facet_grid(Resource_Type~.,space = "free",scales = "free")+
  ggforce::facet_col(facets = vars(Resource_Type), 
                     scales = "free_y", 
                     space = "free") +
  coord_flip(expand = F)+
  scale_y_continuous(labels = scales::percent)+
  # geom_hline(yintercept = c(2.5,5,7.5,10)/10,col="white")+ # lines for the bar plot reading
  scale_fill_manual(values =  c("#6b6b6b", "#ff7f0e", "#1f77b4"))+
  labs(x="",y="Share Resource [%]",fill="")+
  theme(strip.text.y = element_text(angle=0),
        legend.position = "bottom")+
  guides(fill=guide_legend(reverse = TRUE))
f.fig.save(sprintf(url_fig,"Resource_shareDetail"),h=20)



## Grade vs Reserve -----

p1 <- df %>% 
  mutate(Grade_percLi_Reserve=Grade_percLi_Reserve/100) %>% 
  filter(!is.na(Reserve_Li_ktons),!is.na(Grade_percLi_Reserve)) %>% 
  ggplot(aes(Grade_percLi_Reserve,Reserve_Li_ktons,col=Resource_Type))+
  geom_point()+
  scale_x_continuous(labels=scales::percent)+
  labs(y="Reserve [ktons Li]",x="Grade % Li",col="Resource \ntype")+
  scale_color_manual(values=resource_colors)
p1
f.fig.save(sprintf(url_fig,"Grade_reserves"))

# grade cost
p1+aes(y=USD_pertonne_Li)+labs(y="USD/ton Li")
  # geom_smooth(method="lm",se=F)
f.fig.save(sprintf(url_fig,"Grade_cost"))

# Grade cost Brine
df_all %>% 
  # filter(Resource_Type=="Hard Rock") %>%
  filter(Resource_Type=="Brine") %>%
  # filter(Deposit_Name!="Kachi") %>% 
  # filter(Resource_Type=="Volcano-Sedimentary") %>% 
  # SQM in %Li, towards mg/L is 1e4
  mutate(Grade_reserve=if_else(Resource_Type=="Brine"&!str_detect(Grade_original_units,"mg"),
                               1e4,1)*Grade_reserve) %>% 
  filter(!is.na(Reserve_Li_ktons),!is.na(Grade_reserve)) %>% 
  ggplot(aes(Grade_reserve,USD_pertonne_LCE))+
  geom_smooth(method="lm",se=F,col="darkred",
              # formula = "y~x+I(x^2)",
              aes(weight = Reserve_Li_ktons))+
  geom_point(aes(col=Resource_Type,size=Reserve_Li_ktons))+
  geom_text_repel(aes(label=Deposit_Name),col="black",size=10*5/14 * 0.8)+
  labs(y="Extraction Cost \n[USD/t LCE]",
       x="Grade Li [mg/L]",
       # x="Grade Li2O%",
       col="",size="Reserve [ktons Li]")+
  scale_color_manual(values=resource_colors)+
  guides(col="none")+
  # xlim(0,2200)+ylim(0,6100)+coord_cartesian(expand = F)+
  theme_bw(10)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  # scale_x_continuous(labels = scales::comma_format(big.mark = ' ',suffix = "%"))+ # for Hard rock
  theme(legend.position = c(0.8,0.75),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

f.fig.save(sprintf(url_fig,"RegCost_Brine"))
# f.fig.save(sprintf(url_fig,"RegCost_HardRock"))


# resource size vs investment
p1 <- df %>% 
  filter(Investment_original>0) %>% 
  ggplot(aes(Reserve_Li_ktons,Investment_original,col=Resource_Type))+
  geom_point()+
  labs(x="Reserve [ktons Li]",y="Investment \n [USD Million]",col="Resource \ntype")+
  scale_color_manual(values=resource_colors)
p1
# p1+geom_smooth(method = "lm",se=F)
f.fig.save(sprintf(url_fig,"Investment_cost"))

# Production vs Investment
p1 <- df %>% 
  filter(Investment_original>0,Production_Li_ktons>0) %>% 
  ggplot(aes(Production_Li_ktons,Investment_original,col=Resource_Type))+
  geom_point()+
  labs(x="Production [ktons Li per year]",y="Investment \n [USD Million]",col="Resource \ntype")+
  scale_color_manual(values=resource_colors)
p1
# p1+geom_smooth(method = "lm",se=F)
f.fig.save(sprintf(url_fig,"Investment_cost_prod"))





## Marginal Cost Curve -------
data_fig <- df %>% 
  # mutate(USD_pertonne_Li=cost_extraction) %>%  # for curve with all after regression
  # mutate(USD_pertonne_LCE=cost_extraction/5.323) %>% 
  # mutate(Reserve_Li_ktons=reserve) %>% 
  filter(USD_pertonne_Li>0) %>% 
  filter(Reserve_Li_ktons>0) %>% 
  arrange(USD_pertonne_Li) %>% 
  mutate(reserve_cum_end=cumsum(Reserve_Li_ktons),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(Reserve_Li_ktons>200,Deposit_Name,"")) %>% 
  mutate(lab_pos=reserve_cum_start+Reserve_Li_ktons/2) %>% 
  mutate(open_mine=if_else(open_mine,"Open","Feasibility or Permitting")) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling


nrow(data_fig)  # 30
table(data_fig$Status)

# duplicate last row
last_row <- data_fig[nrow(data_fig),]
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/500)*500 # upper by 500
max(data_fig$USD_pertonne_Li)
lim_y <- ceiling(max(data_fig$USD_pertonne_LCE)/500)*500

even_row <- data_fig$even_row

ggplot(data_fig,aes(reserve_cum_start,USD_pertonne_LCE,col=Resource_Type,group=1))+
  geom_step(linewidth=0.75,direction = "hv",aes(alpha=open_mine))+
  # geom_segment(aes(xend=reserve_cum_end))+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 1000*even_row,
                  size=6*5/14 * 0.8)+
  labs(x="Cumulative Reserves [ktons Li]",y="USD/ton LCE",
       col="Resource type",alpha="Status")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,lim_y))+
  scale_color_manual(values=resource_colors)+
  scale_alpha_manual(values = c("Feasibility or Permitting" = 0.4, "Open" = 1)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.25),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=7))
f.fig.save(sprintf(url_fig,"CostCurve"),w = 18)


## Atacama Cost Curve -----------

# Load atacama
atacama <- df %>% filter(str_detect(Deposit_Name,"Albemarle")) %>%
# Get 3 stages, reserve, resources and inferred resources
  mutate(stage1 = Reserve_Li_ktons,
         gr_stage1 = Grade_percLi_Reserve*1e4, # back to mg/L
         stage2 = Resource_Li_ktons*(share_Measured+share_Indicated)-Reserve_Li_ktons,
         gr_stage2 = (gr_Measured*share_Measured+gr_Indicated*share_Indicated)/(share_Measured+share_Indicated),
         stage3 = Resource_Li_ktons*share_Inferred,
         gr_stage3 = gr_Inferred) %>% 
  dplyr::select(Deposit_Name,stage1,gr_stage1,stage2,gr_stage2,stage3,gr_stage3,
                USD_pertonne_LCE)

# Add cost of extraction based on poorer grade of resources
# regression cost is in Li, so need to convert to LCE
coef_brine # in perc Li for now
atacama <- atacama %>% 
  mutate(cost_stage1=USD_pertonne_LCE,
         cost_stage2=USD_pertonne_LCE+(gr_stage2-gr_stage1)*coef_brine[2]/1e4/5.323, 
         cost_stage3=USD_pertonne_LCE+(gr_stage3-gr_stage1)*coef_brine[2]/1e4/5.323,
         # cost with reserve size as well
         cost_stage2=cost_stage2+stage2*coef_brine[3]/5.323,
         cost_stage3=cost_stage3+(stage2+stage3)*coef_brine[3]/5.323,
         USD_pertonne_LCE=NULL)


# Plot as curve
atacama <- atacama %>% 
  dplyr::select(-gr_stage1,-gr_stage2,-gr_stage3) %>% 
  pivot_longer(c(-Deposit_Name), names_to = "key", values_to = "value") %>% 
  mutate(stage=str_remove_all(key,"gr_|cost_")) %>% 
  mutate(key=if_else(str_detect(key,"cost"),"cost","reserve")) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  arrange(cost) %>% 
  mutate(reserve_cum_end=cumsum(reserve),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=stage) %>% 
  mutate(lab_pos=reserve_cum_start+reserve/2) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
last_row <- atacama[nrow(atacama),];last_row$reserve_cum_start <- last_row$reserve_cum_end;
last_row$lab_dep <- "";atacama <- rbind(atacama,last_row);

lim_x <- ceiling(max(atacama$reserve_cum_end)/500)*500 # upper by 500
lim_y <- ceiling(max(atacama$cost)/500)*500
even_row <- atacama$even_row


ggplot(atacama,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv")+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 100*even_row,
                  size=6*5/14 * 0.8)+
  labs(x="Cumulative Reserves [ktons Li]",y="USD/ton LCE",
       title="3-Stage curve example Atacama-Albemarle")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,lim_y))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.25),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=7),
        axis.text.x = element_text(hjust=1))

f.fig.save(sprintf(url_fig,"CostCurveAtacama"),w = 18)


## Hard Rock Cost Curve -----------

# Load 
rock <- df %>% filter(str_detect(Deposit_Name,"Holland")) %>%
  # Get 3 stages, reserve, resources and inferred resources
  mutate(stage1 = Reserve_Li_ktons,
         gr_stage1 = Grade_percLi_Reserve*2.153, # towards Li2O 
         stage2 = Resource_Li_ktons*(share_Measured+share_Indicated)-Reserve_Li_ktons,
         gr_stage2 = (gr_Measured*share_Measured+gr_Indicated*share_Indicated)/(share_Measured+share_Indicated),
         stage3 = Resource_Li_ktons*share_Inferred,
         gr_stage3 = gr_Inferred) %>% 
  dplyr::select(Deposit_Name,stage1,gr_stage1,stage2,gr_stage2,stage3,gr_stage3,
                USD_pertonne_LCE)

# Add cost of extraction based on poorer grade of resources
coef_rock 
rock <- rock %>% 
  mutate(cost_stage1=USD_pertonne_LCE,
         cost_stage2=USD_pertonne_LCE+(gr_stage2-gr_stage1)*coef_rock[2]/2.153/5.323,
         cost_stage3=USD_pertonne_LCE+(gr_stage3-gr_stage1)*coef_rock[2]/2.153/5.323,
         # cost with reserve size as well
         cost_stage2=cost_stage2+stage2*coef_rock[3]/5.323,
         cost_stage3=cost_stage3+(stage2+stage3)*coef_rock[3]/5.323,
         USD_pertonne_LCE=NULL)


# Plot as curve
rock <- rock %>% 
  dplyr::select(-gr_stage1,-gr_stage2,-gr_stage3) %>% 
  pivot_longer(c(-Deposit_Name), names_to = "key", values_to = "value") %>% 
  mutate(stage=str_remove_all(key,"gr_|cost_")) %>% 
  mutate(key=if_else(str_detect(key,"cost"),"cost","reserve")) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  arrange(cost) %>% 
  mutate(reserve_cum_end=cumsum(reserve),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=stage) %>% 
  mutate(lab_pos=reserve_cum_start+reserve/2) %>% 
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
last_row <- rock[nrow(rock),];last_row$reserve_cum_start <- last_row$reserve_cum_end;
last_row$lab_dep <- "";rock <- rbind(rock,last_row);

lim_x <- ceiling(max(rock$reserve_cum_end)/500)*500 # upper by 500
lim_y <- ceiling(max(rock$cost)/500)*500
even_row <- rock$even_row

ggplot(rock,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv")+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 100*even_row,
                  size=6*5/14 * 0.8)+
  labs(x="Cumulative Reserves [ktons Li]",y="USD/ton LCE",
       title="3-Stage curve example Mt Holland/Earl Grey")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,lim_y))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.25),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=7),
        axis.text.x = element_text(hjust=1))

f.fig.save(sprintf(url_fig,"CostCurveHolland"),w = 18)



# DATA CONSOLIDATION -----

# Reserve ---------
# Assumptions - fraction of reserve to fill missing
df$Resource_Type %>% unique()

# fraction per type
(frac_res <- df %>% filter(Reserve_Li_ktons>0) %>% 
  group_by(Resource_Type) %>% 
  reframe(reserve=sum(Reserve_Li_ktons),
          resource=sum(Resource_Li_ktons)) %>% ungroup() %>%  
  mutate(fraction=reserve/resource,reserve=NULL,resource=NULL))


df <- df %>% 
  left_join(frac_res) %>%
  mutate(reserve=if_else(!is.na(Reserve_Li_ktons),Reserve_Li_ktons,
                                  Resource_Li_ktons*fraction)) %>% 
  filter(!is.na(reserve))
sum(df$reserve)/1e3 # should be more than 38 (cumulative demand in baseline)

# Grow reserves to 80% of resources - FOR NOW
df <- df %>% mutate(reserve = if_else(reserve> Resource_Li_ktons*0.8, reserve, Resource_Li_ktons* 0.8))


# Max Production Rate ---------
# 5% max depletion rate

df <- df %>% mutate(max_prod_rate=reserve*0.05)

# Max Ramp Up ----------
# 4 years
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)


# Extraction Costs -----------

# avg by type
(cost_avg <- df %>% group_by(Resource_Type) %>% 
  reframe(avg_cost=mean(USD_pertonne_Li,na.rm=T)))

df <- df %>% 
  left_join(cost_avg) %>% 
  # linear regression or avg.
  mutate(cost_extraction=case_when(
         !is.na(USD_pertonne_Li) ~ USD_pertonne_Li,
         Resource_Type=="Brine" & !is.na(Grade_percLi_Reserve) ~ coef_brine[1]+coef_brine[2]*Grade_percLi_Reserve,
         Resource_Type=="Hard Rock" & !is.na(Grade_percLi_Reserve) ~ coef_rock[1]+coef_rock[2]*Grade_percLi_Reserve,
         Resource_Type=="Volcano-Sedimentary" & !is.na(Grade_percLi_Reserve) ~ coef_volcano[1]+coef_volcano[2]*Grade_percLi_Reserve,
         T ~ avg_cost+runif(nrow(df))*100)) # USD/kton Li
         # cost_extraction = cost_extraction + runif(nrow(df))/10) # random cost for Ties

# Expansion Costs ----------
# Ambrose formula to get expansion factor 
df <- df %>% 
  mutate(resource=Resource_Li_ktons) %>% 
  mutate(cost_expansion=exp((resource-reserve)/resource)*cost_extraction)

# Opening Costs -----
# avg cost and avg size
df %>% filter(Reserve_Li_ktons>0) %>% 
  reframe(Investment_original=mean(Investment_original,na.rm=T),
          Reserve_Li_ktons=mean(Reserve_Li_ktons,na.rm=T))
# on avg 1 Million USD per 1kton of reserve: so 1000 USD per ton

# Use avg
(avg_cost_open <- df %>% group_by(Resource_Type) %>% 
  reframe(avg_open_cost=mean(Investment_original,na.rm=T))) # all in M usd
# or regression based on Resource size (better fit)


table(df$Status)
table(df$open_mine)
df <- df %>% 
  # left_join(avg_cost_open) %>% 
  mutate(cost_opening=case_when(
    open_mine==T ~ 0, # if producing or in construction then 0, investment is already commited
    !is.na(Investment_original) ~ Investment_original,
    T ~ coef_invest[1]+coef_invest[2]*resource)
    # T ~ avg_open_cost+runif(nrow(df))*10)
    *1e6) # to USD

# Current production rate -----
# Give to open mines in countries
country_prod <- tibble(Country=c("Argentina","Australia","Bolivia","Brazil",
                                 "Canada","Chile","DR Congo","Mali","Mexico",
                                 "United States","Zimbabwe"),
                       prod_rate=c(6.2,61,0,2.2, # from USGS 2022, USA invented
                                   0.5,39,0,0,0,
                                   1,0.8))
# equally divide into mines open
country_prod <- df %>% filter(cost_opening==0) %>% group_by(Country,Deposit_Name) %>% tally() %>% 
  ungroup() %>% group_by(Country) %>% 
  mutate(total_country=sum(n)) %>% left_join(country_prod) %>% 
  mutate(prod_rate=prod_rate/total_country, n=NULL,total_country=NULL)

df <- df %>% left_join(country_prod) %>% 
  mutate(prod_rate=if_else(is.na(prod_rate),0,prod_rate))
sum(df$prod_rate)

# Fix issue that current production rate should be less or equal to max prod rate.
df <- df %>% mutate(max_prod_rate=if_else(max_prod_rate>prod_rate,max_prod_rate,prod_rate))

# save -----
df <- df %>% arrange(desc(reserve))
# add rownames
df <- df %>% rownames_to_column() %>% rename(d=rowname)
df$d <- as.numeric(df$d)

head(df)
write.csv(df,"Parameters/Deposit.csv",row.names = F)


# EoF