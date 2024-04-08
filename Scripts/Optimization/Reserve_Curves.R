# Curves for Reserve and Resources based on Grade of Lithium
# March 2024 PBH

# Load Data ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Data/Mine Characterization/%s"

# Collected Lithium data
df <- read_excel(sprintf(url_file,"Prosper_Lithium.xlsx"),
                 sheet="Lithium_Database")
(colnames(df) <- colnames(df) %>% str_replace(" ","_") %>% 
    str_replace("%","perc") %>% str_replace("/","_per"))
df_all <- df
df <- df %>% dplyr::select(Country,Deposit_Name,Resource_Type,Latitude,Longitude,Mt_Li,Status,
                           Production_Li_ktons,Reserve_Li_ktons,Resource_Li_ktons,
                           `Reserve_Proven; Probable`,`Resource_Measured, Indicated, Inferred`,
                           Grade_reserve,Grade_Resource,Grade_original_units,`Grade_Proven;Probable`,
                           Grade_Resource,Grade_units,`Grade_Measured, Indicated, Inferred`,
                           Grade_percLi_Reserve,Grade_percLi_Resource)

df <- df %>% filter(!is.na(Mt_Li) & Mt_Li>0)
nrow(df) # 116 deposits
sum(df$Mt_Li) # 76 Mt

names(df)

# Process Data ------------

# use resource of Benson if NA
df <- df %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Resource_Li_ktons),Mt_Li*1e3,Resource_Li_ktons))
df$Resource_Li_ktons
sum(df$Resource_Li_ktons)/1e3 # 94 Mt


# Correct brine grade units
df <- df %>% 
  # SQM in %Li, towards mg/L is 1e4
  mutate(Grade_reserve=if_else(Resource_Type=="Brine"&!str_detect(Grade_original_units,"mg"),
                   1e4,1)*Grade_reserve,
         Grade_Resource=if_else(Resource_Type=="Brine"&!str_detect(Grade_units,"mg"),
                               1e4,1)*Grade_Resource,
         # Jadar, from %Li2O to ppm
         Grade_reserve=if_else(Resource_Type=="Volcano-Sedimentary"&!str_detect(Grade_original_units,"ppm"),
                               0.464*1e4,1)*Grade_reserve,
         Grade_Resource=if_else(Resource_Type=="Volcano-Sedimentary"&!str_detect(Grade_units,"ppm"),
                               0.464*1e4,1)*Grade_Resource)


url_fig <- "Figures/Deposit/%s.png"

# Curve Creation
# Function based on reserve type
f.curve.res <- function(data_f,res_type="Hard Rock",lab_limit=150,
                        type="Reserve",detail=F){
  
  if(type=="Reserve"){
    data_f <- data_f %>% mutate(res=Reserve_Li_ktons,grade=Grade_reserve)
  }
  if(type=="Resource"){
    data_f <- data_f %>% mutate(res=Resource_Li_ktons,grade=Grade_Resource)
  }
  
  if (detail==F){
    data_f <- data_f %>% mutate(col_line=Resource_Type)
    res_leg <- resource_colors
    leg_pos <- "none"
  }
  if (detail==T & type=="Reserve"){ # detail for proven-probable
    data_f <- data_f %>% mutate(col_line=Reserve)
    res_leg <- c("Proven"="#696969","Probable"="#A52A2A")
    leg_pos <- c(0.8,0.8)
  }
  if (detail==T & type=="Resource"){ # detail for resource
    data_f <- data_f %>% mutate(col_line=Resource)
    res_leg <- c("Inferred"="#6b6b6b","Indicated"="#ff7f0e", "Measured"="#1f77b4")
    leg_pos <- c(0.8,0.8)
  }
  
  
  
  data_fig <- data_f %>% 
    filter(Resource_Type==res_type) %>% 
    filter(res>0) %>% 
    filter(grade>0) %>% 
    arrange(desc(grade)) %>% 
    mutate(reserve_cum_end=cumsum(res),
           reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
    mutate(lab_dep=if_else(res>lab_limit,Deposit_Name,"")) %>% 
    mutate(lab_pos=reserve_cum_start+res/2) %>% 
    mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling
  
  nrow(data_fig)  
  
  # duplicate last row
  last_row <- data_fig[nrow(data_fig),]
  last_row$reserve_cum_start <- last_row$reserve_cum_end
  last_row$lab_dep <- ""
  data_fig <- rbind(data_fig,last_row)
  
  # Limits
  lim_x <- ceiling(max(data_fig$reserve_cum_end)/500)*500 # upper by 500
  lim_y <- ceiling(max(data_fig$grade)*1.1/0.1)*0.1
  
  even_row <- data_fig$even_row
  
  # Labels 
  if(res_type=="Hard Rock"){
   y_lab =  "Grade Li2O %"
   y_cut = 0.5
   y_cut_text = 0.6
   suff_lab = "%"
  }
  if(res_type=="Brine"){
    y_lab =  "Grade Lithium \n[mg/L]"
    y_cut = 400
    y_cut_text = 500
    suff_lab = ""
  }
  if(res_type=="Volcano-Sedimentary"){
    y_lab =  "Grade Lithium \n[ppm]"
    y_cut = 900
    y_cut_text = 1000
    suff_lab = ""
  }
  
  p1 <- ggplot(data_fig,aes(reserve_cum_start,grade,col=col_line,group=1))+
    geom_step(linewidth=0.75,direction = "hv")+
    # geom_segment(aes(xend=reserve_cum_end))+
    geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",nudge_y = 0.1*even_row,
                    size=6*5/14 * 0.8)+
    geom_hline(yintercept = y_cut,linetype="dashed")+
    annotate(geom="text",y=y_cut_text,x=1000,label="Normal Cut-off Grade",col="darkgrey",
             size=6*5/14 * 0.8)+
    facet_wrap(~Resource_Type)+
    labs(x=paste0("Cumulative ",type," [ktons Li]"),y=y_lab,col="")+
    coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(0,lim_y))+
    scale_color_manual(values=res_leg)+
    scale_y_continuous(labels = scales::comma_format(big.mark = ' ',suffix = suff_lab))+
    scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
    theme(legend.position = leg_pos,
          legend.text = element_text(size=6),
          legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.spacing = unit(0.05,"cm"),
          legend.title = element_text(size=7))
  
  return(p1)
}

p1 <- f.curve.res(df)
p2 <- f.curve.res(df,"Brine")
p3 <- f.curve.res(df,"Volcano-Sedimentary")

p <- cowplot::plot_grid(p1,p2,p3,nrow=3)
p

f.fig.save(sprintf(url_fig,"Curve_ReserveGrade"),w = 18,h=18)

p1 <- f.curve.res(df,type="Resource")
p2 <- f.curve.res(df,type="Resource","Brine")
p3 <- f.curve.res(df,type="Resource","Volcano-Sedimentary")
p <- cowplot::plot_grid(p1,p2,p3,nrow=3)
p

f.fig.save(sprintf(url_fig,"Curve_ResourceGrade"),w = 18,h=18)


## Reserve proven-probable -----
# Get reserve_proven probable
# Simple for now, separate by : and then get proportion
df_reserves <- df %>%
  filter(Reserve_Li_ktons>0) %>% 
  filter(Grade_reserve>0) %>% 
  # Get share Proven and Probable
  rename(proven_probable=`Reserve_Proven; Probable`) %>% 
  separate(proven_probable, into = c("proven", "probable"), sep = ";") %>% 
  mutate(proven=as.numeric(proven),
         probable=as.numeric(probable)) %>% 
  mutate(share_proven=proven/(proven+probable),
         share_probable=probable/(proven+probable)) %>% 
  # Get Grade Proven and Probable
  rename(grade_proven_probable=`Grade_Proven;Probable`) %>% 
  separate(grade_proven_probable, into = c("gr_proven", "gr_probable"), sep = ";") %>% 
  mutate(gr_proven=as.numeric(gr_proven),
         gr_probable=as.numeric(gr_probable)) %>% 
  dplyr::select(Deposit_Name,Resource_Type,share_proven,share_probable,Grade_original_units,
                Reserve_Li_ktons,gr_proven,gr_probable) %>% 
  pivot_longer(c(share_proven,share_probable,gr_proven,gr_probable), 
               names_to = "Reserve", values_to = "value") %>%
  mutate(key=if_else(str_detect(Reserve,"share"),"Li_kton","Grade_reserve"),
         Reserve=if_else(str_detect(Reserve,"proven"),"Proven","Probable")) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
# consider all reserves without classification as probable
  filter(!is.na(Li_kton) | Reserve=="Probable") %>% 
  mutate(Reserve_Li_ktons=if_else(is.na(Li_kton),1,Li_kton)*Reserve_Li_ktons) %>% 
  # SQM in %Li, towards mg/L is 1e4
  mutate(Grade_reserve=if_else(Resource_Type=="Brine"&!str_detect(Grade_original_units,"mg"),
                               1e4,1)*Grade_reserve,
         # Jadar, from %Li2O to ppm
         Grade_reserve=if_else(Resource_Type=="Volcano-Sedimentary"&!str_detect(Grade_original_units,"ppm"),
                               0.464*1e4,1)*Grade_reserve)
sum(df$Reserve_Li_ktons,na.rm = T)
sum(df_reserves$Reserve_Li_ktons)
df_reserves %>% group_by(Reserve) %>% reframe(Mt=sum(Reserve_Li_ktons)/1e3)


p1 <- f.curve.res(df_reserves,detail = T)
p2 <- f.curve.res(df_reserves,detail = T,"Brine")
p3 <- f.curve.res(df_reserves,detail = T,"Volcano-Sedimentary")

p <- cowplot::plot_grid(p1,p2,p3,nrow=3)
p

f.fig.save(sprintf(url_fig,"Curve_ReserveGradeProbable"),w = 18,h=18)

## Resources Detail ----------
df_resources <- df %>%
  filter(Resource_Li_ktons>0) %>% 
  filter(Grade_Resource>0) %>% 
  # Separate
  rename(res_cat=`Resource_Measured, Indicated, Inferred`) %>% 
  separate(res_cat, into = c("Measured","Indicated","Inferred"), sep = ";") %>% 
  mutate(Measured_tons=as.numeric(Measured),
         Indicated_tons=as.numeric(Indicated),
         Inferred_tons=as.numeric(Inferred)) %>% 
  mutate(sh_Measured=Measured_tons/(Measured_tons+Indicated_tons+Inferred_tons),
         sh_Indicated=Indicated_tons/(Measured_tons+Indicated_tons+Inferred_tons),
         sh_Inferred=Inferred_tons/(Measured_tons+Indicated_tons+Inferred_tons)) %>% 
  # Get Grade Resource
  rename(grade_cat=`Grade_Measured, Indicated, Inferred`) %>% 
  separate(grade_cat, into = c("gr_Measured", "gr_Indicated","gr_Inferred"), sep = ";") %>% 
  mutate(gr_Measured=as.numeric(gr_Measured),
         gr_Indicated=as.numeric(gr_Indicated),
         gr_Inferred=as.numeric(gr_Inferred)) %>% 
  dplyr::select(Deposit_Name,Resource_Type,
                sh_Measured,sh_Indicated,sh_Inferred,
                Grade_units,Resource_Li_ktons,
                gr_Measured,gr_Indicated,gr_Inferred) %>% 
  pivot_longer(c(sh_Measured,sh_Indicated,sh_Inferred,gr_Measured,gr_Indicated,gr_Inferred), 
               names_to = "Resource", values_to = "value") %>%
  mutate(key=if_else(str_detect(Resource,"sh_"),"Li_kton","Grade_Resource"),
         Resource=case_when(
           str_detect(Resource,"Measu") ~ "Measured",
           str_detect(Resource,"Indicat") ~ "Indicated",
           T ~ "Inferred")) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  # consider all resources without classification as indicated
  filter(!is.na(Li_kton) | Resource=="Indicated") %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Li_kton),1,Li_kton)*Resource_Li_ktons) %>% 
  # SQM in %Li, towards mg/L is 1e4
  mutate(Grade_Resource=if_else(Resource_Type=="Brine"&!str_detect(Grade_units,"mg"),
                                1e4,1)*Grade_Resource,
         # Jadar, from %Li2O to ppm
         Grade_Resource=if_else(Resource_Type=="Volcano-Sedimentary"&!str_detect(Grade_units,"ppm"),
                                0.464*1e4,1)*Grade_Resource)

sum(df$Resource_Li_ktons,na.rm = T)/1e3 #in Mt
sum(df_resources$Resource_Li_ktons)/1e3
df_resources %>% group_by(Resource) %>% reframe(Mt=sum(Resource_Li_ktons)/1e3)


p1 <- f.curve.res(df_resources,type = "Resource",detail = T)
p2 <- f.curve.res(df_resources,type = "Resource",detail = T,res_type = "Brine")
p3 <- f.curve.res(df_resources,type = "Resource",detail = T,res_type = "Volcano-Sedimentary")

p <- cowplot::plot_grid(p1,p2,p3,nrow=3)
p

f.fig.save(sprintf(url_fig,"Curve_ResourceGrade_detail"),w = 18,h=18)




# EoF