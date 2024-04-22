# Load  Deposits  Database and pre-process
# March 2024 PBH


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
                proven_probable,res_cat,grade_cat,Conversion_Grade_Resource,
                USD_pertonne_Li,USD_pertonne_LCE,Investment_original)

# PROCESS DATA ------------

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

## Resources -----

# separate resources into demonstrated (measured+indicated) and inferred
# If no info was found, then we assumed it is inferred
# use resource of Benson if NA, assume inferred
df <- df %>% 
  mutate(resource_all=Resource_Li_ktons) %>% 
  mutate(Resource_Li_ktons=resource_all*(share_Measured+share_Indicated),
         Resource_Inferred_Li_ktons=if_else(is.na(share_Inferred),
                                            resource_all,
                                            resource_all*share_Inferred)) %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Resource_Li_ktons),0,Resource_Li_ktons),
         Resource_Inferred_Li_ktons=if_else(is.na(Resource_Inferred_Li_ktons),     # Benson
                                            Mt_Li*1e3,Resource_Inferred_Li_ktons))

sum(df$Resource_Li_ktons,na.rm = T)/1e3 # 52 Mt
sum(df$Resource_Inferred_Li_ktons,na.rm = T)/1e3 # 66 Mt

df <- df %>% filter(Resource_Li_ktons+Resource_Inferred_Li_ktons>0)
nrow(df) # 145 deposits

# Calculate grade by resource type
df <- df %>% 
  mutate(Indicated=as.numeric(Indicated),Measured=as.numeric(Measured)) %>% 
  mutate(grade_resource=(gr_Indicated*Indicated+gr_Measured*Measured)/(Indicated+Measured)*
                           Conversion_Grade_Resource,
         grade_resource_inferred=gr_Inferred*Conversion_Grade_Resource) 
  

# EoF