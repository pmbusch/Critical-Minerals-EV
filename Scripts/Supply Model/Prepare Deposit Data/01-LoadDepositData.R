# Load  Deposits  Database and pre-process
# March 2024 PBH


# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# old
# url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Data/Mine Characterization/%s"
url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Reporting/Article/%s"

# New to get Table S6 excel from SI of Article
# url_file <- "Data/%s"

# Collected Lithium data
df <- read_excel(sprintf(url_file,"Table S6.xlsx"),
                 sheet="Deposits",range="A5:BN165")
(colnames(df) <- colnames(df) %>% str_replace_all(" ","_") %>% 
    str_replace("%","perc") %>% str_replace("/","_per"))
nrow(df)

df_all <- df
df <- df %>% 
  rename(proven_probable=`Reserve_Proven;_Probable`) %>% 
  rename(res_cat=`Resource_Measured;_Indicated;_Inferred`) %>% 
  rename(grade_cat=`Grade_Measured;_Indicated;_Inferred`) %>% 
  dplyr::select(Country,US_State,Deposit_Name,Resource_Detail,Latitude,Longitude,
                Status,Status_Detail,DLE_Extraction,
                Capacity_2025_Li_ktons,Capacity_2030_Li_ktons,Capacity_Forecast_Status,
                Project_Capacity_Li_ktons,Reserve_Li_ktons,Resource_Li_ktons,
                Grade_percLi_Reserve,Grade_percLi_Resource,
                proven_probable,res_cat,grade_cat,Conversion_Grade_Resource,
                Extraction_Cost_Original,Endproduct,Grade_Li2O_Spodumene_Concentrate,
                Investment_Original,Inflation_Adjusment_2022)

# PROCESS DATA ------------

## Cost and product conversion -----

# All to LCE, hard rock
# from SC6 to LCE, no considering Li mass content
# NO COST FROM LCE TO LIOH FOR NOW
conversion_lce <- 2500
# conversion_lce <- 2000
# conversion_lce <- 3000


table(df$Endproduct)

df <- df %>% 
  mutate(USD_pertonne_LCE=case_when(
    # grade to get weight content in LCE + conversion cost
    Endproduct=="Spodumene Concentrate" ~ Extraction_Cost_Original/Grade_Li2O_Spodumene_Concentrate*0.404+
      conversion_lce,
    Endproduct=="LCE" ~ Extraction_Cost_Original,
    Endproduct=="LiOH" ~ Extraction_Cost_Original*1.135, # from LiOH to LCE
    T ~ NA),
    USD_pertonne_Li=USD_pertonne_LCE*5.323)
           

## Status ----
table(df$Status)
table(df$Status_Detail)
df %>% group_by(Resource_Detail,Status) %>% tally() %>% arrange(desc(n))

# In construction: considered open (sunk cost)
df <- df %>% 
  mutate(open_mine=Status_Detail %in% c("Producing","Producing & suspended","Construction"))
sum(df$open_mine) # 52
df %>% group_by(Resource_Detail) %>% reframe(n=sum(open_mine))

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
df <- df %>% 
  mutate(resource_all=Resource_Li_ktons) %>% 
  mutate(Resource_Li_ktons=resource_all*(share_Measured+share_Indicated),
         Resource_Inferred_Li_ktons=if_else(is.na(share_Inferred),
                                            resource_all,
                                            resource_all*share_Inferred)) %>% 
  mutate(Resource_Li_ktons=if_else(is.na(Resource_Li_ktons),0,Resource_Li_ktons))
  #        Resource_Inferred_Li_ktons=if_else(is.na(Resource_Inferred_Li_ktons),     # Benson
  #                                           Mt_Li*1e3,Resource_Inferred_Li_ktons))

sum(df$Resource_Li_ktons,na.rm = T)/1e3 # 58 Mt
sum(df$Resource_Inferred_Li_ktons,na.rm = T)/1e3 # 71 Mt

df <- df %>% filter((Resource_Li_ktons+Resource_Inferred_Li_ktons)>0)
nrow(df) # 160 deposits

# Calculate grade by resource type
df <- df %>% 
  mutate(Indicated=as.numeric(Indicated),Measured=as.numeric(Measured)) %>% 
  mutate(grade_resource=(gr_Indicated*Indicated+gr_Measured*Measured)/(Indicated+Measured)*
                           Conversion_Grade_Resource,
         grade_resource_inferred=gr_Inferred*Conversion_Grade_Resource) 
  
# DLE
table(df$DLE_Extraction)
df <- df %>% 
  mutate(dle=!is.na(DLE_Extraction))
table(df$Resource_Detail,df$dle)


# EoF