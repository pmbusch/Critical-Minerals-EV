# Master code to estimate GHG impacts
# PBH and YC Nov 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load Data ---------

## Fleet, Sales and LIB -----

fleet <- read.csv("Results/GHG/USA_fleet.csv")
head(fleet)

## LIB Size and Chemistry Share ------

bat_ldv <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_ldv.csv")
bat_rest <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_rest.csv")

# Filter USA
bat_ldv <- bat_ldv %>% filter(Region=="United States")
bat_rest <- bat_rest %>% filter(Region=="United States")

# Reference scenario FOR NOW
bat_ldv <- bat_ldv %>% 
  filter(chem_scenario=="Baseline",capacity_scenario=="Baseline")

head(bat_ldv)
head(bat_rest)

# Vehicle Segment Share -----

# TO DO - Do processing in another script, here we just load


## Energy consumption forecast -----

# TO DO - Do processing in another script, here we just load


# Electricity Grid Upstream -----

# TO DO - Do processing in another script, here we just load


# Gasoline production Upstream Impact -----

# TO DO - Do processing in another script, here we just load


# Battery recycling Impact -----

# TO DO - Do processing in another script, here we just load

# CAN BE DONE IN A 2ND ROUND MODEL DEVELOPMENT

# Parameter Assumption ----

# Vehicle miles travel per year
vmt <- 12000






# EoF