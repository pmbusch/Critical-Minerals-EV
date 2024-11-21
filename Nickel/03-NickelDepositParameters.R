# Compile Parameters for Deposits based on Database of data
# March 2024 PBH


# LOAD DATA ---------
# Load all deposit data
source("Nickel/02-NickelLoadDepositData.R", encoding = "UTF-8")



# COST REGRESSION MODELS ----------

# To Do
# Models by type of nickel deposit: laterite and sulphide
# Adjust for inflation towards 2022, using study year


## Extraction Costs -------

# try simple lm() model first, based on grade ore
# extraction costs ~ grade ore

## Expansion and Opening Costs --------

# try simple lm() model 
# investment cost ~ capacity
# intercept is initial opening cost, slope is capacity expansion

# DATA CONSOLIDATION -----

# Reserve ---------


# 3 stages of extraction
# ONLY IF GOOD ENOUGH DATA ON RESERVES, DEM. RESOURCES AND INF. RESOURCES

# Max Production Rate ---------
# 5% max depletion rate - based on total size

# Max Ramp Up ----------
# 4 years


# Fill Extraction Costs -----------

# Strategy 
# 1 - Use cost if available
# 2 - use regression model for deposits with grade ore
# 3 - fill the rest with quantile random sampling


## Royalties ----------

# Need to have the Table S7 from the Article
royalty <- read_excel(sprintf(url_file,"Nature Sust Submission/Data S3.xlsx"),
                      sheet="Taxes",range="A4:D42")
names(royalty) <- c("Country","Corporate_Tax_Rate","Royalty_Rate","Royalty_Based")
royalty <- royalty %>% 
  mutate(Royalty_Rate=as.numeric(Royalty_Rate)) %>% 
  mutate(State=Country)

tax <- royalty %>% dplyr::select(Country,Corporate_Tax_Rate) %>% 
  mutate(Corporate_Tax_Rate=if_else(str_detect(Corporate_Tax_Rate,"United States"),
                 pull(filter(royalty,Country=="United States"),Corporate_Tax_Rate),
                 Corporate_Tax_Rate)) %>% 
  mutate(Corporate_Tax_Rate=as.numeric(Corporate_Tax_Rate))
royalty <- royalty %>% dplyr::select(State,Royalty_Rate,Royalty_Based)

# Nickel price assumption
ni_price

# CODE TO CALCULATE TAX RATE BASED ON PRICE

# Expansion Costs ----------
# USE regression

# Opening Costs -----
# USE regression

# Current production rate -----
# USGS 2022 and 2023 production data by country
# LOAD AVAILABLE DATA

# Could change the delay based on each deposit status

# Status to delay mines
df <- df %>% 
  mutate(Status_Delay=case_when(
    Status_Detail=="Construction" ~ "Construction", 
    open_mine==T ~ "Open",
    !is.na(Status_Detail) ~ "Evaluation",
    T ~ "No Info"))
table(df$Status_Delay)

# DELAY - years to avoid any expansion (starting from 2022)
df <- df %>% 
  mutate(delay_years=case_when(
    Status_Delay=="Open" ~ 3, # no expansion until 2025
    Status_Delay=="Construction" ~ 5, # until 2027 
    Status_Delay=="Evaluation" ~ 8, # until 2030
    Status_Delay=="No Info" ~ 10, # until 2032
      T ~ 10))
table(df$delay_years)




# Non Monetary Index ----------

## Ease of Doing Business
edb <- read.csv("Data/Supply Model/EDB.csv")
names(edb) <- c("Country","edb")

# add by country
df <- df %>% left_join(edb) %>% 
  mutate(edb=100-edb) # from 0 (better) to 100, to minimize

# SAVE -----
# Select only required columns
# NEED TO COMPLETE
df <- df %>% dplyr::select()


# add rownames
df <- df %>% rownames_to_column() %>% rename(d=rowname)
df$d <- as.numeric(df$d)


head(df)
write.csv(df,"Parameters/Deposit.csv",row.names = F)

# EoF