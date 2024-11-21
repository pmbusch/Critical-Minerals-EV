# Load  Nickel Deposits  Database 
# November 2024 PBH and PO


# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Put Excel file on this local folder
url_file <- "Nickel/Data/%s"


# Collected Nickel data
df <- read_excel(sprintf(url_file,"NICKEL DATABASE MAIN.xlsx"),
                 sheet="Nickel Mine")
(colnames(df) <- colnames(df) %>% str_replace_all(" ","_") %>% 
    str_replace("%","perc") %>% str_replace("/","_per"))
nrow(df)



# PROCESS DATA ------------

## Cost and product conversion -----

# Are all products equal to nickel battery grade, or a conversion cost is required?

table(df$ORE_TYPE)
# TO DO - Merge into 2 big categories

## Status ----
table(df$STATUS)
# TO DO - merge into bigger categories





# EoF