## Load all required libraries to use
## Common file to run from multiple scritps
## PBH Feb 2023

# Library -----
list_libraries <- c("tidyverse", "tidyverse","readr","readxl",
                    "ggplot2","data.table","dplyr","gridExtra",
                    "glmnet","openxlsx","reshape2",
                    "scales",
                    # "plotly", # sankey
                    "RColorBrewer",
                    "sf","ggrepel") # maps

# Install libraries if they are not present
# UNCOMMENT THE CODE TO INSTALL LIBRARIES THE FIRST TIME
# new_libraries <- list_libraries[!(list_libraries %in% installed.packages()[,"Package"])]
# lapply(new_libraries, install.packages)
# rm(new_libraries)

lapply(list_libraries, require, character.only = TRUE)

rm(list_libraries) 

theme_set(theme_bw(11)+ theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.title.y=element_text(angle=0)))

# Functions -----
# load all required functions automatically
file.sources = list.files("Scripts/00-Functions", pattern="*.R$", 
                          full.names=TRUE, ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
rm(file.sources)


# EoF