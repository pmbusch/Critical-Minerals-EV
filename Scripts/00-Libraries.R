### MONET
## Load all required libraries to use
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


# Load Dimensions and Levels ----------

power_level <- read_excel("Data/Dimensions.xlsx",sheet="Powertrain")$Powertrain
region_level <- read_excel("Data/Dimensions.xlsx",sheet="Region")$Region
country_level <- read_excel("Data/Dimensions.xlsx",sheet="Country")$Country
vehicle_level <- read_excel("Data/Dimensions.xlsx",sheet="Vehicle")$Vehicle
scen_level <- c("Baseline","Momentum","Ambitious")

min_interest3 <- c("Lithium","Nickel","Cobalt","Manganese","Phosphorus")
min_interest2 <- c("Lithium","Nickel","Cobalt","Manganese")
min_interest <- c("Lithium","Nickel","Cobalt")


# Create a named vector of colors for each region
region_colors <- c("United States" = "#1f78b4",
                   "Mexico" = "#33a02c",
                   "Canada" = "#ff7f00",
                   "Brazil" = "#e31a1c",
                   "Other Latin America and Caribbean" = "#6a3d9a",
                   "European Union" = "#a6cee3",
                   "EFTA" = "#b2df8a",
                   "United Kingdom" = "#fb9a99",
                   "Other Europe" = "#fdbf6f",
                   "China" = "#ff0000",
                   "Japan" = "#6a5acd",
                   "South Korea" = "#fdb462",
                   "ASEAN" = "#66c2a5",
                   "India" = "#ff7f50",
                   "Australia/NZ" = "#cab2d6",
                   "Other Asia Pacific" = "#8b008b",
                   "Middle East" = "#8b4513",
                   "Africa" = "#4682b4",
                   "Rest of the World"="#808080")

resource_colors <- c("Brine" = "#0000FF33", "Hard Rock" = "#80008080",
                     "Volcano-Sedimentary" = "#FF000080")

# EoF