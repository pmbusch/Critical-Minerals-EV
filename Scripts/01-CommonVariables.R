## Common variables to use in scripts.
## Common file to run from multiple scritps
## PBH Feb 2023

# Load Dimensions and Levels ----------
power_level <- read_excel("Data/Dimensions.xlsx",sheet="Powertrain")$Powertrain
region_level <- read_excel("Data/Dimensions.xlsx",sheet="Region")$Region
country_level <- read_excel("Data/Dimensions.xlsx",sheet="Country")$Country
vehicle_level <- read_excel("Data/Dimensions.xlsx",sheet="Vehicle")$Vehicle
scen_level <- c("Baseline","Momentum","Ambitious")

# Run all demand for all minerals in the model, filter later for min. of interest.
min_interest3 <- c("Lithium","Nickel","Cobalt","Manganese","Phosphorus",
                   "Graphite","Copper")
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

# Scenarios 
scens_selected <- read_excel("Data/Selected_Scenarios.xlsx")
scens_selected <- scens_selected$scen_all

# Names of scenarios
scens_names <- c("(1) Reference",
                 "(2) Large Capacity LIB","(3) Small Capacity LIB",
                 "(4) NMC811 Dominant Chemistry","(5) LFP Dominant Chemistry",
                 "(6) Solid State Adoption","(7) SIB Adoption",
                 "(8) Enhanced Repurposing","(9) Enhanced Recycling",
                 "(10) US Recycling","(11) Medium Recycling + \nSmall Capacity LIB")

# Abbreviation
name_abbr <- c("Ref.","Large \nLIB","Small \nLIB","NMC811","LFP",
               "Solid\nState","SIB","Repurp.","Recyc.","US\nRecyc.",
               "Med. Rec.+\nSmall LIB")

# Color - same order
scen_colors <- c("#000000",
                 "#0072B2","#56B4E9",
                 "#D55E00","#8B4513",
                 "#E69F00","#CC79A7",
                 "#D62728","#009E73",
                 "#FF450080","#6A0DAD80")
names(scen_colors) <- scens_names

# EoF