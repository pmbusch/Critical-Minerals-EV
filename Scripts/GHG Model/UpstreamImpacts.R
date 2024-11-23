## Get LCA Data on Upstream Impacts for Vehicles and Batteries
# Yunzhu Nov 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")


# Location of excel
url_file <- "H:/.shortcut-targets-by-id/1plIa0mi3uOlZiLGrxKO0lx_iQ4Nc08gZ/HSF Critical Minerals/Modeling/GHG/%s"

# in tons CO2eq per vehicle
(ev_ghg <- read_excel(sprintf(url_file,"Emission Data.xlsx"),
                     sheet = "Vehicle Manufacture",range="A1:D6"))

# in kg CO2 per kWh
(lib_ghg <- read_excel(sprintf(url_file,"Emission Data.xlsx"),
                      sheet = "LIB Manufacture",range="A1:C4"))





# EoF