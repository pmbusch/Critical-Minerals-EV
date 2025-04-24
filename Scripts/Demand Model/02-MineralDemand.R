# Load and pre-process ICCT Data
# Mineral Demand Module
# PBH August 2023



####################
# LOAD DATA ---------
####################
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
source("Scripts/Demand Model/01-ParametersDemand.R", encoding = "UTF-8")

## ICCT on-road demand projections -----
# Extended forecast towards 2070
icct <- read.csv("Parameters/Demand Intermediate Results/ICCT_demand.csv")
icct <- icct %>% filter(Sales>0) # reduce computational time
dict_region <- icct %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)

## Battery size -----
bat_ldv <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_ldv.csv")
bat_rest <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_rest.csv")

## Battery for stationary power storage -----
stationary <- read.csv("Results/stationaryPower.csv")
stationary <- stationary %>% rename(Country=ICCT_Country,Region=ICCT_Region)
stationary$Vehicle <- "Stationary Power Storage"
stationary_orig <- stationary

# Sodium for SSPS
stationary_SIB <- read.csv("Results/Sodium_stationaryPower.csv")
stationary_SIB <- stationary_SIB %>% rename(Country=ICCT_Country,Region=ICCT_Region)
stationary_SIB$Vehicle <- "Stationary Power Storage"

## Mineral intensity -----

mineral <- read_excel("Data/Demand Model/Mineral_Intensity.xlsx",sheet = "BatPac")
min_interest3 <- c(min_interest3,"Battery_MWh")

## Reuse statistics from Survival Model -----
reuse <- read.csv("Parameters/Demand Intermediate Results/region_outflows_LIB.csv",
                  stringsAsFactors = FALSE)
# Convert strings to vectors by year - Process the list column back into a list
reuse <- reuse %>%
  mutate(LIB_recycling_vector = str_split(LIB_recycling_vector,"\\|") %>% lapply(as.numeric),
         LIB_Available_vector = str_split(LIB_Available_vector,"\\|") %>% lapply(as.numeric),
         add_LIB_vector = str_split(add_LIB_vector,"\\|") %>% lapply(as.numeric))

# Metric LIB on avg per Car
reuse %>% 
  filter(Vehicle=="Car",Scenario=="Ambitious",Powertrain=="BEV",
         scen_lifetime=="Baseline",Year<2051) %>% 
  group_by(Region,Vehicle) %>% 
  reframe(Sales=sum(Sales)/1e6,add_LIB=sum(add_LIB)/1e6) %>% 
  mutate(metric=(Sales+add_LIB)/Sales) %>% arrange(desc(metric))


## Other sector demand -----
otherSectors <- read.csv("Parameters/Demand Intermediate Results/otherSector_demand.csv")


####################
# JOIN DATA  ---------
####################

# convert vector of age with reference year to vector of years 
# Define a custom function to calculate Y for each row
# Function simply moves with the vector indexing
# Reference year is always 2022, and the length is up to 2070 (49 of size)
# x is always 30 years of length
shift_X <- function(starting_year, x) {
  shift <- starting_year-2022+1
  x=x/sum(x) # do relative terms for flows inside X
  y = x[shift:1]
  if(starting_year<2052){
    y[1] <- y[1]+sum(x[-(1:shift)]) # add prior to 2022
  } else {
    # complete vector before and after, as in 2052 all 2022 cars are gone, for example
    y[1:(starting_year-2052+1)] <- 0 # early than 2022
  }
  # complete vector towards 2070
  if (shift<49){ # avoid border car
      y[(shift+1):49] <- 0 # 49 is 2022 to 2070
  }
  
  return(y) # vector from 2022 to 2070
}

# To calcualte average age for degradation of SOH
weighted_avg_index <- function(vec) {
  sum(vec * seq_along(vec)) / sum(vec)
}


# Do Loop for all demand scenarios - to make sure calculation are correct
# Bad part more data storage and calculations needed, but could be worth it

# scen_level <- c("Baseline","Momentum","Ambitious")
scen_level <- c("Ambitious")
chems_scen <- c("Baseline","Double LFP","Double NMC 811",
                "Solid State adoption","Sodium Battery adoption")
# chems_scen <- c("Baseline")
capacity_scen <- c("Baseline","Low Capacity","High Capacity")
lifetime_scen <- c("Baseline","Long duration")
# lifetime_scen <- c("Baseline")
recycling_scen <- recycling_scenarios$recycling_scenario %>% unique()

# global rec scenarios - uncomment to run demand loop
# recycling_scen <- global_rec_scenarios$recycling_scenarios %>% unique()
# mat_recovery_recycling <- global_rec_scenarios

# results
df_region_final <- c()
df_country_final <- c() # slow, only for selected scenarios
start_time <- proc.time()
# debug
scen=scen_level[1];scen_chem=chems_scen[1];scen_bat=capacity_scen[1];scen_life=lifetime_scen[1];scen_recyc=recycling_scen[2];
# scen=scen_level[3];scen_chem=chems_scen[1];scen_bat=capacity_scen[1];scen_life=lifetime_scen[1];scen_recyc=recycling_scen[1];
length(scen_level)*length(chems_scen)*length(capacity_scen)*length(lifetime_scen)*length(recycling_scen)
for (scen in scen_level){
  cat("Scenario ICCT: ",scen,"\n")
  for (scen_chem in chems_scen){
    cat("  Chemistry Scenario: ",scen_chem,"\n")
    for (scen_bat in capacity_scen){
        cat("    Capacity Scenario: ",scen_bat,"\n")
      for (scen_life in lifetime_scen){
          cat("      Lifetime Scenario: ",scen_life,"\n")
        for (scen_recyc in recycling_scen){
            cat("        Recycling Scenario: ",scen_recyc,"\n")
          
          # all scenarios combined
          scen_all <- paste(scen,scen_chem,scen_bat,scen_life,scen_recyc, sep="-")
          if(!(scen_all %in% scens_selected)){ # RUN ONLY DESIRED SCENARIOS
            next
          }

          df <- icct %>% 
            filter(Powertrain!="ICE") %>% 
            filter(Scenario==scen)
          df$Scenario <- NULL
          
          mat_recovery_recyc_aux <- mat_recovery_recycling %>% 
            filter(recycling_scenarios==scen_recyc)
          mat_recovery_recyc_aux$recycling_scenarios <- NULL  
          
          # Special USA case
          if(scen_recyc=="USA Recycling"){
            mat_recovery_recyc_aux <- mat_recovery_recycling_USA
            mat_recovery_recyc_aux$recycling_scenarios <- NULL  
          }
          
          # 55 recycling level
          if(scen_recyc=="Recycling Medium"){
            mat_recovery_recyc_aux <- global_rec_scenarios %>% 
              filter(recycling_scenarios=="Recycling percentage 55")
            mat_recovery_recyc_aux$recycling_scenarios <- NULL  
          }
          
          
          ## Add Battery size and chemistry ----------
          
          # Sodium scenario for stationary
          stationary <- if (str_detect(scen_chem,"Sodium")) stationary_SIB else stationary_orig
          
          bat_ldv_loop <- bat_ldv %>% 
            filter(chem_scenario==scen_chem) %>% 
            filter(capacity_scenario==scen_bat) %>% 
            mutate(chem_scenario=NULL,capacity_scenario=NULL)
          
          bat_rest_loop <- bat_rest %>% 
            filter(capacity_scenario==scen_bat) %>% 
            mutate(capacity_scenario=NULL)
            
          # LDV
          df_ldv <- df %>% filter(Vehicle=="Car") %>% 
            left_join(bat_ldv_loop)
          # REST
          df_rest <- df %>% filter(Vehicle!="Car") %>% 
            left_join(bat_rest_loop)
          
          # Join them
          df <- rbind(df_ldv,df_rest); rm(df_ldv,df_rest);
          
          ## Failure batteries additional for replacement -----
          # Outflow to SSPS and Recycling as well
          
          # Add new requirement of batteries by sector
          
          # Summarise kwh_veh through years as vectors
          bat_ldv_loop$Year %>% range()
          # bat_ldv_loop$Powertrain %>% unique()
          # count of all years - complete?
          bat_ldv_loop %>% 
            # filter(Powertrain=="BEV") %>% 
            group_by(Country,chemistry,Powertrain) %>% 
            tally() %>% pull(n) %>% range()
          
      
          bat_ldv_chem <- bat_ldv_loop %>% 
            # filter(Powertrain=="BEV") %>%
            arrange(Year) %>% 
            group_by(Powertrain,Country,chemistry) %>%
            summarise(kwh_veh = list(kwh_veh)) %>% ungroup()
          
          # bat_ldv_chem %>% mutate(lenght=length(kwh_veh[[1]])) %>% pull(lenght) %>% unique() # 29 all of them
          # bat_ldv_chem[9:11,]
          
          # the column kwh_chem contains the battery size for each year as a vector, from 2022 to 2050
          # bat_ldv_chem %>% filter(Country=="United States",
          #                         chemistry=="LFP") %>% pull(kwh_veh) # LFP increases in this scenario
          # 
          
          # DO THE SAME for reuse cars data.frame, but move vector of age to vector of years
          # Only cars have moving chemistry and size for now
          reuse_car <- reuse %>% filter(Vehicle=="Car") %>% 
            # filter(Powertrain=="BEV") %>%
            filter(scen_lifetime==scen_life) %>% 
            filter(Scenario==scen) %>% 
            dplyr::select(Region,Year,perc_add_lib,add_LIB_vector,
                          Powertrain,
                          perc_lib_available,LIB_Available_vector,
                          perc_lib_recycling,LIB_recycling_vector)
          
          # Move available LIBs either to SSPS or recycling
          perc_ssps <- recycling_scenarios %>% 
            filter(recycling_scenario==scen_recyc) %>% 
            pull(ssps_perc)
          
          # for recycling scenarios loop, perc SSPS is 50% by default
          if(length(perc_ssps)==0){
            perc_ssps=0.5
          }
          
          # MAYBE DELETE -FEBRUARY 2025
          # reuse_car <- reuse_car %>%
          #   mutate(perc_lib_ssps=perc_lib_available*perc_ssps,
          # # LIBs that failed and LIBs in good condition that are old
          #          perc_lib_recycling=perc_lib_recycling+
          #            perc_lib_available*(1-perc_ssps)) 
          
          # head(reuse_car) # some columns are vectors with the flow of EVs from age 1 to 30
          
          # do it directly with the function
          reuse_car <- reuse_car %>% 
            rowwise() %>%
            mutate(
              # average AGE for SSPS state of health
              avgAge_LIB_available=weighted_avg_index(LIB_Available_vector),
              # Convert vectors from age to year
              add_LIB_vector = list(shift_X(Year, add_LIB_vector))) %>% 
            mutate(LIB_recycling_vector = list(shift_X(Year, LIB_recycling_vector))) %>% 
            mutate(LIB_Available_vector = list(shift_X(Year, LIB_Available_vector))) %>% 
            ungroup()
          
          # join them and multiply them as vectors
          # VECTOR of Bat size x Vector of Replacement needs through years
          # THIS GIVES ME THE DESIRED KWH_VEH for each chemistry based on the past shares!!!
          bat_ldv_chem <- bat_ldv_chem %>% 
            mutate(join_dummy=1) %>% # to join expanding both dataframes
            left_join(dict_region) %>% # add region
            # head() %>% 
            left_join(mutate(reuse_car,join_dummy=1),
                      relationship = "many-to-many") %>% 
            rowwise() %>% # MULTIPLY VECTORS rowise
            mutate(add_kwh_failure = sum(kwh_veh * add_LIB_vector),
          #         # LIBs that failed and LIBs in good condition that are old
                   kwh_veh_recycling = sum(kwh_veh * LIB_recycling_vector),
                     # sum(kwh_veh * LIB_Available_vector)*(1-perc_ssps),
                  kwh_veh_available = sum(kwh_veh * LIB_Available_vector)) %>% 
                   # lib_kwh_ssps = sum(kwh_veh * LIB_Available_vector)*perc_ssps) %>%  
            ungroup() %>% 
            mutate(add_kwh_failure = add_kwh_failure*perc_add_lib,
                   kwh_veh_recycling = kwh_veh_recycling*perc_lib_recycling+
                     kwh_veh_available*perc_lib_available*(1-perc_ssps),
                   lib_kwh_ssps = kwh_veh_available*perc_lib_available*perc_ssps) %>%
            dplyr::select(-kwh_veh,-LIB_recycling_vector,-LIB_Available_vector,-join_dummy,
                          -add_LIB_vector)
          
          # gives the kwh_veh using total sales for that country
          head(bat_ldv_chem)
          
          # total LIB outflow
          df_car <- df %>% filter(Vehicle=="Car")
          
          lib_outflow_car <- df_car %>% left_join(bat_ldv_chem)
          lib_outflow_car <- lib_outflow_car %>% 
            mutate(lib_additional_kwh=add_kwh_failure*Sales,
                   lib_recycling_kwh=kwh_veh_recycling*Sales,
                   lib_ssps_kwh=lib_kwh_ssps*Sales) %>% 
            dplyr::select(-kwh_veh_recycling,-lib_kwh_ssps,-perc_lib_recycling,
                          -perc_add_lib,-add_kwh_failure,-perc_lib_available)
          
          # rest of vehicles: same size and chemistry during whole period
          reuse_aux <- reuse %>% filter(Scenario==scen) %>% mutate(Scenario=NULL) %>% 
            filter(scen_lifetime==scen_life) %>% 
            mutate(perc_lib_ssps=perc_lib_available*perc_ssps,
                   # LIBs that failed and LIBs in good condition that are old
                   perc_lib_recycling=perc_lib_recycling+
                     perc_lib_available*(1-perc_ssps))
          
          lib_outflow <- df %>% 
            filter(Vehicle!="Car") %>% 
            left_join(dplyr::select(reuse_aux,-Sales))
          # unique(lib_outflow$Powertrain);unique(lib_outflow$Vehicle);
          # sum(is.na(lib_outflow$perc_lib_ssps))
          lib_outflow <- lib_outflow %>% 
            rowwise() %>% # get average age of LIB flow 
            mutate(avgAge_LIB_available=weighted_avg_index(LIB_Available_vector)) %>%
            ungroup() %>% 
            mutate(lib_additional_kwh=Sales*perc_add_lib*kwh_veh,
                   lib_ssps_kwh=Sales*perc_lib_ssps*kwh_veh,
                   lib_recycling_kwh=Sales*perc_lib_recycling*kwh_veh) %>% 
            group_by(Region,Country,Year,chemistry) %>% 
            summarise(avgAge_LIB_available=weighted.mean(avgAge_LIB_available,lib_ssps_kwh),
                      lib_additional_kwh=sum(lib_additional_kwh),
                      lib_ssps_kwh=sum(lib_ssps_kwh),
                      lib_recycling_kwh=sum(lib_recycling_kwh)) %>% ungroup()
          
          # join to cars lib_outflow
          lib_outflow_car$Powertrain <- lib_outflow_car$Vehicle <- NULL
          lib_outflow_car$Sales <- lib_outflow_car$kwh_veh <- NULL
          lib_outflow_car$kwh_veh_available <- NULL
          
          lib_outflow <- rbind(lib_outflow,lib_outflow_car) %>% 
            group_by(Region,Country,Year,chemistry) %>% 
            summarise(avgAge_LIB_available=weighted.mean(avgAge_LIB_available,lib_ssps_kwh),
                      lib_additional_kwh=sum(lib_additional_kwh),
                      lib_ssps_kwh=sum(lib_ssps_kwh),
                      lib_recycling_kwh=sum(lib_recycling_kwh)) %>% ungroup()
          rm(lib_outflow_car)
          
          ## kWH ----
          df <- df %>% 
            mutate(kwh_required=Sales*kwh_veh)
          
          ## add additional LIB kWh demand towards main Dataframe
          df_addLib <- lib_outflow %>% 
            rename(kwh_required=lib_additional_kwh) %>% 
            mutate(Powertrain="BEV",
                   kwh_veh=0,Sales=0, # dummy
                   Vehicle="Additional LIB") %>%  # additional new LIB required
            dplyr::select(-lib_ssps_kwh,-lib_recycling_kwh,-avgAge_LIB_available)
          
          df <- rbind(df,df_addLib)
          rm(df_addLib)
          
          
          ## Stationary power storage -----
          stationary_loop <- stationary %>% 
            rename(kwh_required=stationaryPower) %>% # in Mwh 
            mutate(kwh_required=kwh_required*1e3)
          # sum(stationary_loop$kwh_required)/1e9 # 16
          
          # reduce demand by using LIB outflow
          # Consider loss of State of Health capacity by age
          # reduce total demand of stationary, not by chemistry
          # save shares of chem and total
          stationary_shares <- stationary_loop %>% group_by(Country,Vehicle,Region,Year) %>% 
            mutate(share_chem=kwh_required/sum(kwh_required),
                   kwh_required=NULL) %>% ungroup()
          stationary_total <- stationary_loop %>% group_by(Country,Vehicle,Region,Year) %>% 
            reframe(kwh_required=sum(kwh_required)) %>% ungroup()
          lib_outflow_ssps_total <- lib_outflow %>% 
            group_by(Region,Country,Year) %>% 
            reframe(avgAge_LIB_available=weighted.mean(avgAge_LIB_available,lib_ssps_kwh),
                    lib_ssps_kwh=sum(lib_ssps_kwh)) %>% ungroup()
          
          # substract total
          stationary_total <- stationary_total %>% left_join(lib_outflow_ssps_total) %>% 
            mutate(lib_ssps_kwh=lib_ssps_kwh*(1-statHealth_deg*avgAge_LIB_available)) %>%  # yearly SOH degradation
            mutate(allocate=pmin(kwh_required,lib_ssps_kwh)) %>%  # substract only to 0 max
            mutate(kwh_required=kwh_required-allocate,
                   lib_ssps_kwh=lib_ssps_kwh-allocate)
          
          
          # chem scenario and other scenario are derived through the cars outflow
          # re add chem shares to remaining flows
          stationary_loop <- stationary_shares %>% 
            left_join(dplyr::select(stationary_total,-lib_ssps_kwh,-allocate)) %>% 
            mutate(kwh_required=kwh_required*share_chem,
                   share_chem=NULL, avgAge_LIB_available=NULL)
          # sum(stationary_loop$kwh_required)/1e9 # 11, less than before
          
          
          # update lib outflow, other ssps goes to recycling
          stationary_total$Vehicle <- stationary_total$kwh_required <- stationary_total$lib_ssps_kwh <- NULL
          lib_recycling <- lib_outflow %>% 
            dplyr::select(-avgAge_LIB_available) %>% 
            group_by(Region,Country,Year) %>% 
            mutate(share_chem=lib_ssps_kwh/sum(lib_ssps_kwh)) %>% ungroup() %>% 
            left_join(stationary_total) %>% 
            mutate(allocate = if_else(is.na(allocate), 0, allocate),
                  # re-convert to allocate capacity for recycling
                  allocate=allocate/(1-statHealth_deg*avgAge_LIB_available),
                  allocate=allocate*share_chem,
                  lib_ssps_kwh=lib_ssps_kwh-allocate,
                  lib_ssps_kwh=if_else(is.na(lib_ssps_kwh),0,lib_ssps_kwh)) %>% 
            mutate(lib_recycling_kwh=lib_recycling_kwh+lib_ssps_kwh) %>% 
            dplyr::select(-lib_ssps_kwh,-share_chem,-allocate,-lib_additional_kwh,
                          -avgAge_LIB_available) %>% 
            filter(lib_recycling_kwh>0)
          
          rm(stationary_shares,stationary_total,lib_outflow_ssps_total)
          
          # add dummies to join
          stationary_loop <- stationary_loop %>% 
            mutate(Powertrain="SPS",
                   Sales=0,kwh_veh=0) 
          
          df <- rbind(df,stationary_loop) # 1 M rows for now
          
          
          ## Add Mineral Intensity --------
          
          df <- df %>% filter(kwh_required>0) 
          
          # use same chemistry for now
          df$chemistry %>% unique()
          # df %>% group_by(chemistry) %>% summarise(x=sum(kwh_required)) %>% arrange(x)
          # mineral$chemistry %>% unique()
          
          mineral <- mineral %>% filter(Mineral %in% min_interest3) #include P and Mn
          
          df <- df %>% left_join(mineral,relationship = "many-to-many")
          # nrow(df)/1e6 # 4.9 MILLION
          
          ####################
          # CALCULATIONS ---------
          ####################
          
          ## Minerals ----
          df <- df %>% mutate(tons_mineral=kwh_required*kg_per_kwh/1e3)
          
          # Substract due to recycling - remaining SSPS goes to recycling as well
          lib_recycling <- lib_recycling %>% 
            mutate(Year=Year+delay_recycling_year) %>% 
            filter(Year<2071) %>% 
            left_join(mineral, relationship = "many-to-many") %>% 
            rename(kwh_required=lib_recycling_kwh) %>% 
            left_join(mat_recovery_recyc_aux, relationship = "many-to-many") %>% 
            mutate(tons_mineral=-kwh_required*kg_per_kwh/1e3*mat_recov_recyc,
                   Vehicle="Recycling") %>% 
            filter(abs(tons_mineral)>0)
          # BETTER TO DO IT AS ADDITIONAL VEHICLE: Scenario, and negative
          
          # Cathode scrap
          # consider scrap, and also that scrap could go towards recycling with recovery rate %
          df <- df %>% mutate(tons_mineral=tons_mineral/(1-cathode_scrap)) 
          df_scrap <- df %>% 
            left_join(mat_recovery_recyc_aux) %>% 
            mutate(tons_scrap=tons_mineral*cathode_scrap*mat_recov_recyc, #% recov. rate
                   Year=Year+delay_recycling_year) %>%  # delay in rec
            group_by(Region,Country,Year,chemistry,Mineral) %>% 
            reframe(tons_scrap=sum(tons_scrap,na.rm=T)) %>% ungroup()
          
          # join to recycling 
          lib_recycling <- lib_recycling %>% 
            left_join(df_scrap) %>%
            mutate(tons_scrap=if_else(is.na(tons_scrap),0,tons_scrap)) %>% 
            mutate(tons_mineral=tons_mineral-tons_scrap,tons_scrap=NULL)
          rm(df_scrap)
          
          # join recycling
          names(df)
          names(lib_recycling)
          lib_recycling$kwh_veh <- lib_recycling$Sales <-  0
          lib_recycling$mat_recov_recyc1 <- lib_recycling$mat_recov_recyc <- NULL
          lib_recycling$Powertrain <- "Recycling"
          
          df <- rbind(df,lib_recycling)
          ## Add Other sector demand -------- 
          names(df)
          names(otherSectors)
          
          # add region
          otherSectors_loop <- otherSectors %>% left_join(dict_region) %>% 
            mutate(chemistry="Other Sectors",
                   Vehicle="Other Sectors",kwh_veh=0,kwh_required=0,kg_per_kwh=0,Sales=0)
          df <- rbind(df,otherSectors_loop)
          
        # Save results at Region level
        names(df)
        df_region <- df %>% 
          group_by(Year,Region,Powertrain,Vehicle,chemistry,Mineral) %>% 
          reframe(tons_mineral=sum(tons_mineral)) %>% ungroup()
        nrow(df_region) # 516K records
        
        # add scenarios
        df_region$Scenario <- scen
        df_region$chem_scenario <- scen_chem  
        df_region$capacity_scenario <- scen_bat
        df_region$lifetime_scenario <- scen_life
        df_region$recycling_scenario <- scen_recyc
        
        # to speed processing, remove 0
        df_region <- df_region %>% filter(abs(tons_mineral)>0)
        
        df_region_final <- rbind(df_region_final,df_region)
        
        rm(df_region)
        
        # Save results at Coutry level - SLOW!
        # df_country <- df %>%
        #   group_by(Year,Region,Country,Powertrain,Vehicle,chemistry,Mineral) %>%
        #   reframe(tons_mineral=sum(tons_mineral)) %>% ungroup()
        # nrow(df_country) #
        # # add scenarios
        # df_country$Scenario <- scen
        # df_country$chem_scenario <- scen_chem
        # df_country$capacity_scenario <- scen_bat
        # df_country$lifetime_scenario <- scen_life
        # df_country$recycling_scenario <- scen_recyc
        # # bind
        # df_country <- df_country %>% filter(abs(tons_mineral)>0)
        # df_country_final <- rbind(df_country_final,df_country)
        # rm(df_country)

        }
      }
    }
  }
}
rm(scen,scen_chem,scen_bat)
end_time <- proc.time() # Capture the ending time
print(end_time - start_time) # for 9 loops: 1 minute, 6 segs each
# For 270 Loops: 24 minutes
# For 45 loops: 14 minutes
# 9 Scenarios with country level: 140 seg

nrow(df_region_final)/1e6 # 22.6M rows
nrow(df_country_final)/1e6 # 10.4M rows for 9 scenarios


## Save results -----
# only selected scenarios
df_scen <- df_region_final %>% 
  mutate(scen_all=paste(Scenario,chem_scenario,
                        capacity_scenario,
                        lifetime_scenario,recycling_scenario,sep="-"))
  # filter(scen_all %in% scens_selected)
df_scen$scen_all %>% unique()
write.csv(df_scen,"Results/MineralDemand_FewScenarios.csv",row.names = F)
# recycling loop
# write.csv(df_scen,"Results/MineralDemand_RecyclingLoop.csv",row.names = F)

# all scenarios
# write.csv(df_region_final,"Results/MineralDemandRegion.csv",row.names = F)

df_country_final <- df_country_final %>% 
  mutate(scen_all=paste(Scenario,chem_scenario,capacity_scenario,
                        lifetime_scenario,recycling_scenario,sep="-"))
write.csv(df_country_final,"Results/MineralDemand_FewScenarios_Country.csv",row.names = F)



# write.csv(df,"Results/MineralDemand.csv",row.names = F)


# EoF