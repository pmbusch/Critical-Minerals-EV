
# Function to duplicate scenarios results when aggregating (group_by) results from long table
# Key idea: Store results with no Scenario as "No Scenario", then replicate them for results
# that are scenario dependent. This saves a lot of storgage
# Tidyverse pipeline %>% friendly. 
# list_scen: vector with scenario names to duplicate (populate)
f.duplicateScenarios <- function(df,list_scen,
                                 scenario_col = "Scenario",dummy_scen="No Scenario"){
  
  col_position <- which(colnames(df) == scenario_col) 
  colnames(df)[col_position] <- "scen_aux"
  j=1  
  # iteration to create duplicate rows of the scenario called No Scenario in the resulting df
  for (i in list_scen){
    if(j==length(list_scen)){
      df <- df %>% 
        mutate(scen_aux=if_else(scen_aux!=dummy_scen,scen_aux,i))
    } else {
      # last row is not duplicated, simply we change the column scenario name
      df <- df %>% 
        bind_rows(.,mutate(filter(.,scen_aux==dummy_scen),scen_aux=i))
    }
    j=j+1
  }
  
  colnames(df)[col_position] <- scenario_col
  return(df)
}
