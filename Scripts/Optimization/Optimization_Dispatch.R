# Optimization sSupply DIspatch
# PBH February 2024

# local installation
# install.packages("C:/gurobi1100/win64/R/gurobi_11.0-0.zip", repos = NULL)
# install.packages("slam", repos = "https://cloud.r-project.org")


## STEPS
# STORE FEASIBLE SOLUTION FOR STARTING POINT TO TAKE LESS IN SCENARIO ANALYSIS
# Parameters for Gurobi: https://www.gurobi.com/documentation/9.1/refman/parameters.html#sec:Parameters
# Start: https://support.gurobi.com/hc/en-us/articles/360043834831-How-do-I-use-MIP-starts

# remotes::install_github("r-opt/rmpk")
# https://github.com/r-opt/rmpk

# remotes::install_github("r-opt/ROIoptimizer")
# https://github.com/r-opt/ROIoptimizer

# Some references:
# https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html

library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)
library(ROI)
library(ompr)
library(ompr.roi)
library(rmpk)

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Main Model ----

url_file <- "Results/Optimization/%s.csv"
case <- "Base"

# load data
# 2022 to 2026 and 5 mines use demand 0
(demand <- read.csv("Parameters/Demand_Test1.csv"))
(deposit <- read.csv("Parameters/Deposit_Test1.csv"))

# demand <- read.csv("Parameters/Demand.csv")
# deposit <- read.csv("Parameters/Deposit.csv")

# demand <- demand %>% filter(Scenario=="Baseline-Baseline-Baseline")

# debug faster
# demand <- demand[1:10,]
# deposit <- deposit[1:30,]


(d_size <- nrow(deposit))
(t_size <- nrow(demand))

## Parameters -----
demand <- round(demand$Demand,0)
cost_extraction <- round(deposit$cost_extraction/1e3,4)
reserve <- round(deposit$reserve,0)
max_prod_rate <- round(deposit$max_prod_rate,2)
cost_expansion <- round(deposit$cost_expansion/1e3,4)
resource <- round(deposit$resource,0)
cost_opening <- round(deposit$cost_opening/1e3,4)
prod_rate <- round(deposit$prod_rate,4) # starting capacity
max_ramp_up <- round(deposit$max_ramp_up,2)


## CORRECTION FOR NOW, MAX PROD RATE BGGER THAN CURRENT PROD
max_prod_rate <- if_else(max_prod_rate>prod_rate,max_prod_rate,prod_rate)
# cost_opening[,1]*prod_rate # cost 0 for all already opened mines

# regarding big M - use threshold of maximum allowable cap for X
# https://www.gurobi.com/documentation/current/refman/dealing_with_big_m_constra.html
# https://orinanobworld.blogspot.com/2018/09/choosing-big-m-values.html
# choose it as max prod rate of the whole sample of deposits, X will be limited by it nevertheless
bigM_extract <- max(max_prod_rate)
# Could be also time dependent, higher in later years to force the model to comply demand at first
bigM_cost <- 1e6 # opportunity cost of not meeting demand?


# grow reserves to 50% of resources by end of simulation
# slope_reserve <- (resource/2-reserve)/t_size
# slope_reserve <- ifelse(slope_reserve<0,0,slope_reserve)
# slope_reserve <- outer(slope_reserve,0:(t_size-1))
# reserve <- reserve+slope_reserve # is a matrix now

# grow all reserves to resources 80% resources, for NOW
reserve <- ifelse(reserve>resource*0.8,reserve,resource*0.8)


# discount rate for Costs
discount_rate <- 0.03 
discounter <- (1+discount_rate)^(0:(t_size-1))

cost_extraction <- outer(cost_extraction,1/discounter)
cost_expansion <- outer(cost_expansion,1/discounter)
cost_opening <- outer(cost_opening,1/discounter)

# Factor 2-half of costs
cost_factor <- 1.5 # increase price


# Last solution as start point
start <- read.csv(sprintf(url_file,case))


# write.csv(slack,sprintf(url_file,paste0(case,"_slack"))


## Run Solver -----
start_time <- proc.time() # Capture the starting time
result <- MIPModel()  %>% 
  # DECISION VARIABLES
  add_variable(x[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Extraction
  add_variable(x2[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Stepwise cost
  add_variable(y[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Additional capacity
  add_variable(w[d,t], type = "binary", lb = 0,d=1:d_size,t=1:t_size)  %>% # Open or not
  add_variable(z[t], type = "continuous", lb = 0,t=1:t_size) %>% # Slack to match balance
  # BOUNDS
  set_bounds(x[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(x2[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(y[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(w[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(z[t], lb = 0,t=1:t_size) %>%
  # OBJECTIVE FUNCTION
  set_objective(sum_over(cost_extraction[d,t]*x[d,t] +
                           cost_factor*cost_extraction[d,t]*x2[d,t] + # stepwise cost
                           cost_expansion[d,t]*y[d,t] +
                           cost_opening[d,t]*w[d,t],d=1:d_size,t=1:t_size)+
                  sum_over(bigM_cost*z[t],t=1:t_size), "min")  %>% 
  # CONSTRAINT
  # add_constraint(x[d,t] <= sum_over(w[d,t1]*bigM_extract,t1=1:t), d=1:d_size,t=1:t_size) %>% # open mine
  add_constraint(x[d,t]+x2[d,t] <= sum_over(y[d,t1],t1=1:t)+prod_rate[d], d=1:d_size,t=1:t_size) %>% # production less than  capacity built
  add_constraint(sum_over(x[d,t]+x2[d,t],d=1:d_size)+z[t] >= demand[t],t=1:t_size) %>% # Meet demand
  # 50-50% for eachs stepwise cost
  add_constraint(sum_over(x[d,t],t=1:t_size) <= reserve[d]/2,d=1:d_size)  %>% # Reserves are not depleted
  add_constraint(sum_over(x2[d,t],t=1:t_size) <= reserve[d]/2,d=1:d_size)  %>%
  # add_constraint(sum_over(y[d,t1],t1=1:t)+prod_rate[d] <= max_prod_rate[d], d=1:d_size,t=1:t_size) %>% # max production rate
  # CHANGE TO REVIEW: ONLY ADD CAPACITY TO OPEN MINES
  add_constraint(sum_over(y[d,t1],t1=1:t)+prod_rate[d] <= sum_over(w[d,t1],t1=1:t)*max_prod_rate[d], d=1:d_size,t=1:t_size) %>% # max production rate
  add_constraint(sum_over(w[d,t],t=1:t_size) <= 1, d=1:d_size) %>% # open mine only once
  add_constraint(y[d,t] <= max_ramp_up[d], d=1:d_size,t=1:t_size) %>% # ramp limit
  # SOLVER
  solve_model(with_ROI(solver = "gurobi",verbose=T,
                       MIPGap=0.001)) # MIPGap to run faster
end_time <- proc.time() # Capture the ending time
print(end_time - start_time) # 0.08seg for 5x5 problem, 4 seg for 30.8, 58 seg for 30x104, 3min for 60x104 (full problem)


# Consolidate results
df_results <- rbind(get_solution(result, x[d,t]),get_solution(result, x2[d,t]),
                    get_solution(result, y[d,t]),
                     get_solution(result, w[d,t])) %>% 
  # rbind(mutate(get_solution(result, z[t]),d=999)) %>% # no deposit for Slack
  mutate(var=case_when(
    variable=="x"~"tons_extracted1",
    variable=="x2"~"tons_extracted2",
    variable=="y"~"capacity_added",
    variable=="w"~"mine_opened",
    variable=="z"~"demand_unmet"))

slack <- get_solution(result, z[t])

# get total capacity and mine opening
df_results <- df_results %>% 
  mutate(t=t+2021) %>% dplyr::select(-variable) %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  mutate(tons_extracted=tons_extracted1+tons_extracted2) %>% 
  left_join(tibble(d=1:d_size,prod_rate)) %>% 
  group_by(d) %>% mutate(cap_total=cumsum(capacity_added)+prod_rate,
                         mine_open=cumsum(mine_opened)) %>% ungroup()


## save results -----
write.csv(df_results,sprintf(url_file,case),row.names = F)
write.csv(slack,sprintf(url_file,paste0(case,"_slack")),row.names = F)

# Parameter for LP model
openings <- get_solution(result, w[d,t]) %>% dplyr::select(-variable) %>% 
  pivot_wider(names_from = c(t), values_from = value) %>% mutate(d=NULL) %>% 
  as.matrix()

## Shadow Prices -----
# row duals are only available for solve glpk: https://github.com/dirkschumacher/ompr/issues/36
# solution: run GLPK LP model with mine open as parameter

# LP model version
start_time <- proc.time()
result_LP <- MIPModel()  %>% 
  # DECISION VARIABLES
  add_variable(x[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Extraction
  add_variable(x2[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Stepwise cost
  add_variable(y[d,t], type = "continuous", lb = 0,d=1:d_size,t=1:t_size)  %>% # Additional capacity
  add_variable(z[t], type = "continuous", lb = 0,t=1:t_size) %>% # Slack to match balance
  # BOUNDS
  set_bounds(x[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(x2[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(y[d,t], lb = 0, d=1:d_size,t=1:t_size) %>%
  set_bounds(z[t], lb = 0,t=1:t_size) %>%
  # OBJECTIVE FUNCTION
  set_objective(sum_over(cost_extraction[d,t]*x[d,t] +
                           cost_factor*cost_extraction[d,t]*x2[d,t] + # stepwise cost
                           cost_expansion[d,t]*y[d,t],d=1:d_size,t=1:t_size)+
                  sum_over(bigM_cost*z[t],t=1:t_size), "min")  %>% 
  # CONSTRAINT
  # add_constraint(x[d,t] <= sum_over(w[d,t1]*bigM_extract,t1=1:t), d=1:d_size,t=1:t_size) %>% # open mine
  add_constraint(x[d,t]+x2[d,t] <= sum_over(y[d,t1],t1=1:t)+prod_rate[d], d=1:d_size,t=1:t_size) %>% # production less than  capacity built
  add_constraint(sum_over(x[d,t]+x2[d,t],d=1:d_size)+z[t] >= demand[t],t=1:t_size) %>% # Meet demand
  # 50-50% for eachs stepwise cost
  add_constraint(sum_over(x[d,t],t=1:t_size) <= reserve[d]/2,d=1:d_size)  %>% # Reserves are not depleted
  add_constraint(sum_over(x2[d,t],t=1:t_size) <= reserve[d]/2,d=1:d_size)  %>% 
  # add_constraint(sum_over(y[d,t1],t1=1:t)+prod_rate[d] <= max_prod_rate[d], d=1:d_size,t=1:t_size) %>% # max production rate
  # CHANGE TO REVIEW: ONLY ADD CAPACITY TO OPEN MINES
  add_constraint(sum_over(y[d,t1],t1=1:t)+prod_rate[d] <= sum_over(openings[d,t1],t1=1:t)*max_prod_rate[d], d=1:d_size,t=1:t_size) %>% # max production rate
  add_constraint(y[d,t] <= max_ramp_up[d], d=1:d_size,t=1:t_size) %>% # ramp limit
  # SOLVER
  solve_model(with_ROI(solver = "glpk",verbose=T))
end_time <- proc.time() 
print(end_time - start_time) 


# vector for all the shadow prices for constraints, in order
shadow_price <- get_row_duals(result_LP)
dim_combined <- d_size*t_size
length(shadow_price)
dim_combined*4+d_size+t_size
# add and remove from the vector
# matrix are deposit (row) vs year (column)
# sp_openMine <- shadow_price[1:dim_combined];shadow_price=shadow_price[-(1:dim_combined)]; 
sp_capacity <- shadow_price[1:dim_combined];shadow_price=shadow_price[-(1:dim_combined)]; 
sp_demand <- shadow_price[1:t_size];shadow_price=shadow_price[-(1:t_size)];
sp_reserve <- shadow_price[1:d_size];shadow_price=shadow_price[-(1:d_size)];
sp_reserve2 <- shadow_price[1:d_size];shadow_price=shadow_price[-(1:d_size)];
sp_maxProdRate <- shadow_price[1:dim_combined];shadow_price=shadow_price[-(1:dim_combined)];
sp_rampUp <- shadow_price[1:dim_combined];shadow_price=shadow_price[-(1:dim_combined)];

# consolidate shadow prices
sp_demand <- tibble(t=2022:(2021+t_size),sp_demand)
write.csv(sp_demand,sprintf(url_file,paste0(case,"_SP_demand")),row.names = F)
sp_reserve <- tibble(d=1:d_size,sp_reserve)
write.csv(sp_reserve,sprintf(url_file,paste0(case,"_SP_reserve")),row.names = F)
sp_reserve2 <- tibble(d=1:d_size,sp_reserve2)
write.csv(sp_reserve2,sprintf(url_file,paste0(case,"_SP_reserve2")),row.names = F)

# join rest
# ORDER IS KEY TO GET CORRECT SHADOW PRICES FROM THE MATRIX/VECTOR
sp_rest <- expand.grid(d=1:d_size,year=2022:(2021+t_size)) %>% arrange(d) %>% 
  cbind(sp_capacity) %>% 
  cbind(sp_maxProdRate) %>%
  cbind(sp_rampUp)
  # cbind(sp_openMine)

write.csv(sp_rest,sprintf(url_file,paste0(case,"_SP_capacity")),row.names = F)

# Report Markdown ---------

# Figures are in report
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))
fig_name <- "Figures/Optimization/%s.png"
save_figures <- F # change to T or F

rmarkdown::render("Scripts/Optimization/Run_Report.Rmd",
                  output_file = "Report.pdf")


get_solution(result,x[d,t]) %>% group_by(d) %>% reframe(value=sum(value)) %>% head()
df_results %>% group_by(d) %>% reframe(x=sum(tons_extracted)) %>% head()
head(deposit)

## cumulative demand



# Quadratic Example -------

# new library, same author

model <- optimization_model(ROI_optimizer("gurobi"))
x <- model$add_variable("x", type = "continuous", d = 1:d_size,t=1:t_size)
y <- model$add_variable("y", type = "continuous", d = 1:d_size,t=1:t_size)
w <- model$add_variable("w", type = "binary", d = 1:d_size,t=1:t_size)
z <- model$add_variable("z", type = "continuous", t=1:t_size)
model$set_bounds(x[d,t], lb = 0, d=1:d_size,t=1:t_size)
model$set_bounds(y[d,t], lb = 0, d=1:d_size,t=1:t_size)
# model$set_bounds(w[d,t], lb = 0, d=1:d_size,t=1:t_size)
model$set_bounds(z[t], lb = 0,t=1:t_size)
model$set_objective(sum_expr(cost_extraction[d,t]*x[d,t] + 
                               cost_expansion[d,t]*y[d,t] +
                               cost_opening[d,t]*w[d,t],d=1:d_size,t=1:t_size)+
                      sum_expr(10*sum_expr(x[d,t],d=1:d_size)^2,t=1:t_size)+ #Quad. term
                      sum_expr(bigM_cost*z[t],t=1:t_size),
                    sense = "min")
model$add_constraint(x[d,t] <= sum_expr(y[d,t1],t1=1:t)+prod_rate[d], d=1:d_size,t=1:t_size)
model$add_constraint(sum_expr(x[d,t],d=1:d_size)+z[t] >= demand[t],t=1:t_size) # Meet demand
model$add_constraint(sum_expr(x[d,t],t=1:t_size) <= reserve[d],d=1:d_size) # Reserves are not depleted
model$add_constraint(sum_expr(y[d,t1],t1=1:t)+prod_rate[d] <= sum_expr(w[d,t1],t1=1:t)*max_prod_rate[d], d=1:d_size,t=1:t_size) # max production rate
model$add_constraint(sum_expr(w[d,t],t=1:t_size) <= 1, d=1:d_size) # open mine only once
model$add_constraint(y[d,t] <= max_ramp_up[d], d=1:d_size,t=1:t_size) # ramp limit
model$optimize()
# same results as OMPR package - WHEN NO QUADRATIC IS INCLUDED
model$get_variable_value(x[d,t]) %>% arrange(t,d) %>%  head()
get_solution(result, x[d,t]) %>% head()

model$objective_value()
result$objective_value




# PLAYGROUND SOLVERS -----------

## GUROBI -------------

library(gurobi)

# create optimization problem
model <- list()
model$obj        <- c(1, 1, 2)
model$modelsense <- "max"
model$rhs        <- c(4, 1)
model$sense      <- c("<", ">")
model$vtype      <- "B"
model$A          <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3,
                           byrow = TRUE)

# solve the optimization problem using Gurobi
result <- gurobi(model, list())
print(result$objval) # objective
print(result$x)      # decision variables



## OMPR ------------
# https://dirkschumacher.github.io/ompr/index.html

# remotes::install_github("dirkschumacher/ompr")
# remotes::install_github("dirkschumacher/ompr.roi")
# remotes:::install_github("roigrp/ROI.plugin.gurobi")
# install.packages("ROI.plugin.glpk")


library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)
library(ompr)
library(ompr.roi)

result <- MIPModel() |>
  add_variable(x, type = "integer") |>
  add_variable(y, type = "continuous", lb = 0) |>
  set_bounds(x, lb = 0) |>
  set_objective(x + y, "max") |>
  add_constraint(x + y <= 11.25) |>
  solve_model(with_ROI(solver = "gurobi"))
get_solution(result, x)
get_solution(result, y)


## DUALS --------
# Playground check

result1 <- MIPModel() %>%
  add_variable(x[i], i = 1:5) %>%
  add_variable(y[i, j], i = 1:5, j = 1:5) %>%
  add_constraint(x[i] >= 1, i = 1:5) %>%
  add_constraint(y[i,j] >= 3, i = 1:3,j=3:5) %>%
  set_objective(sum_over(x[i], i = 1:5)+sum_over(sum_over(y[i,j],i=1:5)*3,j=1:5),"min") %>%
  solve_model(with_ROI("glpk"))
result1$objective_value
get_solution(result1,x[i])
get_solution(result1,y[i,j])

get_row_duals(result1) # 17


get_solution(result1,x[i],type = "dual")
get_solution(result1,y[i,j],type = "dual")


get_column_duals(result1)


# EoF