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
(demand <- read.csv("Parameters/Demand2.csv"))
(deposit <- read.csv("Parameters/Deposit2.csv"))

# debug faster
demand <- demand[1:30,]
deposit <- deposit[1:20,]

(d_size <- nrow(deposit))
(t_size <- nrow(demand))

## Parameters -----
demand <- demand$Demand
cost_extraction <- deposit$cost_extraction
reserve <- deposit$reserve
max_prod_rate <- deposit$max_prod_rate
cost_expansion <- deposit$cost_expansion
resource <- deposit$resource
cost_opening <- deposit$cost_opening
prod_rate <- deposit$prod_rate # starting capacity
max_ramp_up <- max_prod_rate/4

# regarding big M - use threshold of maximum allowable cap for X
# https://www.gurobi.com/documentation/current/refman/dealing_with_big_m_constra.html
# https://orinanobworld.blogspot.com/2018/09/choosing-big-m-values.html
# choose it as max prod rate of the whole sample of deposits, X will be limited by it nevertheless
bigM_extract <- max(max_prod_rate)
bigM_cost <- 1e3 # opportunity cost of not meeting demand?

# discount rate for Costs
discount_rate <- 0.03 
discounter <- (1+discount_rate)^(0:(t_size-1))

cost_extraction <- outer(cost_extraction,1/discounter)
cost_expansion <- outer(cost_expansion,1/discounter)
cost_opening <- outer(cost_opening,1/discounter)


# cost_opening[1] <- 50000
# demand[5] <- demand[5]+1 # for SP check
# max_prod_rate[4] <- max_prod_rate[4]+1


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
                           1.01*cost_extraction[d,t]*x2[d,t] + # stepwise cost
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
  solve_model(with_ROI(solver = "gurobi",verbose=T))
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

## Sniff Test ----
result$objective_value

# Parameter for LP model
openings <- get_solution(result, w[d,t]) %>% dplyr::select(-variable) %>% 
  pivot_wider(names_from = c(t), values_from = value) %>% mutate(d=NULL) %>% 
  as.matrix()

# NO mine opened twice
df_results %>%  group_by(d) %>% 
  reframe(x=sum(mine_opened)) %>% pull(x) %>% range()
# Number of mines opened
sum(df_results$mine_opened)

# Production at last period vs demand. Note there is existing capacity
sum(df_results$capacity_added)+sum(prod_rate)
demand[t_size]
# total extraction vs reserves
sum(df_results$tons_extracted)
sum(df_results$tons_extracted1);sum(df_results$tons_extracted2);
sum(deposit$reserve)
# No production or extraction at unopened mines
closed_d <- df_results %>% group_by(d) %>% 
  reframe(x=sum(mine_opened)) %>% filter(x==0) %>% pull(d)
df_results %>% filter(d %in% closed_d) %>% 
  reframe(tons_extracted=sum(tons_extracted),
          capacity_added=sum(capacity_added))
# max production rate achieved vs limit
df_results %>% reframe(x=max(cap_total));bigM_extract;


## Shadow Prices -----
# row duals are only available for solve glpk: https://github.com/dirkschumacher/ompr/issues/36
# solution: run GLPK LP model with mine open as parameter

# LP model version
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
                           1.01*cost_extraction[d,t]*x2[d,t] + # stepwise cost
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


# compare MIP (gurobi) solution against LP (glpk)
result$objective_value
# a=sum(matrix(get_solution(result, x[d,t])$value,nrow=d_size,byrow = F)*cost_extraction)
# b=sum(matrix(get_solution(result, y[d,t])$value,nrow=d_size,byrow = F)*cost_expansion)
# c=sum(matrix(get_solution(result, w[d,t])$value,nrow=d_size,byrow = F)*cost_opening)
# d=sum(get_solution(result, z[t])$value)*bigM_cost
# a+b+c+d

result_LP$objective_value+sum(cost_opening*openings)
# a1=sum(matrix(get_solution(result_LP, x[d,t])$value,nrow=d_size,byrow = F)*cost_extraction)
# b1=sum(matrix(get_solution(result_LP, y[d,t])$value,nrow=d_size,byrow = F)*cost_expansion)
# c1=sum(openings*cost_opening)
# d1=sum(get_solution(result_LP, z[t])$value)*bigM_cost
# a1+b1+c1+d1

get_solution(result, x[d,t])$value %>% sum()
get_solution(result_LP, x[d,t])$value %>% sum()
get_solution(result, y[d,t])$value %>% sum()
get_solution(result_LP, y[d,t])$value %>% sum()

get_solution(result, x[d,t]) %>% rename(value_orig=value) %>% 
  left_join(get_solution(result_LP, x[d,t])) # EQUAL
get_solution(result, y[d,t]) %>% rename(value_orig=value) %>% 
  left_join(get_solution(result_LP, y[d,t])) # EQUAL


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


## Figures -------------
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))
fig_name <- "Figures/Optimization/%s.png"

## cumulative demand
get_solution(result,x[d,t]) %>% group_by(d) %>% reframe(value=sum(value)) %>% head()
df_results %>% group_by(d) %>% reframe(x=sum(tons_extracted)) %>% head()
head(deposit)

# Production over time
df_results %>% 
  filter(t<2051) %>% 
  # mutate(d=factor(d)) %>% 
  ggplot(aes(t,tons_extracted,fill=d,group=d))+
  geom_area()+
  coord_cartesian(expand = F)+
  labs(x="",y="Production")+
  theme(legend.position = "none",
        axis.text.x = element_text(hjust=1))
f.fig.save(sprintf(fig_name,"production"))

# Open Mines over time
df_results %>% 
  filter(t<2051) %>%
  rename(value=mine_opened) %>% 
  mutate(value=factor(value)) %>% 
  ggplot(aes(t,d,fill=value))+
  geom_tile()+
  coord_cartesian(expand = F)+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("0" = "white", "1" = "red"))+
  labs(x="",y="Deposit",fill="")+
  theme(legend.position = "none")
f.fig.save(sprintf(fig_name,"minesOpen"))

# Capacity over time
max_cap_openMines <- df_results %>% 
  left_join(deposit) %>% mutate(max_prod=mine_open*max_prod_rate) %>% 
  filter(t<2051) %>% group_by(t) %>% reframe(max_prod=sum(max_prod))

df_results %>% 
  filter(t<2051) %>% 
  ggplot(aes(t,cap_total))+
  geom_area(aes(fill=d,group=d))+
  geom_line(data=max_cap_openMines,aes(y=max_prod),linewidth=1,col="red")+
  coord_cartesian(expand = F)+
  labs(x="",y="Production \n Capacity")+
  theme(legend.position = "none",
        axis.text.x = element_text(hjust=1))
f.fig.save(sprintf(fig_name,"prodCapacity"))

# Shadow prices

# demand marginal costs
sp_demand %>% 
  filter(t<2051) %>% 
  ggplot(aes(t,sp_demand))+
  geom_line()+
  labs(x="",y="Shadow Price \n Demand")
f.fig.save(sprintf(fig_name,"SP_demand"))

# note: all should be negative to make sense
sp_rest %>% reframe(sp_maxProdRate=max(sp_maxProdRate),
                    sp_rampUp=max(sp_rampUp),
                    sp_capacity=max(sp_capacity))

# reserve hits
sp_reserve %>% 
  filter(sp_reserve!=0) %>% # remove zeros
  ggplot(aes(reorder(d,sp_reserve),sp_reserve))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(breaks = NULL)+
  labs(x="Deposit",y="Shadow Price Reserves")
f.fig.save(sprintf(fig_name,"SP_reserve"))

# ORDER IS KEY TO GET CORRECT SHADOW PRICES FROM THE MATRIX/VECTOR
p1 <- sp_rest %>% 
  filter(year<2051) %>% 
  ggplot(aes(year,d,fill=sp_maxProdRate))+
  geom_tile()+
  coord_cartesian(expand = F)+
  scale_fill_gradientn(colors = c("red", "white"))+
  scale_y_continuous(breaks = NULL)+
  labs(x="",y="Deposit",fill="Shadow Price \nMax prod. rate")
# why positive in some cases? Why having more capacity in a mine increases my costs?
p1
f.fig.save(sprintf(fig_name,"SP_MaxProdRate"))
p1+aes(fill=sp_capacity)+labs(fill="Shadow Price \nCapacity")
f.fig.save(sprintf(fig_name,"SP_Capacity"))
p1+aes(fill=sp_rampUp)+labs(fill="Shadow Price \nRamp up")
f.fig.save(sprintf(fig_name,"SP_RampUp"))
# p1+aes(fill=sp_openMine)+labs(fill="Shadow Price \nOpen mine")



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