using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/Demand.csv"))

# Optimization Run Function
# Inputs:
# - Demand 
# - Save folder 
function runOptimization(demand,deposit,saveFolder)
    
    d_size = size(deposit, 1) 
    t_size = size(demand, 1)
    
    # Extract necessary columns
    # Demand in ktons
    demand = demand[!, :Demand]
    # Reserves
    reserve = deposit[!, :reserve]
    resource_demostrated = deposit[!, :resource_demostrated]
    resource_inferred = deposit[!, :resource_inferred]
    # Dynamics
    prod_rate = deposit[!, :prod_rate]
    max_prod_rate = deposit[!, :max_prod_rate]
    max_ramp_up = deposit[!, :max_ramp_up]
    # Costs, all in million USD
    cost_extraction1 = deposit[!, :cost1] ./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton
    cost_extraction2 = deposit[!, :cost2] ./ 1e3
    cost_extraction3 = deposit[!, :cost3] ./ 1e3
    cost_expansion = deposit[!, :cost_expansion] ./ 1e3 # same as extraction
    cost_opening = deposit[!, :cost_opening] ./ 1e6 # million usd
    
    # Set big M values
    bigM_extract = maximum(max_prod_rate)
    # bigM_cost = 1e6
    # historic high: 68000 USD per LCE
    bigM_cost = 100000*5.323/1e3 
    
    
    
    # Discount rates for costs
    discount_rate = 0.07
    discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)
    
    cost_extraction1 = cost_extraction1 .* (1 ./ discounter')
    cost_extraction2 = cost_extraction2 .* (1 ./ discounter')
    cost_extraction3 = cost_extraction3 .* (1 ./ discounter')
    cost_opening = cost_opening .* (1 ./ discounter')
    cost_expansion = cost_expansion .* (1 ./ discounter')
    # Avoid expansion of certain mines with no info
    status = deposit[!, :Status_Delay]     
    for i in 1:size(cost_expansion, 1)
        if status[i] == "Evaluation"
            cost_expansion[i, 1:6] .= 1e6 # Not possible to expand by cost, until 2028
        elseif status[i] == "No Info"
            cost_expansion[i, 1:8] .= 1e6
        end
    end
    
    # Big M effect, should be reduced towards the future?
    bigM_cost = bigM_cost .* (1 ./ discounter') 
        
    
    @time begin
        # Create optimization model
        model = Model(Gurobi.Optimizer)
        
        
        # Decision variables
        @variable(model, x[1:d_size, 1:t_size] >= 0)  # Extraction
        @variable(model, x2[1:d_size, 1:t_size] >= 0)  # Stepwise cost
        @variable(model, x3[1:d_size, 1:t_size] >= 0) 
        @variable(model, y[1:d_size, 1:t_size] >= 0)  # Additional capacity
        @variable(model, w[1:d_size, 1:t_size], Bin)  # Open or not
        @variable(model, z[1:t_size] >= 0)  # Slack to match balance
        
        # Objective function
        @objective(model, Min, sum(cost_extraction1[d, t] * x[d, t] +
        cost_extraction2[d, t] * x2[d, t] +  # Stepwise cost
        cost_extraction3[d, t] * x3[d, t] + 
        cost_expansion[d, t] * y[d, t] +
        cost_opening[d, t] * w[d, t] for d in 1:d_size, t in 1:t_size) +
        sum(bigM_cost[t] * z[t] for t in 1:t_size))
        
        # Constraints
        # Extraction less than available production capacity
        @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] + x2[d, t] +x3[d,t] <= sum(y[d, t1] for t1 in 1:t) + prod_rate[d])
        # Met demand
        @constraint(model, c2[t in 1:t_size], sum(x[d, t] + x2[d, t] +x3[d,t] for d in 1:d_size) + z[t] >= demand[t])
        # Max depletion of reserves, 3 stages
        @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= reserve[d])
        @constraint(model, c4[d in 1:d_size], sum(x2[d, t] for t in 1:t_size) <= resource_demostrated[d])
        @constraint(model, c5[d in 1:d_size], sum(x3[d, t] for t in 1:t_size) <= resource_inferred[d])        
        # Max production rate only on open mines
        @constraint(model, c6[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
        # Open mine only once
        @constraint(model, c7[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)
        # Max Ramp up
        @constraint(model, c8[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])
        
        # Solve the optimization problem
        optimize!(model)
    end
    
    
    # get and store results
    x_values = [value(x[d, t]) for d in 1:d_size, t in 1:t_size]
    x2_values = [value(x2[d, t]) for d in 1:d_size, t in 1:t_size]
    x3_values = [value(x3[d, t]) for d in 1:d_size, t in 1:t_size]
    y_values = [value(y[d, t]) for d in 1:d_size, t in 1:t_size]
    w_values = [value(w[d, t]) for d in 1:d_size, t in 1:t_size]
    z_values = [value(z[ t]) for t in 1:t_size]
    
    df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
    tons_extracted1 = vec(x_values),
    tons_extracted2 = vec(x2_values),
    tons_extracted3 = vec(x3_values),
    capacity_added = vec(y_values),
    mine_opened = vec(w_values))
    
    df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
    
    # save results
    # create directory if not there
    if !isdir("Results/Optimization/"* saveFolder)
        mkdir("Results/Optimization/"* saveFolder)
    end
    url_file = "Results/Optimization/"* saveFolder *"/Base_Julia.csv"
    CSV.write(url_file, df_results)
    url_file = "Results/Optimization/"* saveFolder *"/Slack_Julia.csv"
    CSV.write(url_file, df_z)
    
    # Linear model to get DUALS
    @time begin
        # Create a new optimization model
        model_LP = Model(Gurobi.Optimizer)
        
        # Define variables and parameters
        @variable(model_LP, x_lp[1:d_size, 1:t_size] >= 0)  # Extraction
        @variable(model_LP, x2_lp[1:d_size, 1:t_size] >= 0)  # Stepwise cost
        @variable(model_LP, x3_lp[1:d_size, 1:t_size] >= 0)
        @variable(model_LP, y_lp[1:d_size, 1:t_size] >= 0)  # Additional capacity
        @variable(model_LP, z_lp[1:t_size] >= 0)  # Slack to match balance
        
        @objective(model_LP, Min, sum(cost_extraction1[d, t] * x_lp[d, t] +
        cost_extraction2[d, t] * x2_lp[d, t] +  # Stepwise cost
        cost_extraction3[d, t] * x3_lp[d, t] + 
        cost_expansion[d, t] * y_lp[d, t] for d in 1:d_size, t in 1:t_size) +
        sum(bigM_cost[t] * z_lp[t] for t in 1:t_size))
        
        # Substitute w_values into the constraints
        # Extraction less than available production capacity
        @constraint(model_LP, c1[d in 1:d_size, t in 1:t_size], x_lp[d, t] + x2_lp[d, t] +x3_lp[d,t] <= sum(y_lp[d, t1] for t1 in 1:t) + prod_rate[d])
        # Met demand
        @constraint(model_LP, c2[t in 1:t_size], sum(x_lp[d, t] + x2_lp[d, t] +x3_lp[d,t] for d in 1:d_size) + z_lp[t] >= demand[t])
        # Max depletion of reserves, 3 stages
        @constraint(model_LP, c3[d in 1:d_size], sum(x_lp[d, t] for t in 1:t_size) <= reserve[d])
        @constraint(model_LP, c4[d in 1:d_size], sum(x2_lp[d, t] for t in 1:t_size) <= resource_demostrated[d])
        @constraint(model_LP, c5[d in 1:d_size], sum(x3_lp[d, t] for t in 1:t_size) <= resource_inferred[d])
        # Max production rate only on open mines
        @constraint(model_LP, c6[d in 1:d_size, t in 1:t_size], sum(y_lp[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w_values[d, t1] for t1 in 1:t) * max_prod_rate[d])
        # Max Ramp up
        @constraint(model_LP, c8[d in 1:d_size, t in 1:t_size], y_lp[d, t] <= max_ramp_up[d])
        
        # Solve the new linear optimization problem
        optimize!(model_LP)
    end
    
    
    # get and store results
    x_values = [value(x_lp[d, t]) for d in 1:d_size, t in 1:t_size]
    x2_values = [value(x2_lp[d, t]) for d in 1:d_size, t in 1:t_size]
    x3_values = [value(x3_lp[d, t]) for d in 1:d_size, t in 1:t_size]
    y_values = [value(y_lp[d, t]) for d in 1:d_size, t in 1:t_size]
    z_values = [value(z_lp[ t]) for t in 1:t_size]
    
    df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
    tons_extracted1 = vec(x_values),
    tons_extracted2 = vec(x2_values),
    tons_extracted3 = vec(x3_values),
    capacity_added = vec(y_values))
    
    df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
    
    # save results
    url_file = "Results/Optimization/"* saveFolder *"/Base_Julia_LP.csv"
    CSV.write(url_file, df_results)
    url_file = "Results/Optimization/"* saveFolder *"/Slack_Julia_LP.csv"
    CSV.write(url_file, df_z)
    
    # Save shadow prices
    c1_values = [dual(c1[d, t]) for d in 1:d_size, t in 1:t_size]
    c2_values = [dual(c2[t]) for t in 1:t_size]
    c3_values = [dual(c3[d]) for d in 1:d_size]
    c4_values = [dual(c4[d]) for d in 1:d_size]
    c5_values = [dual(c5[d]) for d in 1:d_size]
    c6_values = [dual(c6[d, t]) for d in 1:d_size, t in 1:t_size]
    c8_values = [dual(c8[d, t]) for d in 1:d_size, t in 1:t_size]
    
    # save as shadow prices
    sp_demand = DataFrame(variable="sp_demand",t = 2022:(t_size+2021),value = vec(c2_values))
    CSV.write("Results/Optimization/"* saveFolder *"/Julia_sp_demand.csv", sp_demand)
    
    sp_reserve = DataFrame(d = 1:d_size,
    sp_reserve = vec(c3_values),
    sp_resource_demostrated = vec(c4_values),
    sp_resource_inferred = vec(c5_values))
    CSV.write("Results/Optimization/"* saveFolder *"/Julia_sp_reserve.csv", sp_reserve)
    
    sp_rest = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
    sp_capacity = vec(c1_values),
    sp_maxProdRate = vec(c6_values),
    sp_rampUp = vec(c8_values))
    CSV.write("Results/Optimization/"* saveFolder *"/Julia_sp_rest.csv", sp_rest)
    
end



# Loop through all Demand Scenarios
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
# unique_scenarios = ["Ambitious-Baseline-Baseline-Baseline-Baseline"] # to debug

for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    runOptimization(demand_scen,depositAll,scen)
end

# Loop through Deposit removing countries
countries = ["Chile","Bolivia","Argentina","Canada","United States","Australia","DR Congo","Tanzania"]
scen2 = "Ambitious-Baseline-Baseline-Baseline-Baseline" # Reference scenario
for con in countries
    println(con)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen2, demandAll)
    deposit_c = filter(row -> row.Country != con, depositAll) # N-1, remove that country
    runOptimization(demand_scen,deposit_c,con)
end


# End of File