using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/Demand.csv"))

# Function to convert binary variables into fixed parameters in a model
function fix!(model::Model)
    solution = Dict(
        v => value(v) for v in all_variables(model)
    )
    for v in all_variables(model)
        if is_binary(v)
            unset_binary(v)
            fix(v, solution[v]; force = true)
        end
    end
    return
end

# Optimization Run Function
# Inputs:
# - Demand 
# - Deposits parameters
# - Save folder 
# - degradationLimit: Set to zero for single objective optimization with shadow prices
function runOptimization(demand,deposit,saveFolder, degradationLimit,sp)
    
    d_size = size(deposit, 1) 
    t_size = size(demand, 1)
    
    # Extract necessary columns
    # Demand in ktons
    demand = demand[!, :Demand]
    # Name
    deposit_name = deposit[!, :Deposit_Name]
    # Reserves
    reserve = deposit[!, :reserve]
    resource_demostrated = deposit[!, :resource_demostrated]
    resource_inferred = deposit[!, :resource_inferred]
    # Dynamics
    prod_rate2022 = deposit[!, :prod_rate2022]
    prod_rate2023 = deposit[!, :prod_rate2023]
    prod_rate2025 = deposit[!, :prod_rate2025]
    prod_rate2030 = deposit[!, :prod_rate2030]
    
    max_prod_rate = deposit[!, :max_prod_rate]
    max_ramp_up = deposit[!, :max_ramp_up]
    min_prod_rate = max_prod_rate ./ 4 
    # Costs, all in million USD
    cost_extraction1 = deposit[!, :cost1] ./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton
    cost_extraction2 = deposit[!, :cost2] ./ 1e3
    cost_extraction3 = deposit[!, :cost3] ./ 1e3
    cost_expansion = deposit[!, :cost_expansion] ./ 1e3 # same as extraction
    cost_opening = deposit[!, :cost_opening] ./ 1e6 # million usd
    # Non Monetary Factors
    edb = deposit[!, :edb] # 0 to 100, 0 is better and 100 is worse


    # Set big M values
    bigM_extract = maximum(max_prod_rate)
    # historic high: 68000 USD per LCE
    #bigM_cost = 1e6
    bigM_cost = 100000*5.323/1e3 
    

    
    # Planned capacity over time, towards 2030
    prod_rate = zeros(d_size, t_size)

    # Fill the matrix for the specific years
    prod_rate[:, 1] .= prod_rate2022          # Year 2022
    prod_rate[:, 2] .= prod_rate2023          # Year 2023
    prod_rate[:, 4] .= prod_rate2025          # Year 2025
    prod_rate[:, 9:end] .= prod_rate2030      # Years 2030 to 2070

    # Linear interpolation for 2024 (index 3)
    prod_rate[:, 3] .= (prod_rate2023 .+ prod_rate2025) ./ 2

    # Linear interpolation for 2026 to 2029 (indices 5 to 8)
    for i in 1:4
        t = i / 5
        prod_rate[:, 4 + i] .= (1 - t) .* prod_rate2025 .+ t .* prod_rate2030
    end

    # Discount rates for costs
    discount_rate = 0.07
    #discount_rate = 0.03
    #discount_rate = 0.001
    discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)
    #discounter2 = (1.001) .^(0:size(demand, 1) - 1) # for extraction costs

    cost_extraction1 = cost_extraction1 .* (1 ./ discounter')
    cost_extraction2 = cost_extraction2 .* (1 ./ discounter')
    cost_extraction3 = cost_extraction3 .* (1 ./ discounter')
    cost_opening = cost_opening .* (1 ./ discounter')
    cost_expansion = cost_expansion .* (1 ./ discounter')
    # Avoid expansion of certain mines with no info
    cost_expansion[:,1:3] .= 1e6 # no expansion until 2025
    status = deposit[!, :Status_Delay]     
    for i in 1:size(cost_expansion, 1)

        if status[i] == "Construction"
            cost_expansion[i, 1:5] .= 1e6 # Not possible to expand, until 2027   
        elseif status[i] == "Evaluation"
            cost_expansion[i, 1:8] .= 1e6 # Not possible to expand by cost, until 2030
        elseif status[i] == "No Info"
            cost_expansion[i, 1:10] .= 1e6 # until 2032
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
        #@objective(model,Min, sum(edb[d]*sum(x[d,t] for t in 1:t_size) for d in 1:d_size))
        
        # Constraints
        # Extraction less than available production capacity
        @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] + x2[d, t] +x3[d,t] <= sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t])
        # Met demand
        @constraint(model, c2[t in 1:t_size], sum(x[d, t] + x2[d, t] +x3[d,t] for d in 1:d_size) + z[t] >= demand[t])
        # Max depletion of reserves, 3 stages
        @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= reserve[d])
        @constraint(model, c4[d in 1:d_size], sum(x2[d, t] for t in 1:t_size) <= resource_demostrated[d])
        @constraint(model, c5[d in 1:d_size], sum(x3[d, t] for t in 1:t_size) <= resource_inferred[d])        
        # Max production rate only on open mines
        @constraint(model, c6[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
        # Open mine only once
        @constraint(model, c7[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)
        # Max Ramp up
        @constraint(model, c8[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])
        # Limit EDB Score average of open deposits
        #@constraint(model, c9, sum(sum(w[d, t] for t in 1:t_size)*edb[d] for d in 1:d_size) <= 5*sum(w[d,t] for d in 1:d_size, t in 1:t_size))
       # Min prod rate
        #@constraint(model, c10[d in 1:d_size, t in 10:t_size],  sum(w[d, t1] for t1 in 1:t) * min_prod_rate[d] <=  sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t]) 
        # Solve the optimization problem
        
        #optimize!(model)        

        if degradationLimit>0
            # Second objective: Non monetary Factors
            # see: https://github.com/jump-dev/Gurobi.jl/issues/294
            # https://github.com/jump-dev/Gurobi.jl/pull/295        
            
            # set slack variable# Get the values of z after solving the first objective
            # Fix the values of z for the subsequent objectives
         #   z_values = value.(z)
          #  for i in 1:t_size
           #     fix(z[i], z_values[i]; force=true)
            #end

            MOI.set(model,Gurobi.NumberOfObjectives(),2)  # Multiobjective
            # Priority
            MOI.set(model, Gurobi.MultiObjectivePriority(1), 10)
            MOI.set(model, Gurobi.MultiObjectivePriority(2), 5)
            #MOI.set(model, Gurobi.MultiObjectiveWeight(1), 1.0)
            #MOI.set(model, Gurobi.MultiObjectiveWeight(2), 0.1)       
            # minimize the sum of the score of EBD index openings of new deposits
            #f2 = @expression(model, sum(edb[d]*sum(x[d,t]+x2[d,t]+x3[d,t] for t in 1:t_size) for d in 1:d_size))
            # Based on expansion capacity
            f2 = @expression(model, sum(edb[d]*sum(y[d,t] for t in 1:t_size) for d in 1:d_size))
            MOI.set(model, Gurobi.MultiObjectiveFunction(2), moi_function(f2))
            # Set the relative tolerance for the second objective
            # EDB is optimized within 10% of the solution based solely on costs
            MOI.set(model,Gurobi.MultiObjectiveAttribute(1,"ObjNRelTol"),degradationLimit) # index start at 0, annoying behavior
        end
        optimize!(model)
    end
    
   
    # get and store results
    x_values = [value(x[d, t]) for d in 1:d_size, t in 1:t_size]
    x2_values = [value(x2[d, t]) for d in 1:d_size, t in 1:t_size]
    x3_values = [value(x3[d, t]) for d in 1:d_size, t in 1:t_size]
    y_values = [value(y[d, t]) for d in 1:d_size, t in 1:t_size]
    w_values = [value(w[d, t]) for d in 1:d_size, t in 1:t_size]
    z_values = [value(z[ t]) for t in 1:t_size]
    
    # mine open cumulative - to indicate if the mine is open in that period
    mine_open = mapslices(cumsum, w_values, dims=2)
    
    # df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
    df_results = DataFrame(d = repeat(deposit_name, outer=t_size),t = repeat(2022:(t_size+2021), inner=length(deposit_name)),
    tons_extracted1 = vec(x_values),
    tons_extracted2 = vec(x2_values),
    tons_extracted3 = vec(x3_values),
    capacity_added = vec(y_values),
    mine_opened = vec(w_values))
    
    df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
    
    # save results
    # create directory if not there
    if !isdir("Results/Optimization/"* saveFolder)
        mkpath("Results/Optimization/"* saveFolder)
    end
    url_file = "Results/Optimization/"* saveFolder *"/Base_Julia.csv"
    CSV.write(url_file, df_results)
    url_file = "Results/Optimization/"* saveFolder *"/Slack_Julia.csv"
    CSV.write(url_file, df_z)

    
    if degradationLimit>0
         # Save final values
         final_obj_values = [objective_value(model)[i] for i in 1:2]
         url_file = "Results/Optimization/"* saveFolder *"/OptimalValue.txt"
         open(url_file, "w") do file
             for (i, val) in enumerate(final_obj_values)
                 write(file, "Objective $i: $val\n")
             end
         end 
    end

    
    if degradationLimit==0
         # Save final values
        final_obj_values = [objective_value(model)[i] for i in 1:1]
        url_file = "Results/Optimization/"* saveFolder *"/OptimalValue.txt"
        open(url_file, "w") do file
            for (i, val) in enumerate(final_obj_values)
                write(file, "Objective $i: $val\n")
            end
        end
        # DUALS
        # https://jump.dev/JuMP.jl/stable/tutorials/linear/mip_duality/
        if sp==true
            discrete_values = value.(w)
            fix.(w, discrete_values; force = true)
            unset_binary.(w)
            optimize!(model)
        end

               # OTHER OPTION
        # Retrieve dual values by fixing binary variables in the model
        # https://discourse.julialang.org/t/solving-fixed-milp-problem/51749/4

        #fix!(model)
        #optimize!(model)
    end

    
    # fix 2nd objective as constraint
    if degradationLimit>0 && sp

        # Linear model to get DUALS - OLD
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
            # TEST TO INCLUDE OPENINGS w_values[d,t] IN EVERY CONSTRAINT
            @constraint(model_LP, c1_LP[d in 1:d_size, t in 1:t_size], x_lp[d, t] + x2_lp[d, t] +x3_lp[d,t] <= sum(y_lp[d, t1]*mine_open[d,t] for t1 in 1:t) + prod_rate[d,t])
            # Met demand
            @constraint(model_LP, c2_LP[t in 1:t_size], sum((x_lp[d, t] + x2_lp[d, t] +x3_lp[d,t])*mine_open[d,t] for d in 1:d_size) + z_lp[t] >= demand[t])
            # Max depletion of reserves, 3 stages
            @constraint(model_LP, c3_LP[d in 1:d_size], sum(x_lp[d, t] for t in 1:t_size) <= reserve[d])
            @constraint(model_LP, c4_LP[d in 1:d_size], sum(x2_lp[d, t] for t in 1:t_size) <= resource_demostrated[d])
            @constraint(model_LP, c5_LP[d in 1:d_size], sum(x3_lp[d, t] for t in 1:t_size) <= resource_inferred[d])
            # Max production rate only on open mines
            @constraint(model_LP, c6_LP[d in 1:d_size, t in 1:t_size], sum(y_lp[d, t1] for t1 in 1:t) + prod_rate[d,t] <= sum(w_values[d, t1] for t1 in 1:t) * max_prod_rate[d])
            # Max Ramp up
            @constraint(model_LP, c8_LP[d in 1:d_size, t in 1:t_size], y_lp[d, t] <= max_ramp_up[d])
            # Second objective as
            limit_edb =  objective_value(model)[2]
            @constraint(model_LP, c9_LP, sum(edb[d]*sum(y_lp[d,t] for t in 1:t_size) for d in 1:d_size) <= limit_edb+0.1)
            
            # Solve the new linear optimization problem
            optimize!(model_LP)
        end
    end
    if sp    
        x_values = [value(x_lp[d, t]) for d in 1:d_size, t in 1:t_size]
        x2_values = [value(x2_lp[d, t]) for d in 1:d_size, t in 1:t_size]
        x3_values = [value(x3_lp[d, t]) for d in 1:d_size, t in 1:t_size]
        y_values = [value(y_lp[d, t]) for d in 1:d_size, t in 1:t_size]
        z_values = [value(z_lp[ t]) for t in 1:t_size]

        #df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
        df_results = DataFrame(d = repeat(deposit_name, outer=t_size),t = repeat(2022:(t_size+2021), inner=length(deposit_name)),
        tons_extracted1 = vec(x_values),
        tons_extracted2 = vec(x2_values),
        tons_extracted3 = vec(x3_values),
        capacity_added = vec(y_values))
        
        df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
        

        final_obj_values = [objective_value(model_LP)[i] for i in 1:1]
        url_file = "Results/Optimization/"* saveFolder *"/OptimalValue_LP.txt"
        open(url_file, "w") do file
            for (i, val) in enumerate(final_obj_values)
                write(file, "Objective $i: $val\n")
            end
        end

        # save results
        url_file = "Results/Optimization/"* saveFolder *"/Base_Julia_LP.csv"
        CSV.write(url_file, df_results)
        url_file = "Results/Optimization/"* saveFolder *"/Slack_Julia_LP.csv"
        CSV.write(url_file, df_z)
        
        # Save shadow prices
        c1_values = [dual(c1_LP[d, t]) for d in 1:d_size, t in 1:t_size]
        c2_values = [dual(c2_LP[t]) for t in 1:t_size]
        c3_values = [dual(c3_LP[d]) for d in 1:d_size]
        c4_values = [dual(c4_LP[d]) for d in 1:d_size]
        c5_values = [dual(c5_LP[d]) for d in 1:d_size]
        c6_values = [dual(c6_LP[d, t]) for d in 1:d_size, t in 1:t_size]
        c8_values = [dual(c8_LP[d, t]) for d in 1:d_size, t in 1:t_size]
        
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
end

# Loops, comment/uncomment to run

# Single Run - DEBUG
demandBase = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
#runOptimization(demandBase,depositAll,"Base",0.1,false)

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    #runOptimization(demand_scen,depositAll,"DemandScenario/$scen",0.1,true)
end

# N-1 COUNTRIES
# Loop through Deposit removing countries N-1
# Lopp for demand scenarios as well
countries = ["Chile","Bolivia","Argentina","Canada","United States","Australia","DR Congo","Tanzania"]
scen2 = "Ambitious-Baseline-Baseline-Baseline-Baseline" # Reference scenario
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for con in countries
        println(con)
        deposit_c = filter(row -> row.Country != con, depositAll) # N-1, remove that country
        #runOptimization(demand_scen,deposit_c,"N1_Countries_Demand/$scen/$con/",0.1,false)
    end
    # Remove lithium triangle
    deposit_c = filter(row -> row.Country != "Chile", depositAll) # N-1, remove that country
    deposit_c = filter(row -> row.Country != "Bolivia", deposit_c) # N-1, remove that country
    deposit_c = filter(row -> row.Country != "Argentina", deposit_c) # N-1, remove that country
    #runOptimization(demand_scen,deposit_c,"N1_Countries_Demand/$scen/Lithium Triangle",0.1,false)
end

    
# SCENARIOS Deposis
# do it for all demand scenarios
unique_scenarios = unique(demandAll.Scenario)
#unique_scenarios = ["Ambitious-Baseline-Baseline-Baseline-Baseline"]
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    
    # No clay deposits
    deposit_clay = filter(row -> row.Resource_Type != "Volcano-Sedimentary", depositAll) 
    runOptimization(demand_scen,deposit_clay,"Scenarios_Deposit/$scen/No Clay",0.1,false)

    # read all excel in the deposit folder
    folder_path = "Parameters/Deposit_scenarios"
    excel_files = readdir(folder_path)
    for file in excel_files
        if endswith(file, ".csv")
            file_path = joinpath(folder_path, file)
            deposit_s = DataFrame(CSV.File(file_path))
            file_name = splitext(file)[1] # remove CSV
            println("Data from $(file_name):")
            runOptimization(demand_scen,deposit_s,"Scenarios_Deposit/$scen/$file_name",0.1,false)
        end
    end
end


# NON MONETARY
# Non Monetary Factor - Ease of Doing Bussiness
demand_scen = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
#deposit=depositAll
#runOptimization(demand_scen,depositAll,"EDB",0.1)
# Loop for multiobjective curve
for i in 1:10
    println(i)
    runOptimization(demand_scen,depositAll,"EDBCurve/EDBLoop $i",i/10,true)
end
            
# End of File