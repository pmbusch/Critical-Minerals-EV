using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load data
demand = DataFrame(CSV.File("Parameters/Demand.csv"))
# Filter scenario
demand = filter(row -> row.Scenario == "Baseline-Baseline-Baseline", demand)
deposit = DataFrame(CSV.File("Parameters/Deposit.csv"))

d_size = size(deposit, 1) 
t_size = size(demand, 1)

# Extract necessary columns
demand = demand[!, :Demand]
cost_extraction = deposit[!, :cost_extraction] ./ 1e3
reserve = deposit[!, :reserve]
max_prod_rate = deposit[!, :max_prod_rate]
cost_expansion = deposit[!, :cost_expansion] ./ 1e3
resource = deposit[!, :resource]
cost_opening = deposit[!, :cost_opening] ./ 1e3
prod_rate = deposit[!, :prod_rate]
max_ramp_up = deposit[!, :max_ramp_up]

# Set big M values
bigM_extract = maximum(max_prod_rate)
# bigM_cost = 1e6
# historic high: 68000 USD per LCE
bigM_cost = 100000*5.323



# Discount rates for costs
discount_rate = 0.03
discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)

cost_extraction = cost_extraction .* (1 ./ discounter')
cost_expansion = cost_expansion .* (1 ./ discounter')
cost_opening = cost_opening .* (1 ./ discounter')

# Big M effect, should be reduced towards the future?
bigM_cost = bigM_cost .* (1 ./ discounter') 

# Factor for costs
cost_factor = 1.5


@time begin
    # Create optimization model
    model = Model(Gurobi.Optimizer)


    # Decision variables
    @variable(model, x[1:d_size, 1:t_size] >= 0)  # Extraction
    @variable(model, x2[1:d_size, 1:t_size] >= 0)  # Stepwise cost
    @variable(model, y[1:d_size, 1:t_size] >= 0)  # Additional capacity
    @variable(model, w[1:d_size, 1:t_size], Bin)  # Open or not
    @variable(model, z[1:t_size] >= 0)  # Slack to match balance

    # Objective function
    @objective(model, Min, sum(cost_extraction[d, t] * x[d, t] +
                                cost_factor * cost_extraction[d, t] * x2[d, t] +  # Stepwise cost
                                cost_expansion[d, t] * y[d, t] +
                                cost_opening[d, t] * w[d, t] for d in 1:d_size, t in 1:t_size) +
                        sum(bigM_cost[t] * z[t] for t in 1:t_size))

    # Constraints
    @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] + x2[d, t] <= sum(y[d, t1] for t1 in 1:t) + prod_rate[d])
    @constraint(model, c2[t in 1:t_size], sum(x[d, t] + x2[d, t] for d in 1:d_size) + z[t] >= demand[t])
    @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= reserve[d] / 2)
    @constraint(model, c4[d in 1:d_size], sum(x2[d, t] for t in 1:t_size) <= reserve[d] / 2)
    @constraint(model, c5[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
    @constraint(model, c6[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)
    @constraint(model, c7[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])

    # Solve the optimization problem
    optimize!(model)
end

# get and store results
x_values = [value(x[d, t]) for d in 1:d_size, t in 1:t_size]
x2_values = [value(x2[d, t]) for d in 1:d_size, t in 1:t_size]
y_values = [value(y[d, t]) for d in 1:d_size, t in 1:t_size]
w_values = [value(w[d, t]) for d in 1:d_size, t in 1:t_size]
z_values = [value(z[ t]) for t in 1:t_size]

df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
                        tons_extracted1 = vec(x_values),
                        tons_extracted2 = vec(x2_values),
                        capacity_added = vec(y_values),
                        mine_opened = vec(w_values))

df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))

# save results
url_file = "Results/Optimization/Base_Julia.csv"
CSV.write(url_file, df_results)
url_file = "Results/Optimization/Slack_Julia.csv"
CSV.write(url_file, df_z)

# Linear model to get DUALS
@time begin
    # Create a new optimization model
    model_LP = Model(Gurobi.Optimizer)

    # Define variables and parameters
    @variable(model_LP, x_lp[1:d_size, 1:t_size] >= 0)  # Extraction
    @variable(model_LP, x2_lp[1:d_size, 1:t_size] >= 0)  # Stepwise cost
    @variable(model_LP, y_lp[1:d_size, 1:t_size] >= 0)  # Additional capacity
    @variable(model_LP, z_lp[1:t_size] >= 0)  # Slack to match balance

    @objective(model_LP, Min, sum(cost_extraction[d, t] * x_lp[d, t] +
                                cost_factor * cost_extraction[d, t] * x2_lp[d, t] +  # Stepwise cost
                                cost_expansion[d, t] * y_lp[d, t] for d in 1:d_size, t in 1:t_size) +
                        sum(bigM_cost[t] * z_lp[t] for t in 1:t_size))

    # Substitute w_values into the constraints
    @constraint(model_LP, c1[d in 1:d_size, t in 1:t_size], x_lp[d, t] + x2_lp[d, t] <= sum(y_lp[d, t1] for t1 in 1:t) + prod_rate[d])
    @constraint(model_LP, c2[t in 1:t_size], sum(x_lp[d, t] + x2_lp[d, t] for d in 1:d_size) + z_lp[t] >= demand[t])
    @constraint(model_LP, c3[d in 1:d_size], sum(x_lp[d, t] for t in 1:t_size) <= reserve[d] / 2)
    @constraint(model_LP, c4[d in 1:d_size], sum(x2_lp[d, t] for t in 1:t_size) <= reserve[d] / 2)
    @constraint(model_LP, c5[d in 1:d_size, t in 1:t_size], sum(y_lp[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w_values[d, t1] for t1 in 1:t) * max_prod_rate[d])
    @constraint(model_LP, c7[d in 1:d_size, t in 1:t_size], y_lp[d, t] <= max_ramp_up[d])

    # Solve the new linear optimization problem
    optimize!(model_LP)
end


# get and store results
x_values = [value(x_lp[d, t]) for d in 1:d_size, t in 1:t_size]
x2_values = [value(x2_lp[d, t]) for d in 1:d_size, t in 1:t_size]
y_values = [value(y_lp[d, t]) for d in 1:d_size, t in 1:t_size]
z_values = [value(z_lp[ t]) for t in 1:t_size]

df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
                        tons_extracted1 = vec(x_values),
                        tons_extracted2 = vec(x2_values),
                        capacity_added = vec(y_values))

df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))

# save results
url_file = "Results/Optimization/Base_Julia_LP.csv"
CSV.write(url_file, df_results)
url_file = "Results/Optimization/Slack_Julia_LP.csv"
CSV.write(url_file, df_z)

# Save shadow prices
c1_values = [dual(c1[d, t]) for d in 1:d_size, t in 1:t_size]
c2_values = [dual(c2[t]) for t in 1:t_size]
c3_values = [dual(c3[d]) for d in 1:d_size]
c4_values = [dual(c4[d]) for d in 1:d_size]
c5_values = [dual(c5[d, t]) for d in 1:d_size, t in 1:t_size]
c7_values = [dual(c7[d, t]) for d in 1:d_size, t in 1:t_size]

# save as shadow prices
sp_demand = DataFrame(variable="sp_demand",t = 2022:(t_size+2021),value = vec(c2_values))
CSV.write("Results/Optimization/Julia_sp_demand.csv", sp_demand)

sp_reserve = DataFrame(d = 1:d_size,
                        sp_reserve = vec(c3_values),
                        sp_reserve2 = vec(c4_values))
CSV.write("Results/Optimization/Julia_sp_reserve.csv", sp_reserve)

sp_rest = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
                        sp_capacity = vec(c1_values),
                        sp_maxProdRate = vec(c5_values),
                        sp_rampUp = vec(c7_values))
                        CSV.write("Results/Optimization/Julia_sp_rest.csv", sp_rest)

# End of File