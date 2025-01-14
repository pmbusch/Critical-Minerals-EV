# Run Optimization Model with demand and deposits paramters created previously
# Calls a user-defined function to run an optimization model
# Has loops to run all the desired scenarios.
# PBH March 2024

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Load built-in optimization function
# other potential path: Scripts/Supply Model/Julia Optimization/
include("RunOptimization.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/Demand.csv"))


# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for r in [0.03, 0.07,0.1, 0.15,0.25] # discount rate
        #runOptimization(demand_scen,depositAll,"DiscountRate/r$r/$scen",0.1,r) 
        # hyperbolic
        runOptimization(demand_scen,depositAll,"DiscountRate/Hyperbolic r$r/$scen",0.1,r,100000*5.323/1e3,true) 
    end
end

