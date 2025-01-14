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
deposit_wgi = DataFrame(CSV.File("Parameters/Deposit.csv"))
deposit_wgi.edb = deposit_wgi.wgi
deposit_wcr = DataFrame(CSV.File("Parameters/Deposit.csv"))
deposit_wcr.edb = deposit_wcr.wcr

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    # No Non-Monetary factors, only cost
    runOptimization(demand_scen,depositAll,"NonMonetary/OnlyCost/$scen",0)
    # Different weights on EDB
    for i in [0.01, 0.03,0.05, 0.1,0.15]
        runOptimization(demand_scen,depositAll,"NonMonetary/EDB Weight $i/$scen",i)
        # WGI - Political Stability 
        runOptimization(demand_scen,deposit_wgi,"NonMonetary/WGI Weight $i/$scen",i)
        # WCR - World Competitiveness Ranking
        runOptimization(demand_scen,deposit_wcr,"NonMonetary/WCR Weight $i/$scen",i)
    end
    
end

