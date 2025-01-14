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

# RECYCLING LOOP DEMAND SCENARIOS
# Extract unique scenarios
demandRec = DataFrame(CSV.File("Parameters/DemandRecyclingLoop.csv"))
unique_scenarios = unique(demandRec.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandRec)
    runOptimization(demand_scen,depositAll,"DemandRecyclingLoop/$scen",0.1)
end