# Run Optimization Model with demand and deposits paramters created previously
# Has loops to run all the desired scenarios.
# PBH November 2024

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load built-in optimization function
# other potential path: Scripts/Supply Model/Julia Optimization/
include("Nickel_Optimization.jl")

# Load data
depositAll = DataFrame(CSV.File("Nickel/Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Nickel/Parameters/Demand.csv"))



# Loops, comment/uncomment to run

# Single Run - DEBUG
demandBase = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
#deposittest = DataFrame(CSV.File("Parameters/Deposit_New.csv"))
runOptimization(demandBase,depositAll,"Base",0.1)

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    #runOptimization(demand_scen,depositAll,"DemandScenario/$scen",0.1) # UNCOMMENT TO RUN LOOP
end

# End of File