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


# NON MONETARY
# Non Monetary Factor - Ease of Doing Bussiness
demand_scen = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
#deposit=depositAll
#runOptimization(demand_scen,depositAll,"EDB",0.1)
# Loop for multiobjective curve
for i in 0:20
    println(i)
    j = i
    runOptimization(demand_scen,depositAll,"EDBCurve/EDBLoop $j",i/100)
end
     
