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


# N-1 COUNTRIES
# Loop through Deposit removing countries N-1
# Lopp for demand scenarios as well
countries = ["Chile","Bolivia","Argentina","Canada","United States","Australia","Tanzania"]
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for con in countries
        println(con)
        deposit_c = filter(row -> row.Country != con, depositAll) # N-1, remove that country
        runOptimization(demand_scen,deposit_c,"N1_Countries_Demand/$scen/$con/",0.1)
    end
    # Remove lithium triangle
    deposit_c = filter(row -> row.Country != "Chile", depositAll) # N-1, remove that country
    deposit_c = filter(row -> row.Country != "Bolivia", deposit_c) # N-1, remove that country
    deposit_c = filter(row -> row.Country != "Argentina", deposit_c) # N-1, remove that country
    runOptimization(demand_scen,deposit_c,"N1_Countries_Demand/$scen/Lithium Triangle",0.1)
end