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

    
# SCENARIOS Deposis
# do it for all demand scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    
    # No clay deposits
    deposit_clay = filter(row -> row.Resource_Type != "Volcano-Sedimentary", depositAll) 
    runOptimization(demand_scen,deposit_clay,"Scenarios_Deposit/$scen/No Clay",0.1)

    # read all excel in the deposit folder (new inputs)
    folder_path = "Parameters/Deposit_scenarios"
    excel_files = readdir(folder_path)
    for file in excel_files
        if endswith(file, ".csv")
            file_path = joinpath(folder_path, file)
            deposit_s = DataFrame(CSV.File(file_path))
            file_name = splitext(file)[1] # remove CSV
            println("Data from $(file_name):")
            runOptimization(demand_scen,deposit_s,"Scenarios_Deposit/$scen/$file_name",0.1)
        end
    end
end


# End of File