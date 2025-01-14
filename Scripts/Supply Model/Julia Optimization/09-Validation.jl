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


# Validation and Calibration with historical datadepositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandValidation = DataFrame(CSV.File("Parameters/Validation/demand.csv"))
depositValidation = DataFrame(CSV.File("Parameters/Validation/deposit.csv"))

#runOptimization(demandValidation,depositValidation,"Validation/EDBLoop 50",0.5,false)

# Loop for different weights of EDB factor
for i in 0:20
    println(i)
    j = i
    runOptimization(demandValidation,depositValidation,"Validation/EDBLoop $j",i/100)
end
