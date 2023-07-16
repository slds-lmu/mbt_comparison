source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/section_4_simulation_study/nonlinear")) dir.create("Data/simulations/section_4_simulation_study/nonlinear", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/section_4_simulation_study/nonlinear/batchtools",
                             source = c("R/simulations/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/simulations/mob_fitting_functions.R",
                                        "R/simulations/section_4_simulation_study/nonlinear/helper_simulations_nonlinear.R"),
                             seed = 111,
                             conf.file = NA
)



# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "nonlinear_data", fun = create_sim_data, reg = reg, seed = 222)
pdes = list("nonlinear_data" = data.frame(n = c(4500), type = c("nonlinear_mixed")))


# add algorithm
source("R/simulations/section_4_simulation_study/nonlinear/helper_simulations_nonlinear.R")

addAlgorithm(name = "get_sim_results_nonlinear", fun = get_sim_results_nonlinear)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = list("get_sim_results_nonlinear" = data.frame(r2 = c(1,0.95,0.9), impr.par = c(0.1, 0.05, 0.05))), 
  repls = 50)

summarizeExperiments()

testJob(1)

submitJobs(resources = list(walltime = 9000))