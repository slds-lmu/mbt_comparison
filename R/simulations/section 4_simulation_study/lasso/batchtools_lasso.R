source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/section_4_simulation_study/lasso")) dir.create("Data/simulations/section_4_simulation_study/lasso", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/section_4_simulation_study/lasso/batchtools",
                             source = c("R/simulations/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/simulations/mob_fitting_functions.R",
                                        "R/simulations/helper_simulations.R",
                                        "R/simulations/section_4_simulation_study/lasso/helper_simulations_lasso.R"),
                             seed = 1
                             , conf.file = NA
)


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "noisy_data", fun = create_sim_data, reg = reg, seed = 100)
pdes = list("noisy_data" = data.frame(n = c(3000), type = c("linear_smooth_lasso")))


# add algorithm
source("R/simulations/section_4_simulation_study/lasso/helper_simulations_lasso.R")

addAlgorithm(name = "get_sim_results_lasso", fun = get_sim_results_lasso)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 250)

summarizeExperiments()
testJob(1)


submitJobs()
