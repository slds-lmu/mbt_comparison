source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/section_4_simulation_study/basic_scenarios")) dir.create("Data/simulations/section_4_simulation_study/basic_scenarios", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/section_4_simulation_study/basic_scenarios/batchtools",
                             source = c("R/simulations/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/simulations/mob_fitting_functions.R",
                                        "R/simulations/helper_simulations.R",
                                        "R/simulations/section_4_simulation_study/basic_scenarios/helper_simulations_basic_scenarios.R"),
                             seed = 1, conf.file = NA)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions
repls = 100L
set.seed(49)
data_stability = lapply(c(linear_smooth = "linear_smooth", linear_abrupt = "linear_abrupt", linear_mixed = "linear_mixed"),
                        function(t){
                          create_sim_data(job = NULL, n = 50000, type = t)$data
                        })


addProblem(name = "basic_scenarios", data = data_stability, fun = create_sim_data, reg = reg, seed = 123)
pdes = list("basic_scenarios" = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed")))


# add algorithm
source("R/simulations/section_4_simulation_study/basic_scenarios/helper_simulations_basic_scenarios.R")

addAlgorithm(name = "get_sim_results", fun = get_sim_results)
ades = list(get_sim_results = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05)))



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = repls)

summarizeExperiments()
testJob(1)


submitJobs()



