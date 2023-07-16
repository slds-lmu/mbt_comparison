# Interpretation of black box models using model-based tree models
This repository contains code, data and figures for the paper "Interpretation of black box models using model-based trees"

    ├── R/                                                       
    |   ├── tree_splitting_slim.R               # Function to generate slim tree            
    |   ├── load_packages.R                     # load all necessary packages      
    |   ├── helper_general.R                    # helper functions for tree splitting and for the evaluation of slim trees    
    |   ├── helper_guide.R                      # guide specific helper functions for tree splitting    
    |   ├── simulations/                         
    |   |   ├── section_4_simulation_study/     # Files for generation and analysis of batchtools experiments for simulation study (section 4 and Appendix)   
    ├── Data/simulations/                                    
    │   |   ├── section_4_simulation_study/     # Location where generated data of batchtools experiments for simulation study are stored
    ├── Figures/
    │   |   ├── simulations/         
    │   |   |   ├── section_4_simulation_study/ # Location where figures for simulation study are stored    
    └── README.md 
    
To reproduce the simulation studies:
1. Install all required packages: ```install.packages(c("R6", "BBmisc", "stringr", "tidyverse", "rlist", "mgcv", "mvtnorm", "data.table",
                   "dplyr", "ggplot2", "ggpubr", "batchtools", "glmnet", "quantreg", "splines",
                   "partitions", "partykit", "checkmate", "mlr3", "mlr3learners", "mlr3measures",
                   "mlr3pipelines", "mlr3tuning", "caTools", "igraph", "fossil", "kableExtra", "GGally"))```
2. Generate and run batchtools experiments with the files "batchtools_....R" in the simulations folder.
3. Run the files "analyse_....R" in the simulations folder to aggregate the data and create the figures shown in the paper.


