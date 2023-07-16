source("R/simulations/mob_fitting_functions.R")
source("R/simulations/helper_simulations.R")

get_sim_results_lasso = function(data, job, instance, 
                                n.quantiles = 100, min.split = 100, maxdepth = 6, 
                                approximate = FALSE, pruning = "forward", impr.par = 0.1, alpha = 0.001,
                                df.max = 2:3, ... ){
  
  
  data = instance$data
  
  
  # -- standalone model 
  
  # train test split
  split_point = nrow(data)/3*2
  train = data[1:split_point,]
  test = data[(split_point+1):nrow(data),]
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  y_train = train$y
  y_test = test$y
  
  # fit trees to the original data
  result_original = fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                              min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                              pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                              exclude.categoricals = FALSE, correct.bias = TRUE, 
                              data_stability = NULL, 
                              extract_variables = TRUE,  
                              tree_methods = c("slim", "mob", "ctree", "guide", "slim_lasso", "slim_lasso_max_df"),
                              df.max = df.max)
  result_original = cbind(surrogate = "standalone", result_original)
  
  # -- surrogate 1 (correctly specified linear model) 
  
  # train linear model (blackbox model 1)
  fm = instance$fm
  lm = gam(fm, data = train)
  
  # extract fitted values (surrogates)
  y_hat_train_lm = lm$fitted.values
  y_hat_test_lm = predict(lm, x_test)
  
  # calculate performance of the lm model (as benchmark for the accuracy of the MBT models)
  mse_train_lm = mean((y_train - y_hat_train_lm)^2)
  r2_train_lm = r_2(y_train, y_hat_train_lm)
  mse_test_lm = mean((y_test - y_hat_test_lm)^2)
  r2_test_lm = r_2(y_test, y_hat_test_lm)
  
  # fit trees to the lm predictions 
  result_surrogate_lm  = fit_trees(x_train = x_train, y_train = y_hat_train_lm, x_test = x_test, y_test = y_hat_test_lm,  
                                   min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                                   pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                                   exclude.categoricals = FALSE, correct.bias = TRUE, 
                                   data_stability = NULL, 
                                   extract_variables = TRUE,  
                                   tree_methods = c("slim", "mob", "ctree", "guide", "slim_lasso", "slim_lasso_max_df"),
                                   df.max = df.max)
  result_surrogate_lm = rbind(result_surrogate_lm, c(mbt = "lm", n_leaves = NA, depth = NA, max_leaf_size = NA,
                                                     min_leaf_size = NA, sd_leaf_size = NA, n_splitting_variables = NA,
                                                     mse_train = mse_train_lm, r2_train = r2_train_lm, 
                                                     mse_test = mse_test_lm, r2_test = r2_test_lm,
                                                     x_wrong = FALSE, share_x1 = NA, share_x2 = NA, share_x3 = NA, share_x4 = NA))
  

  result_surrogate_lm = cbind(surrogate = "lm", result_surrogate_lm)
  res = rbind(result_original, result_surrogate_lm)
  
  return(res)
  
}

