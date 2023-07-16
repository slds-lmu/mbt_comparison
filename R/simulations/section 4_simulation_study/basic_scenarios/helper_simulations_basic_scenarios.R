source("R/simulations/mob_fitting_functions.R")
source("R/simulations/helper_simulations.R")

get_sim_results = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = 100,
                           exclude.categoricals = FALSE, min.split = 50, maxdepth = 7, correct.bias = TRUE, approximate = FALSE,
                           pruning = "forward", impr.par = 0.1, alpha = 0.05, ... ){
  # The data used to compare the stability of the trees are identical across all replicates!
  data_stability = data[[as.character(job$prob.pars$type)]]
  
  # The data used to train the trees and evaluate their performance is re-simulated with each repetition.
  data = instance$data
  
  
  # -- standalone model 
  
  # deterministic train test split (to avoid variability in the algorithm)
  
  split_point = nrow(data)/3*2
  train = data[1:split_point,]
  test = data[(split_point+1):nrow(data),]
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  y_train = train$y
  y_test = test$y

  # fit trees to the original data (i.e. use MBTs as standalone ML model)
  result_original = fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                              min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                              pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                              exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                              tree_methods = tree_methods, data_stability = data_stability)
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
                                   exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                                   tree_methods = tree_methods, data_stability = data_stability)
  
  result_surrogate_lm = rbind(result_surrogate_lm, c(mbt = "lm", n_leaves = list(NA), 
                                                     depth = list(NA), max_leaf_size = list(NA), 
                                                     min_leaf_size = list(NA),
                                                     sd_leaf_size = list(NA),
                                                     n_splitting_variables = list(NA),
                                                     mse_train = list(mse_train_lm), r2_train = list(r2_train_lm), 
                                                     mse_test = list(mse_test_lm), r2_test = list(r2_test_lm),
                                                     share_x1 = list(NA), share_x2 = list(NA), share_x3 = list(NA), share_x4 = list(NA),
                                                     stability = list(NA)))
  result_surrogate_lm = cbind(surrogate = "lm", result_surrogate_lm)
  
  
  # -- surrogate 2 (xgboost model) 
  # train xgboost model (blackbox model 2)
  lrn = instance$lrn
  task_train = as_task_regr(x = train, target = "y")
  task_test = as_task_regr(x = test, target = "y")
  lrn$train(task_train)
  pred_xgboost_train = lrn$predict(task_train)
  pred_xgboost_test = lrn$predict(task_test)
  
  mse_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.mse")))
  r2_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.rsq")))
  mse_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.mse")))
  r2_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.rsq")))
  
  
  y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
  y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
  
  result_surrogate_xgboost  = fit_trees(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost,  
                                        min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                                        pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                                        exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                                        tree_methods = tree_methods, data_stability = data_stability)
  
  result_surrogate_xgboost = rbind(result_surrogate_xgboost, c(mbt = "xgboost", n_leaves = list(NA), 
                                                               depth = list(NA), max_leaf_size = list(NA), 
                                                               min_leaf_size = list(NA), sd_leaf_size = list(NA),
                                                               n_splitting_variables = list(NA), 
                                                               mse_train = list(mse_train_xgboost), r2_train = list(r2_train_xgboost), 
                                                               mse_test = list(mse_test_xgboost), r2_test = list(r2_test_xgboost),
                                                               share_x1 = list(NA), share_x2 = list(NA), share_x3 = list(NA), share_x4 = list(NA),
                                                               stability = list(NA)))
  result_surrogate_xgboost = cbind(surrogate = "xgboost", result_surrogate_xgboost)
  
  res = rbind(result_original, result_surrogate_lm, result_surrogate_xgboost)
  res = cbind(type = as.character(job$prob.pars$type), n = nrow(data), alpha = alpha, impr = impr.par, res)
  
  return(res)
}



