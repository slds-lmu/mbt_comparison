get_sim_results_nonlinear = function(data, job, instance, 
                                     n.quantiles = 50, min.split = 100, n.split = 6, impr.par = 0.05, r2 = 1, ... ){
  

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
  slim_basic = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                             n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = 50, 
                             degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_poly = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                          n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = 100, 
                          degree.poly = 2, penalization = "L1", fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_bsplines = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = TRUE, fit.gam = FALSE)
  
  slim_bsplines_approx = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = TRUE, fit.gam = FALSE,
                                approximate = TRUE)
  
  
  slim_gam = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = TRUE)
                          
                             
  result_original = rbind(slim_basic, slim_poly, slim_bsplines, slim_bsplines_approx, slim_gam)
  result_original = cbind(model = c("basic_lm", "penalized_poly", "bsplines", "bsplines_approx", "gam"), result_original)
  result_original = cbind(surrogate = "standalone", result_original)
  
  
  # fit trees to the xgboost predictions data
  start = Sys.time()
  
  lrn = instance$lrn
  task_train = as_task_regr(x = train, target = "y")
  task_test = as_task_regr(x = test, target = "y")
  lrn$train(task_train)
  end = Sys.time()
  
  pred_xgboost_train = lrn$predict(task_train)
  pred_xgboost_test = lrn$predict(task_test)
  
  mse_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.mse")))
  r2_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.rsq")))
  mse_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.mse")))
  r2_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.rsq")))
  
  
  y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
  y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
  
  slim_basic_xgboost = fit_slim_tree(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost, 
                             n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = 50, 
                             degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_poly_xgboost = fit_slim_tree(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost, 
                            n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = 100, 
                            degree.poly = 2, penalization = "L1", fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_bsplines_xgboost = fit_slim_tree(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost, 
                                n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = TRUE, fit.gam = FALSE)
  
  slim_bsplines_approx_xgboost = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                       n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                       degree.poly = 1, penalization = NULL, fit.bsplines = TRUE, fit.gam = FALSE,
                                       approximate = TRUE)
  
  
  slim_gam_xgboost = fit_slim_tree(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost, 
                           n.split = n.split, n.quantiles = n.quantiles, impr.par = impr.par, r2 = r2, min.split = min.split, 
                           degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = TRUE)
  
  
  result_surrogate_xgboost = rbind(slim_basic_xgboost, slim_poly_xgboost, slim_bsplines_xgboost, 
                                   slim_bsplines_approx_xgboost, slim_gam_xgboost)
  result_surrogate_xgboost = cbind(model = c("basic_lm", "penalized_poly", "bsplines", "bsplines_approx","gam"), result_surrogate_xgboost)
  
  result_surrogate_xgboost = rbind(result_surrogate_xgboost, data.table(model = "xgboost", time = as.numeric((end-start), units = "secs"),
                                                               n_leaves = NA,
                                                               n_splitting_variables = NA,
                                                               share_main_effect_split = NA,
                                                               df = NA,
                                                               mse_train = mse_train_xgboost, r2_train = r2_train_xgboost, 
                                                               mse_test = mse_test_xgboost, r2_test = r2_test_xgboost))
  result_surrogate_xgboost = cbind(surrogate = "xgboost", result_surrogate_xgboost)
  
  res = rbind(result_original, result_surrogate_xgboost)
  
  
  return(res)
  
  
}

fit_slim_tree = function(x_train, y_train, x_test, y_test , 
                         n.split, n.quantiles, impr.par, r2, min.split,
                         degree.poly, fit.bsplines, fit.gam, penalization,
                         approximate = FALSE){
  slim_res = data.table()
  
  start = Sys.time()
  slim = compute_tree_slim(y = y_train, x = x_train , n.split = n.split, n.quantiles = n.quantiles,
                           impr.par = impr.par, r2 = r2, min.split = min.split, degree.poly = degree.poly, 
                           fit.bsplines = fit.bsplines, fit.gam = fit.gam, penalization = penalization,
                           approximate = approximate)
  end = Sys.time()
  slim_res$time = as.numeric((end-start), units = "secs")
  split = as.data.table(extract_split_criteria(slim))
  models = extract_models(slim)
  
  leafnode_ids = split[split.feature == "leafnode",id.node]
  
  slim_res$n_leaves = sum(split$split.feature == "leafnode")
  slim_res$n_splitting_variables = length(unique(split$split.feature))-1 #substract "leafnode"
  # Share of observations split by x2 or x3 in all split observations
  slim_res$share_main_effect_split = sum(split[split.feature %in% c("x2", "x3"), size])/sum(split[split.feature != "leafnode", size])
  
  if(!is.null(penalization)){
    if(penalization == "L1"){
      slim_res$df = mean(sapply(as.character(leafnode_ids), function(id){
        models[[id]][["model"]][["df"]]
      }))
    }
  } else if (fit.bsplines){
    slim_res$df = NA
  } else if (fit.gam){
    slim_res$df = NA
  } else{
    slim_res$df = mean(sapply(as.character(leafnode_ids), function(id){
      length(models[[id]][["model"]][["coefficients"]])
    }))
  }
 
  slim_res$mse_train = mean((predict_slim(slim, x_train, degree.poly = degree.poly)- y_train)^2)
  slim_res$r2_train = r_2(y_train, predict_slim(slim, x_train, degree.poly = degree.poly))
  
  slim_res$mse_test = mean((predict_slim(slim, x_test, degree.poly = degree.poly)- y_test)^2)
  slim_res$r2_test = r_2(y_test, predict_slim(slim, x_test, degree.poly = degree.poly))
  
  
  return(slim_res)
  
}


