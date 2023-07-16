# simulation helper

fit_trees = function(x_train, y_train, x_test, y_test, data_stability, min.split, 
                     maxdepth, impr.par, alpha, pruning, approximate,
                     n.quantiles, exclude.categoricals, correct.bias, 
                     tree_methods = c("slim", "mob", "ctree", "guide"),
                     extract_variables = FALSE, df.max = NULL){
  
  if ("slim_lasso" %in% tree_methods){
    x_wrong = paste0("x", 4:10)
  } else if ("slim_ridge" %in% tree_methods){
    x_wrong = "x1"
  }
  
  if("slim" %in% tree_methods){
    slim = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                             impr.par = impr.par, min.split = min.split, approximate = approximate,
                             split.method = "slim")
    
    slim_res = extract_results_slim(slim, x_train, x_test, y_train, y_test, 
                                    data_stability, x_wrong, extract_variables, lasso = FALSE,
                                    mbt = "SLIM")
    
  } 
  if("slim_ridge" %in% tree_methods){
    slim_ridge_res = list(mbt = "SLIM Ridge")
    
    slim_ridge = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                   impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                   split.method = "slim", penalization = "L2")
    slim_ridge_res = extract_results_slim(slim_ridge, x_train, x_test, y_train, y_test, 
                                          data_stability, x_wrong, extract_variables, lasso = FALSE,
                                          mbt = "SLIM Ridge")

  } 
  if("slim_lasso" %in% tree_methods){
    slim_lasso = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                   impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                   split.method = "slim", penalization = "L1", lambda = NULL, df.max = NULL)
    slim_lasso_res = extract_results_slim(slim_lasso, x_train, x_test, y_train, y_test, data_stability, 
                                          x_wrong, extract_variables, lasso = TRUE,
                                          mbt = "SLIM LASSO")
    
  } 
  if("slim_lasso_max_df" %in% tree_methods){
    slim_lasso_max_df_res_complete = data.frame()
    for(df in df.max){
      slim_lasso_max_df_res = list(mbt = paste("SLIM Lasso max df", df))
      
      slim_lasso_max_df = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                            impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                            split.method = "slim", penalization = "L1", lambda = NULL, df.max = df)
      
      slim_lasso_max_df_res = extract_results_slim(slim_lasso_max_df, x_train, x_test, y_train, y_test, 
                                                   data_stability, x_wrong, extract_variables, lasso = TRUE,
                                                   mbt = paste("SLIM Lasso max df", df))
      
      slim_lasso_max_df_res_complete = rbind(slim_lasso_max_df_res_complete,slim_lasso_max_df_res)
    }
    
  } 
  if("guide" %in% tree_methods){
    guide = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                              impr.par = impr.par, min.split = min.split, split.method = "guide",
                              exclude.categoricals = exclude.categoricals, correct.bias = correct.bias)
    guide_res = extract_results_slim(guide, x_train, x_test, y_train, y_test, 
                                     data_stability, x_wrong, extract_variables, lasso = FALSE,
                                     mbt = "GUIDE")
    
    
    
  } 
  if("mob" %in% tree_methods){
    mob_res = list(mbt = "MOB")
    # formula mob
    fm_mob = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    
    mob = lmtree(fm_mob, 
                 data = cbind(x_train, y = y_train), 
                 minsize = min.split, 
                 maxdepth = maxdepth, 
                 alpha = alpha)
    mob_leaf_sizes = unlist(nodeapply(mob, ids = nodeids(mob, terminal = TRUE), function(nodes){info_node(nodes)$nobs}))
    mob_res$n_leaves = width(mob)
    mob_res$depth = depth(mob)
    mob_res$max_leaf_size = max(mob_leaf_sizes)
    mob_res$min_leaf_size = min(mob_leaf_sizes)
    mob_res$sd_leaf_size = sd(mob_leaf_sizes)
    mobrule = partykit:::.list.rules.party(mob)
    mob_res$n_splitting_variables = length(unique(unlist(str_extract_all(mobrule,"(x+[1-9])"))))
    
    mob_res$mse_train = mean((predict(mob, x_train)- y_train)^2)
    mob_res$r2_train = r_2(y_train, predict(mob, x_train))
    
    mob_res$mse_test = mean((predict(mob, x_test)- y_test)^2)
    mob_res$r2_test = r_2(y_test, predict(mob, x_test))
    
    if(extract_variables){
      mob_res$x_wrong = ifelse(any(x_wrong %in% unique(unlist(str_extract_all(mobrule,"(x+[1-9])")))), TRUE, FALSE)
    }
    
    # calculate share of observations which where split by feature x2
    mob_innernodes = nodeids(mob)[!(nodeids(mob) %in% nodeids(mob, terminal = TRUE))]
    mob_obs = sapply(mob_innernodes, function(id) {
      nrow(data_party(mob[id]))
    })
    mob_inner_rules = str_extract_all(partykit:::.list.rules.party(mob, i = mob_innernodes+1),"(x+[1-9])")
    
    mob_split_feature = unlist(sapply(mob_inner_rules,function(r){
      tail(r,1)
    }))
    
    mob_res$share_x1 = sum(mob_obs[mob_split_feature == "x1"]/sum(mob_obs))
    mob_res$share_x2 = sum(mob_obs[mob_split_feature == "x2"]/sum(mob_obs))
    mob_res$share_x3 = sum(mob_obs[mob_split_feature == "x3"]/sum(mob_obs))
    mob_res$share_x4 = sum(mob_obs[mob_split_feature == "x4"]/sum(mob_obs))
    
    
    if(!is.null(data_stability)){
      mob_res$stability = as.character(predict(mob, data_stability, type = "node"))
    }
    
  } 
  if("ctree" %in% tree_methods){
    ctree_res = list(mbt = "CTree")
    fm_ctree = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    ctree = suppressWarnings(partykit::ctree(fm_ctree,
                                             data = cbind(x_train, y = y_train),
                                             ytrafo = fit_lm,
                                             control = partykit::ctree_control(minbucket = min.split, maxdepth = maxdepth - 1, alpha = alpha)))
    ctree_leaf_sizes = unlist(nodeapply(ctree, ids = nodeids(ctree, terminal = TRUE), function(nodes){info_node(nodes)$nobs}))
    ctree_res$n_leaves =  width(ctree)
    ctree_res$depth = depth(ctree)
    ctree_res$max_leaf_size = max(ctree_leaf_sizes)
    ctree_res$min_leaf_size = min(ctree_leaf_sizes)
    ctree_res$sd_leaf_size = sd(ctree_leaf_sizes)
    
    ctreerule = partykit:::.list.rules.party(ctree)
    ctree_res$n_splitting_variables = length(unique(unlist(str_extract_all(ctreerule,"(x+[1-9])"))))
    fit_ctree = fit_ctree_leaves(ctree, x_train, y_train)
    
    ctree_res$mse_train = mean((predict_ctree(ctree, fit_ctree, x_train)- y_train)^2)
    ctree_res$r2_train = r_2(y_train, predict_ctree(ctree, fit_ctree, x_train))
    
    ctree_res$mse_test = mean((predict_ctree(ctree, fit_ctree, x_test)- y_test)^2)
    ctree_res$r2_test = r_2(y_test, predict_ctree(ctree, fit_ctree, x_test))
    
    # calculate share of observations which where split by feature x2
    ctree_innernodes = nodeids(ctree)[!(nodeids(ctree) %in% nodeids(ctree, terminal = TRUE))]
    ctree_obs = sapply(ctree_innernodes, function(id) {
      nrow(data_party(ctree[id]))
    })
    ctree_inner_rules = str_extract_all(partykit:::.list.rules.party(ctree, i = ctree_innernodes+1),"(x+[1-9])")
    
    ctree_split_feature = unlist(sapply(ctree_inner_rules,function(r){
      tail(r,1)
    }))
    
    ctree_res$share_x1 = sum(ctree_obs[ctree_split_feature == "x1"]/sum(ctree_obs))
    ctree_res$share_x2 = sum(ctree_obs[ctree_split_feature == "x2"]/sum(ctree_obs))
    ctree_res$share_x3 = sum(ctree_obs[ctree_split_feature == "x3"]/sum(ctree_obs))
    ctree_res$share_x4 = sum(ctree_obs[ctree_split_feature == "x4"]/sum(ctree_obs))
    
    
    if(extract_variables){
      ctreerule = partykit:::.list.rules.party(ctree)
      ctree_res$x_wrong = ifelse(any(x_wrong %in% unique(unlist(str_extract_all(ctreerule,"(x+[1-9])")))), TRUE, FALSE)
    }
    
    
    if(!is.null(data_stability)){
      ctree_res$stability = as.character(predict(ctree, data_stability, type = "node"))
    }
  }
  
  if ("slim_ridge" %in% tree_methods){
    res = rbind(slim_res, guide_res, mob_res, ctree_res, slim_ridge_res)
  }
  if ("slim_lasso" %in% tree_methods){
    res = rbind(slim_res, slim_lasso_res, slim_lasso_max_df_res_complete, guide_res, mob_res, ctree_res)
  } else{
    res = rbind(slim_res, guide_res, mob_res, ctree_res)
  }
  
  
  return(res)
} 


# extract results from a slim tree

extract_results_slim = function(tree, x_train, x_test, y_train, y_test, data_stability, x_wrong, extract_variables, lasso = TRUE, mbt){
  split = as.data.table(extract_split_criteria(tree))
  tree_res = list(mbt = mbt)
  tree_res$n_leaves = sum(split$split.feature == "leafnode")
  tree_res$depth = split[split.feature != "leafnode", max(depth)]
  tree_res$max_leaf_size = split[split.feature == "leafnode", max(size)]
  tree_res$min_leaf_size = split[split.feature == "leafnode", min(size)]
  tree_res$sd_leaf_size = sd(split[split.feature == "leafnode", size])
  
  tree_res$n_splitting_variables = length(unique(split$split.feature))-1 #substract "leafnode"
  
  tree_res$mse_train = mean((predict_slim(tree, x_train)- y_train)^2)
  tree_res$r2_train = r_2(y_train, predict_slim(tree, x_train))
  
  tree_res$mse_test = mean((predict_slim(tree, x_test)- y_test)^2)
  tree_res$r2_test = r_2(y_test, predict_slim(tree, x_test))
  
  if(extract_variables){
    tree_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,split.feature])), TRUE, FALSE)
  }
  
  tree_obs = sum(split[split.feature != "leafnode", size])
  
  tree_res$share_x1 = sum(split[split.feature == "x1", size])/tree_obs
  tree_res$share_x2 = sum(split[split.feature == "x2", size])/tree_obs
  tree_res$share_x3 = sum(split[split.feature == "x3", size])/tree_obs
  tree_res$share_x4 = sum(split[split.feature == "x4", size])/tree_obs
  
  
  if(!is.null(data_stability)){
    # tree varies across all repetitions due to slightly different data, but data_stability is identical across all repetitions
    tree_res$stability = predict_slim(tree, data_stability, type = "node")
  }
  
  return(tree_res)
}



# fit models to ctree leaves
fit_ctree_leaves = function(ctree, x, y, fit.bsplines = FALSE){
  node_model = cbind(x, y = y, node = predict(ctree, type = "node"))
  node_model_list = split(node_model, node_model$node, drop = TRUE)
  node_model_list = lapply(node_model_list, function(node){
    x = node[, !(colnames(node) %in% c("y", "node"))]
    x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
    fm = formula(paste("y ~ ", paste(colnames(x), collapse = "+")))
    lm(fm, data = node)
  })
  return(node_model_list)
}



# get model predictions for ctree
predict_ctree = function(ctree, fit_ctree, newdata){
  newdata$row_id = 1:nrow(newdata)
  nodes = predict(ctree, newdata = newdata, type = "node")
  newdata_list = split(newdata, nodes)
  for(node in names(newdata_list)){
    newdata_list[[node]]$y_hat = predict(fit_ctree[[node]], newdata = newdata_list[[node]])
  }
  predictions = lapply(newdata_list, function(el) el[, c("row_id", "y_hat")])
  predictions = do.call(rbind, predictions)
  predictions = predictions[order(predictions$row_id),]
  rownames(predictions) = predictions$row_id
  return(predictions$y_hat)
}
