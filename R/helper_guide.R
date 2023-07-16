# functions for finding the best splitting variable with guide


guide_test = function(y, x, residuals, xgroups = NULL, optimizer, objective, correction.factor) {
  # categorize residuals
  # split Y into 2 parts based on whether residuals are positive or non-positive
  # separately for each parameter
  x_factor = colnames(x)[sapply(x, is.factor)]
  ybin = (-1)^((residuals>0)+1)   # -1 or +1
  
  # curvature test
  curv_test = sapply(x, function(xval){
    test_curvature(xval = xval, ybin = ybin, xgroups = xgroups)
  })
  
  curv_test = t(curv_test)
  curv_test = as.data.frame(curv_test)
  curv_test$z = row.names(curv_test)
  curv_test$type = ifelse(curv_test$z %in% x_factor, "c", "n")
  curv_test$z.value.orig = curv_test$z.value
  curv_test$z.value = ifelse(curv_test$type == "n", curv_test$z.value.orig*correction.factor, curv_test$z.value.orig) 
  curv_test = as.data.table(curv_test)
  
  # interaction test
  # create all possible two-way interaction sets
  interaction_set = combn(colnames(x), 2, simplify = FALSE)
  
  int_test = sapply(interaction_set, function(cols){
    test_interaction(x = x, xvals = cols, ybin = ybin, xgroups = xgroups)
  })
  
  int_test = as.data.table(t(int_test))
  int_test[,z.value := as.numeric(z.value)]
  int_test$type = ifelse(int_test$z1 %in% x_factor | int_test$z2 %in% x_factor, "c", "n")
  int_test$z.value.orig = int_test$z.value
  int_test$z.value = ifelse(int_test$type == "n", int_test$z.value.orig*correction.factor, int_test$z.value.orig) 
  

  res = find_split_variable_from_tests(y = y, x = x, curv_test = curv_test, int_test = int_test, optimizer = optimizer, objective = objective)
  return(res)
  
}



test_curvature = function(xval, ybin, xgroups){
  # exclude features with only one unique value
  if(length(unique(xval))<2) return(list(z.value = log(1-1), statistic = log(0)))
  
  
  # categorize split variable (based on supplementary code of paper "The power of unbiased recursive partitioning: A unifying view of CTree, MOB, and GUIDE", Schlosser, Lisa; Hothorn, Torsten; Zeileis, Achim)
  if(is.numeric(xval)){
    if(length(unique(xval)) <= 4){
      x_cat = as.factor(xval)
    } else{
      if(is.null(xgroups)){
        xgroups = 4
      }
      xbreaks = unique(quantile(xval, c(0:xgroups)/xgroups))
      
      x_cat = cut(xval, breaks = xbreaks, labels = c(1:(length(xbreaks)-1)), 
                  include.lowest = TRUE)
    }
    
  } else {
    x_cat = xval
  }
  
  if(length(unique(x_cat)) == 1) return(list(z.value = log(1-1), statistic = log(0)))
  
  # compute curvature test (for each parameter separately)
  tst_curv = chisq.test(x = ybin, y = x_cat)
  
  ret = c(z.value = qnorm(1 - as.numeric(tst_curv$p.value)/2), statistic = log(as.numeric(tst_curv$statistic)))

  return(ret)
}



test_interaction = function(x, xvals, ybin, xgroups){
  xval1 = x[,xvals[1]]
  xval2 = x[,xvals[2]]
  
  # exclude features pairs where one features has only one unique value
  if(length(unique(xval1)) < 2 | length(unique(xval2)) < 2){
    return(list(z1 = xvals[1], z2 = xvals[2], z.value = log(1-1), statistic = log(0)))
  }

  # categorize both features
  if(is.null(xgroups)){
    xgroups = 2
  } 
  
  if(is.numeric(xval1)){
    if(length(unique(xval1)) <= 2){
      x1_cat = as.factor(xval1)
    } else {
      x1breaks = quantile(xval1, c(0:xgroups)/xgroups)
      x1_cat = cut(xval1, breaks = x1breaks, labels = c(1:xgroups), 
                   include.lowest = TRUE)
    }
  } else {
    x1_cat = xval1
  }
  
  if(is.numeric(xval2)){
    if(length(unique(xval2)) <= 2){
      x2_cat = as.factor(xval2)
    } else {
      x2breaks = quantile(xval2, c(0:xgroups)/xgroups)
      x2_cat = cut(xval2, breaks = x2breaks, labels = c(1:xgroups), 
                   include.lowest = TRUE)
    }
   
  } else {
    x2_cat = xval2
  }
  # combine the two categorized variables in one factor variable (four levels)
  level_comb = as.data.table(expand.grid(unique(x1_cat), unique(x2_cat)))
  colnames(level_comb) = c("x1", "x2")
  level_comb$id = as.factor(1:nrow(level_comb))

  x_cat_df = data.frame(x1 = x1_cat, x2 = x2_cat)
  x_cat_df_new = left_join(x_cat_df, level_comb, by = c("x1","x2"), sort = FALSE)
  x_cat_int = x_cat_df_new$id
  table(x_cat_int)
  
  if(length(unique(x_cat_int)) == 1) return(list(z.value = log(1-1), statistic = log(0)))
  
  
  # compute interaction test 
  tst_int = chisq.test(x = ybin, y = x_cat_int)
  chisq.test(table(x_cat_int,ybin))
  
  
  ret = c(z1 = xvals[1], z2 = xvals[2], z.value = qnorm(1 - as.numeric(tst_int$p.value)/2),
          statistic = log(as.numeric(tst_int$statistic)))

  return(ret)
  
}


find_split_variable_from_tests = function(y, x, curv_test, int_test, optimizer, objective){
  if(max(curv_test[,z.value]) > max(int_test[, z.value])){
    z = curv_test[z.value == max(z.value), z]
    type = "curvature"
  } else {
    z_vec = int_test[z.value == max(z.value), c(z1,z2)]
    # if there is one best interactionpair, do the following:
    if(length(z_vec) == 2){
      # if both are categorical, use the variable with the smaller p value (larger z value) in the curvature test
      if((is.factor(x[,z_vec[1]]) & is.factor(x[,z_vec[2]])) ){
        curv_test_small = curv_test[z %in% z_vec,]
        z = curv_test_small[z.value == max(z.value), z]
      } 
      # if both variables are numeric, use both as potential split variables
      else if (is.numeric(x[,z_vec[1]]) & is.numeric(x[,z_vec[2]])){
        z_split = find_split_point(Y = y, X = x, z = z_vec, n.splits = 1, min.node.size = 10, optimizer = find_best_binary_split,
                                   objective = objective, splitpoints = "mean")
        z = z_split$feature[1]
      }
      # if one is numeric and one is categorical, use the categorical for splitting
      else {
        z = z_vec[sapply(x[,z_vec], is.factor)]
      }
    } else {
      # if there are multiple "best" pairs, count which variable is involved in 
      # the most significant interaction most often and choose this as splitting variable
      z_table = table(z_vec)
      z_candidates = names(z_table)[z_table == max(z_table)]
      if (length(z_candidates) == 1){
        z = z_candidates
      } else {
        if(length(z_candidates[is.factor(x[,z_candidates])]) > 0){
          # if there are still more competing variables including categorical variables include only these as possible splitting variables
          z = z_candidates[is.factor(x[,z_candidates])]
        } else {
          # if all are numerical, use all as possible splitting variables
          z_split = find_split_point(Y = y, X = x, z = z_candidates, n.splits = 1, min.node.size = 10, optimizer = find_best_binary_split,
                                     objective = objective, splitpoints = "mean")
          z = z_split$feature[1]
        }
      }
    }
    
    type = "interaction"
  }
  
  return(list(z = z, type = type))
}


bias_correction = function(y, x, xgroups = NULL, fit, n.bootstrap = 50){
  # if there are only numerical ore only categorical features, no bias correction is needed
  if(sum(sapply(x, is.factor)) %in% c(0L, ncol(x))){
    return(1)
  } else {
    r_grid = seq(0.5, 5, length.out = 46)
    x_factor = colnames(x)[sapply(x, is.factor)]
    
    # Target frequency of a numerical variable
    prob_n_exp = 1 - length(x_factor)/ncol(x)
    
    z_bootstrap = lapply(1:n.bootstrap, function(r){
      # in each bootstrap iteraction, a new y_b is sampled

      y_b = unlist(sample(unlist(y), size = nrow(y), replace = TRUE))
      model_b = fit(y = y_b, x = x)
      residuals_b = y_b - predict(model_b, x)
      ybin_b <- (-1)^((residuals_b>0)+1)   # -1 or +1

      # perform curvature test
      curv_test = sapply(x, function(xval){
        test_curvature(xval = xval, ybin = ybin_b, xgroups = xgroups)
      })
      
      curv_test = t(curv_test)
      curv_test = as.data.frame(curv_test)
      curv_test$z = row.names(curv_test)
      curv_test$type = ifelse(curv_test$z %in% x_factor, "c", "n")
      curv_test$z.value.orig = curv_test$z.value
      curv_test = as.data.table(curv_test)
      
      
      # perform interaction test
      interaction_set = combn(colnames(x), 2, simplify = FALSE)
      
      int_test = sapply(interaction_set, function(cols){
        test_interaction(x = x, xvals = cols, ybin = ybin_b, xgroups = xgroups)
      })
      
      int_test = as.data.table(t(int_test))
      int_test[,z.value := as.numeric(z.value)]
      int_test$type = ifelse(int_test$z1 %in% x_factor | int_test$z2 %in% x_factor, "c", "n")
      int_test$z.value.orig = int_test$z.value
      
      # for each gridpoint evaluate if the largest z value (smallest p-value) comes from a test, 
      # in which a categorical variable is uncluded or from a test with only numerical variables
      z_type = sapply(r_grid, function(r){
        test = rbind(curv_test, int_test, fill = TRUE)
        test$z.value = ifelse(test$type == "n", test$z.value.orig*r, test$z.value.orig) 

        if(r*max(test[type == "n", z.value]) >= max(test[type == "c", z.value])){
          z_type = "n"
        } else {z_type = "c"}
        
        return(z_type)
      })
      names(z_type) = r_grid
      return(z_type)
    })

    # for each grid.point calculate the observed frequency of a numerical splitting variable
    z_bootstrap = as.data.frame(z_bootstrap)
    colnames(z_bootstrap) = 1:n.bootstrap
    prob_n_obs = apply(z_bootstrap,1, function(row){
      prob = sum(row == "n")/n.bootstrap
    })

    if(prob_n_obs["1"] > prob_n_exp){
      f_value = 0
    } else { 
      f_value = 1 
    }
    r = approx(as.numeric(prob_n_obs), as.numeric(names(prob_n_obs)), xout = prob_n_exp, method = "constant", ties = "ordered", f = f_value)$y
    
    return(r)
  }
}
