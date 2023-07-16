create_sim_data = function(job, n = 1000, type, rho = 0, ...){
  
  # basic scenarios
  if (type == "linear_smooth"){
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)

    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = lrn("regr.xgboost",
              max_depth = 5,
              eta = 0.5,
              alpha = 1,
              gamma = 2,
              nrounds = 400,
              interaction_constraints = "[[1,2]]")
    
   
  # linear categorical
  } else if(type == "linear_abrupt"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
  
    
    formula = x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) 

    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    

    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3 + ti(x1,x2)")
    lrn = lrn("regr.xgboost",
              max_depth = 3,
              eta = 0.5,
              alpha = 0.5,
              gamma = 1,
              nrounds = 350,
              interaction_constraints = "[[0,1], [1,2]]")
    
   
    
  } else if(type == "linear_mixed"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
    x4 = as.numeric(rbernoulli(n))
    
    
    formula = 4*x2 + 2*x4 + 4*x2*x1 + ifelse(x3 == 0, 8*x2,0) + 
      ifelse(x4 == 1, 8*x1*x2, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:4)), y)
    fm = as.formula("y ~ x2 + x4 + x2:x1 + x2:x3 + x1:x2:x4")
    lrn = lrn("regr.xgboost",
              max_depth = 5,
              eta = 0.5,
              alpha = 2,
              gamma = 3.5,
              nrounds = 500,
              interaction_constraints = "[[0,1], [1,2], [0,1,3]]")
    
    
  } else if(type == "linear_smooth_corr"){
    
    cor_matrix = matrix(c(1,rho,
                          rho,1), nrow = 2, byrow = TRUE)
    
    
    list_distributions = list(function(n) qunif(n, -1, 1), 
                              function(n) qunif(n, -1, 1))
    vars = marginals_copula(cor_matrix, list_distributions, n = n)
    
    x1 = vars[,1]
    x2 = vars[,2]
    x3 = runif(n, -1, 1)
    
    
    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = NULL
    
    # noise features
  } else if (type == "linear_smooth_lasso"){
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    for(i in 4:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = NULL
    
    
  } else if(type == "nonlinear_mixed"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = as.numeric(rbernoulli(n))
    
    
    formula = x1 + 2*x2^2 + x3*log(abs(x3)) + x4*x5 + x1*x4*ifelse(x6 == 0, 1,0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:6)), y)
    fm = as.formula("y ~ x1 + x2 + x3 + ti(x2, x3) + ti(x1,x2,x4)")
    lrn = lrn("regr.xgboost",
              max_depth = 4,
              eta = 0.825,
              alpha = 0.75,
              gamma = 1,
              nrounds = 700,
              interaction_constraints = "[[3,4],[0,3,5]]")
    
  }

  return(list(data = data, fm = fm, lrn = lrn))
}
