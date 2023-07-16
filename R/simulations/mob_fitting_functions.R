# fitting/trafo functions for mob and ctree


fit_lm = function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  lm(y ~ x , ...)
}

fit_spline = function(y, x, start = NULL, weights = NULL, offset = NULL, degree = 1, df = 10,  ..., estfun = TRUE, object = TRUE) {
  term = c()
  for (n in colnames(x)[-1]){
    if (is.numeric(x[,n]) & length(unique(x[,n]))>2){
      newterm = paste0("bs(", n, ", df =", df, ", degree =",  degree, ")")
    } else {
      newterm = n
    }
    term = c(term, newterm)
  }
  
  fm_spline = as.formula(paste("y~", paste(term, collapse = "+")))
  
  model = lm(fm_spline, data = data.frame(y,x), weights = weights, offset = offset)
  return(list(coefficients = coef(model),
              objfun = sum(model$residuals**2),
              estfun = sandwich::estfun(model),
              object = model))
}



# fit models to ctree leaves
fit_ctree_leaves = function(ctree, x, y, fit.bsplines = FALSE, df.spline){
  node_model = cbind(x, y = y, node = predict(ctree, type = "node"))
  node_model_list = split(node_model, node_model$node, drop = TRUE)
  node_model_list = lapply(node_model_list, function(node){
    x = node[, !(colnames(node) %in% c("y", "node"))]
    x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
    numeric.names = c()
    splines = c()
    
    if (fit.bsplines) {
      numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval) & length(unique(xval)) > 2))]
      if(length(numeric.names)>0){
        splines = paste0("bs(", numeric.names, ", df = ", df.spline, ", degree = 1)")  
      }
    }
    fm = as.formula(paste("y ~", paste(c(names(x)[!(names(x) %in% numeric.names)], splines), collapse = "+")))
    model = lm(fm, data = node)

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