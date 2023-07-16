source("R/load_packages.R")
source("R/helper_general.R")
source("R/helper_guide.R")


#' @description generates a SLIM tree with main effect models in the leaf nodes
#'
#' @param y target vector
#' @param x dataset to use for splitting (data.frame with features in columns) features are used both for splitting and for main effect modelling
#' @param n.split maximum number of splits to be performed
#' @param min.split minimum number of observations per node
#' @param impr.par prepruning: minimum required percentage improvement of the objective by a split compared to the previous split
#' @param r2 prepruning: R2 in a node, from which this node is not to be split any further
#' @param n.quantiles number of quantile splits to be evaluated per splitting variable
#' @param approximate should approximate splitting algorithm be used (only possible for objective = 'MSE', penalization = NULL, fit.gam = FALSE)
#' @param split.method how should splits be performed? "slim" for exhaustive search with SLIM Algorithm, "guide" for using chi-sqare independence test to find the splitting variable
#' @param pruning select pruning method ('forward', 'none') 
#' @param objective character string with objective function to use ('MSE' or 'MAE') "MAE" is only implemented for linear models and polynomial models
#' @param family = "gaussian" experimental (only gaussian was used)
#' @param degree.poly degree of included polynomials
#' @param fit.bsplines should bsplines be fitted
#' @param fit.gam should a gam be fitted
#' @param df.spline number of bspline knots
#' @param penalization "L1" for LASSO "L2" for Ridge and NULL for no penalization
#' @param lambda = NULL penalization parameter
#' @param df.max = NULL maximum effective degrees of freedom for penalized regression
#' @param exclude.categoricals = FALSE exclude categorical variables from the modelling  (for GUIDE replication)
#' @param correct.bias = FALSE perform bootstrap bias correction for GUIDE



# compute single tree based on Class 'Node' 
compute_tree_slim = function(y,
                             x,
                             n.split, 
                             impr.par = 0.1,
                             r2 = 1,
                             min.split = 10,
                             n.quantiles = 100,
                             approximate = FALSE,
                             split.method = "slim",
                             pruning = "forward", 
                             objective = "MSE",
                             family = "gaussian",
                             degree.poly = 1,
                             fit.bsplines = FALSE,
                             fit.gam = FALSE,
                             df.spline = 15,
                             penalization = NULL,
                             lambda = NULL,
                             df.max = NULL,
                             exclude.categoricals = FALSE,
                             correct.bias = FALSE) {
  time.start = Sys.time()
  input.data = list(X=as.data.frame(x), Y=data.frame(y = as.vector(y)))
  
  alpha = 0
  if (objective == "MSE"){
    
    if (fit.gam){
      split.objective = get_objective_gam
      fit.model = get_model_gam
      predict.response = get_prediction_gam
      
    } else {
      if (is.null(penalization)){
        split.objective = get_objective_lm
        fit.model = get_model_lm
        predict.response = get_prediction_lm

      } else if (penalization %in% c("L1", "L2")){
        alpha = ifelse(penalization == "L1", 1, 0)
        split.objective = get_objective_glmnet
        fit.model = get_model_glmnet
        predict.response = get_prediction_glmnet

      } else {
        stop(paste("penalization", penalization, "is not supported."))
      }
      
      
    }
  } else if (objective == "MAE"){
    split.objective = get_objective_lad
    fit.model = get_model_lad
	predict.response = get_prediction_lad
									 
  }
  else {
    stop(paste("objective", objective, "is not supported."))
  } 

  # set arguments of objective and splitting function
  formals(split.objective) = list(y = data.frame(), x = data.frame(), 
                                  .degree.poly = degree.poly,
                                  .df.spline = df.spline,
                                  .fit.bsplines = fit.bsplines,
                                  .family = family,
                                  .alpha = alpha,
                                  .lambda = lambda,
                                  .df.max = df.max,
                                  .exclude.categoricals = exclude.categoricals)
  
  formals(fit.model) = list(y = data.frame(), x = data.frame(), 
                            .degree.poly = degree.poly,
                            .df.spline = df.spline,
                            .fit.bsplines = fit.bsplines,
                            .family = family,
                            .alpha = alpha,
                            .lambda = lambda,
                            .df.max = df.max,
                            .exclude.categoricals = exclude.categoricals,
                            .type = "model")
  
  formals(predict.response)$.exclude.categoricals = exclude.categoricals
  
  # only important for guide: find best bias correction factor r
  if(split.method == "guide" & correct.bias == TRUE){
    correction.factor = bias_correction(y = input.data$Y, x = input.data$X, xgroups = NULL, fit = fit.model, n.bootstrap = 50)
  } else {correction.factor = 1}


  # Initialize the parent node of the tree
  model.parent = fit.model(y = input.data$Y, x = input.data$X)
  term.predictions.parent = predict.response(model.parent, input.data$X)
  
  
  parent = Node$new(id = 0, depth = 1, subset.idx = seq_len(nrow(input.data$X)), improvement.met = FALSE, impr = 0, model.fit = model.parent, 
                    term.predictions.parent = term.predictions.parent, variable.importance = round(apply(term.predictions.parent, MARGIN = 2, var), 4))
  
  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {
    leaves = tree[[depth]]
    
    tree[[depth + 1]] = list()
    
    for (node.idx in seq_along(leaves)) {
      node.to.split = leaves[[node.idx]]
      
      if (!is.null(node.to.split)) {
        node.to.split$computeSplit(X = input.data$X, Y = input.data$Y, objective = split.objective,
                                   fit = fit.model,
                                   impr.par = impr.par, optimizer = ifelse(approximate == FALSE, find_best_binary_split, find_best_binary_split_approx), 
                                   min.split = min.split, pruning = pruning, n.quantiles = n.quantiles,
                                   penalization = penalization, fit.bsplines = fit.bsplines, df.spline = df.spline,
                                   split.method = split.method, correction.factor = correction.factor, r2 = r2)
        node.to.split$computeChildren(input.data$X, input.data$Y, objective = split.objective,
                                      fit = fit.model, predict.response = predict.response, 
                                      pruning = pruning)
        tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)    

      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL,NULL))                
      }
    }

  }
  time.end = Sys.time()
  # print(time.end-time.start)
  return(tree)
}





# Definition of Class Node (customtrees)
Node <- R6Class("Node", list(
  id = NULL,
  
  # on which depth is the node
  depth = NULL,
  
  # ids of the instances of data that are in this node
  subset.idx = NULL,
  
  # objective value in a node
  objective.value = NULL, 
  objective.value.parent = NULL,
  r2 = NULL,
  term.predictions.parent = NULL,
  term.predictions = NULL,
  
  # Parent information
  id.parent = NULL, 
  child.type = NULL, # left or right type
  
  # Split information (if splitting has already taken place)
  split.feature = NULL,
  split.value = NULL,
  split.type = NULL,
  
  # type of test (GUIDE)
  test.type = NULL,
  
  # Append the children of this node
  children = list(),
  
  stop.criterion.met = FALSE,
  improvement.met = NULL,
  
  impr = NULL,
  impr.target = NULL,
  impr.relative = NULL,
  
  # Model information
  model.fit = NULL,
  split.effect = NULL,
  variable.importance = NULL,
  
  
  
  initialize = function(id, depth = NULL, subset.idx, id.parent = NULL, child.type = NULL, objective.value.parent = NULL, objective.value = NULL, r2 = NULL, improvement.met, impr, impr.target = NULL, impr.relative = NULL, model.fit = NULL, term.predictions.parent = NULL, variable.importance = NULL) {
    assert_numeric(id, len = 1)
    assert_numeric(depth, len = 1, null.ok = TRUE)
    
    assert_numeric(subset.idx, min.len = 1)
    assert_numeric(id.parent, len = 1, null.ok = TRUE)
    assert_character(child.type, null.ok = TRUE)
    
    self$id = id
    self$depth = depth
    self$subset.idx = subset.idx
    self$id.parent = id.parent
    self$child.type = child.type
    self$impr = impr
    self$impr.target = impr.target
    self$impr.relative = impr.relative
    self$objective.value.parent = objective.value.parent
    self$objective.value = objective.value
    self$improvement.met = improvement.met
    self$r2 = r2
    
    self$term.predictions.parent = term.predictions.parent
    self$stop.criterion.met = FALSE
    self$model.fit = model.fit
    self$variable.importance = variable.importance
  },
  
  computeSplit = function(X, Y, objective, fit, impr.par, optimizer, min.split = 10, 
                          pruning, n.quantiles, penalization, fit.bsplines, 
                          df.spline, split.method, correction.factor, r2 = r2) {
    
    self$r2 = r_2(Y[self$subset.idx, , drop = FALSE],
                  fit(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ], .type = "fitted.values"))
    
    
    if (length(self$subset.idx) < (2*min.split + 1) | (self$improvement.met == TRUE & pruning == "forward")  | 
        (self$r2 >= r2 & pruning == "forward")) {
      
      self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
      self$stop.criterion.met = TRUE
      self$objective.value = NULL
      self$impr = NULL
      self$impr.target = NULL
      self$impr.relative = NULL
      
    } else {
      self$objective.value.parent = objective(y = Y, x = X)
      self$objective.value = objective(y = Y[self$subset.idx, ,drop = FALSE], x = X[self$subset.idx, ])
      
      tryCatch({
        split = split_parent_node(Y = Y[self$subset.idx, ,drop = FALSE], X = X[self$subset.idx, ], 
                                  objective = objective, optimizer = optimizer, 
                                  fit = fit,
                                  min.node.size = min.split, n.quantiles = n.quantiles,
                                  penalization = penalization, 
                                  fit.bsplines = fit.bsplines, df.spline = df.spline,
                                  split.method = split.method, correction.factor = correction.factor)
        
        
        impr = (self$objective.value - split$objective.value[split$best.split][1]) / self$objective.value.parent
        self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
        
        if(is.null(self$impr)) {
          self$impr = 0
        }
        
        if(self$impr == 0){
          self$impr.target = impr.par
          self$impr.relative = self$impr
        } else{
          self$impr.target = self$impr*impr.par
          self$impr.relative = impr/self$impr
        }
        
        if ((impr < self$impr.target) & pruning == "forward"){
          self$improvement.met = TRUE
          self$objective.value = NULL
          
        } else{
          self$split.feature = split$feature[1]
          self$split.type = split$split.type
          if (self$split.type == "categorical"){
            self$split.value = str_split(split$split.points, ",")[[1]]
          } else if (self$split.type == "numerical"){
            self$split.value = split$split.points
          }
          self$objective.value = split$objective.value[1]
          if(split.method == "guide"){
            self$test.type = split$test.type
          }
        }  
        
        self$impr = impr
        
      },
      error = function(cond) {
        browser()
        # message(paste0("Min.node.size is reached in node ", self$id))
        self$stop.criterion.met = TRUE
      })
    }
  },
  
  computeChildren = function(X, Y, objective, fit, predict.response, pruning) {
    
    if (self$stop.criterion.met | (self$improvement.met & pruning == "forward")) {
      # no further split is performed
      self$children = list("left.child" = NULL, "right.child" = NULL)
    } else {
      if(is.null(self$split.feature))
        stop("Please compute the split first via computeSplit().")
      
      if (self$split.type == "categorical"){
        idx.left = which(as.character(X[self$subset.idx, self$split.feature]) %in% self$split.value)
        idx.right = which(!(as.character(X[self$subset.idx, self$split.feature]) %in% self$split.value))
      } else if (self$split.type == "numerical"){
        idx.left = which(X[self$subset.idx, self$split.feature] <= self$split.value)
        idx.right = which(X[self$subset.idx, self$split.feature] > self$split.value)
      }
      idx.left = self$subset.idx[idx.left]
      if(length(idx.left)==0) idx.left = 0
      idx.right = self$subset.idx[idx.right]
      if(length(idx.right)==0) idx.right = 0
      
      obj.left = objective(y = Y[idx.left, ,drop = FALSE], x = X[idx.left, ])
      obj.right = objective(y = Y[idx.right, ,drop = FALSE], x = X[idx.right, ])
      obj.parent = objective(y = Y[self$subset.idx, ,drop = FALSE], x = X[self$subset.idx, ])
      r2.left = r_2(Y[idx.left, ,drop = FALSE],
                    fit(y = Y[idx.left, ,drop = FALSE], x = X[idx.left, ], .type = "fitted.values"))
      r2.right = r_2(Y[idx.right, ,drop = FALSE],
                     fit(y = Y[idx.right, ,drop = FALSE], x = X[idx.right, ], .type = "fitted.values"))
      
      model.left = fit(y = Y[idx.left, ,drop = FALSE], x = X[idx.left, ])
      model.right = fit(y = Y[idx.right, ,drop = FALSE], x = X[idx.right, ])
      term.predictions.left = predict.response(model.left, x = X[idx.left, ])
      term.predictions.right = predict.response(model.right, x = X[idx.right, ])
      intersection = intersect(colnames(term.predictions.left), colnames(term.predictions.right))
      
      predictions = rbind(term.predictions.left[,intersection, drop = FALSE], term.predictions.right[, intersection, drop = FALSE])
      
      self$term.predictions = predictions[order(as.numeric(rownames(predictions))),, drop = FALSE]
      self$split.effect = calculate_split_effects(self$term.predictions.parent, self$term.predictions, exclude = NULL)
      
      variable.importance.left = round(apply(term.predictions.left, MARGIN = 2, var), 4)
      variable.importance.right = round(apply(term.predictions.right, MARGIN = 2, var), 4)
      
      
      child.type.left = paste(self$split.feature, ifelse(self$split.type == "numerical", paste("<=", self$split.value), ifelse(self$split.type == "categorical", paste0("%in% c(", paste0(self$split.value, collapse = ","), ")"), NULL)))
      child.type.right =  paste(ifelse(self$split.type == "numerical", paste(self$split.feature, ">", self$split.value), ifelse(self$split.type == "categorical", paste0("!(",self$split.feature, " %in% c(", paste0(self$split.value, collapse = ","), "))"), NULL)))
      if(!is.null(self$child.type)){
        child.type.left = paste(self$child.type, child.type.left, sep = " & ")
        child.type.right = paste(self$child.type, child.type.right, sep = " & ")
      }
      left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = child.type.left,  improvement.met = self$improvement.met, 
                            impr = self$impr, impr.target = self$impr.target, impr.relative = self$impr.relative, model.fit = model.left, 
                            term.predictions.parent = term.predictions.left, objective.value.parent = obj.left, r2 = r2.left, variable.importance = variable.importance.left)
      
      right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = child.type.right,  improvement.met = self$improvement.met, 
                             impr = self$impr, impr.target = self$impr.target, impr.relative = self$impr.relative, model.fit = model.right, 
                             term.predictions.parent = term.predictions.right, objective.value.parent = obj.right, r2 = r2.right, variable.importance = variable.importance.right)
      
      self$children = list("left.child" = left.child, "right.child" = right.child)
    }
  }
)
)
