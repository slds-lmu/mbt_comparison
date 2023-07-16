# Helper functions

# ----- SLIM tree evaluation functions -----
# Get predictions of SLIM tree (type response or node)
predict_slim = function(tree, newdata, type = "response", degree.poly = 1){
  node_list = data_to_nodes(tree, newdata, nodes = "leafnode", degree.poly = degree.poly)
  prediction_list = lapply(node_list, function(node){
    if(type == "response"){
      
      new_data_n = node$newdata
      if("glmnet" %in% class(node$model)){
        new_data_n = subset(node$newdata, select = rownames(node$model$beta))					   																															  																					 
      }
      y_hat = predict(node$model, new_data_n)
      
    } else if(type == "node"){
      y_hat = node$node_id
    }
    
    pred = cbind(node$newdata, y_hat = as.vector(y_hat))
    return(pred)
  })
  
  predictions = as.data.frame(do.call(rbind, prediction_list))
  
  predictions = predictions[order(predictions[,"row_id"]),]
  rownames(predictions) = predictions[,"row_id"]
  return(predictions$y_hat)
}													  


# Assign new data to the correct nodes and transform them if necessary for prediction.
# Helper function for predict_slim an plot_slim_pdp

data_to_nodes = function(tree, newdata, nodes = "leafnode", degree.poly = 1){
  
  models = extract_models(tree)
  split = extract_split_criteria(tree)
  if(length(nodes) == 1L){
    if(nodes == "all"){
      split_nodes = split[,c("child.type", "id.node")]
      
    } else if(nodes %in% c("leafnode", "leafnodes")){
      split_nodes = split[split$split.feature == "leafnode",c("child.type", "id.node")]
    } 
  } else {
    split_nodes = split[split$id.node %in% nodes,c("child.type", "id.node")]
  }
  
  newdata = as.data.table(newdata)
  newdata$row_id = 1:nrow(newdata)
  
  node_list = list()
  
  for (n in 1:nrow(split_nodes)){
    node_id = as.character(split_nodes[n,"id.node"])
    node_list[[node_id]] = list()
    
    if(nrow(split_nodes) == 1){
      node_data = as.data.frame(newdata)
    } else{
      if(split_nodes[n, "child.type"] == "rootnode"){
        node_data = as.data.frame(newdata)
      } else{
        node_data = as.data.frame(newdata[eval(parse(text = split_nodes[n, "child.type"])),])
      }
    }
    
											  
    if (nrow(node_data) > 0){
							  
      newdata_n = subset(node_data, select = -c(row_id))
      
      # if penalized polynomial regression was used, the data must be transformed
      if(class(models[[node_id]]$model)[1] %in% c("elnet", "glmnet")){
        if (degree.poly > 1) {
          features = names(newdata_n)
          for(f in features){
            if (is.numeric(newdata_n[[f]])){
              for(d in 2:degree.poly){
                newdata_n = cbind(newdata_n, "new" = newdata_n[[f]]^d)
                colnames(newdata_n)[which(names(newdata_n) == "new")] = paste0(f,"_",d)
									
														
              }
            }
          }
        } 
        
        # newdata_n = newdata_n[, colnames(newdata_n) %in% rownames(models[[node_id]]$model$beta)]
        newdata_n = as.matrix(newdata_n)
      }
      newdata_n = cbind(newdata_n, row_id = node_data$row_id)
      
      # save node_id, model data and chlidtype for ever node
      node_list[[node_id]]$node_id = node_id
      node_list[[node_id]]$newdata = newdata_n
      node_list[[node_id]]$model = models[[node_id]]$model
      node_list[[node_id]]$child_type = split_nodes[n, "child.type"]
      node_list[[node_id]]$effects = models[[node_id]]$effects
      node_list[[node_id]]$subset.idx = models[[node_id]]$subset.idx
    }
  }
  return(node_list)
}


# Function to plot tree structure of a SLIM Tree
# include_coefficiants options: "leafnodes", "all", "none"

plot_slim = function(tree, include_coefficiants = "leafnodes", digits = 3, include_improvement = FALSE,
                     vertex.size = 40, vertex.size2 = 50, vertex.label.cex = 0.8,
                     edge.label.cex = 0.8,...){
  split = as.data.table(extract_split_criteria(tree))
  models = extract_models(tree)
  
  # create tree data.frame
  tree_data = data.frame(parent = split$id.node.parent, node = split$id.node, 
                         text = apply(str_split_fixed(str_trim(str_extract(split$child.type, "[^&]+$")), " ", 2), 
                                      1,
                                      function(rule){
                                        splitpoint = str_split_fixed(rule[2], " ", 2)
                                        splitpoint[2] = round(as.numeric(splitpoint[2]),4)
                                        paste0(c(rule[1], splitpoint), collapse = "\n")
                                      })
                         )[-1,]
                           
  g = graph.data.frame(tree_data)
  
  # define vertex labels
  vertex.label = c()
  
  for(n in 1:nrow(split)){
    vertex.label[n] = paste0(
      "id: ", split[n,id.node],
      "\nsize: ", split[n,size])
    
    if(include_coefficiants == "all" | 
       (include_coefficiants == "leafnodes" & split[n, split.feature] == "leafnode")){

    # print more info in the leafnodes
      id = split[n,as.character(id.node)]
      model = models[[id]]$model
      coeffs = coef(model)
      coeffs_output = c()
      
      if("glmnet" %in% class(model)){
        # keep only non-zero coefficiants
        coeffs = coeffs[(coeffs@i+1),]
      }

      # only include coefficiants, if there are maximal 10
      if(length(coeffs)<10){
        for(co in 1:length(coeffs)){
          coeffs_output = paste0(coeffs_output, "\n", 
                                 paste0(names(coeffs)[co], ": ", round(coeffs[co], digits)))
        }
        vertex.label[n] = paste0(vertex.label[n], coeffs_output)
      }
    } 
    if(include_improvement){
      if(n > 1 & !is.na(split[n,impr.relative])){
        vertex.label[n] = paste0(vertex.label[n],  "\n", "impr: ", round(split[n,impr.relative],3) )
      }
    }
  }

  # match vertex ids with new labels
  names(vertex.label) = as.character(0:(length(vertex.label)-1))
  V(g)$label = vertex.label[V(g)$name]
  
  
  plot(g, layout = layout_as_tree, 
       edge.label=E(g)$text, 
       # vertex.label = vertex.label,
       vertex.color = "grey",
       vertex.label.cex = vertex.label.cex,
       vertex.label.color = "black",
       vertex.size= vertex.size,
       vertex.size2 = vertex.size2,
       vertex.shape = "rectangle",
       edge.arrow.size=0.1,
       edge.label.cex = edge.label.cex,
       edge.label.color = "blue",
       
       
       ...)
  
}


# plot feature input/output dependency
plot_slim_effects = function(tree, data, features, nodes = "leafnode"){
  node_list = data_to_nodes(tree, data, nodes = nodes, degree.poly = degree.poly)
  effect_list = lapply(node_list, function(node){
    effect = node$effects
    colnames(effect) = paste0("effect.", colnames(effect))
    cbind(data[node$subset.idx, features], effect, child_type = node$child_type)
  })
  
  effect_df = do.call(rbind.fill, effect_list)

  plot_list = list()
  for(feat in features){

    effect_feat = effect_df[,c(feat, paste0("effect.", feat), "child_type")]
    colnames(effect_feat) = c("x", "effect", "child_type")
    
    plot_list[[feat]] = ggplot(effect_feat, aes(x = x, y = effect, color = child_type)) + 
      theme_bw()+ 
      geom_point() + 
      geom_line(lwd = 1.5) + 
      theme(legend.position="bottom", legend.text=element_text(size=9)) + 
      labs(x = feat, y = "", color = "rule") +
      guides(color=guide_legend(ncol=1, byrow=TRUE))
  }
  return(plot_list)
    
}



# create plot list with pdp plots by node or by feature
plot_slim_pdp = function(tree, newdata, features, target, nodes = "leafnode", degree.poly = 1, method = "pdp", by = "feature"){
											  
  node_list = data_to_nodes(tree, newdata, nodes = nodes, degree.poly = degree.poly)
  pdp_nodes_plot = lapply(node_list, function(node){
    predictor = Predictor$new(node$model, data = as.data.frame(node$newdata[, features]), y = as.data.frame(node$newdata[,target]))
    pdp = FeatureEffects$new(predictor, method = "pdp")
  })

  if(by == "feature"){
    pdp_features = list()
    pdp_feature_plot = list()
    for(feat in features){
      pdp_features[[feat]] = lapply(names(pdp_nodes_plot), function(node_id){
        pdp = pdp_nodes_plot[[node_id]]
        res = pdp$results[[feat]]
        res$node_id = node_id
							   
        res$child_type = node_list[[node_id]]$child_type
        return(res)
      })
      
      pdp_features[[feat]] = do.call(rbind, pdp_features[[feat]])
      pdp_feature_plot[[feat]] = ggplot(pdp_features[[feat]], aes(x = .borders, y = .value, color = child_type)) + 
        geom_line(lwd = 1.5) + 
        theme_bw()+ 
        theme(legend.position="bottom", legend.text=element_text(size=9)) + 
        ggtitle(paste0("PDP feature ", feat)) +
        labs(x = feat, y = "", color = "rule") +
        guides(color=guide_legend(ncol=2, byrow=TRUE))
      
    }
    return(pdp_feature_plot)
  } else {
    return(pdp_nodes_plot)
  }
													   
}




extract_split_criteria = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      if(is.null(node)){
        df = data.frame("depth" = NA, "id" = NA, 
                        "id.parent" = NA,
                        "objective.value" = NA, 
                        "objective.value.parent" = NA,
                        "r2" = NA,
                        "impr" = NA, 
                        "impr.target" = NA,
                        "impr.relative" = NA,
                        "split.feature" = "final", 
                        "split.value" = NA,
                        "child.type" = "final",
                        "size" = NA,
                        "guide.test" = NA)
      } else{
        df = data.frame("depth" = node$depth, "id" = node$id,
                        "id.parent" = ifelse(is.null(node$id.parent), 0, node$id.parent),
                        "objective.value" = ifelse(is.null(node$objective.value), NA, node$objective.value),
                        "objective.value.parent" = ifelse(is.null(node$objective.value.parent), NA, node$objective.value.parent),
                        "r2" = ifelse(is.null(node$r2), NA, node$r2),
                        "impr" = ifelse(is.null(node$impr), NA, node$impr),
                        "impr.target" = ifelse(is.null(node$impr.target), NA, node$impr.target),
                        "impr.relative" = ifelse(is.null(node$impr.relative), NA, node$impr.relative),
                        "split.feature" = ifelse(is.null(node$split.feature), "leafnode", node$split.feature),
                        "split.value" = ifelse(is.null(node$split.value), NA, node$split.value),
                        "child.type" = ifelse(is.null(node$child.type), "rootnode", node$child.type),
                        "size" = length(node$subset.idx), 
                        "guide.test" = ifelse(is.null(node$test.type), NA, node$test.type))
      }
      df
    })
  })
  
  list.split.criteria = list.clean(list.split.criteria, function(x) length(x) == 0L, TRUE)
  df.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  df.split.criteria = as.data.frame(do.call(rbind, df.split.criteria))
  n.final = length(which(df.split.criteria$child.type == "final"))
  df.split.criteria = df.split.criteria[df.split.criteria$child.type!="final",]
  df.split.criteria$id.node = 0:(nrow(df.split.criteria)-1)
  row.names(df.split.criteria) = df.split.criteria$id.node
  df.split.criteria[] = lapply(df.split.criteria, unlist)
  df.split.criteria = as.data.table(df.split.criteria)
  
  
  # Add unique parent ids
  if(nrow(df.split.criteria)>3){
    id.node.parent = c(0,0,0)
    for(n in 4:nrow(df.split.criteria)){
      split_parent = str_extract(df.split.criteria$child.type[n], "^.*(?=( &))")
      id.node.parent[n] = df.split.criteria[child.type == split_parent, id.node]
    }
    df.split.criteria$id.node.parent = id.node.parent
  } else{
    df.split.criteria$id.node.parent = 0
  }
  
  return(as.data.frame(df.split.criteria))
}

extract_models = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      if(is.null(node)) l = NULL
      else{
        l = list("depth" = node$depth, "id" = node$id,
                 "id.parent" = ifelse(is.null(node$id.parent), "rootnode", node$id.parent),
                 "child.type" = ifelse(is.null(node$child.type), "leafnode", node$child.type),
                 "model" = node[["model.fit"]],
                 "effects" = node$term.predictions.parent,
                 "subset.idx" = node$subset.idx)														  												
        
      }
      l
    })
  })

  list.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  list.split.criteria = list.split.criteria[!sapply(list.split.criteria,is.null)]
  names(list.split.criteria) = 0:(length(list.split.criteria) - 1)
  return(list.split.criteria)
}





# helper functions for tree splitting

# ---- helper splitting ----
# performs one split

split_parent_node = function(Y, X, n.splits = 1, min.node.size = 10, optimizer,
                             objective, fit, approximate, n.quantiles, penalization, 
                             fit.bsplines, df.spline, split.method, correction.factor, ...) {
  if(split.method == "slim"){
    # use all features as possible splitting variable
    z = colnames(X)
  } else if(split.method == "guide"){
    # find splitting variable through Chi-squared test
    optimizer = find_best_binary_split
    X = X %>% dplyr::select(where(~ n_distinct(.) > 1))
    z_guide = find_split_variable_guide(Y = Y, X = X,
                                        objective = objective, 
                                        fit = fit,
                                        optimizer = optimizer, 
                                        correction.factor = correction.factor)
    z = z_guide$z
    guide_type = z_guide$type
  }
  
  # search for beast split point for the potential partitioning variable(s) z
  split_point = find_split_point(Y = Y, X = X, z = z, n.splits = n.splits,
                                 min.node.size = min.node.size, 
                                 optimizer = optimizer,
                                 objective = objective, 
                                 fit = fit,
                                 approximate = approximate, 
                                 n.quantiles = n.quantiles, 
                                 penalization = penalization, 
                                 fit.bsplines = fit.bsplines,
                                 df.spline = df.spline)

  if(split.method == "guide"){
    # save if splitting variable was find through curvutare or through interaction test
    split_point$test.type = guide_type
  }
  
  return(split_point)
}


find_split_variable_guide = function(Y, X, objective, fit, optimizer, correction.factor, ...){
  model = fit(y = Y, x = X)
  residuals = Y - predict(model, X)
  # guide_test function is in defined in file "R\helper_guide.R"
  z_guide = guide_test(y = Y, x = X, residuals = residuals, xgroups = NULL, 
                       optimizer = optimizer, objective = objective, correction.factor = correction.factor)
  return(z_guide)
}



# find best split point over all potential splitting variables z
find_split_point = function(Y, X, z, n.splits = 1, min.node.size = 10, optimizer,
                            objective, fit, approximate = FALSE, n.quantiles, splitpoints = "quantiles", penalization = NULL, 
                            fit.bsplines = FALSE, df.spline = NULL, ...) {
# find best split point per splitting feature z
  opt.feature = lapply(z, function(feat) {
    t1 = proc.time()
    res = optimizer(xval = X[,feat], x = X, y = Y, n.splits = n.splits, min.node.size = min.node.size,
                    objective = objective, fit = fit, n.quantiles = n.quantiles, splitpoints = splitpoints, penalization = penalization, 
                    fit.bsplines = fit.bsplines, df.spline = df.spline, ...)
    t2 = proc.time()
    res$runtime = (t2 - t1)[[3]]
    return(res)
  })
  names(opt.feature) = z
  result = data.table::rbindlist(lapply(opt.feature, as.data.frame), idcol = "feature")
  result$best.split = result$objective.value == min(result$objective.value)
  result = result[best.split == TRUE]
  if (result$split.type == "numerical" & is.factor(result$split.points)){
    result$split.points = as.numeric(levels(result$split.points))[result$split.points]
  } else if (result$split.type == "numerical" & !is.factor(result$split.points)){
    result$split.points = as.numeric(result$split.points)
  } else if (result$split.type == "categorical"){
    result$split.points = as.character(result$split.points)
  }
  
  return(result)
}



generate_node_index = function(Y, X, result) {
  assert_data_table(result)
  feature = unique(result$feature[result$best.split])
  split.points = unlist(result$split.points[result$best.split])
  if (is.vector(X))
    xval = X else
      xval = X[, feature]
  
  cuts = c(min(xval), split.points, max(xval))
  sp = cut(xval, breaks = unique(cuts), include.lowest = TRUE)
  return(list(class = sp, index = split(seq_along(xval), sp)))
}



# performs and evaluates binary splits
find_best_binary_split = function(xval, x, y, n.splits = 1, min.node.size = 10, objective, fit, n.quantiles, splitpoints = "quantiles", ...) {
  if(length(unique(xval)) == 1){
    return(list(split.points = NA, objective.value = Inf, split.type = "categorical"))
  }
  assert_choice(n.splits, choices = 1)

  if(splitpoints == "quantiles"){
    # use different split candidates to perform split
    if (is.null(n.quantiles) & !is.factor(xval)){
      q = unique(xval)
    } else{
      q = generate_split_candidates(xval, n.quantiles = n.quantiles, min.node.size = min.node.size)
    }
  } else if(splitpoints == "mean"){
    q = mean(xval)
  }

  splits = vapply(q, FUN = function(i) {
    perform_split(i, xval = xval, x = x, y = y, min.node.size = min.node.size,
                  objective = objective, ...)
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  
  # select the split point yielding the minimal objective
  best = which.min(splits)
  if (is.list(q[best])){
    split.points = paste(q[best][[1]][[1]], collapse = ",")
    split.type = "categorical"
  } else {
    split.points = q[best]
    split.type = "numerical"
    
  }
  return(list(split.points = split.points, objective.value = splits[best], split.type = split.type))
}

# performs and evaluates binary splits with approximative efficient numerical algorithm (vgl. slim paper)
find_best_binary_split_approx = function(xval, x, y, n.splits = 1, min.node.size = 10, 
                                         objective, fit, n.quantiles, penalization, 
                                         fit.bsplines, df.spline, ...) {
  if(length(unique(xval)) == 1){
    return(list(split.points = NA, objective.value = Inf, split.type = "categorical"))
  }
  assert_choice(n.splits, choices = 1)

  
  # use different split candidates to perform split
  q = generate_split_candidates(xval, n.quantiles = n.quantiles, min.node.size = min.node.size)
  
  # assign intervalnr. according to split points
  if (is.numeric(xval)){
    node.number = findInterval(x = xval, vec = q, rightmost.closed = FALSE, left.open = TRUE)
    
  } else if (is.factor(xval)){
    node.number = xval
  }
  # create gram matrices for all bins
  gram.list = create_gramlist(x = x, y = y, bin = node.number, fit.bsplines = fit.bsplines, penalization = penalization, df.spline = df.spline)
  
  if (is.numeric(xval)){
    splits = perform_gram_splits_numeric(gram.list, x = x, min.node.size = min.node.size, penalization = penalization)
  } else if (is.factor(xval)){
    splits = perform_gram_splits_factor(gram.list, q = q, min.node.size = min.node.size, penalization = penalization)
  }
  
  
  # select the split point yielding the minimal objective
  best = which.min(splits)
  if (is.list(q[best])){
    split.points = paste(q[best][[1]][[1]], collapse = ",")
    split.type = "categorical"
  } else {
    split.points = q[best]
    split.type = "numerical"
  }
  return(list(split.points = split.points, objective.value = splits[best], split.type = split.type))
}


generate_split_candidates = function(xval, n.quantiles = NULL, min.node.size = 10) {
  assert_integerish(min.node.size, upper = floor((length(xval) - 1)/2))
  if (is.factor(xval)){
    xlevels = unique(xval)
    q = rapply(listParts(length(xlevels)), function(v) xlevels[v], how = "replace")
    q = q[lengths(q) == 2]
    
  } else {
    xval = sort.int(xval)
    xadj = xval[-c(1:min.node.size, (length(xval)-min.node.size+1):length(xval))]
    if (!is.null(n.quantiles)) {
      if (n.quantiles < 2){
        print(paste0("Minimum value for n.quantiles is 2"))
        n.quantiles = 2
      }

      qprobs = seq(0, 1, by = 1/n.quantiles)
      qprobs = qprobs[c(-1,-length(qprobs))]
      q = unique(quantile(xadj, qprobs, type = 1))
    } else {
      q = unique(xadj)
    }
    
    # use a value between two subsequent points
    q = adjust_split_point(q, xval)
    q = sort.int(q)
    q = get_closest_point(q, xval, min.node.size)
  }
  
  return(q)
}


create_gramlist = function(x, y, bin = node.number, fit.bsplines, penalization, df.spline){
  # create matrix of spline transformations of the features and use it as x
  if (fit.bsplines){
    spline.matrix = c()
    for(feature in colnames(x)){
      if (is.numeric(x[, feature])){
        new.spline.matrix = bs(x[,feature], df = (df.spline)*2, degree = 1)
        colnames(new.spline.matrix) = paste0(feature, "_bs_", colnames(new.spline.matrix))
        spline.matrix = cbind(spline.matrix,new.spline.matrix)
      } else {
        spline.matrix = cbind(spline.matrix, x[,feature])
        
      }
     
    }
    x = spline.matrix
  }
  data = as.data.frame(cbind(y, x)) 
  if(length(unique(bin)) == 1){
    data.list = list(data)
  } else {
    data.list = split(data, bin, drop = TRUE)
  }
  
  gram.list = lapply(data.list, function(bin){
    x = bin[!(colnames(bin) %in% c("y"))] 
    x = as.matrix(x)
    
    # add intercept column
    x = cbind(1,x)
    y = bin$y
    
    # create gram matrices
    xtx = t(x)%*%x
    xty = t(x)%*%y
    yty = t(y)%*%y
    
    n.bin = nrow(x)
    
    return(list(xtx = xtx, xty = xty, yty = yty, n.bin = n.bin))
  })
  
  return(gram.list)
}

# Performs a single split and measures the objective
perform_split = function(split.points, xval, x, y, min.node.size, objective, ...) {
  if (is.factor(xval)){
    node.number = ifelse(xval %in% split.points[[1]], 1L, 2L)
  } else {
    # assign intervalnr. according to split points
    node.number = findInterval(x = xval, split.points, rightmost.closed = TRUE) + 1
  }
  
  # compute size of each childnode
  node.size = tabulate(node.number)
  # if minimum node size is violated, return Inf
  # TODO: instead of returning Inf try to avoid that this happens by fixing split points
  if (min(node.size) < min.node.size)
    return(Inf)
  # compute objective in each interval and sum it up
  y.list = split(y, node.number)
  x.list = split(x, node.number) 
  
  res = vapply(seq_along(y.list), FUN = function(i) {
    objective(y = y.list[[i]], x = x.list[[i]])
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  
  return(sum(res))
}

# perform and evaluate splits based on gram matrices
perform_gram_splits_numeric = function(gram.list,
                                       x,
                                       min.node.size, 
                                       penalization, 
                                       include.parent.loss = FALSE){
  n.bins = length(gram.list)
  
  # for test purposes
  if (n.bins == 1){
    xtx = gram.list[[1]]$xtx
    xty = gram.list[[1]]$xty
    yty = gram.list[[1]]$yty
    loss.total = calculate_loss_closed(xtx, xty, yty, nrow(x), min.node.size, penalization)
    splits = loss.total
  } else {
    # calculate aggregated gram matrices 
    cxtx.t = gram.list[[1]]$xtx
    cxty.t = gram.list[[1]]$xty
    cyty.t = gram.list[[1]]$yty
    for (b in 2:n.bins){
      cxtx.t = cxtx.t + gram.list[[b]]$xtx
      cxty.t = cxty.t + gram.list[[b]]$xty
      cyty.t = cyty.t + gram.list[[b]]$yty
    }
    loss.parent = calculate_loss_closed(cxtx.t, cxty.t, cyty.t, nrow(x), min.node.size, penalization)
   
    # calculate aggregated gram matrices for the first split (i.e. first bin in left node, rest in right node)
    cxtx.l = gram.list[[1]]$xtx
    cxtx.r = cxtx.t - cxtx.l 
    
    cxty.l = gram.list[[1]]$xty
    cxty.r = cxty.t - cxty.l
    
    cyty.l = gram.list[[1]]$yty
    cyty.r = cyty.t - cyty.l
    
    n.bin.l = gram.list[[1]]$n.bin
    n.bin.r = nrow(x) - n.bin.l
    loss.l = calculate_loss_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
    loss.r = calculate_loss_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
      
    loss.total = loss.l + loss.r

    splits = c(loss.total)
    
    
    # calculate the gram matrices and models of the remaining splits in a for loop
    if (n.bins > 2){
      for (s in 2:n.bins){
        cxtx.l = cxtx.l + gram.list[[s]]$xtx
        cxtx.r = cxtx.r - gram.list[[s]]$xtx
        cxty.l = cxty.l + gram.list[[s]]$xty
        cxty.r = cxty.r - gram.list[[s]]$xty
        cyty.l = cyty.l + gram.list[[s]]$yty
        cyty.r = cyty.r - gram.list[[s]]$yty
        
        n.bin.l = n.bin.l + gram.list[[s]]$n.bin
        n.bin.r = nrow(x) - n.bin.l
        
        loss.l = calculate_loss_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
        loss.r = calculate_loss_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
          
        loss.total = loss.l + loss.r
     
        splits = c(splits, loss.total)
      }
    }
  }
  
  # include parent.loss (only experimental)
  if (include.parent.loss){
    return(list(parent.loss = loss.parent, splits = splits))
  } else {
    return(splits)
  }

}


perform_gram_splits_factor = function(gram.list,
                                      q,
                                      min.node.size, 
                                      penalization, 
                                      include.parent.loss = FALSE){
  
  splits = c()
  for(split in 1:length(q)){
    bins.l = as.character(q[[split]][[1]])
    bins.r = as.character(q[[split]][[2]])
    cxtx.l = gram.list[[bins.l[1]]]$xtx
    cxty.l = gram.list[[bins.l[1]]]$xty
    cyty.l = gram.list[[bins.l[1]]]$yty
    
    n.bin.l = gram.list[[bins.l[1]]]$n.bin
    
    if(length(bins.l)>1){
      for(b in 2:length(bins.l)){
        cxtx.l = cxtx.l + gram.list[[bins.l[b]]]$xtx
        cxty.l = cxty.l + gram.list[[bins.l[b]]]$xty
        cyty.l = cyty.l + gram.list[[bins.l[b]]]$yty
        n.bin.l = n.bin.l + gram.list[[bins.l[b]]]$n.bin
        
      }
    }
    
    cxtx.r = gram.list[[bins.r[1]]]$xtx
    cxty.r = gram.list[[bins.r[1]]]$xty
    cyty.r = gram.list[[bins.r[1]]]$yty
    
    n.bin.r = gram.list[[bins.r[1]]]$n.bin
    
    if(length(bins.r)>1){
      for(b in 2:length(bins.r)){
        cxtx.r = cxtx.r + gram.list[[bins.r[b]]]$xtx
        cxty.r = cxty.r + gram.list[[bins.r[b]]]$xty
        cyty.r = cyty.r + gram.list[[bins.r[b]]]$yty
        n.bin.r = n.bin.r + gram.list[[bins.r[b]]]$n.bin
      }
    }
    
    loss.l = calculate_loss_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
    loss.r = calculate_loss_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
    
    loss.total = loss.l + loss.r
    
    splits = c(splits, loss.total)
    
  }
  return(splits)
}




calculate_loss_closed = function(xtx, xty, yty, n.bin, min.node.size, penalization){
  if (n.bin >= min.node.size){
    try({beta = solve(xtx) %*% xty}, silent = TRUE)
    
    if (!is.matrix(beta)) {
      try({beta = as.matrix(MASS::ginv(xtx) %*% xty )}, silent = TRUE)
      if (!is.matrix(beta)) {
        return(Inf)
      }
    }
    
    loss = yty - 2 * t(beta) %*% xty + t(beta) %*% xtx %*% beta
  } else {
    loss = Inf
  }
  return(loss)
}



adjust_nsplits = function(xval, n.splits) {
  # max. number of splits to be performed must be unique.x-1
  unique.x = length(unique(xval))
  if (n.splits >= unique.x)
    n.splits = unique.x - 1
  return(n.splits)
}



# replace split.points with closest value from xval taking into account min.node.size
get_closest_point = function(split.points, xval, min.node.size = 10) {
  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed if many duplicated values exist)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  # xadj = unique(xval[chunk.ind]) # unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))
  xadj = unique(xval[-c(1:min.node.size, (length(xval)-min.node.size+1):length(xval))])
  
  # xval = xval[-c(1, length(xval))]
  split.adj = numeric(length(split.points))
  for (i in seq_along(split.adj)) {
    d = xadj - split.points[i]
    ind.closest = which.min(abs(d))
    split.adj[i] = xadj[ind.closest]
    xadj = xadj[-ind.closest] # remove already chosen value
  }
  
  return(sort.int(split.adj))
}

adjust_split_point = function(split.points, xval) {
  # use a value between two subsequent points
  q = split.points
  x.unique = sort.int(unique(xval))
  ind = which(x.unique %in% q)
  ind = ind[ind < length(x.unique)]
  if (length(ind) != length(q)) {
    eps = min(diff(x.unique))/2
  } else {
    eps = (x.unique[ind + 1] - x.unique[ind])/2
  }
  q = q + eps #+ c(diff(q)/2, 0)
  q[q < x.unique[2]] = mean(x.unique[1:2])
  q[q > x.unique[length(x.unique) - 1]] = mean(x.unique[(length(x.unique) - 1):length(x.unique)])
  #q = q[q <= max(xval) & q >= min(xval)]
  return(q)
}








# ---- SLIM specific functions ----
# according to the slim paper
calculate_split_effects = function(term.predictions.parent, term.predictions, exclude = NULL){
  intersection = intersect(colnames(term.predictions.parent), colnames(term.predictions))
  if (!is.null(exclude)){
    intersection = intersection[intersection != exclude]
  } 
  if (length(intersection) == 1 | is.null(interaction)){
    return(intersection)
  } else{
    d = term.predictions.parent[,intersection] - term.predictions[,intersection]
    c = apply(d, MARGIN = 2, var)
    p = round(c/sum(c), 4)
    return(p)
  }
}

# calculate r squared
r_2 = function(y_true, y_hat){
  y_true = unlist(y_true)
  y_hat = unlist(y_hat)
  rss = sum((y_hat - y_true) ^ 2)  ## residual sum of squares
  tss = sum((y_true - mean(y_true)) ^ 2)  ## total sum of squares
  r_2 = 1 - rss/tss
  return(r_2)
}




# objectives and fitting functions (for each model three function are necessary)

get_model_lm = function(y, x, .family, .degree.poly, .fit.bsplines, .df.spline,
                        .exclude.categoricals, .type = "model", ...) {
  # only use features as regressors, whoch contain at least two different values
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  
  # if exclude.categoricals is set TRUE, categorical features are only used for splitting
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  
  fm_vec = c()
  
  for(i in 1:ncol(x)){
    xval = x[,i]
    xname = colnames(x)[i]
    if(.degree.poly > 1){
      # define polynomials
      if(is.numeric(xval) & length(unique(xval)) > .degree.poly+1){
        fm_vec[i] = paste0("poly(", xname, ", degree =", .degree.poly,", raw = TRUE)")
      } else{
        fm_vec[i] = xname
      } 
    } else if(.fit.bsplines){
      # define bspline transformations
      if(is.numeric(xval) & length(unique(xval)) > 2){
        fm_vec[i] = paste0("bs(", xname, ", df = ", .df.spline, ", degree = 1)")
      } else{
        fm_vec[i] = xname
        
      }
    } else{
      fm_vec[i] = xname
    }
  }

  fm = as.formula(paste("y ~", paste(fm_vec, collapse = "+")))
  data = cbind(y,x)
  model = lm(fm, data)
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_lm = function(y, x, .family, .degree.poly, .fit.bsplines = FALSE, .df.spline = 15,
                            .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  model = get_model_lm(y, x, .degree.poly = .degree.poly, 
                       .fit.bsplines = .fit.bsplines, .df.spline = .df.spline, .exclude.categoricals = .exclude.categoricals, .type = "model")
  loss = crossprod(model$residuals)
  return(loss)
}

# get term predictions (to visualize input-output relation and calculate interaction effect as in SLIM paper)
get_prediction_lm= function(model, x, .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  prediction = predict.lm(model, x, type = "terms")
  colnames(prediction) = colnames(x)

  return(data.frame(prediction))
}


# Reguralized regression with glmnet
get_model_glmnet = function(y, x, .family, .alpha, .degree.poly = 1, .df.spline, .fit.bsplines, .exclude.categoricals, .lambda, .df.max, .type = "model", ...) {
  y = unlist(y)
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  if (.degree.poly > 1) {
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]]) & (length(unique(x[[f]])) > .degree.poly+1)){
        for(d in 2:.degree.poly){
          x = cbind(x, "new" = x[[f]]^d)
          colnames(x)[which(names(x) == "new")] = paste0(f,"_",d)
        }
      }
    }
  } else if(.fit.bsplines){
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]])){
        splined = bs(x[[f]], df = .df.spline, degree = 1)
        df_splined = as.data.frame(splined)
        x[[f]]= NULL
        colnames(df_splined) = paste0(f,"_",colnames(df_splined))
        x = cbind(x, df_splined)
      }
    }
  }
  
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  }  
  if(is.null(.lambda)){
    fit = glmnet(x, y, nlambda=100)
    rss = deviance(fit)
    n = nrow(x)
    bic = n*log(rss/n) + log(n)*fit$df
    if(!is.null(.df.max)){
      # allow only lambda values which yield to df <= .df.max
      bic[fit$df > .df.max] = Inf
    } 
    lambda = fit$lambda[which.min(bic)]

  } else{
    if(!is.null(.df.max)){
      warning("df.max is ignored because lambda is not NULL. ")
    }
    lambda = .lambda
  }
  

  # cv.model = cv.glmnet(x, y, alpha = .alpha, family = .family)
  model = glmnet(x, y, alpha = .alpha, family = .family, lambda = lambda)
  if(.type == "fitted.values"){
    predictions = predict.glmnet(model, newx = x, s = lambda)
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_glmnet = function(y, x, .family , .alpha, .degree.poly = 1, .df.spline, .use.bsplines, .exclude.categoricals, .lambda, .df.max, ...) {
  model = get_model_glmnet(y, x, .family = .family , .alpha = .alpha,
                           .degree.poly = .degree.poly, .df.spline = .df.spline, .fit.bsplines = .fit.bsplines,
                           .exclude.categoricals, .exclude.categoricals = .exclude.categoricals,
                           .lambda = .lambda,
                           .df.max = .df.max, .type = "model")
  y = unlist(y)
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  if (.degree.poly > 1) {
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]]) & (length(unique(x[[f]])) > .degree.poly+1)){
        for(d in 2:.degree.poly){
          x = cbind(x, "new" = x[[f]]^d)
          colnames(x)[which(names(x) == "new")] = paste0(f,"_",d)
        }
      }
    }
  } else if(.fit.bsplines){
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]])){
        splined = bs(x[[f]], df = .df.spline, degree = 1)
        df_splined = as.data.frame(splined)
        colnames(df_splined) = paste0(f,"_",colnames(df_splined))
        x[[f]]= NULL
        x = cbind(x, df_splined)
      }
    }
  }
  
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  } 
  
  predictions = predict.glmnet(model, newx = x, s = model$lambda)
  coefs =  coef(model, s = "lambda.min")

  if(.alpha == 1){
    loss = sum((y - predictions)^2) + model$lambda * sum(abs(coefs[-1]))
  } else if (.alpha == 0){
    loss = sum((y - predictions)^2) + model$lambda * sum((coefs[-1])^2)
  }

  
  return(loss)
}

get_prediction_glmnet = function(model, x, .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  }  
  prediction = t(t(x)*as.vector(model$beta))
  return(prediction)
}


# LAD regression
get_model_lad = function(y, x, .exclude.categoricals, .type = "model", ...){
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  fm = as.formula(paste("y~", paste(names(x), collapse = "+")))
  data = cbind(y,x)
  model = rq(fm, data = data, tau = 0.5)
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_lad = function(y, x, .exclude.categoricals,  ...){
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  model = get_model_lad(y, x, .exclude.categoricals = .exclude.categoricals, .type = "model")
  sae = sum(abs(model$residuals))
  return(sae)
}

get_prediction_lad = function(model, x, .exclude.categoricals, ...) {
  prediction =  t(t(model$x)*as.vector(model$coefficiants))
  return(prediction)
}																	   



# GAM
get_model_gam = function(y, x, .family, .df.spline, .exclude.categoricals, .type = "model", ...) {
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  term = c()
  for (n in names(x)){
    newterm = n
    if (is.numeric(x[,n]) & length(unique(x[,n])) > 10){
      newterm = paste0("s(", n, ")")    
    }
    term = c(term, newterm)
  }
  fm = as.formula(paste("y ~", paste(term, collapse = "+")))
  data = cbind(y,x)
  model = gam(formula = fm, data = data, family = .family, method = "REML")
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_gam = function(y, x, .family, .df.spline, .exclude.categoricals,  ...) {
  model = get_model_gam(y, x, .family = .family, .df.spline = .df.spline, .exclude.categoricals = .exclude.categoricals, .type = "model")
  loss = crossprod(residuals(model))
  return(loss)
}

get_prediction_gam = function(model, x, .exclude.categoricals, ...) {
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  prediction = predict.gam(model, x, type = "terms")
  colnames(prediction) = colnames(x)
  return(prediction)
}



