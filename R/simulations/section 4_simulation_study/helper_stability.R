# helper functions to assess stability of trees through region compability measure

source("R/helper_general.R")


# function to create list of subregion indices from node predictions
create_index_list = function(node_ids){
  id_vec = as.character(sort(as.numeric(unique(node_ids))))
  index_list = list()
  for(id in id_vec){
    index_list[[id]] = which(node_ids == id)
  }
  return(index_list)
}

# calculate basic probability assignment 

bpa = function(region_list_r, region_list_f){
  m = sapply(region_list_r, function(r){
    # is decision region R_i in the set of all decision regions F?
    r_in_f = sapply(region_list_f, function(f){
      r_equal_f = FALSE
      if (length(r) == length(f)){
        if(all(sort(r) == sort(f))){
          r_equal_f = TRUE
          } 
      }
      return(r_equal_f)
    }) %>% any()
    
    if (r_in_f){
      m_i = length(r)/length(unlist(region_list_f))
    } else{
      m_i = 0
    }
    return(m_i)
  })
  return(m)
  
}

RC_id = function(m1,m2){
  v = m1-m2
  # delete zero entries (i.e. bpa for regions, which are identical in f1 and f2)
  # v = v[!sapply(v, function(v_i){identical(v_i, 0)})]
  sqrt(t(v)%*%v)
}

jaccard_index = function(set1, set2) {
  intersection = length(intersect(set1, set2))
  union = length(set1) + length(set2) - intersection
  return (intersection/union)
}

jaccard_matrix = function(region_list_r){
  jaccard_matrix = matrix(data = 0, nrow = length(region_list_r), ncol = length(region_list_r))
  for(i in 1:length(region_list_r)){
    for(j in 1:length(region_list_r)){
      jaccard_matrix[i,j] = jaccard_index(region_list_r[[i]], region_list_r[[j]])
    }
  }
  return(jaccard_matrix)
}

RC_jac = function(m1, m2, W){
  v = m1-m2
  # delete zero entries (i.e. bpa for regions, which are identical in f1 and f2)
  # v = v[!sapply(v, function(v_i){identical(v_i, 0)})]
  sqrt(t(v)%*%W%*%v)
}



RC = function(node_ids_1, node_ids_2, region_set_1 = NULL, region_set_2 = NULL){
  if(is.null(region_set_1) & is.null(region_set_2)){
    region_set_1 = create_index_list(node_ids_1)
    region_set_2 = create_index_list(node_ids_2)
  }
  # browser()
  m1 = bpa(c(region_set_1, region_set_2), region_set_1)
  m2 = bpa(c(region_set_1, region_set_2), region_set_2)
  # rc_id = RC_id(m1, m2)
  
  W = jaccard_matrix(c(region_set_1,region_set_2))
  rc_jac = RC_jac(m1, m2, W)
  return(rc_jac)
  # return(list(RC_id = rc_id, RC_jac = rc_jac))
  
}



# Adjusted Rand Index for fixed number of clusters (vgl. Alexander J. Gates and Yong-Yeol Ahn,The Impact of Random Models on Clustering Similarity, http://jmlr.org/papers/v18/17-039.html)

# ARI_fixed = (RI - E_fixed(RI))/(max(RI) - E_fixed(RI))
exp_ri = function(node_ids_1, node_ids_2, type = "fixed"){
  
  N = length(node_ids_1)
  if(type == "fixed"){
    K_A = length(unique(node_ids_1))
    K_B = length(unique(node_ids_2))
    
    if(N>100){
      # approximative
      E_RI = 1/(K_A*K_B) + (1-1/K_A) * (1-1/K_B)
    } else {
      #exact
      S_N_KA = copula::Stirling2(N, K_A)
      S_N_1_KA = copula::Stirling2(N-1, K_A)
      
      if(K_A == K_B){
        S_N_KB = S_N_KA
        S_N_1_KB = S_N_1_KA
      } else {
        S_N_KB = copula::Stirling2(N, K_B)
        S_N_1_KB = copula::Stirling2(N-1, K_B)
      }
      
      E_RI = (S_N_1_KA/S_N_KA)*(S_N_1_KB/S_N_KB) +
        (1-S_N_1_KA/S_N_KA)*(1-S_N_1_KB/S_N_KB)
    }
  } else if (type == "all"){
    #approximative
    E_RI = (log(N)/N)^2 + (1 - log(N)/N)^2
  }
  
  
  
  
  return(E_RI)
}
ari_uniform = function(node_ids_1, node_ids_2, type = "fixed"){
  ri = fossil::rand.index(as.numeric(node_ids_1), as.numeric(node_ids_2))
  e_ri = exp_ri(node_ids_1, node_ids_2, type = type)
  ari = (ri-e_ri)/(1-e_ri)
  return(ari)
}


# source("R/simulations/batchtools/simulation_setting_definition.R")
# source("R/helper_general.R")
# source("R/helper_guide.R")
# source("R/tree_splitting_slim.R")
# 
# 
# 
# data_linear = create_sim_data(n = 1000, type = "basic_linear_smooth")$data
# 
# sample1 = sample(1:nrow(data_linear), 700, replace = FALSE)
# data_linear_1  = data_linear[sample1, ]
# x_linear_1 = data_linear_1[,colnames(data_linear_1) != "y"]
# y_linear_1 = data_linear_1$y
# slim_linear_1 = compute_tree_slim(y = y_linear_1, x = x_linear_1, n.split = 4, split.method = "slim", impr.par = 0.2)
# split_slim_linear_1 = extract_split_criteria(slim_linear_1)
# regions_1 = predict_slim(slim_linear_1, data_linear, type = "node")
# # table(regions_1)
# 
# sample2 = sample(1:nrow(data_linear), 700, replace = FALSE)
# data_linear_2  = data_linear[sample2, ]
# x_linear_2 = data_linear_2[,colnames(data_linear_2) != "y"]
# y_linear_2 = data_linear_2$y
# slim_linear_2 = compute_tree_slim(y = y_linear_2, x = x_linear_2, n.split = 4, split.method = "slim", impr.par = 0.2)
# split_slim_linear_2 = extract_split_criteria(slim_linear_2)
# regions_2 = predict_slim(slim_linear_2, data_linear, type = "node")
# # table(regions_2)
# 
# index_list_1 = create_index_list(regions_1)
# index_list_2 = create_index_list(regions_2)
# 
# m1 = bpa(c(index_list_1, index_list_2), index_list_1)
# m2 = bpa(c(index_list_1, index_list_2), index_list_2)
# RC_id(m1, m2)
# 
# W = jaccard_matrix(c(index_list_1,index_list_2))
# RC_jac(m1, m2, W)
# 
# 
# 
# RC(regions_1, regions_2)$RC_jac
# adj.rand.index(regions_1, regions_2)
# 

# 
# RC(rep_1, rep_2)$RC_jac
# adj.rand.index(rep_1, rep_2)
# 
# 
# 
# 
# 
# F1 = list(c(1,2,3), c(4,5,6), c(7,8,9,10,11,12))
# F2 = list(c(1,2,3), c(4,7,8,12), c(5,6,9,10,11))
# 
# m1 = bpa(c(F1, F2), F1)
# m2 = bpa(c(F1, F2), F1)
# 
# 
# RC(node_ids_1 = NULL, node_ids_2 = NULL, region_set_1 = F1, region_set_2 = F2)
# 
# 
# 
# 
