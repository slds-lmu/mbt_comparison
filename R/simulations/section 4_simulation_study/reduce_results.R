# reduce data simulation 
source("R/simulations/section_4_simulation_study/helper_stability.R")


reduce_trees = function(ades, pdes, savedir, reg){
  if(!is.null(ades)){
    experiments = merge(ades, pdes, by=NULL)
  } else{
    experiments = pdes
  }
  if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

  res_mean = data.table()
  res_sd = data.table()

  
  for(exp in 1:nrow(experiments)){
    # find all job ids which contain repetitions of the experiment
    pars = unwrap(getJobPars(reg = reg))
    toreduce = ijoin(experiments[exp,], pars)
    reduce = function(res) rbind(res)
    res = reduceResultsDataTable(ids = toreduce$job.id, fun = reduce, reg = reg)
    res_list = lapply(1:nrow(res), function(job){
      job.id = rep(res[job,job.id], nrow(res[job,result][[1]]))
      job.df = cbind(job.id, res[job,result][[1]])
      return(job.df)
    })
    res_df = data.table(do.call("rbind", res_list))
    
    measure_cols = c("mse_train", "r2_train", "mse_test", "r2_test", paste0("share_x",1:4), "n_leaves")   
    measure_cols = measure_cols[measure_cols %in% colnames(res_df)]
    
    group_cols = c("type", "n", "alpha", "impr", "surrogate", "mbt")
    group_cols = group_cols[group_cols %in% colnames(res_df)]
    
    if("rho" %in% colnames(res_df)){
      group_cols = c("type", "n", "surrogate", "mbt", "rho")
    }
    
    cols_unwrap = colnames(res_df)[sapply(res_df, is.list) & colnames(res_df) != "stability"]
    if(length(cols_unwrap>0)){
      res_df = unwrap(res_df, cols = cols_unwrap)
      setnames(res_df, paste0(cols_unwrap, ".1"),
               cols_unwrap)
    }
    
    res_df[, config_id:=.GRP,by = group_cols]
    
    
    # summarize results (calculate mean, sd, min, max, qunatiles, ... by group)
    group_cols = c(group_cols, "config_id")
    
    res_mean_exp = res_df[, lapply(.SD, function(col){mean(as.numeric(col), na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
    res_sd_exp = res_df[, lapply(.SD, function(col){sd(as.numeric(col), na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
  
    
    lower_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(as.numeric(col))-t_score*(sd(as.numeric(col))/sqrt(length(col)))
    }
    
    upper_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(as.numeric(col))+t_score*(sd(as.numeric(col))/sqrt(length(col)))
    }
    
    res_lower_bound_exp = res_df[, lapply(.SD, lower_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_lower_bound_exp, measure_cols, paste0(measure_cols, "_05"))
    res_upper_bound_exp = res_df[, lapply(.SD, upper_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_upper_bound_exp, measure_cols, paste0(measure_cols, "_95"))
    
    res_n_leaves = res_df[, .(n_leaves_min = min(as.numeric(n_leaves)),
                              n_leaves_max = max(as.numeric(n_leaves))), by = group_cols]
    
    res_int_exp = ijoin(res_lower_bound_exp, res_upper_bound_exp, by = group_cols)
    res_int_exp = ijoin(res_int_exp, res_n_leaves, by = group_cols)
    res_mean_exp = ijoin(res_mean_exp, res_int_exp, by = group_cols)
    
    if("n_splitting_variables" %in% colnames(res_df)){
      res_split_feat = res_df[, .(
        n_splitting_variables = mean(as.numeric(n_splitting_variables)),
        n_splitting_variables_min = min(as.numeric(n_splitting_variables)),
        n_splitting_variables_max = max(as.numeric(n_splitting_variables))), by = group_cols]
      res_mean_exp = ijoin(res_mean_exp, res_split_feat, by = group_cols)
      
    }


    # calculate rand indices (only for basic scenarios)
    if("stability" %in% colnames(res_df)){
      # create all possible pairs of simulation repititions (4950)
      pair_ids = combn(unique(res_df$job.id), 2, simplify = FALSE)
      
      stability_list = lapply(seq_along(pair_ids), function(p){
        pair = pair_ids[[p]]
        stability_df = data.frame(config_id = integer(), ri = double(),
                                  job.id = integer(), evaluationset_seed = integer(), stability_same_size = logical())
        
        # set seed to make results reproducable
        set.seed(p+10000)
        stability_index_set = sample(1:50000, 1000)
        for(conf in unique(res_df$config_id)){
          s1_region =  res_df[job.id == pair[[1]] & config_id == conf, stability][[1]][stability_index_set]
          s2_region =  res_df[job.id == pair[[2]] & config_id == conf, stability][[1]][stability_index_set]
          
          if(length(s1_region)>1){
            stability_same_size = (length(unique(s1_region)) == length(unique(s2_region)))
            
            if(stability_same_size){
              ri = rand.index(as.numeric(s1_region), as.numeric(s2_region))
              
              stability_df = rbind(stability_df,
                                   c(config_id = conf, ri = ri, job.id = pair[[1]],
                                     evaluationset_seed = p+10000, stability_same_size = stability_same_size),
                                   c(config_id = conf, ri = ri, job.id = pair[[2]],
                                     evaluationset_seed = p+10000, stability_same_size = stability_same_size))
            }
          }
        }
        
        colnames(stability_df) = c("config_id", "ri", "job.id", "evaluationset_seed", "stability_same_size")
        return(stability_df)
      })
      
      stability_df = data.table(do.call("rbind", stability_list))
  
      res_df[, ":="(stability = NULL)]
      res_save = ijoin(res_df, stability_df, by = c("job.id", "config_id"))
      
      # save (detailed) results inclusive rand indices
      saveRDS(res_save, paste0(savedir, exp, "_res_experiments.rds" ))
      
      stability_mean = stability_df[, .(ri = mean(ri, na.rm = TRUE),
                                        ri_05 = lower_bound(ri),
                                        ri_95 = upper_bound(ri)), by = config_id]
      
      
      stability_sd = stability_df[, lapply(.SD, function(col){sd(col, na.rm = TRUE)}), by = config_id, .SDcols = c("ri")]
      
      res_mean_exp = ljoin(res_mean_exp, stability_mean)
      res_sd_exp = ljoin(res_sd_exp, stability_sd)
      
    } else if(!("stability" %in% colnames(res_df))){
      saveRDS(res_df, paste0(savedir, exp, "_res_experiments.rds" ))
    }
    
    if("x_wrong" %in% colnames(res_df)){
      x_wrong_mean = res_df[, .(x_wrong = mean(as.logical(x_wrong))), by = config_id]
      res_mean_exp = ijoin(res_mean_exp, x_wrong_mean)
      
    }
    
    res_mean_exp$experiment_id = exp
    res_mean_exp = cbind(experiments[exp,], res_mean_exp)
    res_sd_exp$experiment_id = exp
    res_sd_exp = cbind(experiments[exp,], res_sd_exp)
    
    
    res_mean = rbind(res_mean, res_mean_exp, fill = TRUE)
    res_sd = rbind(res_sd, res_sd_exp, fill = TRUE)
    
  }
  
  
  saveRDS(list(mean = res_mean, sd = res_sd), paste0(savedir, "result_summary.rds" ))
  
  return(list(mean = res_mean, sd = res_sd))
  
}






