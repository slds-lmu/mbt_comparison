reduce_trees_nonlinear = function(ades, pdes, savedir, reg){
  if(!is.null(ades)){
    experiments = merge(ades, pdes, by=NULL)
  } else{
    experiments = pdes
  }
  if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)
  
  result = data.table()

  for(exp in 1:nrow(experiments)){
    # find all job ids which contain repititions of the experiment
    pars = unwrap(getJobPars(reg = reg))
    toreduce = ijoin(experiments[exp,], pars)
    # toreduce = ijoin(toreduce$job.id, findDone(reg = reg))
    reduce = function(res) rbind(res)
    res = reduceResultsDataTable(ids = toreduce$job.id, fun = reduce, reg = reg)
    res_list = lapply(1:nrow(res), function(job){
      job.id = rep(res[job,job.id], nrow(res[job,result][[1]]))
      job.df = cbind(job.id, res[job,result][[1]])
      return(job.df)
    })
    res_df = data.table(do.call("rbind", res_list))
    saveRDS(res_df, paste0(savedir, exp, "_res_experiments.rds" ))
    

    measure_cols = c("mse_train", "r2_train", "mse_test", "r2_test", "share_main_effect_split", "df", "time")    
    group_cols = c("model", "surrogate")
  
    # save gourp result data
    res_df[, config_id:=.GRP,by = group_cols]
    
    
    # summarize results
    res_mean_exp = res_df[, lapply(.SD, function(col){mean(col, na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
    res_sd_exp = res_df[, lapply(.SD, function(col){sd(col, na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
    setnames(res_sd_exp, measure_cols, paste0(measure_cols, "_sd"))
    
    
    
    lower_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(col)-t_score*(sd(col)/sqrt(length(col)))
    }
    
    upper_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(col)+t_score*(sd(col)/sqrt(length(col)))
    }
    
    res_lower_bound_exp = res_df[, lapply(.SD, lower_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_lower_bound_exp, measure_cols, paste0(measure_cols, "_05"))
    res_upper_bound_exp = res_df[, lapply(.SD, upper_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_upper_bound_exp, measure_cols, paste0(measure_cols, "_95"))
    
    res_n_leaves = res_df[, .(n_leaves_min = min(n_leaves),
                              n_leaves_max = max(n_leaves)), by = group_cols]
    
    if("n_splitting_variables" %in% colnames(res_df)){
      res_discrete = res_df[, .(
        n_splitting_variables = mean(n_splitting_variables),
        n_splitting_variables_min = min(n_splitting_variables),
        n_splitting_variables_max = max(n_splitting_variables),
        n_leaves = mean(n_leaves),
        n_leaves_min = min(n_leaves),
        n_leaves_max = max(n_leaves)
        ), by = group_cols]
    }
    
    res_int_exp = ijoin(res_lower_bound_exp, res_upper_bound_exp, by = group_cols)
    res_int_exp = ijoin(res_int_exp, res_discrete, by = group_cols)
    res_mean_exp = ijoin(res_mean_exp, res_int_exp, by = group_cols)

    res_mean_exp$experiment_id = exp
    res_mean_exp = cbind(experiments[exp,], res_mean_exp)
    result_exp = ijoin(res_mean_exp, res_sd_exp)


    
    
    result = rbind(result, result_exp)

  }
  
  saveRDS(result, paste0(savedir, "result_summary.rds" ))
  
  return(result)
  
}

pdes = data.frame(n = c(4500), type = c("nonlinear_mixed"))
ades = data.frame(r2 = c(1,0.95,0.9), impr.par = c(0.1, 0.05, 0.05))
reg_nonlinear = loadRegistry("Data/simulations/section_4_simulation_study/nonlinear/batchtools/",
                             conf.file = NA)
savedir = "Data/simulations/section_4_simulation_study/nonlinear/results/"


reduce_trees_nonlinear(ades, pdes, savedir, reg_nonlinear)
