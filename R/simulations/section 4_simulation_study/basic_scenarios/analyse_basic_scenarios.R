# reduce results basic scenarios

# source("R/simulations/section_4_simulation_study/reduce_results.R")
# reg_basic = loadRegistry("Data/simulations/section_4_simulation_study/basic_scenarios/batchtools/"
#                          ,conf.file = NA
#                          )
# 
# ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
# pdes_basic = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
# 
# savedir_basic = "Data/simulations/section_4_simulation_study/basic_scenarios/results/"
# 
# 
# reduce_trees(ades_basic, pdes_basic, savedir_basic, reg_basic)





colors_mbt =c("SLIM" = 'purple', "SLIM low symmetry" = "purple3", "GUIDE" = 'olivedrab3', "GUIDE low symmetry" = 'olivedrab4', 
              "MOB" ='skyblue', "CTree" = 'salmon')
colors_surrogate = c("standalone" = "white", "lm" = "lightgrey", xgboost = "cornsilk")



ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
pdes_basic = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
experiments = merge(ades_basic, pdes_basic, by = NULL)


# ----- 1. Linear Smooth -----

result_basic = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/result_summary.rds")
result_basic_mean = result_basic$mean
result_basic_sd = result_basic$sd
setnames(result_basic_sd, c("r2_train", "r2_test", paste0("share_x",1:4), "n_leaves"), 
         c("r2_train_sd", "r2_test_sd", paste0("share_x",1:4,"_sd"), "n_leaves_sd"))
result_basic_mean = cbind(result_basic_mean, 
                          result_basic_sd[,.(r2_train_sd, r2_test_sd, share_x2_sd, n_leaves_sd)])
result_basic_mean[type == "linear_mixed" & mbt == "SLIM" & n == 1500]

list.files("Data/simulations/section_4_simulation_study/basic_scenarios/results/")





# --- overview ----

save_dir = "Figures/simulations/section_4_simulation_study/basic_scenarios/linear_smooth/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

res_ls_n1500_alpha001 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/1_res_experiments.rds")

res_ls_n1500_alpha01 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/2_res_experiments.rds")

res_ls_n1500_alpha05 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/3_res_experiments.rds")




# lm

overview_sur = rbind(unique(res_ls_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_ls_n1500_alpha05[mbt %in% c("MOB", "CTree") ,
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))


p_ls_1000_lm_overview =  ggpairs(overview_sur[!is.na(ri)  & surrogate == "lm" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1))  +
  theme_bw()

ggexport(p_ls_1000_lm_overview, filename = paste0(save_dir, "ls_1000_lm_overview.png"), width = 600, height = 300)



# --- Interpretability ----
interpretabiliy_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)


# Standalone & as surrogate on lm
p_ls_1000_int = ggplot(interpretabiliy_ls[surrogate %in% c("standalone", "lm", "xgboost"),],
                                  aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = n_leaves,
                                      fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")), 
                                      color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  theme_bw() +
  labs(x="MBT", y="Number of leafnodes",
       fill = "black box")
ggexport(p_ls_1000_int, filename = paste0(save_dir, "ls_1000_int.png"), width = 800, height = 300)



# ---- Stability -----

stability_sur = rbind(res_ls_n1500_alpha05[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_ls_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)




# LM
p_ls_1000_lm_sta = ggplot(stability_sur[surrogate == "lm" & stability_same_size == TRUE 
                                       & n_leaves > 10 & n_leaves < 17
                                       ],
                                  aes(x = as.factor(n_leaves), 
                                      y = ri, 
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data =  function(y){
      return(data.frame(y = 0.8,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    fun = 0.75,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = colors_mbt) +
  theme_bw() +
  labs(x="number of leafnodes", y="RI", fill = "MBT")

ggexport(p_ls_1000_lm_sta, filename = paste0(save_dir, "ls_1000_lm_sta.png"), width = 800, height = 300)

# XGBoost
p_ls_1000_xgb_sta = ggplot(stability_sur[surrogate == "xgboost" & stability_same_size == TRUE 
                                        & n_leaves > 10 & n_leaves < 17
],
aes(x = as.factor(n_leaves), 
    y = ri, 
    fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data =  function(y){
      return(data.frame(y = 0.8,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    fun = 0.75,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = colors_mbt) +
  theme_bw() +
  labs(x="number of leafnodes", y="RI", fill = "MBT")

ggexport(p_ls_1000_xgb_sta, filename = paste0(save_dir, "ls_1000_xgboost_sta.png"), width = 800, height = 300)





# lm and xgboost

p_ls_1000_xgboost_sta = ggplot(stability_sur[surrogate %in% c("lm", "xgboost")& stability_same_size == TRUE & 
                                         n_leaves >= 10 & n_leaves <=14],
                          aes(x = as.factor(n_leaves), y = ri, 
                              color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                              fill = factor(surrogate, levels = c("lm", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.755,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = c("lm" = "white", "xgboost" = "lightgrey"))+
  theme_bw() +
  labs(x="MBT", y="RI", fill = "black box", color = "MBT")
ggexport(p_ls_1000_xgboost_sta, filename = paste0(save_dir, "ls_1000_standalone_xgboost_sta.png"), width = 1200, height = 450)



# ---- Performance ----

p_ls_1000_xgboost = ggpairs(unique(overview_sur[ surrogate == "xgboost",.(n_leaves, r2_train, r2_test, mbt, job.id, config_id)]),
                               columns = 1:2,        # Columns
                               aes(color = mbt, alpha = 0.1),
                               upper = "blank",
                               legend = c(1,1),
                               columnLabels = c("n leaves", "R2 train")) +
  theme_bw() +
  theme(legend.position = "right")

p_ls_1000_lm = ggpairs(unique(overview_sur[ surrogate == "lm",.(n_leaves, r2_train, r2_test, mbt, job.id, config_id)]),
                            columns = 1:2,        # Columns
                            aes(color = mbt, alpha = 0.1),
                            upper = "blank",
                            legend = c(1,1),
                            columnLabels = c("n leaves", "R2 train")) +
  theme_bw() +
  theme(legend.position = "right")
 

ggexport(p_ls_1000_standalone, filename = paste0(save_dir, "ls_1000_standalone_r2_nleaves.png"), width = 500, height = 250)




performance_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),
                                                    .(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves, max_leaf_size, sd_leaf_size)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),
                                                   .(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves, max_leaf_size, sd_leaf_size)]), use.names = FALSE)

plot(performance_ls[mbt == "SLIM", sd_leaf_size], performance_ls[mbt == "SLIM", r2_train])
performance_ls[, mbt_incl_assymmetry := mbt]
performance_ls[mbt %in% c("SLIM", "GUIDE") & n_leaves %in% 7:9 & max_leaf_size > 350, 
               mbt_incl_assymmetry := paste0(mbt, " low symmetry")]





# LM as underlying blackbox model
p_ls_1000_lm_r2_train = ggplot(performance_ls[surrogate == "lm" & n_leaves > 12 & n_leaves < 17],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.997,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_lm_r2_train, filename = paste0(save_dir, "ls_1000_lm_r2_train.png"), width = 800, height = 300)

p_ls_1000_lm_r2_test = ggplot(performance_ls[surrogate == "lm" & n_leaves > 11 & n_leaves < 17],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_test,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.996,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_lm_r2_test, filename = paste0(save_dir, "ls_1000_lm_r2_test.png"), width = 800, height = 300)


# xgboost

p_ls_1000_xgboost_r2_test = ggplot(performance_ls[surrogate == "xgboost" & n_leaves > 7 & n_leaves < 13],
                              aes(x = as.factor(n_leaves), 
                                  y = r2_test,
                                  fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                              )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.97,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_xgboost_r2_test, filename = paste0(save_dir, "ls_1000_xgboost_r2_test.png"), width = 800, height = 300)




# ------ 2. Linear abrupt ------



result_basic_mean[n == 1500 & type == "linear_abrupt" &  ((surrogate %in% c("lm", "xgboost") & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")) ),
                  .(surrogate, mbt, impr, alpha, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd, share_x2)] %>% 
  arrange(., desc(surrogate))%>%
  kbl(caption="Mean simulation results on 100 simulation runs as stand alone models on scenario linear categorical with sample size n = 1000 for different values of impr and alpha",
      format="latex",
      col.names = c("black box","MBT","impr", "alpha","mean n leaves", "n leaves min", "n leaves max", "mean R2 train", "sd R2 train", "mean R2 test", "sd R2 test", "share_x2"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)


# --- overview ----
save_dir = "Figures/simulations/section_4_simulation_study/basic_scenarios/linear_abrupt/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)


# alpha = 0.001 for mob and ctree
res_la_n1500_alpha001 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/7_res_experiments.rds")

# alpha = 0.05 i.e. impr = 0.05 for slim and guide
res_la_n1500_alpha05 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/9_res_experiments.rds")

# standalone
overview_la = rbind(unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))


overview_sur = rbind(unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_la_n1500_alpha05[mbt %in% c("MOB", "CTree") ,
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))


p_la_1000_standalone_overview =  ggpairs(overview_la[!is.na(ri) & stability_same_size == TRUE & surrogate == "standalone" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1)) +
  theme_bw()
  
ggexport(p_la_1000_standalone_overview, filename = paste0(save_dir, "la_1000_standalone_overview.png"), width = 600, height = 300)



# --- Interpretability ----
interpretabiliy_la = rbind(unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)


# Standalone & as surrogate on lm and xgboost
p_la_1000_int = ggplot(interpretabiliy_la[surrogate %in% c("standalone", "lm", "xgboost"),],
                                     aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), 
                                         y = n_leaves, 
                                         color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                                         fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  theme_bw() +
  labs(x="MBT", y="Number of leafnodes", fill = "black box")
ggexport(p_la_1000_int, filename = paste0(save_dir, "la_1000_int.png"), width = 800, height = 300)



# ---- Stability -----
stability_la = rbind(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)





# ---- Performance ----
performance_la = rbind(unique(res_la_n1500_alpha05[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]), use.names = FALSE)


# LM as underlying blackbox model
p_la_1000_lm_r2_train = ggplot(performance_la[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_train,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.97,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
 # ggtitle("Fidelity of MBTs on lm predictions Linear Abrupt Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_lm_r2_train, filename = paste0(save_dir, "la_1000_lm_r2_train.png"), width = 400, height = 150)



p_la_1000_lm_r2_test = ggplot(performance_la[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_test,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.96,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  #ggtitle("Fidelity of MBTs on lm predictions Linear Abrupt test", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_lm_r2_test, filename = paste0(save_dir, "la_1000_lm_r2_test.png"), width = 1500, height = 450)

p_la_1000_xgboost_r2_test = ggplot(performance_la[surrogate == "xgboost" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                              aes(x = as.factor(n_leaves), 
                                  y = r2_test,
                                  fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                              )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.942,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
 # ggtitle("Fidelity of MBTs on lm predictions Linear Abrupt test", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_xgboost_r2_test, filename = paste0(save_dir, "la_1000_xgboost_r2_test.png"), width = 800, height = 300)


# ----- 3. Linear Mixed -----

result_basic_mean[,":="(share_x1_x2 = share_x1 + share_x2,
                        share_x3_x4 = share_x3 + share_x4)]
result_basic_mean[n == 1500 & type == "linear_mixed" &  ((surrogate %in% c("lm","xgboost") & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")) ),
                  .(surrogate, mbt, impr, alpha, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd, share_x1_x2)] %>% 
  arrange(., desc(surrogate))%>%
  kbl(caption="Mean simulation results on 100 simulation runs as stand alone models on scenario linear mixed with sample size n = 1000 for different values of impr and alpha",
      format="latex",
      col.names = c("Black box", "MBT","impr", "alpha","mean n leaves", "n leaves min", "n leaves max", "mean R2 train", "sd R2 train", "mean R2 test", "sd R2 test", "share_x1_x2"),
      align="r",
      digits = 4,
      midrule = "" )  %>%
  kable_minimal(full_width = F)







# --- overview ---
save_dir = "Figures/simulations/section_4_simulation_study/basic_scenarios/linear_mixed/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

res_lm_n1500_alpha001 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/13_res_experiments.rds")
res_lm_n1500_alpha001[, ":="(share_x1_x2 = share_x1+share_x2,
                             share_x3_x4 = share_x3+share_x4)]

res_lm_n1500_alpha05 = readRDS("Data/simulations/section_4_simulation_study/basic_scenarios/results/15_res_experiments.rds")
res_lm_n1500_alpha05[, ":="(share_x1_x2 = share_x1+share_x2,
                             share_x3_x4 = share_x3+share_x4)]

# --- Overview ----
overview_lm = rbind(unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate, share_x3_x4)]),
                    unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree") & surrogate == "standalone",
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate, share_x3_x4)]))





# --- Interpretability ----
interpretabiliy_lm = rbind(unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone & as surrogate on lm and xgboost
p_lm_1000_int = ggplot(interpretabiliy_lm[surrogate %in% c("standalone", "lm", "xgboost"),],
                       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), 
                           y = n_leaves, 
                           color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                           fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  theme_bw() +
  labs(x="MBT", y="Number of leafnodes", fill = "black box")
ggexport(p_lm_1000_int, filename = paste0(save_dir, "lm_1000_int.png"), width = 800, height = 300)





# ---- Stability -----
stability_lm = rbind(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)


#  lm and xgboost
stability_sur = rbind(res_lm_n1500_alpha05[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)


p_lm_1000_xgboost_lm_sta = ggplot(stability_sur[surrogate %in% c("xgboost", "lm")& stability_same_size == TRUE 
                                                    & n_leaves > 12 & n_leaves < 17],
                          aes(x = as.factor(n_leaves), y = ri, 
                              color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                              fill = factor(surrogate, levels = c("xgboost", "lm")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.88,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = colors_surrogate)+
  theme_bw() +
  labs(x="MBT", y="RI", fill = "black box", color = "MBT")+
  theme(legend.position = "bottom")
ggexport(p_lm_1000_xgboost_lm_sta, filename = paste0(save_dir, "lm_1000_xgboost_lm_sta.png"), width = 1100, height = 450)


# ---- Performance ----
performance_lm = rbind(unique(res_lm_n1500_alpha05[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves, share_x3_x4)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves, share_x3_x4)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves, share_x3_x4)]), use.names = FALSE)


p_lm_1000_lm_r2_test = ggplot(performance_lm[surrogate == "lm" & n_leaves %in% 13:16],
                                      aes(x = as.factor(n_leaves), 
                                          y = r2_test,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.96,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_lm_r2_test, filename = paste0(save_dir, "lm_1000_lm_r2_test.png"), width = 800, height = 300)


p_lm_1000_xgboost_r2_test = ggplot(performance_lm[surrogate == "xgboost" & n_leaves %in% 13:16],
                                      aes(x = as.factor(n_leaves), 
                                          y = r2_test,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.96,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_xgboost_r2_test, filename = paste0(save_dir, "lm_1000_xgboost_r2_test.png"), width = 800, height = 300)


p_lm_1000_xgboost_share_x3_x4 = ggplot(performance_lm[surrogate == "xgboost" & n_leaves %in% 13:16],
                                      aes(x = as.factor(n_leaves), 
                                          y = share_x3_x4,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = -0.05,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="share x3 x4", fill = "MBT")

ggexport(p_lm_1000_xgboost_share_x3_x4, filename = paste0(save_dir, "lm_1000_xgboost_share_x3x4.png"), width = 800, height = 300)


# lm

p_lm_1000_lm_r2_train = ggplot(performance_lm[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.982,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_lm_r2_train, filename = paste0(save_dir, "lm_1000_lm_r2_train.png"), width = 800, height = 300)

p_lm_1000_lm_r2_test = ggplot(performance_lm[surrogate == "lm"& mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                                      aes(x = as.factor(n_leaves), 
                                          y = r2_test,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.976,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_lm_r2_test, filename = paste0(save_dir, "lm_1000_lm_r2_test.png"), width = 800, height = 300)


# xgboost
p_lm_1000_xgboost_r2_test = ggplot(performance_lm[surrogate == "xgboost"& mbt %in% c("SLIM", "GUIDE", "MOB", "CTree") &
                                                    n_leaves %in% 13:16],
                              aes(x = as.factor(n_leaves), 
                                  y = r2_test,
                                  fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                              )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.95,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_xgboost_r2_test, filename = paste0(save_dir, "lm_1000_xgboost_r2_test.png"), width = 800, height = 300)


