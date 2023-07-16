
# Reduce results
# source("R/simulations/section_4_simulation_study/reduce_results.R")
# reg_lasso = loadRegistry("Data/simulations/section_4_simulation_study/lasso/batchtools/"
#                          ,conf.file = NA)
# 
# ades_lasso = NULL
# pdes_lasso = data.frame(n = c(3000), type = c("linear_smooth_lasso"))
# 
# savedir_lasso = "Data/simulations/batchtools/lasso/results/"
# 
# reduce_trees(ades_lasso, pdes_lasso, savedir_lasso, reg_lasso)
# 



# analyse results


colors_mbt =c("SLIM" = 'purple', "SLIM LASSO" = "plum", "SLIM Lasso max df 2" = "pink3", "SLIM Lasso max df 3" = "plum4", "GUIDE" = 'olivedrab3', 
              "MOB" ='skyblue', "CTree" = 'salmon')
colors_symmetry = c("high" = "darkgrey", "low" = "black")
colors_surrogate = c("standalone" = "white", "lm" = "lightgrey", xgboost = "cornsilk")


# analyse scenario noise features results
result_lasso = readRDS("Data/simulations/section_4_simulation_study/lasso/results/result_summary.rds")
result_lasso_sd = result_lasso$sd
setnames(result_lasso_sd, c("r2_train", "r2_test"), c("r2_train_sd", "r2_test_sd"))
result_lasso_mean = cbind(result_lasso$mean, result_lasso_sd[,.(r2_train_sd, r2_test_sd)])


result_lasso_mean[,.(surrogate, mbt, x_wrong, share_x3, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>%
  arrange(.,desc(surrogate), desc(mbt)) %>%
  kbl(caption="Mean simulation results on 250 simulation runs as stand alone model on scenario Linear Smooth - Noise features with n = 1000, alpha = 0.001, impr = 0.01",
      format="latex",
      col.names = c("black box","MBT", "x_wrong", "share x3", "n leaves", "n leaves min", "n leaves max", "R2 train", "R2 train sd", "R2 test", "R2 test sd"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)


save_dir = "Figures/simulations/section_4_simulation_study/lasso/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

res_lasso = readRDS("Data/simulations/section_4_simulation_study/lasso/results/1_res_experiments.rds")

# ---- standalone ----
# Performance

p_lasso_standalone_r2_train = ggplot(res_lasso[surrogate == "standalone" & str_detect(mbt, "SLIM") & n_leaves %in% 5:15],
                                       aes(x = as.factor(as.numeric(n_leaves)), 
                                           y = as.numeric(r2_train),
                                           fill = factor(mbt, 
                                                         levels = unique(res_lasso[surrogate == "standalone" & str_detect(mbt, "SLIM"), mbt]))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
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

ggexport(p_lasso_standalone_r2_train, filename = paste0(save_dir, "lasso_standalone_r2_train.png"), width = 800, height = 300)

p_lasso_standalone_r2_train_points = ggplot(res_lasso[surrogate == "standalone"  & n_leaves %in% 5:15],
                                     aes(x = as.factor(as.numeric(n_leaves)), 
                                         y = as.numeric(r2_test),
                                         color = factor(mbt, 
                                                        levels = unique(res_lasso[surrogate == "standalone" , mbt]))
                                     )) +
  geom_point() +
  scale_color_manual(values = c(colors_mbt, colors_surrogate)) +
  theme_bw() +
  labs(x="max leaf size", y="R2", color = "MBT")

# Symmetry

plot(res_lasso[mbt == "SLIM Lasso max df 2" & surrogate == "standalone" & n_leaves %in% 6:9, max_leaf_size],
     res_lasso[mbt == "SLIM Lasso max df 2" & surrogate == "standalone" & n_leaves %in% 6:9, r2_test])


res_lasso[, symmetry := ""]
res_lasso[n_leaves %in% 6:10 & max_leaf_size > 700, 
          symmetry := "low"]
res_lasso[n_leaves %in% 6:10 & max_leaf_size <= 700, 
          symmetry := "high"]

p_lasso_standalone_r2_test_slim = ggplot(res_lasso[surrogate == "standalone"  & n_leaves %in% 7:10],
                                         aes(x = as.factor(as.numeric(n_leaves)),
                                             y = as.numeric(r2_train),
                                             fill = factor(mbt, 
                                                           levels = c(unique(res_lasso[surrogate == "standalone" & str_detect(mbt, "SLIM"), mbt]), "GUIDE")),
                                             color = symmetry
                                         )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  scale_color_manual(values = colors_symmetry) +
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

ggexport(p_lasso_standalone_r2_test_slim, filename = paste0(save_dir, "lasso_standalone_r2_test_slim.png"), width = 800, height = 300)




p_lasso_standalone_r2_test = ggplot(res_lasso[surrogate == "standalone"  & n_leaves %in% 11:15],
                                         aes(x = as.factor(as.numeric(n_leaves)), 
                                             y = as.numeric(r2_test),
                                             fill = factor(mbt, 
                                                           levels = unique(res_lasso[surrogate == "standalone" , mbt]))
                                         )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.9825,
                        label = length(y)))},
    geom = "text",
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lasso_standalone_r2_test, filename = paste0(save_dir, "lasso_standalone_r2_test.png"), width = 900, height = 300)








# ---- lm ----

p_lasso_lm_r2_train = ggplot(res_lasso[surrogate == "lm" & str_detect(mbt, "SLIM") & n_leaves %in% 8:18],
                                     aes(x = as.factor(as.numeric(n_leaves)), 
                                         y = as.numeric(r2_train),
                                         fill = factor(mbt, 
                                                       levels = unique(res_lasso[surrogate == "lm" & str_detect(mbt, "SLIM"), mbt]))
                                     )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.98,
                        label = length(y)))},
    geom = "text",
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_lasso_lm_r2_train, filename = paste0(save_dir, "lasso_lm_r2_train.png"), width = 800, height = 300)


p_lasso_lm_r2_test = ggplot(res_lasso[surrogate == "lm" & str_detect(mbt, "SLIM") & n_leaves %in% 8:18],
                                    aes(x = as.factor(as.numeric(n_leaves)), 
                                        y = as.numeric(r2_test),
                                        fill = factor(mbt, 
                                                      levels = unique(res_lasso[surrogate == "lm" & str_detect(mbt, "SLIM"), mbt]))
                                    )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
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

ggexport(p_lasso_lm_r2_test, filename = paste0(save_dir, "lasso_lm_r2_test.png"), width = 800, height = 300)



