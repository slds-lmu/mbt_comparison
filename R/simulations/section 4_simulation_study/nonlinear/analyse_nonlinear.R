
result_nonlinear = readRDS("Data/simulations/section_4_simulation_study/nonlinear/results/result_summary.rds")


result_nonlinear[r2 == 0.9,.(surrogate, model, n_leaves, n_leaves_min, n_leaves_max, n_splitting_variables, n_splitting_variables_min, n_splitting_variables_max, share_main_effect_split,
               df, df_sd)] %>%
  arrange(., surrogate) %>%
  kbl(caption="Interpretability simulation results on 50 simulation runs as stand alone model on scenario nonlinear with n = 3000, impr = 0.1",
      format="latex",
      col.names = c("black box", "model", "n leaves", "n l min", "n l max","n split feat", "n sf min", "n sf max",
                    "share main effect split", "df", "sd df"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)

result_nonlinear[r2 == 0.9, .(surrogate, model, r2_train, r2_train_sd, r2_test, r2_test_sd, time)] %>%
  arrange(., surrogate) %>%
  kbl(caption="Performance simulation results on 50 simulation runs as stand alone model on scenario nonlinear with n = 3000, impr = 0.1",
      format="latex",
      col.names = c("black box", "MBT", "R2 train", "R2 train sd", "R2 test", "R2 test sd", "time"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)
