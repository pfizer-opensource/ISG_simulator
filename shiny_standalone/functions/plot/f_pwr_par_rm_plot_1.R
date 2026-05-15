f_pwr_par_rm_plot_1 = function(delta_v, sd = sd, n_per_arm_fix = n_per_arm_fix, n_trt = n_trt){

  pwr_v = rep(NA, length(delta_v))

  for(i in 1:length(delta_v)){
    pwr_v[i] = pwr_hc_par_rm(
      n = n_per_arm_fix,
      delta = delta_v[i],
      sd = sd,
      K = n_trt,
      p = p,
      alpha = 0.05,
      alternative = "two.sided"
    )
  }

  df <- data.frame(delta = delta_v, pwr = pwr_v)
  df$label <- paste0("Delta: ", round(df$delta, 2), "<br>Power: ", round(df$pwr, 3))
  df_annotate <- df[df$delta %in% c(5, 10, 15, 20), ]

  plot = ggplot(df, aes(y = pwr, x = delta, label = label)) +
    geom_line(color = "blue", size = 1) +
    geom_point(data = df_annotate, aes(x = delta, y = pwr), color = "red", size = 3) +
    geom_text(data = df_annotate,
              aes(x = delta, y = pwr, label = sprintf("(%.0f, %.3f)", delta, pwr)),
              vjust = -0.8, hjust = 0.5, size = 5) +
    labs(x = "Effect Size (Delta)",
         y = "Power",
         title = "Parallel-RM: Power vs Effect Size",
          subtitle = paste0("Sample Size per Arm: ", n_per_arm_fix)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 14)
    )

  return(ggplotly(plot, tooltip = c("x", "y", "label")))
}
