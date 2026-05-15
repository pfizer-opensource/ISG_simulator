f_pwr_seq_curve_data_1 = function(delta_v, sd, n_per_seq_fix, n_trt){

  pwr_v = rep(NA, length(delta_v))

  for(i in 1:length(delta_v)){
    pwr_v[i] = pwr_hc_seq(
      n = n_per_seq_fix,
      delta = delta_v[i],
      sd = sd,
      K = n_trt,
      alpha = 0.05,
      alternative = "one.sided"
    )
  }

  data.frame(delta = delta_v, pwr = pwr_v)
}

f_pwr_seq_points_tb_1 = function(sd, n_per_seq_fix, n_trt, x_points = c(5, 10, 15, 20)) {
  df_points <- f_pwr_seq_curve_data_1(
    delta_v = x_points,
    sd = sd,
    n_per_seq_fix = n_per_seq_fix,
    n_trt = n_trt
  )
  x_row <- c("Effect Size (x)", as.character(x_points))
  y_row <- c("Power (y)", as.character(round(df_points$pwr, 4)))
  tb <- as.data.frame(rbind(x_row, y_row), stringsAsFactors = FALSE)
  colnames(tb) <- c("Metric", as.character(x_points))
  tb
}

f_pwr_seq_plot_1 = function(delta_v, sd = sd, n_per_seq_fix = n_per_seq_fix, n_trt = n_trt){

  df <- f_pwr_seq_curve_data_1(
    delta_v = delta_v,
    sd = sd,
    n_per_seq_fix = n_per_seq_fix,
    n_trt = n_trt
  )

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
         title = "Sequential: Power vs Effect Size",
          subtitle = paste0("Sample Size per Sequence: ", n_per_seq_fix)) +
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
