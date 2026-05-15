f_pwr_seq_plot_2 = function(n_v = 2:10, sd = sd, power_fix = power_fix, n_trt = n_trt){

  delta_v = rep(NA, length(n_v))

  for(i in 1:length(n_v)){
    delta_v[i] = delta_hc_seq(
      n = n_v[i],
      sd = sd,
      K = n_trt,
      alpha = 0.05,
      power = power_fix,
      alternative = "one.sided"
    )
  }

  df <- data.frame(delta = delta_v, n = n_v)
  df$label <- paste0("n: ", round(df$n, 2), "<br>Delta: ", round(df$delta, 2))

  plot = ggplot(df, aes(y = delta, x = n, label = label)) +
    geom_point(color = "blue", size = 3) +
    geom_text(aes(label = sprintf("(%.1f, %.1f)", n, delta)),
          vjust = -0.6, hjust = 0.5, size = 5) +
    labs(x = "Sample Size per Sequence (n)",
         y = "Effect Size (Delta)",
         title = "Sequential: Sample Size vs Effect Size",
          subtitle = paste0("Target Power: ", power_fix)) +
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

f_pwr_seq_points_tb_2 = function(sd, power_fix, n_trt, n_v = 2:10) {
  delta_v = rep(NA, length(n_v))
  for (i in seq_along(n_v)) {
    delta_v[i] = delta_hc_seq(n = n_v[i], sd = sd, K = n_trt, alpha = 0.05, power = power_fix, alternative = "one.sided")
  }
  x_row <- c("Sample Size (n)", as.character(n_v))
  y_row <- c("Effect Size (delta)", as.character(round(delta_v, 2)))
  tb <- as.data.frame(rbind(x_row, y_row), stringsAsFactors = FALSE)
  colnames(tb) <- c("Metric", as.character(n_v))
  tb
}
