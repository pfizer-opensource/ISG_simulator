# power plot No.2: n-delta table
f_pwr_par_plot_2 = function(n_v = 2:10, sd = sd, power_fix = power_fix, n_trt = n_trt){
  
  delta_v  = rep(NA, length(n_v))
  
  for(i in 1: length(n_v)){
    delta_v[i] = delta_hc_par(n = n_v[i], sd = sd, K = n_trt, alpha = 0.05, power = power_fix, alternative = "two.sided")
  }
  
  df <- data.frame(delta = delta_v, n = n_v)
  df$label <- paste0("n: ", round(df$n, 2), "<br>Delta: ", round(df$delta, 2))
  
  plot = ggplot(df, aes(y = delta, x = n, label = label)) +
    geom_point(color = "blue", size = 3) +
    geom_text(aes(label = sprintf("(%.1f, %.1f)", n, delta)), 
          vjust = -0.6, hjust = 0.5, size = 5) +
    labs(x     = "Sample Size Per Arm (n)",
         y     = "Effect Size (Delta)",
         title = "Sample Size vs Effect Size Curve",
          subtitle = paste0("Target Power: ", power_fix)
    ) +
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
