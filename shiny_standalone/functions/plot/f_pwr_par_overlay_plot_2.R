f_pwr_par_overlay_plot_2 = function(n_v = 2:10, sd_bl = sd_bl, sd_no_bl = sd_no_bl, power_fix = power_fix, n_trt = n_trt) {
  delta_bl_v    = rep(NA, length(n_v))
  delta_no_bl_v = rep(NA, length(n_v))

  for (i in 1:length(n_v)) {
    delta_bl_v[i]    = delta_hc_par(n = n_v[i], sd = sd_bl, K = n_trt, alpha = 0.05, power = power_fix, alternative = "two.sided")
    delta_no_bl_v[i] = delta_hc_par(n = n_v[i], sd = sd_no_bl, K = n_trt, alpha = 0.05, power = power_fix, alternative = "two.sided")
  }

  df <- data.frame(
    n = rep(n_v, 2),
    delta = c(delta_bl_v, delta_no_bl_v),
    group = rep(c("With baseline", "Without baseline"), each = length(n_v))
  )
  df$label <- paste0("n: ", round(df$n, 2), "<br>Delta: ", round(df$delta, 2))

  plot = ggplot(df, aes(y = delta, x = n, color = group, label = label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_text(
      aes(label = sprintf("(%.1f, %.1f)", n, delta)),
      size = 5,
      vjust = ifelse(df$group == "With baseline", -0.8, 1.2),
      check_overlap = TRUE
    ) +
    labs(
      x = "Sample Size Per Arm (n)",
      y = "Effect Size (Delta)",
      title = "Sample Size vs Effect Size Curve",
      subtitle = paste0("Target Power: ", power_fix),
      color = "Scenario"
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

f_pwr_par_overlay_points_tb_2 = function(sd_bl, sd_no_bl, power_fix, n_trt, n_v = 2:10) {
  delta_bl_v    = rep(NA, length(n_v))
  delta_no_bl_v = rep(NA, length(n_v))
  for (i in seq_along(n_v)) {
    delta_bl_v[i]    = delta_hc_par(n = n_v[i], sd = sd_bl,    K = n_trt, alpha = 0.05, power = power_fix, alternative = "two.sided")
    delta_no_bl_v[i] = delta_hc_par(n = n_v[i], sd = sd_no_bl, K = n_trt, alpha = 0.05, power = power_fix, alternative = "two.sided")
  }
  x_row <- c("Sample Size (n)", as.character(n_v))
  y_bl_row <- c("Effect Size δ (With Baseline)", as.character(round(delta_bl_v, 2)))
  y_nobl_row <- c("Effect Size δ (Without Baseline)", as.character(round(delta_no_bl_v, 2)))
  tb <- as.data.frame(rbind(x_row, y_bl_row, y_nobl_row), stringsAsFactors = FALSE)
  colnames(tb) <- c("Metric", as.character(n_v))
  tb
}
