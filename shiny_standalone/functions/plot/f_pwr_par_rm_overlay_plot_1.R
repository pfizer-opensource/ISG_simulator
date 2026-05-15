f_pwr_par_rm_overlay_curve_data_1 = function(delta_v, sd_bl, sd_no_bl, n_per_arm_fix, n_trt, p) {

  pwr_bl_v    = rep(NA, length(delta_v))
  pwr_no_bl_v = rep(NA, length(delta_v))

  for (i in 1:length(delta_v)) {
    pwr_bl_v[i] = pwr_hc_par_rm(
      n = n_per_arm_fix,
      delta = delta_v[i],
      sd = sd_bl,
      K = n_trt,
      p = p,
      alpha = 0.05,
      alternative = "two.sided"
    )
    pwr_no_bl_v[i] = pwr_hc_par_rm(
      n = n_per_arm_fix,
      delta = delta_v[i],
      sd = sd_no_bl,
      K = n_trt,
      p = p,
      alpha = 0.05,
      alternative = "two.sided"
    )
  }

  data.frame(
    delta = rep(delta_v, 2),
    pwr = c(pwr_bl_v, pwr_no_bl_v),
    group = rep(c("With baseline", "Without baseline"), each = length(delta_v))
  )
}

f_pwr_par_rm_overlay_points_tb_1 = function(sd_bl, sd_no_bl, n_per_arm_fix, n_trt, p, x_points = c(5, 10, 15, 20)) {
  df_points <- f_pwr_par_rm_overlay_curve_data_1(
    delta_v = x_points,
    sd_bl = sd_bl,
    sd_no_bl = sd_no_bl,
    n_per_arm_fix = n_per_arm_fix,
    n_trt = n_trt,
    p = p
  )

  x_row <- c("Effect Size (x)", as.character(x_points))
  y_bl_row <- c("Power (y) (With baseline)", as.character(round(df_points$pwr[df_points$group == "With baseline"], 4)))
  y_nobl_row <- c("Power (y) (Without baseline)", as.character(round(df_points$pwr[df_points$group == "Without baseline"], 4)))
  tb <- as.data.frame(rbind(x_row, y_bl_row, y_nobl_row), stringsAsFactors = FALSE)
  colnames(tb) <- c("Metric", as.character(x_points))
  tb
}

f_pwr_par_rm_overlay_plot_1 = function(delta_v, sd_bl = sd_bl, sd_no_bl = sd_no_bl, n_per_arm_fix = n_per_arm_fix, n_trt = n_trt, p = p) {

  df <- f_pwr_par_rm_overlay_curve_data_1(
    delta_v = delta_v,
    sd_bl = sd_bl,
    sd_no_bl = sd_no_bl,
    n_per_arm_fix = n_per_arm_fix,
    n_trt = n_trt,
    p = p
  )

  df$label <- paste0("Delta: ", round(df$delta, 2), "<br>Power: ", round(df$pwr, 3))
  df_annotate <- df[df$delta %in% c(5, 10, 15, 20), ]

  plot = ggplot(df, aes(y = pwr, x = delta, color = group, label = label)) +
    geom_line(linewidth = 1) +
    geom_point(data = df_annotate, aes(x = delta, y = pwr), color = "red", size = 2.5) +
    geom_text(
      data = df_annotate,
      aes(label = sprintf("(%.1f, %.3f)", delta, pwr)),
      size = 5,
      vjust = ifelse(df_annotate$group == "With baseline", -0.5, 1.2),
      check_overlap = TRUE
    ) +
    labs(
      x = "Effect Size (Delta)",
      y = "Power",
      title = "Parallel-RM: Power vs Effect Size",
      subtitle = paste0("Sample Size per Arm: ", n_per_arm_fix),
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
