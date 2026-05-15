plot_output_dir <- "/home/liub122/alpha/safety_pharm_design_compare/outputs/plot_preview"
if (!dir.exists(plot_output_dir)) {
  dir.create(plot_output_dir, recursive = TRUE)
}

save_plot_png <- function(file_name, plot_expr, width = 7, height = 5, res = 150) {
  png(filename = file.path(plot_output_dir, file_name), width = width, height = height, units = "in", res = res)
  on.exit(dev.off(), add = TRUE)
  
  p <- eval.parent(substitute(plot_expr))
  if (inherits(p, "ggplot")) {
    print(p)
  }
}

