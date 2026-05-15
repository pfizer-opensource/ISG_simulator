if (n_trt == 4) {
  colnames(M) <- c("p41_nsim", "p31_nsim", "p21_nsim")
  list <- list(
    tp1err41 = sum(M$p41_nsim < 0.05) / n_sim,
    tp1err31 = sum(M$p31_nsim < 0.05) / n_sim,
    tp1err21 = sum(M$p21_nsim < 0.05) / n_sim
  )
} else if (n_trt == 3) {
  colnames(M) <- c("p41_nsim", "p31_nsim")
  list <- list(
    tp1err41 = sum(M$p41_nsim < 0.05) / n_sim,
    tp1err31 = sum(M$p31_nsim < 0.05) / n_sim
  )
} else if (n_trt == 2) {
  colnames(M) <- c("p41_nsim")
  list <- list(
    tp1err41 = sum(M$p41_nsim < 0.05) / n_sim
  )
}
