if (n_trt == 4) {
  base_names <- c(
    "p41_nsim", "d41_nsim", "d41_l_nsim", "d41_u_nsim", "bias41_nsim", "p.cover41_id",
    "p31_nsim", "d31_nsim", "d31_l_nsim", "d31_u_nsim", "bias31_nsim", "p.cover31_id",
    "p21_nsim", "d21_nsim", "d21_l_nsim", "d21_u_nsim", "bias21_nsim", "p.cover21_id"
  )
  if (ncol(M) == length(base_names) + 2) {
    colnames(M) <- c(base_names, "v_wt_nsim", "v_bt_nsim")
  } else {
    colnames(M) <- c(base_names, "v_bt_nsim")
  }
  list <- list(
    d41 = mean(M$d41_nsim),
    d41_l = mean(M$d41_l_nsim),
    d41_u = mean(M$d41_u_nsim),
    bias41 = mean(M$bias41_nsim),
    power41 = sum(M$p41_nsim < 0.05) / n_sim,
    p.cover41 = sum(M$`p.cover41_id`) / n_sim,
    d31 = mean(M$d31_nsim),
    d31_l = mean(M$d31_l_nsim),
    d31_u = mean(M$d31_u_nsim),
    bias31 = mean(M$bias31_nsim),
    power31 = sum(M$p31_nsim < 0.05) / n_sim,
    p.cover31 = sum(M$`p.cover31_id`) / n_sim,
    d21 = mean(M$d21_nsim),
    d21_l = mean(M$d21_l_nsim),
    d21_u = mean(M$d21_u_nsim),
    bias21 = mean(M$bias21_nsim),
    power21 = sum(M$p21_nsim < 0.05) / n_sim,
    p.cover21 = sum(M$`p.cover21_id`) / n_sim,
    v_bt = mean(M$v_bt_nsim)
  )
} else if (n_trt == 3) {
  base_names <- c(
    "p41_nsim", "d41_nsim", "d41_l_nsim", "d41_u_nsim", "bias41_nsim", "p.cover41_id",
    "p31_nsim", "d31_nsim", "d31_l_nsim", "d31_u_nsim", "bias31_nsim", "p.cover31_id"
  )
  if (ncol(M) == length(base_names) + 2) {
    colnames(M) <- c(base_names, "v_wt_nsim", "v_bt_nsim")
  } else {
    colnames(M) <- c(base_names, "v_bt_nsim")
  }
  list <- list(
    d41 = mean(M$d41_nsim),
    d41_l = mean(M$d41_l_nsim),
    d41_u = mean(M$d41_u_nsim),
    bias41 = mean(M$bias41_nsim),
    power41 = sum(M$p41_nsim < 0.05) / n_sim,
    p.cover41 = sum(M$`p.cover41_id`) / n_sim,
    d31 = mean(M$d31_nsim),
    d31_l = mean(M$d31_l_nsim),
    d31_u = mean(M$d31_u_nsim),
    bias31 = mean(M$bias31_nsim),
    power31 = sum(M$p31_nsim < 0.05) / n_sim,
    p.cover31 = sum(M$`p.cover31_id`) / n_sim,
    v_bt = mean(M$v_bt_nsim)
  )
} else if (n_trt == 2) {
  base_names <- c("p41_nsim", "d41_nsim", "d41_l_nsim", "d41_u_nsim", "bias41_nsim", "p.cover41_id")
  if (ncol(M) == length(base_names) + 2) {
    colnames(M) <- c(base_names, "v_wt_nsim", "v_bt_nsim")
  } else {
    colnames(M) <- c(base_names, "v_bt_nsim")
  }
  list <- list(
    d41 = mean(M$d41_nsim),
    d41_l = mean(M$d41_l_nsim),
    d41_u = mean(M$d41_u_nsim),
    bias41 = mean(M$bias41_nsim),
    power41 = sum(M$p41_nsim < 0.05) / n_sim,
    p.cover41 = sum(M$`p.cover41_id`) / n_sim,
    v_bt = mean(M$v_bt_nsim)
  )
}
