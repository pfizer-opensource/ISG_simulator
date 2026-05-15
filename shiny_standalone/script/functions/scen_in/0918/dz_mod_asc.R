# mod_asc
{ 
  set.seed(seed)
  
  n_grp_mod_asc           <- 2
  n_ani_mod_asc           <- n_per_grp_mod_asc * n_grp_mod_asc
  
  design_mod_asc     <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_mod_asc,replace = FALSE),each = n_trt),
                                         period_code      = rep(rep(1:n_trt, n_per_grp_mod_asc), 2),
                                         treatment_code   = c(rep(1:n_trt, n_per_grp_mod_asc), rep(1, n_trt * n_per_grp_mod_asc)),
                                         grp              = rep(c(2,1), each = n_trt * n_per_grp_mod_asc)
  ) %>%
    mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code")) %>%
    arrange(grp,animal_code,period_code,treatment_code)
  
}
