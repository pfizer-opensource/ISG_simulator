# mod_asc1
{ 
  set.seed(seed)
  
  n_grp_mod_asc1           <- 2
  n_ani_mod_asc1           <- n_per_grp_mod_asc1 * n_grp_mod_asc1
  
  # Modified ascending design 1:
  #
  #   grp  period 1   period 2   period 3
  #-----------------------------------------------------
  #    1       V12         V13        V14
  #    2       L           M          H
  #
  design_mod_asc1     <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_mod_asc1,replace = FALSE),each = n_trt-1),
                                          period_code      = rep(rep(1:(n_trt-1), n_per_grp_mod_asc1), 2),
                                          treatment_code   = c(rep(2:(n_trt), n_per_grp_mod_asc1), rep(1, (n_trt-1) * n_per_grp_mod_asc1)),
                                          grp              = rep(c(2,1), each = (n_trt-1) * n_per_grp_mod_asc1)
  ) %>%
    mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code")) %>%
    arrange(grp,animal_code,period_code,treatment_code)
  
}
