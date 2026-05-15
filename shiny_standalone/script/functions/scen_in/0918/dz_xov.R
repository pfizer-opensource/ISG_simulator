# xov
{ 
  set.seed(seed)
  
  n_grp_xov           <- n_trt                                                                   # Number of groups in Crossover  Design
# n_per_grp_xov       <- 1                                                                       # Number of animals per group in Crossover  Design (- for a square design, this eqs the n_trt)
  n_ani_xov           <- n_per_grp_xov*n_grp_xov                                                 # Number of animals in Crossover Design
  
  design_xov    <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_xov,replace = F),each = n_trt ),
                                    period_code      = rep(1:n_trt, n_grp_xov),
                                    treatment_code   = as.vector(t(crossdes::williams(n_trt))),
                                    grp              = rep(1:n_trt , each = n_trt)
  )%>%
    mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
    arrange(grp,animal_code,period_code,treatment_code)
  

}



