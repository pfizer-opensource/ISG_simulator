# parallel
{ 
  set.seed(seed)
  
  n_grp_par           <- n_trt                                                                   # Number of groups in Parallel   Design
# n_per_grp_par       <- n_trt                                                                   # Number of animals per group in Parallel   Design (- for a square design, this eqs the n_trt)
  n_ani_par           <- n_per_grp_par*n_grp_par                                                 # Number of animals in Parallel Design
  
  design_par     <- cbind.data.frame(animal_code     = sample(1:n_ani_pool, n_ani_par,replace = F),
                                     period_code     = 1,
                                     treatment_code  = rep(1:n_trt, each = n_per_grp_par),
                                     grp             = rep(1:n_trt, each = n_per_grp_par)
                      )%>%
                        mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                        left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
                        arrange(grp,animal_code,period_code,treatment_code)
  
}