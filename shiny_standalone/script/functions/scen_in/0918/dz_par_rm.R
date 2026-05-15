# parallel RM
{ 
  set.seed(seed)
  
  n_grp_par           <- n_trt                                                                   # Number of groups in Parallel   Design
# n_per_grp_par       <- n_trt                                                                   # Number of animals per group in Parallel   Design (- for a square design, this eqs the n_trt)
  n_ani_par           <- n_per_grp_par_rm*n_grp_par                                                 # Number of animals in Parallel Design
  np                  <- n_trt
  
  design_par_rm  <- cbind.data.frame(animal_code     = rep(sample(1:n_ani_pool, n_ani_par,replace = F),each=np),
                                     period_code     = rep(c(1:np), n_ani_par),
                                     treatment_code  = rep(1:n_trt   , each = np*n_per_grp_par_rm),
                                     grp             = rep(1:n_trt   , each = np*n_per_grp_par_rm)
  )%>%  mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
    arrange(grp,animal_code,period_code,treatment_code) 
  
}

 