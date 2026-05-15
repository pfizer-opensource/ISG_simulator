# asc
{ 
  set.seed(seed)
  
  n_grp_asc           <- 1                                                                       # Number of groups in Ascending  Design
# n_per_grp_asc       <- n_trt                                                                   # Number of animals per group in Ascending  Design (- for a square design, this eqs the n_trt)
  n_ani_asc           <- n_per_grp_asc*n_grp_asc                                                 # Number of animals in Ascending Design
  
  design_asc     <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_asc,replace = F),each = n_trt),
                                     period_code      = rep(1:n_trt  , n_per_grp_asc),
                                     treatment_code   = rep(1:n_trt, n_per_grp_asc),
                                     grp              = rep(n_grp_asc, each = n_per_grp_asc)
  )%>%
    mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
    arrange(grp,animal_code,period_code,treatment_code)
  
}



