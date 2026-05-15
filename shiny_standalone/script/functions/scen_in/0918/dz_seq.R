# seq
{ 
  set.seed(seed)
  
  n_grp_seq           <- n_trt-1                                                                 # Number of groups in Sequential Design
# n_per_grp_seq       <- n_trt                                                                   # Number of animals per group in Sequential Design (- for a square design, this eqs the n_trt)
  n_ani_seq           <- n_per_grp_seq*n_grp_seq                                                 # Number of animals in Sequential Design
  
  design_seq     <-   cbind.data.frame(animal_code       = rep(sample(1:n_ani_pool,n_ani_seq,replace = F), each = 2),
                                       period_code       = rep(1:2 , (n_trt-1)*n_per_grp_seq),
                                       treatment_code    = as.numeric(unlist(strsplit(  paste(1,rep(seq(2,n_trt,1),each=n_per_grp_seq)) ,
                                                                                        " "
                                       )
                                       )
                                       ),
                                       grp               = rep(1:(n_trt-1) , each = n_per_grp_seq*2)
  )%>%
    mutate_at(vars(contains("code"),"grp"),as.factor) %>%
    left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
    arrange(grp,animal_code,period_code,treatment_code)
  
}



