 

#f_tp1_par_rm_bl = function(seed,
#                     n_sim,
#                     n_trt,
#                     n_ani_pool,
#                     n_ani_par,
#                     n_per_grp_par){
  
  cores <- detectCores()
  cl    <- makeCluster(cores[1]-1)  
  registerDoParallel(cl)
  
  M = foreach (i         =  1:n_sim,
               .combine  =  cbind,
               .packages =  c("dplyr","emmeans","lme4")) %dopar% {
      
                 seed  = seed_vec[i]
                 
                 #-------------------------------
                 # RUN scenario  
                 #  source(file, local = TRUE )  
                 #-------------------------------
                 
    np             <- n_trt
    design_par_rm  <- cbind.data.frame(animal_code     = rep(sample(1:n_ani_pool, n_ani_par,replace = F),each=np),
                                       period_code     = rep(c(1:np), n_ani_par),
                                       treatment_code  = rep(c(1,2,3,4), each = np*n_per_grp_par),
                                       grp             = rep(1:n_trt   , each = np*n_per_grp_par)
    )%>%  mutate_at(vars(contains("code"),"grp"),as.factor) %>%
          left_join(pool_0, by = c("animal_code","period_code","treatment_code"))%>%
          arrange(grp,animal_code,period_code,treatment_code) 
                    
   
    mod             = lmer(y ~ baseline + treatment_code  + period_code + (1|animal_code),design_par_rm)     
    rg              = ref_grid(mod)
    lsm             <- emmeans(rg,"treatment_code")                                    ;lsm  # lsmeans(mod,"treatment_code")
    ctst            <- contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)                 ;ctst
    
    ctst_df         <- as.data.frame( contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)) ;ctst_df
    ci              <- confint(ctst)                                                   ;ci
    ci_df           <- as.data.frame(ci)                                               ;ci_df
    p41             <- ctst_df[nrow(ctst_df),ncol(ctst_df)]                          ;p41
    p31             <- ctst_df[nrow(ctst_df)-1,ncol(ctst_df)]                          ;p31
    p21             <- ctst_df[nrow(ctst_df)-2,ncol(ctst_df)]                          ;p21
    
    c(p41,p31 ,p21)
    
  }
  stopCluster(cl)
  
  M           <- as.data.frame(t(M))
  colnames(M) <- c('p41_nsim', 
                   'p31_nsim',
                   'p21_nsim')
  attach(M) 
  
  list = list(  tp1err41  = sum(p41_nsim < 0.05) / n_sim, 
                tp1err31  = sum(p31_nsim < 0.05) / n_sim,
                tp1err21  = sum(p21_nsim < 0.05) / n_sim
  )
  
  
#  return( list)
  
#}

  