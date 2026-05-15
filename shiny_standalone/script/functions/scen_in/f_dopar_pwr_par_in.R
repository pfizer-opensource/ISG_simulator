 
  #------------------------------------------------------------------
  # Parallel Design (par)
  #
  #          period
  #             1    
  #          .-------------------
  #       1  |  V       (n = n_per_grp_par)
  # group 2  |  L       (n = n_per_grp_par)
  #       3  |  M       (n = n_per_grp_par)
  #       4  |  H       (n = n_per_grp_par)
  #------------------------------------------------------------------
  
  
  #f_pwr_par = function(seed,
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
               .packages =  c("dplyr","emmeans","lme4" )  ) %dopar% { 
              
                 seed  = seed_vec[i]
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file,local = TRUE)  
                 #-------------------------------
                 
                 design_par     <- cbind.data.frame(animal_code     = sample(1:n_ani_pool, n_ani_par,replace = F),
                                                    period_code     = 1,
                                                    treatment_code  = rep(1:n_trt, each = n_per_grp_par),
                                                    grp             = rep(1:n_trt  , each = n_per_grp_par)
                 )%>%
                   mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                   left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
                   arrange(grp,animal_code,period_code,treatment_code)
                 
                 
                 mod             <- lm(y ~ treatment_code , data = design_par)
                 rg              <- ref_grid(mod)
                 lsm             <- emmeans(rg,"treatment_code")                                    ;lsm  # lsmeans(mod,"treatment_code")
                 ctst            <- contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)                 ;ctst
                 ctst_df         <- as.data.frame( contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)) ;ctst_df
                 ci              <- confint(ctst)                                                   ;ci
                 ci_df           <- as.data.frame(ci)                                               ;ci_df
                 
                 p41             <- ctst_df[nrow(ctst_df),ncol(ctst_df)]                            ;p41
                 d41             <- ci_df[nrow(ci_df),2]                                            ;d41
                 d41_l           <- ci_df[nrow(ci_df),ncol(ci_df)-1]                                ;d41_l
                 d41_u           <- ci_df[nrow(ci_df),ncol(ci_df)  ]                                ;d41_u
                 
                 p31             <- ctst_df[nrow(ctst_df)-1,ncol(ctst_df)]                          ;p31
                 d31             <- ci_df[nrow(ci_df)-1,2]                                          ;d31
                 d31_l           <- ci_df[nrow(ci_df)-1,ncol(ci_df)-1]                              ;d31_l
                 d31_u           <- ci_df[nrow(ci_df)-1,ncol(ci_df)  ]                              ;d31_u
                 
                 p21             <- ctst_df[nrow(ctst_df)-2,ncol(ctst_df)]                          ;p21
                 d21             <- ci_df[nrow(ci_df)-2,2]                                          ;d21
                 d21_l           <- ci_df[nrow(ci_df)-2,ncol(ci_df)-1]                              ;d21_l
                 d21_u           <- ci_df[nrow(ci_df)-2,ncol(ci_df)  ]                              ;d21_u
                 
                 smr             = summary(mod)
                 rmse            = sqrt(mean(smr$residuals^2)) 
                 
                 anova           = anova(mod)
                 RMSE            = anova$`Mean Sq`[2]
                 v_bt            = RMSE # rmse
                 
                 c(p41,
                   d41,
                   d41_l,
                   d41_u,
                   d41-tau_vec[n_trt], 
                   (tau_vec[n_trt]>=d41_l & tau_vec[n_trt]<=d41_u),
                   
                   p31,
                   d31,
                   d31_l,
                   d31_u,
                   d31-tau_vec[n_trt-1] ,
                   (tau_vec[n_trt-1]>=d31_l & tau_vec[n_trt-1]<=d31_u),
                   
                   p21,
                   d21,
                   d21_l,
                   d21_u,
                   d21-tau_vec[n_trt-2] ,
                   (tau_vec[n_trt-2]>=d21_l & tau_vec[n_trt-2]<=d21_u),
                   
                   sqrt(v_bt)     
                 )
                 
                 
                 
               }
  stopCluster(cl)
  
  M           <- as.data.frame(t(M))
  colnames(M) <- c('p41_nsim','d41_nsim', 'd41_l_nsim','d41_u_nsim','bias41_nsim','p.cover41_id',
                   'p31_nsim','d31_nsim', 'd31_l_nsim','d31_u_nsim','bias31_nsim','p.cover31_id',
                   'p21_nsim','d21_nsim', 'd21_l_nsim','d21_u_nsim','bias21_nsim','p.cover21_id',
                   'v_bt_nsim')
  attach(M)
  
  list = list( d41       = mean(d41_nsim),
               d41_l     = mean(d41_l_nsim),
               d41_u     = mean(d41_u_nsim),
               bias41    = mean(bias41_nsim),
               power41   = sum(p41_nsim < 0.05) / n_sim,
               p.cover41 = sum(p.cover41_id) / n_sim,
               
               d31       = mean(d31_nsim),
               d31_l     = mean(d31_l_nsim),
               d31_u     = mean(d31_u_nsim),
               bias31    = mean(bias31_nsim),
               power31   = sum(p31_nsim < 0.05) / n_sim,
               p.cover31 = sum(p.cover31_id) / n_sim,
               
               d21       = mean(d21_nsim),
               d21_l     = mean(d21_l_nsim),
               d21_u     = mean(d21_u_nsim),
               bias21    = mean(bias21_nsim),
               power21   = sum(p21_nsim < 0.05) / n_sim,
               p.cover21 = sum(p.cover21_id) / n_sim,
               
               v_bt    = mean(v_bt_nsim)
  )
  #  return(list )
  
  
  #}
 