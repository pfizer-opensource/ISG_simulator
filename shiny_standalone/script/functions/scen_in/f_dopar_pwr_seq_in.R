 
  #------------------------------------------------------------------
  # Sequential Design  
  #
  #            period
  #            1    2    
  #          .----------- 
  #       1  |  V    L        (n = n_per_grp_seq) <-4
  # group 2  |  V    M        (n = n_per_grp_seq) <-4
  #       3  |  V    H        (n = n_per_grp_seq) <-4
  #------------------------------------------------------------------
  
  
  #f_pwr_seq <- function( seed,
  #                       n_sim,
  #                       n_trt,
  #                       n_ani_pool,
  #                       n_ani_seq,
  #                       n_per_grp_seq){
 
  cores <- detectCores()
  cl    <- makeCluster(cores[1]-1)  
  registerDoParallel(cl)
  
  M = foreach (i         =  1:n_sim,
               .combine  =  cbind,
               .packages =  c("dplyr","emmeans")  ) %dopar% { 
                 
                 seed  = seed_vec[i]
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file,local = TRUE)  
                 #-------------------------------
          
                 
                 design_seq     <-   cbind.data.frame(animal_code       = rep(sample(1:n_ani_pool,n_ani_seq,replace = F), each = 2),
                                                      period_code       = rep(1:2 , (n_trt-1)*n_per_grp_seq),
                                                      treatment_code    = c( rep(c(1,2), n_per_grp_seq),
                                                                             rep(c(1,3), n_per_grp_seq),
                                                                             rep(c(1,4), n_per_grp_seq) ),
                                                      grp               = rep(1:(n_trt-1) , each = n_per_grp_seq*2)
                 )%>%
                   mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                   left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
                   arrange(grp,animal_code,period_code,treatment_code)
                 
                 
                 dat              = design_seq
                 dat_p1           = dat %>% filter(period_code==1) %>% dplyr::select(grp,animal_code,y) %>% rename(y1=y)
                 dat_p2           = dat %>% filter(period_code==2) %>% dplyr::select(grp,animal_code,y) %>% rename(y2=y)
                 dat_w            = dat_p1 %>% left_join(dat_p2,by=c("grp", "animal_code")) %>% mutate(d=y2-y1) 
                 mod              = lm(d~grp,dat_w)
                 rg              <- ref_grid(mod)
                 lsm             <- emmeans(rg,"grp")                                                        ;lsm  # lsmeans(mod,"treatment_code")
                 test            <- test(lsm,null=0,side=">")                                                ;test
                 test_df         <- as.data.frame(test)
                 ci              <- confint(lsm)                                                             ;ci
                 ci_df           <- as.data.frame(ci)                                                        ;ci_df
                 
                 p41             <-  test_df[nrow( test_df),ncol( test_df)]                            ;p41
                 d41             <- ci_df[nrow(ci_df),2]                                            ;d41
                 d41_l           <- ci_df[nrow(ci_df),ncol(ci_df)-1]                                ;d41_l
                 d41_u           <- ci_df[nrow(ci_df),ncol(ci_df)  ]                                ;d41_u
                 
                 p31             <-  test_df[nrow( test_df)-1,ncol( test_df)]                           ;p31
                 d31             <- ci_df[nrow(ci_df)-1,2]                                          ;d31
                 d31_l           <- ci_df[nrow(ci_df)-1,ncol(ci_df)-1]                              ;d31_l
                 d31_u           <- ci_df[nrow(ci_df)-1,ncol(ci_df)  ]                              ;d31_u
                 
                 p21             <- test_df[nrow( test_df)-2,ncol( test_df)]                        ;p21
                 d21             <- ci_df[nrow(ci_df)-2,2]                                          ;d21
                 d21_l           <- ci_df[nrow(ci_df)-2,ncol(ci_df)-1]                              ;d21_l
                 d21_u           <- ci_df[nrow(ci_df)-2,ncol(ci_df)  ]                              ;d21_u
                 
                 smr             = summary(mod)
                 rmse            = mean(smr$residuals^2)
                 v_wt            = rmse
                 # anova           = anova(mod)
                 #  RMSE            = sqrt(anova$`Mean Sq`[2])
                 
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
                   
                   sqrt(v_wt)    
                 )
                 
               }
  stopCluster(cl)
  
  M           <- as.data.frame(t(M))
  colnames(M) <- c('p41_nsim','d41_nsim', 'd41_l_nsim','d41_u_nsim','bias41_nsim','p.cover41_id',
                   'p31_nsim','d31_nsim', 'd31_l_nsim','d31_u_nsim','bias31_nsim','p.cover31_id',
                   'p21_nsim','d21_nsim', 'd21_l_nsim','d21_u_nsim','bias21_nsim','p.cover21_id',
                   'v_wt_nsim')
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
               
               v_wt    = mean(v_wt_nsim)
  )
  
  
  
  #  return(list )
  
  #}
  
 