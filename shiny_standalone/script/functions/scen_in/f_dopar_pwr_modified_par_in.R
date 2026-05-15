 
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
                 
                 design_modified_par     <- cbind.data.frame( animal_code     = rep( sample(1:n_ani_pool, n_ani_par,replace = F),2),
                                                              period_code     = rep(c(2,1),each = n_ani_par),
                                                              treatment_code  = c( rep(c(1,2,3,4), each = n_per_grp_par),rep(1,n_ani_par)),
                                                              grp             = rep( rep(1:n_trt  , each = n_per_grp_par),2)
                 )%>%
                   mutate_at( vars(contains("code"),"grp"),as.factor) %>%
                   left_join( pool, by = c("animal_code","period_code","treatment_code"))%>%
                   arrange( grp,animal_code,period_code)
                 
                 
                 # Modifed parallel design:
                 #
                 #   grp  period 1   period 2    
                 #---------------------------------
                 #    1     V11        V12          
                 #    2     V21        L            
                 #    3     V31        M
                 #    4     V41        H
                 # 
                 # Statistical Analysis Steps:
                 #
                 #  [1]. Analyze period effect: Get data V11, V12
                 
                     y_p1    = design_modified_par  %>% filter(grp==1, period_code==1) %>% select(y)
                     y_p2    = design_modified_par  %>% filter(grp==1, period_code==2) %>% select(y)
                     
                 #  [2]. Conduct paired t test to test period effect
                 
                     t        = t.test(y_p1$y, y_p2$y, paired = TRUE)
                     pvalue   = t$p.value
                 
                 #  [3]. if pvalue<=0.05, go to step 4  , else go to step 5
                 #
                 #  [4]. Since there's significant evidence for period effect, analyze period 2 data only:
                 #       Get period 2 data:  grp   period 2   
                 #                            1     V12
                 #                            2     L
                 #                            3     M
                 #                            4     H
                 #       and analyze as parallel design, using model: y ~ treatment_code + baseline
                 #
                 #  [5]. Since there's no evidence for period effect, analyze grp2-4 as sequential design:
                 #       Get data of grp2,3,4 :  grp   period 1  period 2
                 #                                2      V21        L
                 #                                3      V31        M
                 #                                4      V41        H
                 #       and analyze as sequential design, using model: d ~ grp, where d = y2 - y1
                 
                     if(pvalue<=0.05){
                   
                   # [4].
                   design_par     <- design_modified_par  %>% filter( period_code==2)
                   mod             <- lm(y ~ treatment_code + baseline, data = design_par)
                   rg              <- ref_grid(mod)
                   lsm             <- emmeans(rg,"treatment_code")                                    ;lsm  # lsmeans(mod,"treatment_code")
                   ctst            <- contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)                 ;ctst
                   ctst_df         <- as.data.frame( contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)) ;ctst_df
                   ci              <- confint(ctst)                                                   ;ci
                   ci_df           <- as.data.frame(ci)                                               ;ci_df
                   
                   p41             <- ctst_df[3 ,ncol(ctst_df)]                                ;p41
                   d41             <-   ci_df[3 ,2]                                            ;d41
                   d41_l           <-   ci_df[3 ,ncol(ci_df)-1]                                ;d41_l
                   d41_u           <-   ci_df[3 ,ncol(ci_df)  ]                                ;d41_u
                   
                   p31             <- ctst_df[2 ,ncol(ctst_df)]                              ;p31
                   d31             <-   ci_df[2 ,2]                                          ;d31
                   d31_l           <-   ci_df[2 ,ncol(ci_df)-1]                              ;d31_l
                   d31_u           <-   ci_df[2 ,ncol(ci_df)  ]                              ;d31_u
                   
                   p21             <- ctst_df[1 ,ncol(ctst_df)]                              ;p21
                   d21             <-   ci_df[1 ,2]                                          ;d21
                   d21_l           <-   ci_df[1 ,ncol(ci_df)-1]                              ;d21_l
                   d21_u           <-   ci_df[1 ,ncol(ci_df)  ]                              ;d21_u
                   
                   
                 }else{
                   
                   # [5].
                   dat             <- design_modified_par  %>% filter( grp !=1)
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
                   
                   p41             <- test_df[3,ncol( test_df)]                            ;p41
                   d41             <- ci_df[3,2]                                            ;d41
                   d41_l           <- ci_df[3,ncol(ci_df)-1]                                ;d41_l
                   d41_u           <- ci_df[3,ncol(ci_df)  ]                                ;d41_u
                   
                   p31             <-  test_df[2,ncol( test_df)]                           ;p31
                   d31             <- ci_df[2,2]                                          ;d31
                   d31_l           <- ci_df[2,ncol(ci_df)-1]                              ;d31_l
                   d31_u           <- ci_df[2,ncol(ci_df)  ]                              ;d31_u
                   
                   p21             <-  test_df[1,ncol( test_df)]                           ;p21
                   d21             <- ci_df[1,2]                                          ;d21
                   d21_l           <- ci_df[1,ncol(ci_df)-1]                              ;d21_l
                   d21_u           <- ci_df[1,ncol(ci_df)  ]                              ;d21_u
                   
                   
                 }
                 
                 
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
                   (tau_vec[n_trt-1]>=d31_l & tau_vec[n_trt-1]<=d31_u) ,
                   
                   p21,
                   d21,
                   d21_l,
                   d21_u,
                   d21-tau_vec[n_trt-2] ,
                   (tau_vec[n_trt-2]>=d21_l & tau_vec[n_trt-2]<=d21_u) 
                 )
                 
                 
                 
               }
  stopCluster(cl)
  
  M           <- as.data.frame(t(M))
  colnames(M) <- c('p41_nsim','d41_nsim', 'd41_l_nsim','d41_u_nsim','bias41_nsim','p.cover41_id',
                   'p31_nsim','d31_nsim', 'd31_l_nsim','d31_u_nsim','bias31_nsim','p.cover31_id',
                   'p21_nsim','d21_nsim', 'd21_l_nsim','d21_u_nsim','bias21_nsim','p.cover21_id' )
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
               p.cover21 = sum(p.cover21_id) / n_sim 
  )
  #  return(list )
  
  
  #}
 