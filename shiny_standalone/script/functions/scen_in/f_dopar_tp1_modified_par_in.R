#------------------------------------------------------------------
# Parallel Design (par)
#
#                 period
#            1    2    3    4
#          .-------------------
#       1  |  V    V    V    V     (n = n_per_grp_par)
# group 2  |  L    L    L    L     (n = n_per_grp_par)
#       3  |  M    M    M    M     (n = n_per_grp_par)
#       4  |  H    H    H    H     (n = n_per_grp_par)
#------------------------------------------------------------------

  
#f_tp1_par = function(seed,
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
               .packages =  c("dplyr","emmeans")) %dopar% {
    
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
                   left_join( pool_0, by = c("animal_code","period_code","treatment_code"))%>%
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
                   mod             <- lm(y ~ treatment_code + baseline , data = design_par)
                   rg              <- ref_grid(mod)
                   lsm             <- emmeans(rg,"treatment_code")                                    ;lsm  # lsmeans(mod,"treatment_code")
                   ctst            <- contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)                 ;ctst
                   ctst_df         <- as.data.frame( contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)) ;ctst_df
                   ci              <- confint(ctst)                                                   ;ci
                   ci_df           <- as.data.frame(ci)                                               ;ci_df     
                   p41             <- ctst_df[3 ,ncol(ctst_df)]                                ;p41
                   p31             <- ctst_df[2 ,ncol(ctst_df)]                              ;p31
                   p21             <- ctst_df[1 ,ncol(ctst_df)]                              ;p21
                   
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
                   p31             <-  test_df[2,ncol( test_df)]                           ;p31 
                   p21             <-  test_df[1,ncol( test_df)]                           ;p21
                    
                   
                 }
                 
                 
               
    
    c(p41,p31,p21 )
    
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

  