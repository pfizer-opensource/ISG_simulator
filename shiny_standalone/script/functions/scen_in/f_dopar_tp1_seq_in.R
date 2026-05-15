
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
 

#f_tp1_seq <- function( seed,
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
               .packages =  c("dplyr","emmeans")) %dopar% {
    
                 seed  = seed_vec[i]
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file, local = TRUE)  
                 #-------------------------------
                 
            design_seq     <-   cbind.data.frame(animal_code       = rep(sample(1:n_ani_pool,n_ani_seq,replace = F), each = 2),
                                                 period_code       = rep(1:2 , (n_trt-1)*n_per_grp_seq),
                                                 treatment_code    = c( rep(c(1,2), n_per_grp_asc),
                                                                        rep(c(1,3), n_per_grp_asc),
                                                                        rep(c(1,4), n_per_grp_asc) ),
                                                 grp               = rep(1:(n_trt-1) , each = n_per_grp_seq*2)
            )%>%
              mutate_at(vars(contains("code"),"grp"),as.factor) %>%
              left_join(pool_0, by = c("animal_code","period_code","treatment_code"))%>%
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
            p41             <-  test_df[nrow( test_df),ncol( test_df)]                           ;p41
            p31             <-  test_df[nrow( test_df)-1,ncol( test_df)]                         ;p31
            p21             <- test_df[nrow(test_df)-2 ,ncol(test_df)]                          ;p21
            
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
