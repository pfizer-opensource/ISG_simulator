 
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
 
  is_shinyapps_runtime <- identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps") || nzchar(Sys.getenv("SHINY_PORT"))
  max_workers <- as.integer(Sys.getenv("SHINY_MAX_WORKERS", if (is_shinyapps_runtime) "1" else "2"))
  if (is.na(max_workers) || max_workers < 1) {
    max_workers <- 1
  }

  if (is_shinyapps_runtime && n_sim > 500) {
    n_sim <- 500
    seed_vec <- seed_vec[seq_len(n_sim)]
  }

  cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores) || cores < 2) {
    workers <- 1
  } else {
    workers <- min(max_workers, max(1, cores - 1))
  }

  cl <- NULL
  if (workers > 1) {
    cl <- parallel::makeCluster(workers)
    doParallel::registerDoParallel(cl)
  } else {
    foreach::registerDoSEQ()
  }
  
  M = foreach (i         =  1:n_sim,
               .combine  =  cbind,
               .packages =  c("dplyr","emmeans")  ) %dopar% { 
                 
                 seed  = seed_vec[i];set.seed(seed)
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file,local = TRUE)  
                 #-------------------------------
          
                 
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
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  
  M           <- as.data.frame(t(M))
  
  source("script/functions/scen_in/0918/outpwr_conditional.R", local=TRUE)
  
  
  #  return(list )
  
  
  #}
  