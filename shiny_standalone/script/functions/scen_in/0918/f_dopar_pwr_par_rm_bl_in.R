
  #------------------------------------------------------------------
  # Parallel Design (par)
  #
  #                 period
  #            1    2    3    4
  #          .-------------------
  #       1  |  V    V    V    V     (n = n_per_grp_par_rm)
  # group 2  |  L    L    L    L     (n = n_per_grp_par_rm)
  #       3  |  M    M    M    M     (n = n_per_grp_par_rm)
  #       4  |  H    H    H    H     (n = n_per_grp_par_rm)
  #------------------------------------------------------------------
  
  #f_pwr_par_rm_bl = function(seed,
  #                     n_sim,
  #                     n_trt,
  #                     n_ani_pool,
  #                     n_ani_par,
  #                     n_per_grp_par_rm){
  
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
  
  # for (i in 1:n_sim) { 
  M = foreach (i         =  1:n_sim,
               .combine  =  cbind, 
               .packages =  c("dplyr","emmeans","lme4")  ) %dopar% { 
                 
                 seed  = seed_vec[i];set.seed(seed)
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file,local = TRUE)  
                 #-------------------------------
                 
                 np             <- n_trt
                 design_par_rm  <- cbind.data.frame(animal_code     = rep(sample(1:n_ani_pool, n_ani_par,replace = F),each=np),
                                                    period_code     = rep(c(1:np), n_ani_par),
                                                    treatment_code  = rep(1:n_trt   , each = np*n_per_grp_par_rm),
                                                    grp             = rep(1:n_trt   , each = np*n_per_grp_par_rm)
                 )%>%  mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                   left_join(pool, by = c("animal_code","period_code","treatment_code"))%>%
                   arrange(grp,animal_code,period_code,treatment_code) 
                 
                 
                 mod             = lmer(y ~ treatment_code + baseline + period_code + (1|animal_code),design_par_rm)     
                 rg              = ref_grid(mod)
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
                 vardf           = as.data.frame(smr$varcor)
                 anim_sd         = vardf$sdcor[1]                            
                 res_sd          = vardf$sdcor[2]                           # or : = smr$sigma  | rmse is a bit off: = sqrt(mean(smr$residuals^2)) 
                 sd_bl          = sd(design_par_rm$baseline)
                 
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
                   
                   sqrt(sd_bl^2+anim_sd^2),
                   res_sd    
                 )
                 
               }
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  
  M           <- as.data.frame(t(M))
  
  source("script/functions/scen_in/0918/outpwr_conditional.R", local=TRUE)
  
  
  #  return(list )
  
  
  #}
  