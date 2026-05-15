
#------------------------------------------------------------------
# Ascending Design (Dose Escalation)
#
#                 period
#            1    2    3    4
#          -------------------
# group      1  |  V    L    M    H   (n = n_per_grp_asc)
#
#------------------------------------------------------------------
 
#f_tp1_asc <- function(seed,
#                      n_sim,
#                      n_trt,
#                      n_ani_pool,
#                      n_ani_asc,
#                      n_per_grp_asc){
  
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
               .packages =  c("dplyr","emmeans")) %dopar% {
    
                 seed  = seed_vec[i];set.seed(seed)
                 
                 #-------------------------------
                 # RUN scenario  
                 # source(file, local = TRUE)  
                 #-------------------------------
                 
    design_asc     <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_asc,replace = F),each = n_trt),
                                       period_code      = rep(1:n_trt  , n_per_grp_asc),
                                       treatment_code   = rep(1:n_trt  , n_per_grp_asc),
                                       grp              = rep(n_grp_asc, each = n_per_grp_asc)
                                      )%>%
                                        mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                                        left_join(pool_0, by = c("animal_code","period_code","treatment_code"))%>%
                                        arrange(grp,animal_code,period_code,treatment_code)
    
 
    mod             <- lm(y ~ treatment_code + animal_code, data = design_asc)
    rg              <- ref_grid(mod)
    lsm             <- emmeans(rg,"treatment_code")                                    ;lsm  # lsmeans(mod,"treatment_code")
    ctst            <- contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)                 ;ctst
    ctst_df         <- as.data.frame( contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1)) ;ctst_df
    ci              <- confint(ctst)                                                   ;ci
    ci_df           <- as.data.frame(ci)                                               ;ci_df
    p41             <- ctst_df[nrow(ctst_df),ncol(ctst_df)]                          ;p41
    p31             <- ctst_df[nrow(ctst_df)-1,ncol(ctst_df)]                          ;p31
    p21             <- ctst_df[nrow(ctst_df)-2 ,ncol(ctst_df)]                          ;p21
    
    c(p41,p31,p21 )
    
  }
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  
  M           <- as.data.frame(t(M))
  
  source("script/functions/scen_in/0918/outtp1_conditional.R", local=TRUE)
  
  
  #  return(list )
  
  
  #}
  