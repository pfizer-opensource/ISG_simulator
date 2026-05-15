
#------------------------------------------------------------------
# Ascending Design (Dose Escalation)
#
#                 period
#            1    2    3    4
#          -------------------
# group      1  |  V    L    M    H   (n = n_per_grp_asc)
#
#------------------------------------------------------------------

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

               design_mod_asc     <- cbind.data.frame(animal_code      = rep(sample(1:n_ani_pool, n_ani_mod_asc,replace = FALSE),each = n_trt),
                                                      period_code      = rep(rep(1:n_trt, n_per_grp_mod_asc), 2),
                                                      treatment_code   = c(rep(1:n_trt, n_per_grp_mod_asc), rep(1, n_trt * n_per_grp_mod_asc)),
                                                      grp              = rep(c(2,1), each = n_trt * n_per_grp_mod_asc)
               ) %>%
                 mutate_at(vars(contains("code"),"grp"),as.factor) %>%
                 left_join(pool_0, by = c("animal_code","period_code","treatment_code")) %>%
                 arrange(grp,animal_code,period_code,treatment_code)

               dat = design_mod_asc  %>% filter(grp==1)

               mod     = lm(y ~ period_code + baseline, data = dat)
               anova   = anova(mod)
               pvalue  = anova$`Pr(>F)`[1]

               if(pvalue<=0.05){

                 dat_p2 = design_mod_asc %>% filter(period_code==2)
                 dat_p3 = design_mod_asc %>% filter(period_code==3)
                 dat_p4 = design_mod_asc %>% filter(period_code==4)

                 fit_p2 = lm(y ~ treatment_code + baseline, data = dat_p2)
                 fit_p3 = lm(y ~ treatment_code + baseline, data = dat_p3)
                 fit_p4 = lm(y ~ treatment_code + baseline, data = dat_p4)

                 rg              <- ref_grid(fit_p2)
                 lsm             <- emmeans(rg,"treatment_code")
                 ctst_df         <- as.data.frame(contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1))
                 p21             <- ctst_df[1,ncol(ctst_df)]

                 rg              <- ref_grid(fit_p3)
                 lsm             <- emmeans(rg,"treatment_code")
                 ctst_df         <- as.data.frame(contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1))
                 p31             <- ctst_df[1,ncol(ctst_df)]

                 rg              <- ref_grid(fit_p4)
                 lsm             <- emmeans(rg,"treatment_code")
                 ctst_df         <- as.data.frame(contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1))
                 p41             <- ctst_df[1,ncol(ctst_df)]

               } else {

                 design_asc = design_mod_asc %>% filter(grp==2)

                 mod             <- lm(y ~ treatment_code + animal_code, data = design_asc)
                 rg              <- ref_grid(mod)
                 lsm             <- emmeans(rg,"treatment_code")
                 ctst_df         <- as.data.frame(contrast(lsm,"trt.vs.ctrl",adjust="none",ref=1))
                 p41             <- ctst_df[nrow(ctst_df),ncol(ctst_df)]
                 p31             <- ctst_df[nrow(ctst_df)-1,ncol(ctst_df)]
                 p21             <- ctst_df[nrow(ctst_df)-2,ncol(ctst_df)]
               }

               c(p41,p31,p21)

             }

if (!is.null(cl)) {
  parallel::stopCluster(cl)
}

M           <- as.data.frame(t(M))
colnames(M) <- c('p41_nsim',
                 'p31_nsim',
                 'p21_nsim')
attach(M)
list = list(  tp1err41  = sum(p41_nsim < 0.05) / n_sim,
              tp1err31  = sum(p31_nsim < 0.05) / n_sim,
              tp1err21  = sum(p21_nsim < 0.05) / n_sim
)
