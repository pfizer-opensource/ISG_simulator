{
  n_ani_pool          <- n_trt*10  # n_trt * length( beta_vec)                                                               
  nrow_per_ani        <- length(beta_vec)*length(tau_vec)
  baseline_sd         <- between_sd*sqrt(P_bl)                                                    
  baseline            <- rnorm(n_ani_pool, eta, baseline_sd) 
  beta_bl             <- 1
  y_bl                <- rep(baseline,each=nrow_per_ani)
  alpha_vec           <- rnorm(n_ani_pool,0,sd=between_sd*sqrt(1-P_bl))
  
  # continue "scenario/scen_1.R"
  ijk                 <- cbind.data.frame(i = rep(1:n_ani_pool  , each = nrow_per_ani),
                                          j = rep(1:length(beta_vec), length(tau_vec)*n_ani_pool),
                                          k = rep( rep(1:length(tau_vec ), each=length(beta_vec)), n_ani_pool)
  )
  pool                 <- ijk 
  
}
{
  # source("scenario/scen_1.R")
  
  for(rn in 1:nrow(pool)){ 
    i                = pool$i[rn]
    j                = pool$j[rn]
    k                = pool$k[rn]
    alpha_i          = alpha_vec[i]
    beta_j           = beta_vec[j]
    tau_k            = tau_vec[k]
    y_bl_i           = y_bl[rn] 
    y_ijk            = beta_bl* y_bl_i  + alpha_i + beta_j + tau_k + rnorm(1,0,within_sd)
    
    pool$baseline[rn]  = y_bl_i
    pool$alpha_i[rn] = alpha_i
    pool$beta_j[rn]  = beta_j
    pool$tau_k[rn]   = tau_k
    pool$y[rn]       = y_ijk
  }  
  pool                  <- pool %>% rename(animal_code = i, period_code = j, treatment_code = k) %>% mutate_at(vars(contains("code")),as.factor)
  
  pool_0                 <- ijk
  for(rn in 1:nrow(pool_0)){ 
    i                = pool_0$i[rn]
    j                = pool_0$j[rn]
    k                = pool_0$k[rn]
    alpha_i          = alpha_vec[i]
    beta_j           = beta_vec[j]
    tau_k            = tau_vec_0[k]
    y_bl_i           = y_bl[rn] 
    y_ijk            = beta_bl* y_bl_i  + alpha_i + beta_j + tau_k + rnorm(1,0,within_sd)
    
    pool_0$baseline[rn]  = y_bl_i
    pool_0$alpha_i[rn] = alpha_i
    pool_0$beta_j[rn]  = beta_j
    pool_0$tau_k[rn]   = tau_k
    pool_0$y[rn]       = y_ijk
  }     
  pool_0                  <- pool_0 %>% rename(animal_code = i, period_code = j, treatment_code = k) %>% mutate_at(vars(contains("code")),as.factor)
  
}