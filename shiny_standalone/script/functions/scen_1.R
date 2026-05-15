# all inputs
{
  n_sim               <- 5 
  n_ani_pool          <- 16  
  seed                <- 100
  set.seed(seed)
  seed_vec            <- ceiling(rnorm(n_sim, 2500, 3000))            
  seed_vec            <- ifelse(seed_vec < 0, -seed_vec, seed_vec)  
  
  set.seed(seed)
  within_sd           <- 4.037                                                                   # Within-subject variability
  between_sd          <- 11.706                                                                  # Between-subject variability
  eta                 <- 200
  P_bl                <- 0.8
  alpha_vec           <- rnorm(n_ani_pool,0,sd=between_sd*sqrt(1-P_bl))
  beta_vec            <- c(0,rnorm(3,0,1.587) )                                                       # period effect
  tau_vec             <- c(0, 6, 10, 16)                                                         # dose effect under H1
  tau_vec_0           <- c(0, 0, 0, 0)                                                           # dose effect under H0
  
  n_trt               <- length(tau_vec)                                                         # number of treatments
  n_ani_pool          <- n_trt * length( beta_vec)                                                               
  nrow_per_ani        <- length(beta_vec)*length(tau_vec)
  baseline_sd         <- between_sd*sqrt(P_bl)                                                    
  baseline            <- rnorm(n_ani_pool, eta, baseline_sd) 
  beta_bl             <- 1
  y_bl                <- rep(baseline,each=nrow_per_ani)
  
  n_grp_par           <- n_trt                                                                   # Number of groups in Parallel   Design
  n_per_grp_par       <- n_trt                                                                         # Number of animals per group in Parallel   Design (- for a square design, this eqs the n_trt)
  n_ani_par           <- n_per_grp_par*n_grp_par                                                 # Number of animals in Parallel Design
  
  n_grp_xov           <- n_trt                                                                   # Number of groups in Crossover  Design
  n_per_grp_xov       <- 1                                                                       # Number of animals per group in Crossover  Design (- for a square design, this eqs the n_trt)
  n_ani_xov           <- n_per_grp_xov*n_grp_xov                                                 # Number of animals in Crossover Design
  
  n_grp_asc           <- 1                                                                       # Number of groups in Ascending  Design
  n_per_grp_asc       <- n_trt                                                                        # Number of animals per group in Ascending  Design (- for a square design, this eqs the n_trt)
  n_ani_asc           <- n_per_grp_asc*n_grp_asc                                                 # Number of animals in Ascending Design
  
  n_grp_seq           <- n_trt-1                                                                 # Number of groups in Sequential Design
  n_per_grp_seq       <- n_trt                                                                       # Number of animals per group in Sequential Design (- for a square design, this eqs the n_trt)
  n_ani_seq           <- n_per_grp_seq*n_grp_seq                                                 # Number of animals in Sequential Design
  
}
#n_grp_par           <- n_trt                                                                   # Number of groups in Parallel   Design
#n_per_grp_par       <- 4                                                                       # Number of animals per group in Parallel   Design (- for a square design, this eqs the n_trt)
#n_ani_par           <- n_per_grp_par*n_grp_par                                                 # Number of animals in Parallel Design

#n_grp_xov           <- n_trt                                                                   # Number of groups in Crossover  Design
#n_per_grp_xov       <- 1                                                                       # Number of animals per group in Crossover  Design (- for a square design, this eqs the n_trt)
#n_ani_xov           <- n_per_grp_xov*n_grp_xov                                                 # Number of animals in Crossover Design

#n_grp_asc           <- 1                                                                       # Number of groups in Ascending  Design
#n_per_grp_asc       <- 4                                                                       # Number of animals per group in Ascending  Design (- for a square design, this eqs the n_trt)
#n_ani_asc           <- n_per_grp_asc*n_grp_asc                                                 # Number of animals in Ascending Design

#n_grp_seq           <- n_trt-1                                                                 # Number of groups in Sequential Design
#n_per_grp_seq       <- 4                                                                       # Number of animals per group in Sequential Design (- for a square design, this eqs the n_trt)
#n_ani_seq           <- n_per_grp_seq*n_grp_seq                                                 # Number of animals in Sequential Design

ijk                 <- cbind.data.frame(i = rep(1:n_ani_pool  , each = nrow_per_ani),
                                        j = rep(1:length(beta_vec), length(tau_vec)*n_ani_pool),
                                        k = rep( rep(1:length(tau_vec ), each=length(beta_vec)), n_ani_pool)
)
pool                 <- ijk
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

 
