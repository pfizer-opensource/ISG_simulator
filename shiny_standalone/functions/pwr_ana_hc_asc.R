
#######################
# DESIGN - ASCENDING
####################### 
#
# Power hand calculation will be a two sample t test with customized:
# df = nK - K - n + 1
# sd = sd_within
#
# Number of sequence       :  s   = 1
# Sample size per seq      :  n   = 4
# Number of periods per seq:  p   = n_trt = K
# Total number of obs      :  nKs = nK
#  
# SOV          DOF
#--------------------
# Trt        K-1
# animal     sn-1
# Err        nKs -K - (ns-1) = nK - K - n + 1                     
# Tot        nKs -1   
 
#---------------
# power
#---------------
 
pwr_hc_asc <- function(n, delta, sd, K, alpha = 0.05, alternative = c("two.sided", "one.sided")) {

        alternative <- match.arg(alternative)

        # df and noncentrality parameter under H1
          df <- n*K - K - n + 1
          se <- sd * sqrt(2 / n)
          lambda <- delta / se  # noncentrality parameter
        
        # power calculation based on noncentral t distribution
          if (alternative == "two.sided") {
            tcrit <- qt(1 - alpha/2, df)
            # Power = P(|T_{df,λ}| > tcrit) = P(T > tcrit) + P(T < -tcrit)
            p_upper <- 1 - pt(tcrit, df = df, ncp = lambda)
            p_lower <- pt(-tcrit, df = df, ncp = lambda)
            power <- p_upper + p_lower
          } else {  # one.sided, testing delta > 0 by convention
            tcrit <- qt(1 - alpha, df)
            power <- 1 - pt(tcrit, df = df, ncp = lambda)
          }
        
        as.numeric(power)
  }
  
#---------------
# sample size
#---------------
  #    Uses uniroot() to invert the power function in n

  n_hc_asc <- function(delta, sd, K, alpha = 0.05, power = 0.80,
                                      alternative = c("two.sided", "one.sided"),
                                      n_lower = 2 + 1e-8, n_upper = 1e6) {
              alternative <- match.arg(alternative)
              
              # Define objective: power(n) - target = 0
              f <- function(n_cont) {
                    n <- ceiling(n_cont)  # n must be integer per group
                    pwr_hc_asc(n, delta, sd, K, alpha, alternative) - power
              }
              
              # Bracket and solve
              uniroot(f, lower = n_lower, upper = n_upper)$root %>% ceiling()
  }
  
#---------------
# effect size (delta)
#---------------
 
  delta_hc_asc <- function(n, sd, K, alpha = 0.05, power = 0.80,
                                          alternative = c("two.sided", "one.sided"),
                                          d_lower = 0, d_upper = 1e6) {
    alternative <- match.arg(alternative)
    
    g <- function(delta) {
      pwr_hc_asc(n, delta, sd, K, alpha, alternative) - power
    }
    
    uniroot(g, lower = d_lower, upper = d_upper)$root
  }
  


