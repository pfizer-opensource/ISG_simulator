
#################################
# DESIGN - PARALLEL RM 
#################################
#
# Power hand calculation will be a two sample t test with customized:
# df = K(n-1)
# (with BL): sd = sqrt(sd_bt^2*(1-P_bl)+ sd_within^2/p)
# (w/o BL) : sd = sqrt(sd_bt^2         + sd_within^2/p)
#
# Sample size per arm    : n=4
# Number of arms         : K=4
# Number of periods      : p=4
# Total number of obs    : nKp = 64

# when interested in [within anm] comparison
# SOV          DOF
#--------------------
# Trt          K-1
# animal(Trt)  K(n-1)   [denominator df for treatment contrast]
# Period       p-1
# Residual     nKp -Kn  - p + 1   
# Tot          nKp-1   

#---------------
# power
#---------------

  pwr_hc_par_rm <- function(n, delta, sd, K, p, alpha = 0.05, alternative = c("two.sided", "one.sided")) {

    alternative <- match.arg(alternative)
    # df and noncentrality parameter under H1
    df <- K * (n - 1)
    se <- sd * sqrt(2 / n)
    lambda <- delta / se  # noncentrality parameter
    
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

  n_hc_par_rm <- function(delta, sd, K, p, alpha = 0.05, power = 0.80,
                                      alternative = c("two.sided", "one.sided"),
                                      n_lower = 2 + 1e-8, n_upper = 1e6) {
    alternative <- match.arg(alternative)
    
    # Define objective: power(n) - target = 0
    f <- function(n_cont) {
      n <- ceiling(n_cont)  # n must be integer per group
      pwr_hc_par_rm(n = n, delta = delta, sd = sd, K = K, p = p, alpha = alpha, alternative = alternative) - power
    }
    
    # Bracket and solve
    uniroot(f, lower = n_lower, upper = n_upper)$root |> ceiling()
  }
  
#---------------
# effect size
#---------------

  delta_hc_par_rm <- function(n, sd, K, p, alpha = 0.05, power = 0.80,
                                          alternative = c("two.sided", "one.sided"),
                                          d_lower = 0, d_upper = 1e6) {
    alternative <- match.arg(alternative)
    
    g <- function(delta) {
      pwr_hc_par_rm(n = n, delta = delta, sd = sd, K = K, p = p, alpha = alpha, alternative = alternative) - power
    }
    
    uniroot(g, lower = d_lower, upper = d_upper)$root
  }
  


  