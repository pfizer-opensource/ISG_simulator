  
  ####################
  # DESIGN - PARALLEL
  ####################
  #
  # Power hand calculation will be a two sample t test with customized:
  # df = 4n-4
  # (with BL): sd = sqrt(sd_bt^2*(1-P_bl)+ sd_within^2)
  # (w/o BL) : sd = sqrt(sd_bt^2         + sd_within^2)
  #
  # Sample size per arm    : n=4
  # Number of arms         : K=4
  # Number of periods      : p=1
  # Total number of obs    : nKp = nK = 16
  #  
  # SOV      DOF
  #--------------------
  # Trt       K-1
  # Err       nK-K    ( = 4n-4 when K=4)
  # Tot       nK-1   
  #

  #---------------
  # power
  #---------------

  pwr_hc_par <- function(n, delta, sd, K, alpha = 0.05, alternative = c("two.sided", "one.sided")) {

      alternative <- match.arg(alternative)
      # df and noncentrality parameter under H1
      df <- K*n - K
      se <- sd * sqrt(2 / n)
      lambda <- delta / se  # noncentrality parameter
      
      if (alternative == "two.sided") {
        tcrit <- qt(1 - alpha/2, df)
        # Power = P(|T_{df,λ}| > tcrit) = P(T > tcrit) + P(T < -tcrit)
        p_upper <- 1 - pt(tcrit, df = df, ncp = lambda)
        power  <- p_upper
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
    n_hc_par <- function(delta, sd, K, alpha = 0.05, power = 0.80,
                                        alternative = c("two.sided", "one.sided"),
                                        n_lower = 2 + 1e-8, n_upper = 1e6) {
      alternative <- match.arg(alternative)
      
      # Define objective: power(n) - target = 0
      f <- function(n_cont) {
        n <- ceiling(n_cont)  # n must be integer per group
        pwr_hc_par(n, delta, sd, K, alpha, alternative) - power
      }
      
      # Bracket and solve
      uniroot(f, lower = n_lower, upper = n_upper)$root %>% ceiling()
    }
    
  #---------------
  # effect size
  #---------------
 
     delta_hc_par <- function(n, sd, K, alpha = 0.05, power = 0.80,
                                            alternative = c("two.sided", "one.sided"),
                                            d_lower = 0, d_upper = 1e6) {
      alternative <- match.arg(alternative)
      
      g <- function(delta) {
        pwr_hc_par(n, delta, sd, K, alpha, alternative) - power
      }
      
      uniroot(g, lower = d_lower, upper = d_upper)$root
    }
    
    
  
  
  