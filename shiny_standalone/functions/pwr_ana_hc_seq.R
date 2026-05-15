
#######################
# DESIGN - SEQUENTIAL
#######################
#
# Power hand calculation is based on d = y2 - y1 and pooled model d ~ grp:
# df = 3n - 3 (three groups, n animals each)
# sd = sd_within
#
# Number of sequence       :  s   = n_trt = K 
# Sample size per seq      :  n   = 4
# Number of periods per seq:  p   = 2
# Total number of obs      :  nps = 24
#  
# ANOVA TABLE FOR d ~ grp (K = 4 treatment levels, so grp = 3)
#  
#  SOV    SS     DF           MS     
#-------------------------------
#  grp    SSTr   (K-1)-1      MSTr
#  Err    SSE    (K-1)(n-1)   MSE           (df = 3n-3 when K=4)
#  TOT    SST    (K-1)n-1
# ...             

#---------------
# power
#---------------

  pwr_hc_seq <- function(n, delta, sd, K, alpha = 0.05,
                                 alternative = c("one.sided", "two.sided"),
                                 strict = FALSE) {
    alternative <- match.arg(alternative)
    df <- (K-1)*(n-1)
    # d = y2 - y1, so Var(d) = 2*sd_within^2 when period-specific errors are independent
    se <- sd * sqrt(2 / n)
    lambda <- delta / se  # noncentrality parameter under H1
    
    if (alternative == "two.sided") {
      tcrit <- qt(1 - alpha/2, df)
      # Power = P(|T_{df,λ}| > tcrit) = P(T > tcrit) + P(T < -tcrit)
      p_upper <- 1 - pt(tcrit, df = df, ncp = lambda)
      p_lower <- pt(-tcrit, df = df, ncp = lambda)
      power <- p_upper + p_lower
      
      # Optional "strict" behavior: include rejection prob in opposite direction
      # This aligns with the documented 'strict' argument in power.t.test().
      if (strict) {
        # When delta = 0, strict power includes alpha (not alpha/2); when |delta| is small,
        # this adds the tiny tail on the "wrong" side. Numerically it's already included
        # by the two-sided tail calculation above; 'strict' mainly matters in edge cases.
        # Kept for API parity; no extra adjustment needed here.
        power <- power
      }
    } else {
      # One-sided test, by convention testing delta > 0
      tcrit <- qt(1 - alpha, df)
      power <- 1 - pt(tcrit, df = df, ncp = lambda)
    }
    as.numeric(power)
  }
  
#---------------
# sample size
#---------------
#    Uses uniroot() to invert power(n) - target = 0
  n_hc_seq <- function(delta, sd, K, alpha = 0.05, power = 0.80,
                                      alternative = c("one.sided", "two.sided"),
                                      strict = FALSE,
                                      n_lower = 2 + 1e-8, n_upper = 1e7) {
    alternative <- match.arg(alternative)
    
    f <- function(n_cont) {
      n <- ceiling(n_cont) # integer sample size
      pwr_hc_seq(n, delta, sd, K, alpha, alternative, strict) - power
    }
    
    # Validate bracket, then solve
    val_low  <- f(n_lower)
    val_high <- f(n_upper)
    if (is.nan(val_low) || is.nan(val_high) || sign(val_low) == sign(val_high)) {
      stop("Failed to bracket a solution in [n_lower, n_upper]. Try widening bounds or check inputs.")
    }
    
    uniroot(f, lower = n_lower, upper = n_upper)$root |> ceiling()
  }
  
#---------------
# effect size
#---------------

  delta_hc_seq <- function(n, sd, K, alpha = 0.05, power = 0.80,
                                          alternative = c("one.sided", "two.sided"),
                                          strict = FALSE,
                                          d_lower = 0, d_upper = 1e6) {
    alternative <- match.arg(alternative)
    
    g <- function(delta) {
      pwr_hc_seq(n, delta, sd, K, alpha, alternative, strict) - power
    }
    
    # Validate bracket, then solve
    val_low  <- g(d_lower)
    val_high <- g(d_upper)
    if (is.nan(val_low) || is.nan(val_high) || sign(val_low) == sign(val_high)) {
      stop("Failed to bracket a solution in [d_lower, d_upper]. Try widening bounds or check inputs.")
    }
    
    uniroot(g, lower = d_lower, upper = d_upper)$root
  }
  



 