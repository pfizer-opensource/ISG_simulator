
#######################
# DESIGN - CROSSOVER 
#######################
#
# Power hand calculation uses lm(y ~ treatment + period + animal) treatment contrasts:
# n     = animals per sequence (Williams design)
# Total animals = n*K
# df    = nK^2 - nK - 2K + 2   [numerator treatment K-1, denominator error df]
# sd    = sd_within
#
# Sample size per sequence: n
# Number of treatments    : K (typically 4)
# Number of periods       : K (balanced/square design)
# Total number of animals : n*K
# Total number of obs     : n*K^2
#
# SOV           DOF
#-----------------------------------------
# Treatment     K-1
# Period        K-1  
# Animal        n*K - 1
# Error         n*K^2 - 1 - (K-1) - (K-1) - (n*K-1) = n*K^2 - n*K - 2*K + 2
# Total         n*K^2 - 1
#
# When n=1, K=4: df = 1*16 - 1*4 - 8 + 2 = 6 ✓ (matches 3(N-2) where N=4 total animals) 
#

#---------------
# power
#---------------

  pwr_hc_xov <- function(n, delta, sd, K, alpha = 0.05, alternative = c("two.sided", "one.sided")) {

              alternative <- match.arg(alternative)
              
              # df and noncentrality parameter under H1
              # n = animals per sequence; total animals = n*K
              df <- n*K^2 - n*K - 2*K + 2
              se <- sd * sqrt(2 / (n*K))   # SE for treatment contrast in crossover
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
  
  # 2) Solve for n given (delta, sd, alpha, target power)
  #    Uses uniroot() to invert the power function in n
  n_hc_xov <- function(delta, sd, K, alpha = 0.05, power = 0.80,
                                      alternative = c("two.sided", "one.sided"),
                                      n_lower = 2 + 1e-8, n_upper = 1e6) {
              alternative <- match.arg(alternative)
              
              # Define objective: power(n) - target = 0
              f <- function(n_cont) {
                n <- ceiling(n_cont)  # n must be integer per group
                pwr_hc_xov(n, delta, sd, K, alpha, alternative) - power
              }
              
              # Bracket and solve
              uniroot(f, lower = n_lower, upper = n_upper)$root %>% ceiling()
  }
  
  # 3) Convenience: solve for delta (detectable difference) given n
  delta_hc_xov <- function(n, sd, K, alpha = 0.05, power = 0.80,
                                          alternative = c("two.sided", "one.sided"),
                                          d_lower = 0, d_upper = 1e6) {
              alternative <- match.arg(alternative)
              
              g <- function(delta) {
                pwr_hc_xov(n, delta, sd, K, alpha, alternative) - power
              }
              
              uniroot(g, lower = d_lower, upper = d_upper)$root
  }
  

 
