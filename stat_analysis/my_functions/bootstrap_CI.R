# Load the packages
library(lme4)
library(boot)
library(parallel)

# Function to obtain bootstrapped confidence intervals
bootstrap_CI <- function(model, n_cores = 70) {
  boot_stats <- function(m) {
    fixef(m)
  }
  
  boot_results <- bootMer(x = model, 
                          FUN = boot_stats, 
                          nsim = 1000, # Adjust number of simulations as needed
                          seed = 12345, 
                          parallel = "snow", 
                          ncpus = n_cores) # Adjust ncpus based on your available cores
  
  # Compute the 95% confidence intervals
  boot_quantiles <- apply(boot_results$t, 2, function(x) quantile(x, c(0.025, 0.975)))
  return(boot_quantiles)
}