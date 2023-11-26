library(lme4)
library(boot)

# Compute index of moderated mediation for each bootstrap sample
# this function is used in run_bootstrap.R

bootstrap_index_of_moderated_mediation <- function(data, indices) {
  library(lme4)
  sample_data <- data[indices,]
  
  # Fit the mediator model
  med_fit_bootstrap <- lmer(EES ~ X*Z + (1|sj), data = sample_data)
  
  # Fit the outcome model
  out_fit_bootstrap <- glmer(Y ~ X + Z + M + X:Z + M:Z + (1|sj), 
                             data = sample_data, 
                             family = "binomial",
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 2e5)))
  
  # Extract coefficients
  med_coefs <- fixef(med_fit_bootstrap)
  out_coefs <- fixef(out_fit_bootstrap)
  
  # Calculate the index of moderated mediation
  index_moderated_mediation <- as.numeric(med_coefs['Xnegative:Z']) *
    as.numeric(out_coefs['M'])
  
  return(index_moderated_mediation)
}
