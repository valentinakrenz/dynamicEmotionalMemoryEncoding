# linear mixed model function for univariate and multivariate analyses 

library(lme4)
library(lmerTest)
library(dplyr)


run_LMM_for_univ_ROI <- function(ROI_i, this_df) {
  
  ROI_i <- as.character(ROI_i)
  
  this_ROI_df <- this_df[this_df$ROI == ROI_i, ]
  
  model <- suppressMessages(suppressWarnings(lmerTest::lmer(beta_z ~ subsMemory * 
                                                              emotion * run_centered + 
                                                              (1 | sj) + (1 | item), 
                                                            data = this_ROI_df)))
  model_summary <- summary(model)
  
  # Check for singularity warning in the summary
  singular_warning <- grep("boundary \\(singular\\) fit", capture.output(print(model_summary)))
  # Check for convergence issues based on optimizer messages
  convergence_issue <- grep("Model failed to converge", capture.output(print(model_summary)))
  
  should_refit <- length(singular_warning) > 0 || length(convergence_issue) > 0
  
  if (should_refit) {
    # Extract variances of random effects
    random_effects <- lme4::VarCorr(model)
    
    # Identify the random effect with the smallest variance
    min_variance <- which.min(sapply(random_effects, diag))
    random_effect_to_remove <- names(random_effects)[min_variance]
    
    message(paste("Removing random effect:", random_effect_to_remove))
    
    if (random_effect_to_remove == "sj") {
      model <- lmerTest::lmer(beta_z ~ subsMemory * emotion * run_centered + 
                                (1 | item), data = this_ROI_df)
    } else if (random_effect_to_remove == "item") {
      model <- lmerTest::lmer(beta_z ~ subsMemory * emotion * run_centered + 
                                (1 | sj), data = this_ROI_df)
    }
  }
  
  return(model)
}


run_LMM_for_EES_ROI <- function(ROI_i, this_df) {
  
  ROI_i <- as.character(ROI_i)
  
  this_ROI_df <- this_df[this_df$ROI == ROI_i, ]
  
  model <- suppressMessages(suppressWarnings(lmerTest::lmer(EES ~ subsMemory * 
                                                              emotion + 
                                                              (1 | sj) + (1 | item), 
                                                            data = this_ROI_df)))
  model_summary <- summary(model)
  
  # Check for singularity warning in the summary
  singular_warning <- grep("boundary \\(singular\\) fit", capture.output(print(model_summary)))
  # Check for convergence issues based on optimizer messages
  convergence_issue <- grep("Model failed to converge", capture.output(print(model_summary)))
  
  should_refit <- length(singular_warning) > 0 || length(convergence_issue) > 0
  
  if (should_refit) {
    # Extract variances of random effects
    random_effects <- lme4::VarCorr(model)
    
    # Identify the random effect with the smallest variance
    min_variance <- which.min(sapply(random_effects, diag))
    random_effect_to_remove <- names(random_effects)[min_variance]
    
    message(paste("Removing random effect:", random_effect_to_remove))
    
    if (random_effect_to_remove == "sj") {
      model <- lmerTest::lmer(EES ~ subsMemory * 
                                emotion + 
                                (1 | item), 
                              data = this_ROI_df)

    } else if (random_effect_to_remove == "item") {
      model <- lmerTest::lmer(EES ~ subsMemory * 
                                emotion + 
                                (1 | sj), 
                              data = this_ROI_df)
    }
  }
  
  return(model)
}
