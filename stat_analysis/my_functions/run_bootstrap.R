# apply bootstrapping and compute CI for index of moderated mediation

library(dplyr)
library(boot)
library(lme4)

run_bootstrap <- function(combination, EES_select_df, aMTL_df) {
  
  ROI_EES_i <- combination$EES_ROI
  ROI_aMTL_i <- combination$aMTL_ROI
  
  # Subset for aMTL ROI
  aMTL_sub_df <- subset(aMTL_df, beta_ROI == ROI_aMTL_i)
  
  EES_sub_df <- subset(EES_select_df, ROI == ROI_EES_i) %>%
    group_by(sj) %>%
    mutate(EES_ROI = ROI,
           M = scale(EES, scale = FALSE)) %>%
    ungroup() %>%
    dplyr::select(sj, item, EES_ROI, EncRuns, emotion, EES, M, subsMemory)
  
  # join EES and univ df
  merged_df <- inner_join(EES_sub_df, aMTL_sub_df, by = c("sj", "item")) %>%
    mutate(Y = as.integer(subsMemory),
           X = factor(emotion, levels=c("neutral","negative")), # emotion as predictor
           M = M, # within-subject centered EES as mediator
           Z = beta_z) # witin-sj centered initial Amy activity as modulator
  
  set.seed(2023)  # Setting the seed for reproducibility
  
  # Perform bootstrapping for this subset
  boot_obj <- boot(data = merged_df, statistic = bootstrap_index_of_moderated_mediation, R = 10000)
  
  # Initialize variables to store CI results
  ci_bca <- NA
  ci_perc <- NA
  
  # Try to calculate 95% CI using "bca" method
  # for bias corrected CIs bootstrap iterations must be higher than data rows
  tryCatch({
    ci_bca <- boot.ci(boot.out = boot_obj, type = "bca")
  }, error = function(e) {
    print(paste("Error computing bca CI for ROI_EES_i:", ROI_EES_i, "Error:", e))
    ci_bca <- NA
  })
  
  # Store the results
  return(list("Bootstrap Object" = boot_obj, "CI_BCA" = ci_bca))
  
}