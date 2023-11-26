# reshape and rename bootstrapped results

library(dplyr)

prepare_bootstrapped_CIs <- function(results_list, selected_models) {
  
  # Name the results list with the ROI names
  names(results_list) <- names(selected_models)
  
  # Convert the results list into a long data frame
  bootstrap_df <- do.call(rbind, lapply(names(results_list), function(roi) {
    df <- as.data.frame(t(results_list[[roi]]))
    df$ROI <- roi
    df$effect <- rownames(df)
    return(df)
  }))
  
  # Rename the effect names to match those in full_LMM_df
  bootstrap_df <- bootstrap_df %>%
    mutate(all_effects = rename_effect(effect)) %>%
    dplyr::select(ROI, all_effects, `2.5%`, `97.5%`) %>%
    dplyr::rename(lower_CI = `2.5%`, upper_CI = `97.5%`)
  
  return(bootstrap_df)  
}