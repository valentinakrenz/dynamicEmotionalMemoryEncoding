library(openxlsx)

# function for running FDR correction ##############
run_FDR_cor <- function(complete_mixed_df) {
  
  # Loop over each level of effect
  for (e_i in 1:length(unique(complete_mixed_df$effect))) {
    this_effect <- as.character(unique(complete_mixed_df$effect)[e_i])
    effect_df <- subset(complete_mixed_df, effect == this_effect)
    #print(head(effect_df$Pvalue))
    effect_df$Pfdr <- p.adjust(effect_df$Pvalue, method = "fdr")
    row_indices <- complete_mixed_df$effect == this_effect & complete_mixed_df$ROI %in% effect_df$ROI
    complete_mixed_df[row_indices, "Pfdr"] <- effect_df$Pfdr
  }
  cor_mixed_df <- complete_mixed_df
  return(cor_mixed_df)
  
}