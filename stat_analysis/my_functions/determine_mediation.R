
# Function to determine mediation type
determine_mediation <- function(IE_pvalue, DE_pvalue, IE_estimate, DE_estimate) {
  if (IE_pvalue < 0.05 && DE_pvalue >= 0.05) {
    return("Full Mediation")
  } else if (IE_pvalue < 0.05 && DE_pvalue < 0.05) {
    if (sign(IE_estimate) == sign(DE_estimate)) {
      return("Partial Mediation (Complementary)")
    } else {
      return("Partial Mediation (Competitive)")
    }
  } else if (IE_pvalue >= 0.05) {
    return("No Mediation")
  } else {
    return("Undefined")
  }
}


get_mediation_df <- function(result_df){

# Initializing an empty dataframe to store results
compiled_df <- data.frame()

# Loop through the results_list
for (univ_ROI in names(results_list)) {
  for (EES_ROI in names(results_list[[univ_ROI]])) {
    for (emotion in c("neutral", "negative")) {
      summary_info <- results_list[[univ_ROI]][[EES_ROI]][[emotion]]$summary
      stats_df <- summary_info$stats
      
      # Convert matrix to dataframe for ease of manipulation
      temp_df <- as.data.frame(stats_df)
      
      # Adding extra columns
      temp_df$univ_ROI <- univ_ROI
      temp_df$EES_ROI <- EES_ROI
      temp_df$emotion <- emotion
      temp_df$mediation_type <- summary_info$mediation
      temp_df$effect <- rownames(stats_df)
      
      # Rename the 'p-value' column to 'Pvalue'
      colnames(temp_df)[which(colnames(temp_df) == "p-value")] <- "Pvalue"
      
      # Bind rows to the final compiled dataframe
      compiled_df <- rbind(compiled_df, temp_df)
      rownames(compiled_df) <- NULL
    }
  }
}

# Reordering columns for clarity
compiled_df <- compiled_df[, c("univ_ROI", "EES_ROI", "emotion", "effect", 
                               "mediation_type", "Estimate", "95% CI Lower", 
                               "95% CI Upper", "Pvalue")]

# Initialize an empty dataframe to hold the corrected values
corrected_df <- data.frame()
for (roi in unique(compiled_df$EES_ROI)){
  for (emo in unique(compiled_df$emotion)){
    for (e in unique(compiled_df$effect)){
      subset_df <- subset(compiled_df, EES_ROI == roi & 
                            effect == e & emotion == emo)
      
      # Directly run FDR correction on the subset
      subset_df$Pfdr <- p.adjust(subset_df$Pvalue, method = "fdr")
      
      # Bind this corrected subset to our new dataframe
      corrected_df <- rbind(corrected_df, subset_df)
    }
  }
}

# Initialize an empty dataframe to hold the corrected values
corrected_df$mediation_type <- NULL
updated_df <- data.frame()

# Loop through and assign mediation type
for (ees in unique(corrected_df$EES_ROI)) {
  for (uni in unique(corrected_df$univ_ROI)) {
    for (emo in unique(corrected_df$emotion)) {
      
      subset_df <- subset(corrected_df, EES_ROI == ees & univ_ROI == uni & emotion == emo)
      
      if (nrow(subset_df) > 0) {
        IE_row <- subset_df[subset_df$effect == "ACME (average)", ] # mediated effect
        DE_row <- subset_df[subset_df$effect == "ADE (average)", ] # direct effect when taking mediated effect
        mediation_label <- determine_mediation(IE_row$Pfdr, DE_row$Pfdr, IE_row$Estimate, DE_row$Estimate)
        
        # Add mediation_label to the subset_df
        subset_df$mediation_type <- mediation_label
        
        # Append the subset_df to the updated_df
        updated_df <- rbind(updated_df, subset_df)
      }
    }
  }
}

return(updated_df)

}