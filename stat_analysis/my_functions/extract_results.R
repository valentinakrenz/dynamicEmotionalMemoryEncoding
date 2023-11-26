# find memory-related ROI results ####
extract_results <- function(cor_mixed_df = cor_mixed_df, predictor_name = memory_predictor) {
  sign_mixed_df <- subset(cor_mixed_df, Pfdr < 0.050) # Save all sign findings in one file with prefix 'all'
  sign_memory_df <- subset(cor_mixed_df, Pfdr < 0.050 & grepl(predictor_name, effect, ignore.case = TRUE)) # Save all sign memory-related findings in one file with prefix 'memory'
  
  # Print number of sign results
  print(paste("Number of rows in all:", nrow(sign_mixed_df)))
  print(paste("Number of rows in memory-related:", nrow(sign_memory_df)))
  
  return(list(sign_mixed_df = sign_mixed_df, sign_memory_df = sign_memory_df))
}