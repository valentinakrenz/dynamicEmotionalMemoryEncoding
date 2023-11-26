library(dplyr)

print_mediation_model_resuts <- function(model){
  # Fetching the statistics for the current effect and cluster
  temp_df <- model %>%
    filter(p < 0.05) %>% droplevels()
  
  outcome <- unique(temp_df$outcome)
  z_test <- "z_value" %in% colnames(temp_df)
  
  if (z_test){
    test_string <- "a gLMM"
  } else {
    test_string <- "an LMM"
  }
  
  # Printing the statistics for the current cluster and top ROI
  cat(paste0("\n\n Estimating **", outcome, "** by means of ", test_string, " indicated: \n"))
  
  for (i in 1:nrow(temp_df)){
    
    this_df <- temp_df[i, ]
    effect_string <- unique(this_df$effect_description)[1]
    
    if (z_test){
      stat_string <- "*z*"
      stat_value <- this_df$z_value[1] 
      stat_index <- which(colnames(this_df) == "z_value")
      
      df_string_1 <- ""
      df_string_2 <- ""
      df_col <- ""
      
    } else {
      stat_string <- "*t*"
      stat_value <- this_df$t_value[1] 
      stat_index <- which(colnames(this_df) == "t_value")
      
      df_string_1 <- "("
      df_string_2 <- ")"
      df_col <- round(this_df$df[1], 3)
    }
    
    estimate_index <- which(colnames(this_df) == "Estimate")
    
    p_col_index <- which(colnames(this_df) == "p")
    
    this_df <- round_three(this_df, estimate_index, end_col = p_col_index-1)
    
    this_df <- round_p(this_df, p_col = p_col_index)
    
    p_value_output <- ifelse(is.character(this_df$p[1]),this_df$p[1], 
                             paste0("= ", this_df$p[1]))
    
    # Printing the statistics for the current cluster and top ROI
    cat(paste0(" \n a **significant ", effect_string, 
               "** (*\u03B2* = ", this_df$Estimate[1], 
               ", ", stat_string, df_string_1, df_col, df_string_2, " = ", this_df[, stat_index][1],
               ", *p* ", p_value_output, ") "))
  }
}