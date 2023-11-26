# prints conditional indirect and direct effects

print_conditional_mediation_effects <- function(final_df){
  
  moderator <- unique(final_df$moderator)
  mediator <- unique(final_df$mediator)
  mediation_status <- unique(final_df$mediation_status)
  
  for (mod in levels(final_df$moderator_value)){
    
    sub_df <- subset(final_df, moderator_value == mod) #%>%
      #dplyr::select(EES_ROI, beta_ROI, effect, Estimate, 
      #              `95% CI Lower`, `95% CI Upper`, mediation_status, 
      #              Pfdr) 
    
    T_col_index <- which(colnames(sub_df) == "Estimate")
    p_col_index <- which(colnames(sub_df) == "Pfdr")
    
    sub_df <- round_three(sub_df, start_col = T_col_index, end_col = p_col_index-1)
    sub_df <- round_p(sub_df, p_col = p_col_index)
    
    Pfdr_value_output <- ifelse(is.character(sub_df$Pfdr[1]),sub_df$Pfdr[1], 
                                paste0("= ", sub_df$Pfdr[1]))
    
    sub_ACME_df <- subset(sub_df, effect == "ACME (average)")
    sub_ADE_df <- subset(sub_df, effect == "ADE (average)")
    cat("\n\n")
    cat(paste0("For **", mod, "** levels of the moderator, ", moderator,
                 " the conditional indirect effect of emotion on memory through ", 
                 mediator, " was ", sub_ACME_df$Estimate, " (CI[", 
                 sub_ACME_df$`95% CI Lower`, ", ", sub_ACME_df$`95% CI Upper`,
                 "], p~corr~ ",  sub_ACME_df$Pfdr,") with a direct effect of ", sub_ADE_df$Estimate, " (CI[", 
                 sub_ADE_df$`95% CI Lower`, ", ", sub_ADE_df$`95% CI Upper`,
                 "], p~corr~ ",  Pfdr_value_output,"), speaking for a ", mediation_status, "."
    ))
  }
}