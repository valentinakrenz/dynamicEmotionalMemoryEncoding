library(dplyr)

# print stats specifically for each peak in each cluster

print_peak_stats <- function(full_stat_df, contrast_df) {
  
  # Initialize an empty list to store the results for top ROIs
  top_ROI_results <- list()
  
  # Looping through each unique effect in full_int_stat_df
  for (effect in unique(full_stat_df$effect)) {
    
    effect_df <- full_stat_df[full_stat_df$effect == effect, ]
    posthoc_effects <- unique(effect_df$posthoc_effect)
    
    # Looping through each unique posthoc_effect in contrast_df
    for(posthoc_effect in posthoc_effects) {
      
      # Print the current posthoc effect
      cat(paste0("\n\n\nWe found a significant **", posthoc_effect, "** \n"))
      
      posthoc_effect_df <- effect_df[effect_df$posthoc_effect == posthoc_effect, ]
      clusters <- unique(posthoc_effect_df$cluster)

      # Looping through each unique cluster_name in contrast_df for the current posthoc_effect
      for(cluster in clusters) {
        
        cluster_ROIs <- unique(contrast_df$ROI[contrast_df$cluster_name == cluster & 
                                               contrast_df$posthoc_effect == posthoc_effect])
        
        # Fetching the statistics for the current effect and cluster
        temp_df <- posthoc_effect_df %>%
          filter(all_effects == effect, Pfdr < 0.05, ROI %in% cluster_ROIs)
        
        # Ordering the statistics by T value and fetching the top ROI
        temp_df <- temp_df[order(-abs(temp_df$T)), ]
        top_ROI <- temp_df$ROI[1]
        
        # Fetching detailed stats for the top ROI
        top_ROI_stats <- posthoc_effect_df %>%
          filter(ROI == top_ROI & all_effects == effect)
        
        
        T_col_index <- which(colnames(top_ROI_stats) == "T")
        p_col_index <- which(colnames(top_ROI_stats) == "Pfdr")
        
        top_ROI_stats <- round_three(top_ROI_stats, start_col = T_col_index, end_col = p_col_index-1)
        top_ROI_stats <- round_p(top_ROI_stats, p_col = p_col_index)
        
        Pfdr_value_output <- ifelse(is.character(top_ROI_stats$Pfdr[1]),top_ROI_stats$Pfdr[1], 
                                 paste0("= ", top_ROI_stats$Pfdr[1]))
        
        # Printing the statistics for the current cluster and top ROI
        cat(paste0(" \n in a **", cluster, 
                   "** cluster with a peak activity in ", top_ROI_stats$lat[1], " ", top_ROI_stats$full_label[1], 
                   " (LMM, ", top_ROI_stats$effect[1], ": *\u03B2* = ", top_ROI_stats$Beta[1], 
                   ", CI[", top_ROI_stats$lower_CI[1],", ", top_ROI_stats$upper_CI[1],"], ",
                   " *t*(", top_ROI_stats$Df[1], ") = ", top_ROI_stats$T[1], 
                   ", *p*<sub>corr</sub> ", Pfdr_value_output, ") "))
        
        # Fetching the related contrasts for the top ROI
        related_contrasts <- contrast_df %>%
          filter(ROI == top_ROI & p < 0.05 & posthoc_effect == posthoc_effect)
        
        if (nrow(related_contrasts)>0){

        
        estimate_index <- which(colnames(related_contrasts) == "estimate")
        p_col_index <- which(colnames(related_contrasts) == "p")
        
        related_contrasts <- round_three(related_contrasts, start_col = estimate_index, end_col = p_col_index - 1)
        related_contrasts <- round_p(related_contrasts, p_col = p_col_index)
        estimate_label <- "estimate"
        
        
        # Printing the related contrasts
        for(i in 1:nrow(related_contrasts)){
          
          if (effect == "memory × run × emotion"){
          
          if(related_contrasts$contrast_type[i] == "difference between memory categories") next
          
          estimate_label <- ifelse(related_contrasts$contrast_type[i] == "difference between emotions in slope related to successfull memory" |
                                     related_contrasts$contrast_type[i] == "slope related to successfull memory",
                                   "EMS",
                                   ifelse(related_contrasts$contrast_type[i] == "difference between emotions in memory type",
                                          "EMM",
                                          "estimate"))
          
          } else if (effect == "memory × emotion" | effect == "main effect memory"){
            
            if(related_contrasts$contrast_type[i] == "difference between emotion in memory categories") next
            estimate_label <- "EMM"
          }

          
          p_value_output <- ifelse(
            is.character(related_contrasts$p[i]) && substr(related_contrasts$p[i], 1, 1) == "<", 
            related_contrasts$p[i], 
            paste0("= ", related_contrasts$p[i])
          )
          
          cat(paste0("  with a significant ", related_contrasts$contrast_type[i], 
                     " in contrast ", related_contrasts$contrast[i], 
                     " (", estimate_label,  " = ", related_contrasts$estimate[i], ", CI[",related_contrasts$lower_CI[i],", ", 
                     related_contrasts$upper_CI[i],"], *t*(", related_contrasts$df[i], ") = ", related_contrasts$t[i], 
                     ", *p* ", p_value_output, ")\n"))
        }
        }
        
        # Store the results in the list for this top_ROI
        top_ROI_results[[top_ROI]] <- list(
          posthoc_effect = posthoc_effect,
          cluster = cluster,
          top_ROI_stats = top_ROI_stats,
          related_contrasts = related_contrasts
        )
      }
    }
  }
  
  # Return the results list at the end
  return(top_ROI_results)
}