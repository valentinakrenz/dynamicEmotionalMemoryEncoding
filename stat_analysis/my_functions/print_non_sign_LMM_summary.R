print_non_sign_LMM_summary <- function(input_df) {
  
  input_df <- input_df %>% filter(Pfdr > 0.05)
  
  effect_column <- if ("all_effects" %in% names(input_df)) {
    "all_effects"
  } else {
    "effect"
  }
  
  # Filter rows where 'memory' is in the 'effect' or 'all_effects' column
  input_df <- filter(input_df, str_detect(!!sym(effect_column), "memory"))
  
  unique_effects <- unique(input_df[[effect_column]])
  
  cat(paste0("\n\n"))
  
  for (effect in unique_effects) {
    effect_df <- filter(input_df, !!sym(effect_column) == !!effect)

    # Get maximum absolute value of Beta, round up to two decimal places
    max_abs_beta <- max(abs(effect_df$Beta))
    rounded_max_abs_beta <- ceiling(max_abs_beta * 1000) / 1000
    
    # Get minimum value of Pfdr, round down to two decimal places
    min_pfdr <- min(effect_df$Pfdr)
    floored_min_pfdr <- floor(min_pfdr * 1000) / 1000
    cat("\n")
    cat(paste0("For ", effect, ": all |*\u03B2*| < ", rounded_max_abs_beta, ", all *p*~corr~ > ", floored_min_pfdr))
  }
}

print_non_sign_contrast_summary <- function(input_df, this_contrast) {
  
  effect_df <- filter(input_df, contrast == this_contrast)
  contrast_type <- unique(effect_df$contrast_type)
  
  # Get unique cluster names and corresponding contrast types
  unique_clusters <- unique(effect_df$cluster_name)
  cluster_names_str <- paste(unique_clusters, collapse = ", ")
  
  # Print the statement with cluster names
  cat(paste0("\n In ", cluster_names_str, ", there was no significant ", effect_df$contrast_type[1], " in contrast ", this_contrast))
  
  # Get maximum absolute value of Beta, round up to two decimal places
  max_abs_beta <- max(abs(effect_df$estimate))
  rounded_max_abs_beta <- ceiling(max_abs_beta * 1000) / 1000
  
  # Get minimum value of Pfdr, round down to two decimal places
  min_pfdr <- min(effect_df$p)
  floored_min_pfdr <- floor(min_pfdr * 1000) / 1000
  
  estimate_label <- ifelse(contrast_type == "difference between emotions in slope related to successfull memory" |
                             contrast_type == "slope related to successfull memory",
                           "EMS",
                           ifelse(contrast_type == "difference between emotions in memory type" | 
                                    contrast_type == "difference between memory categories",
                                  "EMM",
                                  "estimate"))
  
  cat(paste0(" (all |", estimate_label,"| < ", rounded_max_abs_beta, ", all *p* > ", floored_min_pfdr,")"))
}
