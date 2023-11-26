

# function to prepare outputfile for lmer ####
prepare_mixed_df <- function(ROIs=ROIs, n_factors) {
  nROIs <- length(ROIs)
  nIV_total <- 2^n_factors - 1
  
  mixed_df <- data.frame(array(NaN, c(nROIs * nIV_total, ncol = 8)))
  colnames(mixed_df) <- c('ROI', 'effect', 'T', 'Df', 'Pvalue','Beta', 'SE', 'rand')
  
  return(list(nIV_total = nIV_total, mixed_df = mixed_df, ROIs = ROIs))
}


# function to fill output file from linea mixed model results
get_mixed_df <- function(ROIs, models_list, n_factors = 3){

  nROIs <- length(ROIs)
  
  # prepare df for lmm results
  prep_result <- prepare_mixed_df(ROIs, n_factors)
  mixed_df <- prep_result$mixed_df
  row = 0
  
  for (this_ROI in ROIs) {
    
    this_model <- models_list[[this_ROI]]
    
    # get LMM estimates
    invisible(stats <- summary(this_model)$coefficients)
    nIV <- nrow(stats) - 1 # number of fixed effects, discounting intercept
    
    mixed_df$ROI[(row + 1):(row + nIV)] <- this_ROI
    mixed_df$effect[(row + 1):(row + nIV)] <- row.names(stats)[2:(nIV + 1)]
    mixed_df[(row + 1):(row + nIV), 
             c("Beta", "SE", "T", "Df", "Pvalue")] <- stats[2:(nIV + 1),
                                                            c("Estimate", 
                                                              "Std. Error", 
                                                              "t value", 
                                                              "df", 
                                                              "Pr(>|t|)")]
    
    # Get the names of the random effects for this ROI
    random_effect_names <- names(ranef(this_model))
    
    # Check if 'sj' and 'item' are present in random_effects_roi
    if ("sj" %in% random_effect_names && "item" %in% random_effect_names) {
      rand_str <- "(1 | sj) + (1 | item)"
    } else if ("sj" %in% random_effect_names) {
      rand_str <- "(1 | sj)"
    } else if ("item" %in% random_effect_names) {
      rand_str <- "(1 | item)"
    } 
    
    # Fill in the rand column for this ROI
    mixed_df$rand[(row + 1):(row + nIV)] <- rand_str
    
    # new rows
    row <- row + nIV

  }
  return(mixed_df)
}




# function to fill output file from linea mixed model results
get_anova_df <- function(ROIs, models_list, n_factors=3){
  nROIs <- length(ROIs)
  
  # prepare df for model test results (ME and lower level interactions regardless of reference)

  nIV_total <- 2^n_factors - 1
  anova_df <- data.frame(array(NaN, c(nROIs * nIV_total, ncol = 8)))
  colnames(anova_df) <- c("ROI","effect","SumSq","MeanSq","NumDf","DenDf","F","Pvalue")
  row <- 0
  
  ## run LMM for each roi
  for (roi in ROIs) {

    this_model <- models_list[[roi]]
    
    table <- data.frame(anova(this_model))
    colnames <- colnames(table)
    
    # check significance of predictors regardless of reference category
    nIV <- nrow(table) #- 1 # number of fixed effects, discounting intercept
    anova_df$ROI[(row + 1):(row + nIV)] <- roi
    anova_df$effect[(row + 1):(row + nIV)] <- row.names(table)[1:(nIV)]
    anova_df[(row + 1):(row + nIV), c("SumSq",
                                      "MeanSq",
                                      "NumDf",
                                      "DenDf",
                                      "F",
                                      "Pvalue")] <- table[1:nIV,c("Sum.Sq",
                                                                  "Mean.Sq",
                                                                  "NumDF",
                                                                  "DenDF",
                                                                  "F.value",
                                                                  "Pr..F.")]
    # new rows
    row <- row + nIV
  }
  return(anova_df)
}

# function to prepare outputfile for lmer ####
prepare_mixed_glmm_df <- function(ROIs=ROIs, n_factors) {
  nROIs <- length(ROIs)
  nIV_total <- 2^n_factors - 1
  
  mixed_df <- data.frame(array(NaN, c(nROIs * nIV_total, ncol = 7 )))
  colnames(mixed_df) <- c('ROI', 'effect', 'Z', 'Pvalue','Beta', 'SE', 'rand')
  
  return(list(nIV_total = nIV_total, mixed_df = mixed_df, ROIs = ROIs))
}


# function to fill output file from linea mixed model results
get_mixed_glmm_df <- function(ROIs, models_list, n_factors = 2){
  
  nROIs <- length(ROIs)
  this_ROI <- ROIs[1]
  
  # prepare df for lmm results
  prepare_results <- prepare_mixed_glmm_df(ROIs, n_factors)
  mixed_df <- prepare_results$mixed_df
  row = 0
  
  for (r in 1:length(ROIs)) {
    
    this_ROI <- as.character(ROIs[r]) 
    this_model <- models_list[[r]]
    summary(this_model)
    
    # get LMM estimates
    invisible(stats <- summary(this_model)$coefficients)
    nIV <- nrow(stats) - 1 # number of fixed effects, discounting intercept
    
    mixed_df$ROI[(row + 1):(row + nIV)] <- this_ROI
    mixed_df$effect[(row + 1):(row + nIV)] <- row.names(stats)[2:(nIV + 1)]
    mixed_df[(row + 1):(row + nIV), 
             c("Beta", "SE", "Z", "Pvalue")] <- stats[2:(nIV + 1),
                                                            c("Estimate", 
                                                              "Std. Error", 
                                                              "z value", 
                                                              "Pr(>|z|)")]
    
    # Get the names of the random effects for this ROI
    random_effect_names <- names(ranef(this_model))
    
    # Check if 'sj' and 'item' are present in random_effects_roi
    if ("sj" %in% random_effect_names && "item" %in% random_effect_names) {
      rand_str <- "(1 | sj) + (1 | item)"
    } else if ("sj" %in% random_effect_names) {
      rand_str <- "(1 | sj)"
    } else if ("item" %in% random_effect_names) {
      rand_str <- "(1 | item)"
    } 
    
    # Fill in the rand column for this ROI
    mixed_df$rand[(row + 1):(row + nIV)] <- rand_str
    
    # new rows
    row <- row + nIV
    
  }
  return(mixed_df)
}
