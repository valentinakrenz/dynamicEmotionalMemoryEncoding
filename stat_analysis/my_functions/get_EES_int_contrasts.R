library(emmeans)
library(lmerTest)
library(dplyr)
library(stringr)
library(stats)

# sort emmean and emmean data ####
get_EES_int_contrast <- function(models_list = models_list, ROIs = ROIs){
  
  main_contrast_df <- data.frame(
    contrast = character(),
    contrast_type = character(),
    ROI = character(),
    estimate = numeric(), 
    SE = numeric(),
    upper_CI = numeric(),
    lower_CI = numeric(),
    t = numeric(),
    df = numeric(),
    p = numeric(),
  )
  
  for (roi in ROIs){
    
    this_model <- models_list[[roi]]
    
    # remembered > forgotten estimates for plotting:
    # memory contrasts within emotion between memory types
    emm <- emmeans(this_model, pairwise ~ subsMemory | emotion, 
                           lmer.df = "satterthwaite", 
                           lmerTest.limit = 99999, adjust = "fdr")

    # add conf interval and save in df
    mem_contrast_df <- as.data.frame(emm$contrast) %>%
      left_join(as.data.frame(confint(emm$contrast)), 
                by = c("contrast", "estimate", "SE", "df", "emotion")) %>%
      mutate(contrast_type = "difference between memory categories",
             contrast = case_when(
               emotion == "neutral" ~ "neut rem > forg",
               emotion == "negative" ~ "neg rem > forg"
             ),# change direction as the contrast actually tested forg > rem 
               estimate = -estimate,
               t.ratio = -t.ratio,
             temp_lower = lower.CL,  # Temporarily store the lower.CL in a new column
             lower.CL = -upper.CL,  # Invert and negate the upper.CL to become the new lower.CL
             upper.CL = -temp_lower   # Swap the confidence limits
             ) %>%
      dplyr::select(-temp_lower, -emotion)  # Remove the temporary column and emotion
    
    # get interaction contrasts
    # Getting emmeans for specific combinations
    emm <- emmeans(this_model, 
                   specs = ~ emotion * subsMemory, 
                   lmer.df = "satterthwaite", 
                   lmerTest.limit = 99999)
    
    # Initialize 12-element vectors with zeros
    neut_forg <- rep(0, 4)
    neg_forg <- rep(0, 4)
    neut_rem <- rep(0, 4)
    neg_rem <- rep(0, 4)
    
    # Assign 1 to the specific combination of interest for each vector
    neut_forg[1] <- 1
    neg_forg[2] <- 1
    neut_rem[3] <- 1
    neg_rem[4] <- 1
    
    # memory contrasts within emotion and run
    emmean_int_contrasts <- pairs(contrast(emm,
                                     method = list("neg rem > forg" = 
                                                     neg_rem > neg_forg,
                                                   "neut rem > forg" = 
                                                     neut_rem > neut_forg
                                     ), adjust = "fdr"))
    
    # compute effect size and apply way to complicated renaming
    int_contrast_df <- as.data.frame(emmean_int_contrasts) %>%
      left_join(as.data.frame(confint(emmean_int_contrasts)), 
                by = c("contrast", "estimate", "SE", "df")) %>%
      mutate(contrast_type = "difference between emotion in memory categories")
    
    # combine both emmean computations
    emmean_df <- rbind(mem_contrast_df, int_contrast_df) %>%
      mutate(ROI = roi)
    
    colnames(emmean_df) <- c("contrast", "estimate", "SE", "df", "t",
                             "p", "lower_CL", "upper_CL", "contrast_type",
                             "ROI")  
    
    main_contrast_df <- rbind(main_contrast_df, emmean_df)
    
  }
  
 return(main_contrast_df) 
  
}
