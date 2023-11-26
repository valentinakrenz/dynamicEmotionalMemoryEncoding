
library(emmeans)
library(lmerTest)
library(dplyr)
library(stringr)
library(stats)

# sort emmean and emmean data ####
get_emmeans_emtrends <- function(models_list = models_list, ROIs = ROIs){
  
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
    d = numeric(),
    p = numeric()
  )
  
  for (roi in ROIs){
    
    this_model <- models_list[[roi]]
    
    # Getting emmeans for specific combinations
    emm <- emmeans(this_model, 
                   specs = ~ emotion * subsMemory * run_centered, 
                   at = list(run_centered = c(-1, 0, 1)), lmer.df = "satterthwaite", 
                   lmerTest.limit = 99999)
    
    # Initialize 12-element vectors with zeros
    neg_rem_run1 <- rep(0, 12)
    neg_forg_run1 <- rep(0, 12)
    neut_rem_run1 <- rep(0, 12)
    neut_forg_run1 <- rep(0, 12)
    
    neg_rem_run2 <- rep(0, 12)
    neg_forg_run2 <- rep(0, 12)
    neut_rem_run2 <- rep(0, 12)
    neut_forg_run2 <- rep(0, 12)
    
    neg_rem_run3 <- rep(0, 12)
    neg_forg_run3 <- rep(0, 12)
    neut_rem_run3 <- rep(0, 12)
    neut_forg_run3 <- rep(0, 12)
    
    # Assign 1 to the specific combination of interest for each vector
    neut_forg_run1[1] <- 1
    neg_forg_run1[2] <- 1
    neut_rem_run1[3] <- 1
    neg_rem_run1[4] <- 1
    
    neut_forg_run2[5] <- 1
    neg_forg_run2[6] <- 1
    neut_rem_run2[7] <- 1
    neg_rem_run2[8] <- 1
    
    neut_forg_run3[9] <- 1
    neg_forg_run3[10] <- 1
    neut_rem_run3[11] <- 1
    neg_rem_run3[12] <- 1 
    
    # memory contrasts within emotion and run
    emmean_mem_contrasts <- contrast(emm,
                                     method = list("neut rem > forg in run 1" = 
                                                     neut_rem_run1 > neut_forg_run1,
                                                   "neg rem > forg in run 1" = 
                                                     neg_rem_run1 > neg_forg_run1,
                                                   "neut rem > forg in run 2" = 
                                                     neut_rem_run2 > neut_forg_run2,
                                                   "neg rem > forg in run 2" = 
                                                     neg_rem_run2 > neg_forg_run2,
                                                   "neut rem > forg in run 3" = 
                                                     neut_rem_run3 > neut_forg_run3,
                                                   "neg rem > forg in run 3" = 
                                                     neg_rem_run3 > neg_forg_run3
                                     ), adjust = "fdr")
    
    
    # compute effect size and apply way to complicated renaming
    within_run_emo_btw_mem_df <- as.data.frame(emmean_mem_contrasts) %>%
      left_join(as.data.frame(confint(emmean_mem_contrasts, 
                                      method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df")) %>%
      mutate(contrast_type = "difference between memory categories")
    
    # run 1
    run1_contrasts <- pairs(contrast(emm, method = 
                                       list("neg rem > forg in run 1" = 
                                              neg_rem_run1 > neg_forg_run1,
                                            "neut rem > forg in run 1" = 
                                              neut_rem_run1 > neut_forg_run1
                                       )))
    
    run1_df <- as.data.frame(run1_contrasts) %>%
      left_join(as.data.frame(confint(run1_contrasts, method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df"))
    
    # run 2
    run2_contrasts <- pairs(contrast(emm, method = 
                                       list("neg rem > forg in run 2" = 
                                              neg_rem_run2 > neg_forg_run2,
                                            "neut rem > forg in run 2" = 
                                              neut_rem_run2 > neut_forg_run2
                                       )))
    
    run2_df <- as.data.frame(run2_contrasts) %>%
      left_join(as.data.frame(confint(run2_contrasts, method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df"))
    
    # run 3
    run3_contrasts <- pairs(contrast(emm, 
                                     method = 
                                       list("neg rem > forg in run 3" = 
                                              neg_rem_run3 > neg_forg_run3,
                                            "neut rem > forg in run 3" 
                                            = neut_rem_run3 > neut_forg_run3
                                       )))
    
    run3_df <- as.data.frame(run3_contrasts) %>%
      left_join(as.data.frame(confint(run3_contrasts, method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df"))
    
    # combine within run dfs and apply fdr correction
    within_run_df <- rbind(run1_df, run2_df, run3_df) %>% 
      mutate(contrast_type = "difference between emotions in memory type")
    
    within_run_df$p.value <- p.adjust(within_run_df$p.value, 
                                      method = "fdr")
    # combine both emmean computations
    emmean_df <- rbind(within_run_df, within_run_emo_btw_mem_df) 
    
    ####
    # slope tests
    emtrend <- emtrends(this_model, specs = ~ emotion * subsMemory , 
                        var = "run_centered", adjust = "mvt",
                        lmer.df = "satterthwaite", lmerTest.limit = 99999)
    
    neut_forg <- c(1,0,0,0)
    neg_forg <- c(0,1,0,0)
    neut_rem <- c(0,0,1,0)
    neg_rem <- c(0,0,0,1)
    
    emtrend_dif_contrast_memory <- contrast(emtrend, 
                                            method = 
                                              list("neutral remembered > forgotten" 
                                                   = neut_rem > neut_forg,
                                                   "negative remembered > forgotten" 
                                                   = neg_rem > neg_forg), adjust = 
                                              "fdr")
    
    emtrend_df <- as.data.frame(emtrend_dif_contrast_memory)%>%
      left_join(as.data.frame(confint(emtrend_dif_contrast_memory, method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df")) %>%
      mutate(contrast_type = "slope related to successfull memory")
    
    # interaction contrast
    emtrend_dif_int_contrast <- pairs(contrast(emtrend, method = 
                                                 list("neg rem > forg" = 
                                                        neg_rem > neg_forg,
                                                      "neut rem > forg" = 
                                                        neut_rem > neut_forg), 
                                               adjust = "fdr"))
    
    emtrend_dif_int_df <- as.data.frame(emtrend_dif_int_contrast) %>%
      left_join(as.data.frame(confint(emtrend_dif_int_contrast, method = "mvt")), 
                by = c("contrast", "estimate", "SE", "df")) %>%
      mutate(contrast_type = 
               "difference between emotions in slope related to successfull memory")%>%
      rbind(emtrend_df) %>%
      rbind(emmean_df) %>%
      mutate(ROI = roi,
             contrast = str_replace_all(contrast, "emotionneutral", 
                                        "neutral"),
             contrast = str_replace_all(contrast, "emotionnegative", 
                                        "negative"),
             contrast = str_replace_all(contrast, "subsMemory0", 
                                        "forgotten"),
             contrast = str_replace_all(contrast, "subsMemory1", 
                                        "remembered"),
             contrast = str_replace_all(contrast, "\\(|\\)", ""),
             contrast = str_replace_all(contrast, "effect", "")) %>%
      dplyr::select(contrast, contrast_type, ROI, estimate, SE, lower.CL, 
                    upper.CL, t.ratio, df, p.value)
    
    colnames(emtrend_dif_int_df) <- c("contrast", "contrast_type","ROI", "estimate", 
                                      "SE", "lower_CI", "upper_CI", "t", "df", "p")
    
    main_contrast_df <- rbind(main_contrast_df, emtrend_dif_int_df)
    
  }
  
  return(main_contrast_df)
}