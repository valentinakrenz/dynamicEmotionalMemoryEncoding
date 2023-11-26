library(emmeans)
library(dplyr)
library(stringr)

# get slope results from emtrends #####
get_posthoc_trends <- function(models_list = models_list, ROIs = ROIs){
  trend_df <- data.frame(
    contrast = character(),
    ROI = character(),
    estimate = numeric(), 
    SE = numeric(),
    upper_CI = numeric(),
    lower_CI = numeric(),
    t = numeric(),
    p = numeric(),
    p_unrounded = numeric()
  )
  
  for (roi in ROIs){
    this_model <- models_list[[roi]]
    
    emtrend_contrast <- contrast(emtrends(this_model,  ~ emotion_centered:subsMemory_centered, 
                                          var = "run_centered", adjust = "FDR",
                                          lmer.df = "satterthwaite", lmerTest.limit = 99999))
    
    emtrend_df <-       invisible(as.data.frame(emtrend_contrast)) %>%
      left_join(as.data.frame(confint(emtrend_contrast, 
                                      adjust = "mvt")), 
                by = c("contrast","estimate","SE","df"))%>%
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
             contrast = str_replace_all(contrast, "effect", ""),
             p_unrounded = p.value) %>%
      dplyr::select(contrast, ROI, estimate, SE, lower.CL, 
                    upper.CL, t.ratio, df, p.value, p_unrounded)
    
    colnames(emtrend_df) <- c("contrast", "ROI", "estimate", "SE", "lower_CI", 
                              "upper_CI", "t", "df", "p", "p_unrounded")
    
    emtrend_df <- round_two_spaces(emtrend_df, start_col = 3, 
                                   end_col = ncol(emtrend_df)-2)
    
    emtrend_df <- round_p(emtrend_df, p_col = ncol(emtrend_df)-1)
    
    trend_df <- rbind(emtrend_df, trend_df)
    
  }
  
  return(trend_df)
}