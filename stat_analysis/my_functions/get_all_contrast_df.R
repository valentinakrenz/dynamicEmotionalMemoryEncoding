# get results of all post-hoc analyses for remembered > forgotten in each ROI and each effect

# required packages
library(dplyr)
library(emmeans)
library(lmerTest)
library(purrr)  
library(tidyr)   
library(broom)  

# custom functions needed # should be in same folder
#source("get_emmeans_emtrends.R")
#source("analyze_posthoc_effects.R")

get_all_contrast_df <- function(stat_df, models_list){
  # Extract unique ROIs for each effect
  stat_df <- subset(stat_df, effect == all_effects)
  memory_effect_ROIs <- unique(stat_df$ROI[stat_df$effect == 
                                             "main effect memory"])
  memory_run_effect_ROIs <- unique(stat_df$ROI[stat_df$effect == 
                                                 "memory × run"])
  memory_run_emotion_effect_ROIs <- unique(stat_df$ROI[stat_df$effect == 
                                                         "memory × run × emotion"])
  memory_emotion_effect_ROIs <- unique(stat_df$ROI[stat_df$effect == 
                                                         "memory × emotion"])
  
  # Initialize the final results dataframe
  final_df <- data.frame()
  
  # Loop for "main effect memory"
  for (this_ROI in memory_effect_ROIs) {
    
    this_stat <- subset(stat_df, ROI == this_ROI)
    effect <- "main effect memory"
    emmean <- emmeans(models_list[[this_ROI]], pairwise ~ subsMemory, 
                      lmer.df = "satterthwaite", lmerTest.limit = 99999)
    
    contrast_data <- as.data.frame(emmean$contrasts)
    conf_data <- as.data.frame(confint(emmean, adjust = "mvt")$contrasts)
    
    this_contrast_type <- "overall difference between memory types"
    this_contrast <- "remembered > forgotten" 
    
    # Create a new dataframe with the data
    temp_df <- data.frame(
      effect = this_stat$effect[1],
      cluster_name = this_stat$cluster_name[1],
      ROI = this_ROI,
      contrast = this_contrast,
      contrast_type = this_contrast_type,
      estimate = -(contrast_data$estimate),
      SE = contrast_data$SE,
      lower_CI = -(conf_data$lower.CL),
      upper_CI = -(conf_data$upper.CL),
      t = -(contrast_data$t.ratio),
      df = contrast_data$df,
      p = contrast_data$p.value
    ) %>%
      mutate(posthoc_effect = case_when(
        t > 0 ~ "higher activity for subsequently remembered compared to forgotten",
        t < 0 ~ "lower activity for subsequently remembered compared to forgotten"
      ),
      full_label = this_stat$full_label[1],
      short_label = this_stat$short_label[1],
      lat = this_stat$lat[1]
      )
    
    final_df <- rbind(final_df, temp_df) 
    
  }
  
  # Loop for "memory × run"
  for (this_ROI in memory_run_effect_ROIs) {
    
    this_stat <- subset(stat_df, ROI == this_ROI)
    effect <- "memory × run"
    
    emtrend <- emtrends(models_list[[this_ROI]], pairwise ~ subsMemory_centered, 
                        var = "run_centered", 
                        lmer.df = "satterthwaite", lmerTest.limit = 99999)
    
    contrast_data <- as.data.frame(emtrend$contrasts)
    conf_data <- as.data.frame(confint(emtrend, adjust = "mvt")$contrasts)
    
    this_contrast_type <- "difference in slope between memory types"
    this_contrast <- "change over runs for rem > forg"
    
    temp_df <- data.frame(
      effect = this_stat$effect[1],
      cluster_name = this_stat$cluster_name[1],
      ROI = this_ROI,
      contrast = this_contrast,
      contrast_type = this_contrast_type,
      estimate = -(contrast_data$estimate),
      SE = contrast_data$SE,
      lower_CI = -(conf_data$lower.CL),
      upper_CI = -(conf_data$upper.CL),
      t = -(contrast_data$t.ratio),
      df = contrast_data$df,
      p = contrast_data$p.value,
      p_unrounded = contrast_data$p.value
    ) %>%
      mutate(posthoc_effect = case_when(
        t > 0 ~ "significant increase for subsequently remembered",
        t < 0 ~ "significantly decrease for subsequently remembered"
      ),combined_effect = paste(this_stat$cluster_name[1], 
                                "(", posthoc_effect, ")"),
      full_label = this_stat$full_label[1],
      short_label = this_stat$short_label[1],
      lat = this_stat$lat[1]
      )
    
    
    final_df <- rbind(final_df, temp_df) 
  }
  
  # Loop for "memory × emotion"
  for (this_ROI in memory_emotion_effect_ROIs) {
    
    this_stat <- subset(stat_df, ROI == this_ROI)
    effect <- "memory × emotion"
    
    this_model <- models_list[[this_ROI]]
      
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
      contrast_data <- as.data.frame(emmean_int_contrasts) %>%
        left_join(as.data.frame(confint(emmean_int_contrasts, 
                                        method = "mvt")), 
                  by = c("contrast", "estimate", "SE", "df")) %>%
        mutate(contrast_type = "difference between emotion in memory categories",
               posthoc_effect = case_when(
                 t.ratio > 0 ~ "higher for negative compared to neutral",
                 t.ratio < 0 ~ "lower for negative compared to neutral"
               ))
      
       temp_df <- data.frame(
         effect = this_stat$effect[1],
         cluster_name = this_stat$cluster_name[1],
         ROI = this_ROI,
         contrast = contrast_data$contrast,
         contrast_type = contrast_data$contrast_type[1],
         estimate = contrast_data$estimate,
         SE = contrast_data$SE,
         lower_CI = contrast_data$lower.CL,
         upper_CI = contrast_data$upper.CL,
         t = contrast_data$t.ratio,
         df = contrast_data$df,
         p = contrast_data$p.value,
         posthoc_effect = contrast_data$posthoc_effect[1],
         full_label = this_stat$full_label[1],
         short_label = this_stat$short_label[1],
         lat = this_stat$lat[1]
         )
       
       final_df <- rbind(final_df, temp_df)
       
       # remembered > forgotten estimates for plotting:
       # memory contrasts within emotion between memory types
       emm <- emmeans(this_model, pairwise ~ subsMemory | emotion, 
                      lmer.df = "satterthwaite", 
                      lmerTest.limit = 99999, adjust = "fdr")
       
       # add conf interval and save in df
       contrast_data <- as.data.frame(emm$contrast) %>%
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
         dplyr::select(-temp_lower, -emotion) %>%  # Remove the temporary column and emotion
         mutate(contrast_type = "difference between memory categories",
                posthoc_effect = temp_df$posthoc_effect[1])
       
       temp_df <- data.frame(
         effect = this_stat$effect[1],
         cluster_name = this_stat$cluster_name[1],
         ROI = this_ROI,
         contrast = contrast_data$contrast,
         contrast_type = contrast_data$contrast_type[1],
         estimate = contrast_data$estimate,
         SE = contrast_data$SE,
         lower_CI = contrast_data$lower.CL,
         upper_CI = contrast_data$upper.CL,
         t = contrast_data$t.ratio,
         df = contrast_data$df,
         p = contrast_data$p.value,
         posthoc_effect = contrast_data$posthoc_effect[1],
         full_label = this_stat$full_label[1],
         short_label = this_stat$short_label[1],
         lat = this_stat$lat[1]
       )
       
       final_df <- rbind(final_df, temp_df)

  }
  
  # Loop for "memory × run × emotion"
  for (this_ROI in memory_run_emotion_effect_ROIs) {
    
    effect <- "memory × run × emotion"
    this_stat <- subset(stat_df, ROI == this_ROI)
    
    result_df <- get_emmeans_emtrends(models_list = models_list, this_ROI) %>%
      mutate(cluster_name = this_stat$cluster_name[1],
             effect = effect,
             full_label = this_stat$full_label[1],
             short_label = this_stat$short_label[1],
             lat = this_stat$lat[1])
    
    result_df <- analyze_posthoc_effects(result_df)
    
    final_df <- bind_rows(final_df, result_df)
  }
  
  final_df <- final_df %>% unique()
  return(final_df)
}