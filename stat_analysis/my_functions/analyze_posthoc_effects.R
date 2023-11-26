# check what drives the three-way interaction in each ROI

library(dplyr)

analyze_posthoc_effects <- function(contrast_d) {
  # Initialize an empty dataframe to hold the results
  res_df <- data.frame()
  
  # Loop over each ROI
  for (name in unique(contrast_d$ROI)) {
    
    # Filter data for current ROI
    current_data <- contrast_d %>% filter(ROI == name)
    
    # Initialize a variable to hold posthoc_effect
    posthoc_effect <- NA # default value
    
    # Check both conditions for 'decrease for emotionally negative'
    if (
      (
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(p) < 0.05 & 
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(t) > 0
        ) &
        (
          (current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(p) < 0.05 & 
           current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(t) < 0) |
          (current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(p) > 0.05)
        )
      ) |
      (
        current_data %>% filter(contrast == "negative remembered > forgotten") %>% 
        pull(p) < 0.05 &
        current_data %>% filter(contrast == "negative remembered > forgotten") %>% 
        pull(t) < 0 & 
        (
          current_data %>% filter(contrast == "neutral remembered > forgotten") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == "neutral remembered > forgotten") %>% 
          pull(t) > 0
        )
      )
    ) {
      posthoc_effect <- 'decrease for emotionally negative items'

    } 
    
    # Check for 'increase for emotionally neutral'
    else if (
      (
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
          pull(p) < 0.05 &
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
          pull(t) < 0
        ) &
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(t) > 0
        )
      ) | 
      (
        current_data %>% filter(contrast == 
                                "neutral remembered > forgotten") %>% 
        pull(p) < 0.05 &
        current_data %>% filter(contrast == 
                                "neutral remembered > forgotten") %>% 
        pull(t) > 0 & 
        (
          current_data %>% filter(contrast == 
                                  "negative remembered > forgotten") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == 
                                  "negative remembered > forgotten") %>% 
          pull(t) < 0
        )
      )
    ){
      posthoc_effect <- "increase for emotionally neutral items"

    }
    else if (   # Check for 'decrease for emotionally neutral'
      (
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(p) < 0.05 & 
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(t) < 0
        ) &
        (
          (current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(p) < 0.05 & 
           current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(t) > 0) |
          (current_data %>% filter(contrast == 
                                   "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
           pull(p) > 0.05)
        )
      ) |
      (
        current_data %>% filter(contrast == 
                                "neutral remembered > forgotten") %>% 
        pull(p) < 0.05 &
        current_data %>% filter(contrast == 
                                "neutral remembered > forgotten") %>% 
        pull(t) < 0 & 
        (
          current_data %>% filter(contrast == 
                                  "negative remembered > forgotten") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == 
                                  "negative remembered > forgotten") %>% 
          pull(t) > 0
        )
      )
    ){
      posthoc_effect <- "decrease for emotionally neutral items"
      
    }
    # Check for 'increase for emotionally negative'
    else if (
      (
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
          pull(p) < 0.05 &
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 3 - neut rem > forg in run 3") %>% 
          pull(t) > 0
        ) &
        (
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == 
                                  "neg rem > forg in run 1 - neut rem > forg in run 1") %>% 
          pull(t) < 0
        )
      ) | 
      (
        current_data %>% filter(contrast == 
                                "negative remembered > forgotten") %>% 
        pull(p) < 0.05 &
        current_data %>% filter(contrast == 
                                "negative remembered > forgotten") %>% 
        pull(t) > 0 & 
        (
          current_data %>% filter(contrast == 
                                  "neutral remembered > forgotten") %>% 
          pull(p) > 0.05 |
          current_data %>% filter(contrast == 
                                  "neutral remembered > forgotten") %>% 
          pull(t) < 0
        )
      )
    ){
      posthoc_effect <- "increase for emotionally negative items"
      
    }
    
    # Append the current ROI and posthoc_effect to the res_df
    res_df <- rbind(res_df, data.frame(ROI = name, posthoc_effect = posthoc_effect))
    
  }
  
  # Merge the results back into the original data
  final_contrast_d <- merge(contrast_d, res_df, by = "ROI", all.x = TRUE)
  
  return(final_contrast_d)
}