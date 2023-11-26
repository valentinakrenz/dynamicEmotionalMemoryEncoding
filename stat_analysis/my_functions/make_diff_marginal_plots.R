
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)


# get marginal contrast plots for three-way interaction #
make_diff_marginal_plots <- function(ROIs = ROIs, this_contrast_df = int_contrast_df, 
                                     save = TRUE, n_q = 3, n_p = 5, this_output_name = 
                                       "univ_marginalPlots"){
  
  
  grey <- "#717978" # Way Cooler, More Desaturated Darkest Petroly Grey
    pink = "hotpink4"
      
    
    my_theme <- theme_classic() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22, family = "Roboto Condensed"),
        axis.title.y = element_text(size = 18, family = "Roboto Condensed"),
        axis.text.y = element_text(size = 16, colour = "black", family = "Roboto Condensed"),
        axis.title.x = element_text(size = 18, family = "Roboto Condensed"),
        axis.text.x = element_text(size = 16, colour = "black", family = "Roboto Condensed"),
        legend.title = element_text(size = 18, family = "Roboto Condensed"),
        legend.text = element_text(size = 16, family = "Roboto Condensed"),
        axis.line = element_line(linewidth = 0.8),  
        axis.ticks = element_line(linewidth = 0.8, colour = "black"), 
        axis.ticks.length = unit(0.2, "cm"),
        strip.text = element_text(size = 16, family = "Roboto Condensed"),
        strip.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
  
  this_contrast_df <- this_contrast_df %>% unique()
  
  marginal_effects_df <- subset(this_contrast_df, ROI %in% ROIs & 
                                  contrast_type == 
                                  "difference between memory categories") %>%
    # Create the "emotion" and "run" columns
    mutate(
      emotion = case_when(
        str_detect(contrast, "^neut") ~ "neutral",
        str_detect(contrast, "^neg") ~ "negative",
        TRUE ~ "other"
      ),
      run = as.integer(str_extract(contrast, "\\d+$")),
      annotation = case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        p < 0.06 ~ "+",
        TRUE ~ NA_character_)
    )
  
  my_y_title = "remembered > forgotten"
  
  q_y_title <- paste("slope") 
  
  # Initialize an empty list to hold plot
  plot_list <- list()
  
  # get min and max for ymin and ymax for standardized axes
  value_plus_SE <- c()
  value_min_SE <- c()
  
  for (row in 1:nrow(marginal_effects_df)){
    this_row <- marginal_effects_df[row,]
    # Calculate min predicted value - its SE
    value_min_SE <- c(value_min_SE, this_row$estimate - this_row$SE)
    # Calculate max predicted value + corresponding SE
    value_plus_SE <- c(value_plus_SE, this_row$estimate + this_row$SE)
  }
  
  # Calculate ymin and ymax across all predicted values
  ymin <- min(value_min_SE)
  ymax <- max(value_plus_SE)
  SEmax <- max(marginal_effects_df$SE)
  
  # Round ymin and ymax
  ymin_rounded = floor(ymin * 100) / 100  # Round down to two decimal places
  ymax_rounded = ceiling(ymax * 100) / 100  # Round up to two decimal places
  ymax_SE_rounded = ceiling((ymax+3*SEmax) * 100) / 100
  
  # dfs for annotations
  annotation_int_WithinRun_df <- subset(this_contrast_df, 
                                        contrast_type == 
                                          "difference between emotions in memory type") %>%
    mutate(annotation = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.06 ~ "+",
      TRUE ~ NA_character_),
      run = as.integer(str_extract(contrast, "run \\d+$") %>% str_extract("\\d+")), 
      contrast = str_replace_all(contrast, " in run \\d+", "")  # Remove " in run N" from everywhere
    )
  
  # Modify the data frame
  annotation_mem_df <- subset(this_contrast_df) %>%
    filter(contrast_type == "difference between memory categories") %>%
    mutate(
      emotion = str_extract(contrast, "neg|neut"),
      run = as.numeric(str_extract(contrast, "\\d")),
      annotation = case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        p < 0.06 ~ "+",
        TRUE ~ NA_character_),
    )
  
  # create a df with annotations for significance of slope in rem > forg
  slope_df <- subset(this_contrast_df, 
                     contrast_type == 
                       "slope related to successfull memory")  %>%
    mutate(annotation = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.06 ~ "+",
      TRUE ~ NA_character_)) %>%
    mutate(
      emotion = str_extract(contrast, "neutral|negative")
    ) %>% unique %>%
    mutate(emotion = factor(emotion, levels=c("neutral", "negative")))
  
  # min max slopes for y axis
  min_max_df <- slope_df   %>%
    mutate(min_val = estimate - SE,
           max_val = estimate + 2.5*SE) %>%
    dplyr::summarise(min_value = min(min_val, na.rm = TRUE),
              max_value = max(max_val, na.rm = TRUE),
              max_SE = max(SE))
  
  max_slope_SE <- min_max_df$max_SE
  
  # Round ymin and ymax
  min_slope_value = floor(min_max_df$min_value * 100) / 100  # Round down to two decimal places
  max_slope_value = ceiling( min_max_df$max_value * 100) / 100  # Round up to two decimal places
  
  # dfs for annotations
  dif_slope_df <- subset(this_contrast_df, contrast_type == 
                           "difference between emotions in slope related to successfull memory") %>%
    mutate(annotation = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.06 ~ "+",
      TRUE ~ NA_character_))
  
  if (save) {
    # Create the 'univ_sjplots' folder if it doesn't exist
    individual_output_dir <- file.path(plot_dir, paste(this_output_name, sep="_"))
    
    if (!dir.exists(individual_output_dir)) {
      dir.create(individual_output_dir)
    }
  }
  
  
  # Loop through marginal_effects_df and plot
  for (r_i in 1:length(ROIs)) {
    
    this_ROI <- as.character(ROIs[r_i])
    
    this_marginal_effect <- marginal_effects_df %>%
      filter(ROI == this_ROI) 
    
    # annotation for memory effect within each emotion within run
    this_annotation_mem_df <- subset(annotation_mem_df, ROI == this_ROI) 
    
    # Create max_df with the max estimates for each run
    max_df <- this_annotation_mem_df %>%
      dplyr::group_by(run) %>%
      dplyr::summarise(
        y_neut = max(estimate + 1.5 * SE, na.rm = TRUE),
        y_neg = max(estimate + 2 * SE, na.rm = TRUE)
      )
    
    
    # Add the y_neut and y_neg values to this_annotation_mem_df based on run
    this_annotation_mem_df <- this_annotation_mem_df %>%
      dplyr::left_join(max_df, by = "run") %>%
      dplyr::mutate(
        y = case_when(
          emotion == "neut" ~ y_neut,
          emotion == "neg" ~ y_neg,
          TRUE ~ NA_real_
        ),
        emotion = factor(emotion, levels = c("neut", "neg"), 
                         labels = c("neutral", "negative"))
      )
    
    max_dif_df <- this_annotation_mem_df %>%
      dplyr::group_by(run) %>%
      dplyr::summarise(y = max(estimate + 2.5*SE, na.rm = TRUE))
    
    # annotation for dif in emos in memory effect within run
    this_annotation_int_WithinRun_df <- subset(annotation_int_WithinRun_df, 
                                               ROI == this_ROI) %>%
      left_join(max_dif_df, by = "run")
    
    this_slope_df <- subset(slope_df, ROI == this_ROI) %>%
      mutate(y = case_when(
        p < 0.06 & estimate < 0 ~ -(1.40 * max_slope_SE),
        p < 0.06 & estimate > 0 ~ (max_slope_SE/2.65)
      ))
    
    slope_dif_max_df <- this_slope_df %>%
      dplyr::summarise(
        y_line = max(estimate + 1.75 * SE, na.rm = TRUE),
        y_star = max(estimate + 2.5*SE, na.rm = TRUE)
      )
    
    this_dif_slope_df <- subset(dif_slope_df, ROI == this_ROI) %>%
      mutate(
        y_line = case_when(
          p <= 0.06 ~ slope_dif_max_df$y_line[1],
          TRUE ~ NA_real_
        ),
        y_star = case_when(
          p <= 0.06 ~ slope_dif_max_df$y_star[1],
          TRUE ~ NA_real_
        )
      )
    
    ROI_contrast_df <- subset(this_contrast_df, ROI == this_ROI) 
    
    this_title <- paste(unique(ROI_contrast_df$lat),unique(ROI_contrast_df$short_label))
    
    this_file <- paste(unique(ROI_contrast_df$lat), unique(ROI_contrast_df$short_label), sep="_")
    
    
    q <- ggplot(this_slope_df, aes(x = emotion, y = estimate, fill = emotion, 
                                   colour = emotion)) +
      geom_col(alpha = 0.6, color = NA) + # Bar plot with no border
      geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                    width = 0.15, size = 0.7, linetype = "solid", alpha = 1) +  # Error bars
      scale_fill_manual(values = c(neutral = grey,
                                   negative = pink)) +  # Manual color setting for fill
      scale_color_manual(values = c(neutral = grey,
                                    negative = pink))+  # Manual color setting for points
      
      scale_y_continuous(
        limits = c(min_slope_value, max_slope_value),
        breaks = unique(c(pretty(c(min_slope_value, max_slope_value), n = n_q)))
      ) +
      guides(y = "axis_truncated")+
      ggtitle(q_y_title) +
      my_theme + 
      theme(
        axis.ticks = element_line(size = 0.9),
        axis.text.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),#element_text(size = 14),
        axis.line.y = element_line(size = 0.6),
        axis.ticks.y = element_line(size = 0.6),
        aspect.ratio = 3)+ 
      
      geom_text(data = this_slope_df,
                aes(x = emotion, y = y, label = annotation), 
                colour = "white", size = 5, 
                family = "Roboto Condensed") + 
      
      geom_text(data = this_dif_slope_df,
                aes(x = 1.5, y = y_star, 
                    label = annotation), 
                colour = "black", size = 5, inherit.aes = FALSE, 
                family = "Roboto Condensed") + 
      
      geom_segment(data = this_dif_slope_df, inherit.aes = FALSE,colour="black",
                   linetype = "solid", size = 0.7,
                   aes(x = 1, y = y_line, 
                       xend = 2, 
                       yend = y_line))
    
    p <- ggplot(this_marginal_effect, aes(x = run, y = estimate, 
                                          color = emotion)) +
      geom_line(size = 1.5, alpha = 0.6) +
      
      geom_point(size=1.5) + # Adding the dots for the predicted values
      
      geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                    width = 0.1, size = 0.8, alpha = 1, linetype = "solid") + # Adding error bars
      scale_color_manual(values = c(neutral = grey,
                                    negative = pink)) +
      labs(y = my_y_title) + my_theme +
      
      scale_x_continuous(
        limits = c(1-0.2, 3+0.2),
        breaks = c(1, 2, 3),
        labels = c("1", "2", "3")) + 
      
      scale_y_continuous(
        limits = c(ymin_rounded, ymax_SE_rounded),
        breaks = unique(c(pretty(c(ymin_rounded, ymax_SE_rounded), n = n_p), 0))
      ) +
      
      ggtitle(this_title) + 
      
      theme(legend.position = "none",
            strip.text.x = element_text(margin = margin(b = 15, t = 10)),
            aspect.ratio = 1.8,
            axis.text.y = element_text(size = 18),
            axis.text.x = element_text(size = 18),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=20))+
            #plot.title = element_blank())+
      
      geom_text(data = this_annotation_int_WithinRun_df, 
                aes(x = this_annotation_int_WithinRun_df$run, y = y, 
                    label = annotation), size = 6, 
                inherit.aes = FALSE, family = "Roboto Condensed")+
      
      guides(x = "axis_truncated") +
      guides(y = "axis_truncated")
    
    
    # Plot the main plot first
    main_plot <- ggdraw(p)
    
    # Add the subplot
    main_plot_with_sub <- main_plot +
      draw_plot(q, x = .7, y = .4, width = .2, height = .6)
    
    # Print the final plot
    print(main_plot_with_sub)
    
    # Add the plot to the list
    plot_list[[this_ROI]] <- main_plot_with_sub
    
    if (save){
      # Save the plot using the specified ROI_i and output_name
      svg_file_path <- file.path(individual_output_dir, 
                                 paste0(this_output_name, 
                                 "_memoryXrunXemo_inklSubplot", 
                                 this_file, ".svg"))
      
      ## Arrange and save the plots for this effect
      ggsave(filename = svg_file_path, plot = main_plot_with_sub)
      
    }
  }
  return(plot_list)
}
