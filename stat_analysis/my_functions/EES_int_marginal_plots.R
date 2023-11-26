
library(dplyr)
library(ggplot2)
library(stringr)


grey <- "#717978" 
pink <- "hotpink4"
  
my_theme <- theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 24, family = "Roboto Condensed"),  # Size updated to 24
    axis.title.y = element_text(size = 20, family = "Roboto Condensed"),  # Size updated to 24
    axis.text.y = element_text(size = 18, colour = "black", family = "Roboto Condensed"),  # Size updated to 22
    axis.title.x = element_blank(),  # Removed x-axis title
    axis.text.x = element_blank(),  # Removed x-axis text
    axis.ticks.x = element_blank(),  # Removed x-axis ticks
    axis.line.x = element_blank(),  # Removed x-axis line
    legend.title = element_blank(),  # Size updated to 24
    legend.text = element_text(size = 18, family = "Roboto Condensed"),  # Size updated to 22
    axis.line = element_line(linewidth = 1),  # Use linewidth instead of size
    axis.ticks = element_line(linewidth = 1, colour = "black"),  # Use linewidth instead of size
    axis.ticks.length = unit(0.2, "cm"),
    strip.text = element_text(size = 16, family = "Roboto Condensed", margin = margin(b = 15, t = 10)),  # Margin added
    strip.background = element_blank(),  # Transparent background for strip
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line.y = element_line(size = 1.1, colour = "black"),  # Line width updated to 1.1
    axis.ticks.y = element_line(size = 1.1),  # Ticks size updated to 1.1
    aspect.ratio = 2.2  # Aspect ratio set to 1.8
  )
  

# get marginal contrast plots for three-way interaction #
EES_int_marginal_plots <- function(ROIs = ROIs, this_contrast_df = contrast_df, int_EES_df = int_EES_df, 
                                   save = TRUE, n_breaks = 4, this_output_name = 
                                     "EES_marginalPlots_"){
  
  this_contrast_df <- this_contrast_df %>% unique()
  
  # plot contrast values per emotion
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
      annotation = case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        p < 0.06 ~ "+",
        TRUE ~ NA_character_)
    )
  
  my_y_title = expression("remembered > forgotten (Fisher-transf. " * italic("r") * ")")

  # Initialize an empty list to hold plot
  plot_list <- list()
  
  # get min and max for ymin and ymax 
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

  # annotation for difference between emotions in memory effect
  annotation_int_df <- subset(int_EES_df, ROI %in% ROIs & 
                                effect == "memory × emotion") %>%
    mutate(annotation = case_when(
      Pfdr < 0.001 ~ "***",
      Pfdr < 0.01 ~ "**",
      Pfdr < 0.05 ~ "*",
      Pfdr < 0.06 ~ "+",
      TRUE ~ NA_character_),
      y_line = case_when(
        Pfdr <= 0.06 ~ (ymax_rounded - 0.5 * SEmax),
        TRUE ~ NA_real_
      ),
      y_star = case_when(
        Pfdr <= 0.06 ~ (ymax_rounded - 0.15 * SEmax),
        TRUE ~ NA_real_
      )
    )
  
  if (save) {
    # Create folder if it doesn't exist
    individual_output_dir <- file.path(plot_dir, paste(this_output_name, 
                                                       "subsMemory", sep="_"))
    
    if (!dir.exists(individual_output_dir)) {
      dir.create(individual_output_dir)
    }
  }
  
  # Prepare the data for plotting
  plot_data <- marginal_effects_df %>%
    mutate(y_mem = case_when(
      p < 0.06 & estimate < 0 ~ -(0.05 * SEmax),
      p < 0.06 & estimate > 0 ~ (SEmax/3)
    ))
  
  plot_list <- list()
  
  for (r in ROIs){
    
    this_plot_data <- subset(plot_data, ROI == r)
    
    this_annotation_int_df <- subset(annotation_int_df, ROI == r)
    
    this_ROI <- paste(this_annotation_int_df$lat, this_annotation_int_df$short_label, sep = " ")
  
  q <- ggplot(this_plot_data, aes(x = emotion, y = estimate, fill = emotion, 
                             colour = emotion)) +
    geom_col(alpha = 0.6, color = NA) + # Bar plot with no border
    geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                  width = 0.1, size = 0.8, alpha = 1, linetype = "solid") +  # Error bars
    scale_fill_manual(values = c(neutral = grey,
                                 negative = pink)) +  # Manual color setting for fill
    scale_color_manual(values = c(neutral = grey,
                                  negative = pink))+  # Manual color setting for points
    
    scale_y_continuous(
      limits = c(ymin_rounded, ymax_rounded),
      breaks = unique(c(pretty(c(ymin_rounded, ymax_rounded), n = n_breaks)))
    ) +
    guides(y = "axis_truncated")+
    my_theme +
    labs(y = my_y_title)+
    scale_x_discrete(limits = c("neutral", "negative")) +
    
    ggtitle(this_ROI) + 
    
    geom_text(data = this_plot_data,
              aes(x = emotion, y = y_mem, label = annotation), 
              colour = "white", size = 10, 
              family = "Roboto Condensed") + 
    
    geom_text(data = this_annotation_int_df,
              aes(x = 1.5, y = y_star, 
                  label = annotation), 
              colour = "black", size = 12, inherit.aes = FALSE, 
              family = "Roboto Condensed") + 
    
    geom_segment(data = this_annotation_int_df, inherit.aes = FALSE,
                 colour="black",
                 linetype = "solid", size = 1,
                 aes(x = 1, y = y_line, 
                     xend = 2, 
                     yend = y_line))
    
  plot_list[[r]] <- q
  
  # Save if necessary
  if (save) {
    individual_output_dir <- file.path(plot_dir, paste(this_output_name, 
                                                       "subsMemory", 
                                                       sep = "_"))
    if (!dir.exists(individual_output_dir)) {
      dir.create(individual_output_dir)
    }
    
    svg_file_path <- file.path(individual_output_dir, 
                               paste0(this_output_name, this_ROI,
                                      "_memoryXemo.svg"))
    
    ggsave(filename = svg_file_path, plot = q)
  }
  }
  
  return(plot_list)
} 

