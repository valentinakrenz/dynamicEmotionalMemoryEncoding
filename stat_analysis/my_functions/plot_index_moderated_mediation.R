library(ggplot2)

plot_index_moderated_mediation <- function(final_df, save = TRUE){
  
  my_theme <- theme_classic() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 22, family = "Roboto Condensed"),
      axis.title.y = element_text(size = 16, family = "Roboto Condensed"),
      axis.text.y = element_text(size = 14, colour = "black", family = "Roboto Condensed"), 
      axis.title.x = element_text(size = 16, family = "Roboto Condensed"),
      axis.text.x = element_text(size = 14, colour = "black", family = "Roboto Condensed"), 
      legend.title = element_blank(),
      legend.text = element_blank(),
      axis.line = element_line(linewidth = 0.8),  
      axis.ticks = element_line(linewidth = 0.8, colour = "black"),  
      axis.ticks.length = unit(0.2, "cm"),
      strip.text = element_text(size = 16, family = "Roboto Condensed"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      aspect.ratio = 2
    )
  
  lower_CI <- unique(final_df$lower_CI)
  upper_CI <- unique(final_df$upper_CI)
  
  index_value <- paste0("95% CI [", 
                        round(lower_CI, 3), ", ", 
                        round(upper_CI, 3), "]")
  
  EES_ROI <- unique(final_df$mediator)
  beta_ROI <- unique(final_df$moderator)
  
  plot_data <- subset(final_df, mediator == EES_ROI & 
                        moderator == beta_ROI & effect == "ACME (average)") %>%
    mutate(lowerCI = `95% CI Lower`,
           upperCI = `95% CI Upper`,
           moderator = factor(moderator_value, levels = c("low",
                                                          "average",
                                                          "high"), 
                              labels = c(-1, 0, 1)),
           indirectEffect = Estimate,
    ) %>%
    dplyr::select(indirectEffect, lowerCI, upperCI, Pfdr, moderator) %>%
    mutate(
      annotation = case_when(
        Pfdr < 0.001 ~ "***",
        Pfdr < 0.01 ~ "**",
        Pfdr < 0.05 ~ "*",
        Pfdr < 0.06 ~ "+",
        TRUE ~ NA_character_),
      y = upperCI + 0.001
    )
  
  my_x_title = expression("amygdala activity at initial exposure (" * italic(z) * "-transf. beta)")
  
  my_y_title = paste("indirect effect of emotion on memory throuh \n superior parietal encoding pattern similarity" )
  
  # Ensure the moderator is numeric for the plot
  plot_data$moderator <- as.numeric(as.character(plot_data$moderator))
  
  # couldn't get alpha to work for the arrow, so I computed the shade when the original colour includes alpha
  # Alpha value
  # Original color components
  original_red <- 31
  original_green <- 75
  original_blue <- 102
  alpha <- 0.7
  
  # Calculate the new color components
  new_red <- round(original_red * alpha + 255 * (1 - alpha))
  new_green <- round(original_green * alpha + 255 * (1 - alpha))
  new_blue <- round(original_blue * alpha + 255 * (1 - alpha))
  
  # Convert to hexadecimal
  new_color <- rgb(new_red, new_green, new_blue, maxColorValue = 255)
  
  # Plotting
  p <- ggplot(plot_data, aes(x = moderator, y = indirectEffect)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.1, size = 0.8) +
    geom_line(aes(group = 1), alpha = alpha, size =  2.5, color = "#1F4B66") +
    labs(x = my_x_title, y = my_y_title) +
    geom_text(aes(x = moderator, y = y, 
                  label = annotation), size = 8, 
              family = "Roboto Condensed") +
    # Add index_value to the plot
    annotate("text", x = 0, y = 0.0245, 
             label = index_value, hjust = 0.5, color = new_color, 
             family = "Roboto Condensed", size = 6) +
    
    geom_segment(aes(x = 0, y = 0.0235, xend = 0.5, yend = 0.0135),
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed"), 
                 color =  new_color, size = 1.2) + 

    
    scale_y_continuous(limits = c(0,0.025))+
    scale_x_continuous(limits = c(-1.25, 1.25),
                       breaks = c(-1, 0, 1),
                       labels = c(-1, 0, 1)) +
    
    guides(x = "axis_truncated") +
    guides(y = "axis_truncated") +
    
    my_theme 

  # Save the plot using the specified ROI_i and output_name
  if (save){
    svg_file_path <- file.path(output_dir, 
                               paste0("index_moderated_mediation.svg"))
    
    # Arrange and save the plots for this effect
    ggsave(filename = svg_file_path, plot = p)}
  
  # return p
  return(p)
}
