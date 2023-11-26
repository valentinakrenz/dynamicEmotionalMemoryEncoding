# plot free recall

plot_free_recall <- function(mean_df, save = TRUE){
my_theme <- theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 24, family = "Roboto Condensed"),
    axis.title.y = element_text(size = 24, family = "Roboto Condensed"),
    axis.text.y = element_text(size = 22, colour = "black", family = "Roboto Condensed"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_blank(),
    axis.line = element_line(linewidth = 1.1, colour = "black"),  
    axis.ticks = element_line(linewidth = 1.1, colour = "black"),  
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 22, family = "Roboto Condensed", margin = margin(b = 15, t = 10)),
    strip.background = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "none",  # Remove the legend
    aspect.ratio = 3  # Set the aspect ratio to 3
  )

# prepare data 
dodge <- 0.025

plot_data_summary <- mean_df %>%
  group_by(emotion) %>%
  dplyr::summarise(
    mean_emotion = mean(mean_subsMemory, na.rm = TRUE),
    SE_emotion = sd(mean_subsMemory, na.rm = TRUE) / sqrt(n())
  )

plot_data <- mean_df %>% #for connected data points
  mutate(x = case_when(emotion == "neutral" ~ 1 + dodge + 0.2,
                       emotion == "negative" ~ 2 - dodge - 0.2))

# Define the dodge distance (e.g., 0.2)
dodge <- 0.2

p <- ggplot() +
  # Plot individual data points
  geom_point(data = plot_data, 
             aes(x = x, y = mean_subsMemory, group = sj, fill = "black", colour = NULL),
             size = 1.5, alpha = 0.3) +
  
  geom_line(data = plot_data,
            aes(x = x, group = sj, y = mean_subsMemory), 
            alpha = 0.3) +
  # Plot mean bars
  geom_col(data = plot_data_summary, 
           aes(x = emotion, y = mean_emotion, fill = emotion),
           position = position_dodge(width = dodge), 
           alpha = 0.6) +
  
  # Plot error bars
  geom_errorbar(data = plot_data_summary, 
                aes(x = emotion, 
                    ymin = mean_emotion - SE_emotion, 
                    ymax = mean_emotion + SE_emotion,
                    colour = emotion),
                width = 0.1, size = 0.8, 
                alpha = 1, linetype = "solid") +
  
  scale_fill_manual(values = c(neutral = grey,
                               negative = pink)) +  # Manual color setting for fill
  scale_color_manual(values = c(neutral = grey,
                                negative = pink)) +  # Manual color setting for points
  scale_x_discrete(name = "emotion", expand = c(0, 0)) +  
  scale_y_continuous(name = "subsequent memory (%)") +
  scale_x_discrete(name = "emotion") +
  guides(y = "axis_truncated")+
  my_theme 


if(save){
ggsave(file.path(plot_dir,"freeRecall.svg"), p)}

return(p)

}
