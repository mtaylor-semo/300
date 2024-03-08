library(tidyverse)
library(ggthemes)
#library(extrafont)

# Registers ttf fonts for use with PDF output (ggsave), 
# enables them to be saved in PDF file (embed_fonts).
#loadfonts() 

#Change exam name and numbers for each grade.
# Number is order from A to F.

part1_plot <- "num_giraffe_spp1.png"
part2_plot <- "num_giraffe_spp2.png"

plot_colors <- RColorBrewer::brewer.pal(9, "GnBu")

part1_colors <- c(plot_colors[7], "#FFFFFF")
part2_colors <- c(plot_colors[7], plot_colors[9])

label_size = 13 # For x- and y-axis labels

part1 <- c(1, 0, 6, 7, 1, 1, 3, 0, 0, 2) # after part 1
#part <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10+) # to help me add values to the parts
part2 <- c(0, 1, 1, 5, 8, 4, 0, 0, 0, 1) # after part 2
max_val <- max(part1, part2)

giraffe_spp <- c("1","2","3","4","5", "6", "7", "8", "9", "10+")
giraffe_spp <- factor(giraffe_spp,
                      levels = c("1","2","3","4","5", "6", "7", "8", "9", "10+"),
                      ordered = TRUE)
df <- tibble(giraffe_spp, part1, part2) %>% 
  pivot_longer(!giraffe_spp, names_to = "part", values_to = "num_students") %>% 
  mutate(run = ifelse(part == "part1", 1, 2)) %>% 
  mutate(part = fct_reorder2(part, part, run))
  

# Load the extra fonts so they will be recognized for use.
loadfonts(device = "pdf")

# Part 1 plot (see below for Part 2) --------------------------------------
giraffe_plot <- df %>% 
#  mutate(part = fct_reorder2(part, part, run)) %>% 
  ggplot(aes(x = giraffe_spp)) + 
  geom_col(aes(y = num_students, fill = part), 
           #fill = "#2171B5", # Fill color is Blue6 from Beamer lectures
           width = 0.7,
           position = "dodge") + 
  theme_tufte() + 
  scale_x_discrete(limits = sort(giraffe_spp, 
                                 decreasing = TRUE)) + 
  labs(x = "Number of giraffe species",
       y = "Number of students",
       fill = "Part") +
  scale_y_continuous(breaks = seq(1, max_val, 1)) + # Adjust largest
  theme(axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        text = element_text(size = label_size,  family = "Linux Biolinum O"),
        axis.text.x = element_text(margin = margin(b = 10))) +
  coord_flip() +
  scale_fill_manual(values = part1_colors,
                    breaks = c("part1", "part2"),
                    labels = c("1", "2"))

# Obtain the location of the x.major source grid lines. See
# https://stackoverflow.com/questions/31223818/accessing-vector-of-axis-ticks-for-an-existing-plot-in-ggplot2
# But note modified format below to fit with newer ggplot2 structure
# Because the graph is flipped, use x.major_source instead of y.major_source
# y_intercept <- ggplot_build(grade_plot)$layout$panel_ranges[[1]]$x.major_source # No longer works.
# See update.
#y_intercept <- ggplot_build(giraffe_plot)$layout$coord$labels(ggplot_build(giraffe_plot)$layout$panel_params)[[1]]$x.major_source

y_intercept <- ggplot_build(giraffe_plot)$layout$panel_params[[1]]$x.sec$breaks
giraffe_plot <- giraffe_plot + geom_hline(yintercept = y_intercept, color = "white", linewidth =  0.25)

giraffe_plot
# Save the PDf file.
ggsave(part1_plot, plot = giraffe_plot, width = 4.5, height = 3)


# Part 1 plot (see below for Part 2) --------------------------------------
giraffe_plot <- df %>% 
#  mutate(part = fct_reorder2(part, part, run)) %>%
  ggplot(aes(x = giraffe_spp)) + 
  geom_col(aes(y = num_students, fill = part), 
           #fill = "#2171B5", # Fill color is Blue6 from Beamer lectures
           width = 0.7,
           position = "dodge") + 
  theme_tufte() + 
  scale_x_discrete(limits = sort(giraffe_spp, 
                                 decreasing = TRUE)) + 
  labs(x = "Number of giraffe species",
       y = "Number of students",
       fill = "Part") +
  scale_y_continuous(breaks = seq(1, max_val, 1)) + # Adjust largest
  theme(axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        text = element_text(size = label_size,  family = "Linux Biolinum O"),
        axis.text.x = element_text(margin = margin(b = 10))) +
  coord_flip() +
  scale_fill_manual(values = part2_colors,
                    breaks = c("part1", "part2"),
                    labels = c("1", "2"))

y_intercept <- ggplot_build(giraffe_plot)$layout$panel_params[[1]]$x.sec$breaks
giraffe_plot <- giraffe_plot + geom_hline(yintercept = y_intercept, color = "white", linewidth =  0.25)

giraffe_plot

# Save the PDf file.
ggsave(part2_plot, plot = giraffe_plot, width = 4.5, height = 3)


# Embed the Linux Biolinum font.
# embed_fonts(plot_name) # Not used for png files

