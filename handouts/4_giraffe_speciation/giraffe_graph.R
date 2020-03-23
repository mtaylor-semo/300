library(ggplot2)
library(ggthemes)
library(extrafont)

# Registers ttf fonts for use with PDF output (ggsave), 
# enables them to be saved in PDF file (embed_fonts).
#loadfonts() 

#Change exam name and numbers for each grade.
# Number is order from A to F.

plot_name <- "num_giraffe_spp.png"
num_giraffes <- c(1,1,0,5,11,5,1,1,0,1)

giraffe_spp <- c("1","2","3","4","5", "6", "7", "8", "9", "10+")
giraffe_spp <- factor(giraffe_spp,
                      levels = c("1","2","3","4","5", "6", "7", "8", "9", "10+"),
                      ordered = TRUE)
df <- data.frame(giraffe_spp, num_giraffes)

# Load the extra fonts so they will be recognized for use.
#loadfonts(device = "pdf")

# Build plot
giraffe_plot <- ggplot(df, aes(giraffe_spp, num_giraffes)) + 
  geom_col(fill = "#2171B5", # Fill color is Blue6 from Beamer lectures
           width=0.7) + 
  theme_tufte() + 
  scale_x_discrete(limits = sort(giraffe_spp, 
                                 decreasing = TRUE)) + 
  labs(x = "", 
       y = "") + 
  scale_y_continuous(breaks=seq(0,40,5)) +
  theme(axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        text=element_text(size=16,  family="Linux Biolinum"),
        axis.text.x = element_text(margin = margin(b = 10))) +
  coord_flip() 

# Obtain the location of the x.major source grid lines. See
# https://stackoverflow.com/questions/31223818/accessing-vector-of-axis-ticks-for-an-existing-plot-in-ggplot2
# But note modified format below to fit with newer ggplot2 structure
# Because the graph is flipped, use x.major_source instead of y.major_source
# y_intercept <- ggplot_build(grade_plot)$layout$panel_ranges[[1]]$x.major_source # No longer works.
# See update.
y_intercept <- ggplot_build(giraffe_plot)$layout$coord$labels(ggplot_build(giraffe_plot)$layout$panel_params)[[1]]$x.major_source
giraffe_plot <- giraffe_plot + geom_hline(yintercept = y_intercept, color="white", size =  0.25)

# Save the PDf file.
ggsave(plot_name, plot=giraffe_plot, width=4.5, height=3)

# Embed the Linux Biolinum font.
# embed_fonts(plot_name) # Not used for png files

