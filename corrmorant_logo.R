###############################################################################
#
# Generate R elements of the corrmorant hex logo
#
# Code: Roman M. Link
#
###############################################################################

# 1. Load packages ------------------------------------------------------------ 
library(tidyverse)

# 2. Create bell curves -------------------------------------------------------
# prepare data
curve <- tibble(x = seq(0, 10, 0.01),
                y1 = 0.6 * dnorm(x, 4, 0.9),
                y2 = dnorm(x, 5, 1.3),
                y3 = 0.7 * dnorm(x, 6, 1)) %>% 
  gather(key, y, -x)
  
# create plot
curves <- ggplot(aes(x, y, group = key), data = curve) +
  geom_density(aes(fill = key), col = NA, stat = "identity", alpha = .6) +
  #geom_line(col = "#424242", stat = "identity", size = 2, alpha = .6) +
  theme_void() +
  theme_transparent() + 
  scale_fill_manual(values = c("#5e65bc", "#7f66bc", "#3e66bf")) +
  theme(legend.position ="none") 

# export output
ggsave("img/curves.svg", plot = curves, width = 18, height = 6, units = "cm", dpi = 1000)


# 3. Create ggplot2 logo style point an line elements -------------------------
# define colors
ggplot_blue <- c("#a8d6ff", "#51a7f9", "#0365c0", "#164f86", "#002452", "#000643")
ggplot_grey <- c("#f2f2f2", "#424242")

# prepare datasets
points1 <- tibble(x = 1:6, y = c(1:3, 2:4 + .2), id = factor(1:length(x)), group = 1)
points2 <- tibble(x = 1:6, y = c(2,2.6,1.5,2.5,3.5,2), id = factor(1:length(x)), group = 2)
points3 <- tibble(x = 1:6, y = c(3.5,4.1,2.9,3.6,2.2,2.8), id = factor(1:length(x)), group = 2)

# create function for plotting
pointsplot <- function(points){
  ggplot(aes(x, y, group = group), data = points) +
    geom_line(col = ggplot_grey[2], size = 1.8) +
    geom_point(aes(col = id), size = 8) +
    theme_void() +
    xlim(min(points$x) - 1, max(points$x) + 1) +
    ylim(min(points$y) - 1, max(points$y) + 1) +
    scale_color_manual(values = ggplot_blue) +
    theme(legend.position ="none") 
}

# export output
ggsave("img/points1.svg", plot = pointsplot(points1), width = 6, height = 4, units = "cm", dpi = 1000)
ggsave("img/points2.svg", plot = pointsplot(points2), width = 6, height = 4, units = "cm", dpi = 1000)
ggsave("img/points3.svg", plot = pointsplot(points3), width = 6, height = 4, units = "cm", dpi = 1000)
