
library(scales)
library(grDevices)
library(tidyverse)
options(scipen=999)

# functions ---------------------------------------------------------------

#' Note these functions are used to facilitate the production of other graphs
#' rather than being used as standalone functions


#' @title Colour range for number of categories
#' 
#' Generate a three-part colour range for a specified number of categories.
#' @param colour.start First colour in sequence
#' @param colour.middle Middle colour in sequence
#' @param colour.end Last colour in sequence
#' @param num.categories Number of categories in the sequence
pal_range_cats <- function(colour.start, colour.middle, colour.end, num.categories) {
  grDevices::colorRampPalette(colors = c(colour.start, colour.middle, colour.end))(num.categories)
}

#' @title Colour range, smooth range
#' 
#' Generate a three-part colour range, applied as a smooth function
#' @param colour.start First colour in sequence
#' @param colour.middle Middle colour in sequence
#' @param colour.end Last colour in sequence
pal_range_interpolate <- function(colour.start, colour.middle, colour.end) {
  grDevices::colorRampPalette(colors = c(colour.start, colour.middle, colour.end), interpolate ='spline')
}



#' @title Plot themes
#' 
#' Theme to be applied to graphs produced by other functions in this exploratory data analysis project
#' @param plt.title Title of the plot
#' @param plt.subtitle Subtitle of the plot
#' @param x.title x-axis title of the plot
#' @param y.title y-axis title of the plot
#' @param plt.caption Caption of the plot
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"

plot_theme_fn <- function(plt.title = NULL, 
                          plt.subtitle = NULL, 
                          x.title = NULL, 
                          y.title = NULL, 
                          plt.caption = NULL, 
                          legend.position = "right") {
  
  theme <- list(
    labs(
      title = str_wrap(plt.title, 100),
      subtitle = if (!gtools::invalid(plt.subtitle)) str_wrap(plt.subtitle, 100) else NULL,
      x = x.title,
      y = y.title,
      caption = if (!gtools::invalid(plt.caption)) str_wrap(plt.caption, 150) else NULL
    ),
    theme_minimal(),
    theme(
      plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(color = "black", hjust = 0.5, size = 10),
      axis.title.y = element_text(size = 11),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.caption = element_text(face = "italic"),
      legend.position = legend.position,
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect("lightgrey")
    )
  )
  
  return(theme)
}
