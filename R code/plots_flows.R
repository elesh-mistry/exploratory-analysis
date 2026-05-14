
# functions ---------------------------------------------------------------

#' @title Sankey flow plots
#' 
#' Generate Sankey plots. Recommend keeping relatively simple given slow performance of the package.
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,nodes.title Text for plot title, subtitle, caption and x-axis titles.
#' @param nodes Vector of variables which form each set of nodes. Place these in order they should appear on the chart left-to-right.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param node.text.color,node.text.size Colour and size for the node labels
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_sankey <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                       nodes, nodes.title = NULL, 
                       pal.choice = NULL, node.text.color = "black", node.text.size = 4, 
                       legend.position = NULL,
                       save.chart = FALSE, save.filepath, save.filename, ...
) {

  if (length(nodes) < 2) {
    stop("Must have at least two variables in argument nodes.")
  }
  
  df.plt.long <- df %>% 
    ggsankey::make_long(unlist(nodes))
  
  layer_fill <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_fill_manual(values = pal.choice)
  }
  
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = nodes.title, 
                              y.title = NULL, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  plt <- ggplot(df.plt.long,
         aes(x = x, 
             next_x = next_x, 
             node = if(is.factor(node)) {forcats::fct_rev(node)} else {node}, 
             next_node = next_node,
             fill = node,
             label = node)) +
    ggsankey::geom_alluvial(flow.alpha = 0.5, node.alpha = 0.7) +
    ggsankey::geom_alluvial_text(size = node.text.size, color = node.text.color) +
    layer_fill +
    plot_theme +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()
          )
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}


# examples ----------------------------------------------------------------

## Sankey plot ----

# remotes::install_github("davidsjoberg/ggsankey", dependencies = TRUE)
library(ggsankey)
data(mtcars)

plt.sankey <- plt_sankey(df = mtcars,
                         plt.title = "Car features",
                         plt.subtitle = NULL,
                         plt.caption = NULL,
                         nodes = c("cyl", "vs", "am", "gear", "carb"),
                         nodes.title = "Features",
                         pal.choice = NULL,
                         node.text.color = "black", # for node labels
                         node.text.size = 4,
                         legend.position = "top",
                         save.chart = FALSE, # decide if and where to save your charts
                         save.filepath = "output/",
                         save.filename = "test_sankey"
)
