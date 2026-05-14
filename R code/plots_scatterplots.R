

# functions ---------------------------------------------------------------

#' @title Scatter chart
#' 
#' Generate scatter charts. Includes options for contour lines.
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param x.var,y.var Variable to plot on the x- and y-axis.
#' @param x.low,x.high,y.low,y.high. Fixed axes range for the axes. Default is no limits.
#' @param x.ref.value,y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param contour.lines Add contour lines to the plot. Note this will run slowly for lots of datapoints and facet grids.
#' @param best.fit,best.fit.ci,best.fit.type Add a line of best fit and 95% confidence intervals. Choose from ggplot preset types of best fit: "lm" (default), "glm", "gam", or "loess"
#' @param point.opacity range 0 (transparent) to 1 (opaque). Default is 0.8
#' @param point.color Either numeric or categorical variable for colouring points. Default is NA for a uniform colour.
#' @param point.shape Categorical variable for changing point shapes. Default is NA for a uniform shape
#' @param point.size Either numeric variable for varying size of the point or fixed numeric for uniform size (default = 2).
#' @param jitter.points Uses geom_jitter rather than geom_point. Useful for discrete scales and many overlapping points. Default is FALSE.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if x/y.low or x/y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_scatter <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                        y.var, y.low = NA, y.high = NA, y.title = NULL, y.ref.value = NA,
                        x.var, x.low = NA, x.high = NA, x.title = NULL, x.ref.value = NA,
                        contour.lines = FALSE, best.fit = FALSE, best.fit.type = "lm", best.fit.ci = FALSE,
                        point.opacity = 0.8, point.color = NA, point.shape = NA, 
                        point.size = 2, jitter.points = FALSE,
                        row.grid.var = NA, row.grid.axis = "free", 
                        col.grid.var = NA, col.grid.axis = "free",
                        pal.choice = NULL, legend.position = NULL,
                        save.chart = FALSE, save.filepath, save.filename,...
) {
  
  x.var <- as.name(x.var)
  y.var <- as.name(y.var)
  
  point.color <- if (gtools::invalid(point.color)) {NULL} else {as.name(point.color)}
  point.shape <- if (gtools::invalid(point.shape)) {NULL} else {as.name(point.shape)}
  
  if(is.numeric(point.size) | is.integer(point.size)) {
    df$size <- point.size
    point.size.field <- as.name("size")
  } else {
    point.size.field <- as.name(point.size)
  }
  
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  best.fit.type.clean <- tolower(best.fit.type)
  if (!best.fit) {
    best.fit.type.clean <- NULL
  } else if (!best.fit.type.clean %in% c("lm", "glm", "gam", "loess")) {
    message("best.fit.type should be one of 'lm', 'glm', 'gam', 'loess'. Selected 'lm' (linear model) as default.")
    best.fit.type.clean <- "lm"
  }
  
  # base dataframe
  df.plt <- df %>% 
    select(!!x.var, !!y.var, !!point.color, !!point.shape, !!point.size.field, !!row.grid.var, !!col.grid.var)
  
  # add reference lines
  layer_vline <- if (!gtools::invalid(x.ref.value)) geom_vline(xintercept = x.ref.value, color = "black", linetype = "dashed") else NULL
  layer_hline <- if (!gtools::invalid(y.ref.value)) geom_hline(yintercept = y.ref.value, color = "black", linetype = "dashed") else NULL
  
  # add contours if needed
  layer_contour <- if (contour.lines == TRUE) geom_density_2d(data = df.plt, aes(x = .data[[x.var]], y = .data[[y.var]]), 
                                                              color = "#363737", linewidth = 0.3, inherit.aes = FALSE)
  
  # add line of best fit
  layer_bestfit <- if(best.fit == TRUE) geom_smooth(data = df.plt, aes(x = .data[[x.var]], y = .data[[y.var]]), 
                                                    method = best.fit.type.clean, se = best.fit.ci, color = "black", 
                                                    linetype = "solid", linewidth = 0.5, inherit.aes = FALSE)
  
  
  # add facet grids
  layer_scales_option <- if(gtools::invalid(row.grid.var) & gtools::invalid(col.grid.var)) {
    NULL
  } else if (gtools::invalid(row.grid.var)) {
    scales = col.grid.axis
  } else if (gtools::invalid(col.grid.var)) {
    scales = row.grid.axis
  } else if (row.grid.axis == col.grid.axis) {
    scales = row.grid.axis
  } else if (row.grid.axis == "free") {
    scales = "free_y"
  } else if (col.grid.axis == "free") {
    scales = "free_x"
  } 
  
  layer_grid <- facet_grid(
    rows = if (gtools::invalid(row.grid.var)) NULL else vars(!!row.grid.var),
    cols = if (gtools::invalid(col.grid.var)) NULL else vars(!!col.grid.var),
    scales = layer_scales_option
  )
  
  
  # check if fill colour is a continous variable, and if so, set colour scale
  
  numeric_color <- is.numeric(df.plt[[as_label(point.color)]])
  
  layer_colors <- if(gtools::invalid(pal.choice)) {
    NULL
    } else if (numeric_color) {
    scale_color_gradient(low = pal.choice[1], high = pal.choice[2])
      } else {
    scale_color_manual(values = pal.choice)
        }
  
  layer_fill <- if(gtools::invalid(pal.choice)) {
    NULL
    } else if (numeric_color) {
    scale_fill_gradient(low = pal.choice[1], high = pal.choice[2])
      } else {
    scale_fill_manual(values = pal.choice)
        }
  
  # adjust the legend
  
  layer_guide <- if(is.numeric(point.size)) {
    guides(shape = guide_legend(override.aes = list(size = 5)),
           colour = guide_legend(override.aes = list(size = 5)),
           fill = "none",
           size = "none"
    )
  } else {
    guides(shape = guide_legend(override.aes = list(size = 5)),
           colour = guide_legend(override.aes = list(size = 5)),
           fill = "none"
    )
  }
  
  
  # Plot
  
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  if (jitter.points) {
    plt <- ggplot(df.plt, aes(x = !!x.var, y = !!y.var, 
                              fill = !!point.color, 
                              color = !!point.color, 
                              size = if (is.numeric(point.size) | is.integer(point.size)) point.size else !!point.size.field,  
                              shape = !!point.shape)) +
      geom_jitter(alpha = point.opacity, stroke = 0) +
      
      
      layer_bestfit +
      layer_contour +
      layer_vline +
      layer_hline +
      layer_grid +
      
      layer_fill +
      layer_colors +
      
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      coord_cartesian(ylim = c(y.low, y.high),
                      xlim = c(x.low, x.high)) +
      
      scale_shape_manual(values = c(15:20, 0:14)) +
      
      plot_theme +
      layer_guide
    
  } else if (!jitter.points) {
    
    plt <- ggplot(df.plt, aes(x = !!x.var, y = !!y.var, 
                              fill = !!point.color, color = !!point.color, 
                              size = if (is.numeric(point.size) | is.integer(point.size)) point.size else !!point.size.field,  
                              shape = !!point.shape)) +
      
      geom_point(alpha = point.opacity, stroke = 0) +
      
      layer_bestfit +
      layer_contour +
      layer_vline +
      layer_hline +
      layer_grid +
      
      layer_fill +
      layer_colors +
      
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      coord_cartesian(ylim = c(y.low, y.high),
                      xlim = c(x.low, x.high)) +
      
      scale_shape_manual(values = c(15:20, 0:14)) +
      
      plot_theme +
      layer_guide
  }
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}

#' @title Heatmap
#' 
#' Generate coloured hex-heatmap for two numeric variables
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param x.var,y.var Variable to plot on the x- and y-axis.
#' @param x.low,x.high,y.low,y.high. Fixed axes range for the axes. Default is no limits.
#' @param x.ref.value,y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param bin.size how many datapoints to aggregate into a single point for the heatmap. Needs trial and error to get right. Default to "round(nrow(df)/50,0)" to give ~50 bins.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_heatmap <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                        y.var, y.low = NA, y.high = NA, y.title = NULL, y.ref.value = NA,
                        x.var, x.low = NA, x.high = NA, x.title = NULL, x.ref.value = NA,
                        bin.size = round(nrow(df)/50,0),
                        row.grid.var = NA, row.grid.axis = "free", 
                        col.grid.var = NA, col.grid.axis = "free",
                        pal.choice = NULL, legend.position = NULL,
                        save.chart = FALSE, save.filepath, save.filename,...
) {
  
  x.var <- as.name(x.var)
  y.var <- as.name(y.var)
  
  if (!pal.choice %in% c("black-red-amber", "black-red", "temperature")) {
    stop("pal.choice must be 'black-red-amber', 'black-red', or 'temperature'")
  }
  pal.choice.code <- if (pal.choice == "black-red-amber") {"B"} else if (pal.choice == "black-red") {"F"} else if (pal.choice == "temperature") {"H"}
  
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  # base dataframe
  df.plt <- df %>% 
    select(!!x.var, !!y.var, !!row.grid.var, !!col.grid.var)
  
  # add reference lines
  layer_vline <- if (!gtools::invalid(x.ref.value)) geom_vline(xintercept = x.ref.value, color = "black", linetype = "dashed") else NULL
  layer_hline <- if (!gtools::invalid(y.ref.value)) geom_hline(yintercept = y.ref.value, color = "black", linetype = "dashed") else NULL
  
  # add facet grids
  layer_scales_option <- if(gtools::invalid(row.grid.var) & gtools::invalid(col.grid.var)) {
    NULL
  } else if (gtools::invalid(row.grid.var)) {
    scales = col.grid.axis
  } else if (gtools::invalid(col.grid.var)) {
    scales = row.grid.axis
  } else if (row.grid.axis == col.grid.axis) {
    scales = row.grid.axis
  } else if (row.grid.axis == "free") {
    scales = "free_y"
  } else if (col.grid.axis == "free") {
    scales = "free_x"
  } 
  
  layer_grid <- facet_grid(
    rows = if (gtools::invalid(row.grid.var)) NULL else vars(!!row.grid.var),
    cols = if (gtools::invalid(col.grid.var)) NULL else vars(!!col.grid.var),
    scales = layer_scales_option
  )
  
  # Plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  plt <- ggplot(df.plt, aes(x = !!x.var, y = !!y.var)) +
    geom_hex(bins = nrow(df.plt) / bin.size, aes(fill = after_stat(count))) +
    
    layer_vline +
    layer_hline +
    layer_grid +
    
    scale_fill_viridis_c(option = pal.choice.code) +
    
    coord_cartesian(ylim = c(y.low, y.high),
                    xlim = c(x.low, x.high)) +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}

#' @title Pairwise scatterplot and correlation coefficients. 
# Significance: * < 0.05, ** < 0.01, *** < 0.005
#' 
#' Generates pairwise scatterplots and correlation coefficients for selected numeric fields
#' @param df Dataframe being plotted
#' @param plt.title Text for plot title
#' @param vars Variables to be plotted. Default uses all fields in df
#' @param corr.type Choose which type of correlation coefficients to present, from "pearson" (default), "spearman", or "kendall". Distance correlation unavailable - use function stat_cor_distance.
#' @param colour.negative,colour.middle,colour.positive Background colours for the correlation coefficient grid, running from -1 to 0 to +1. Default is red-white-blue.
#' @param text.colour,text.size Text colour and size for correlation coefficients
#' @param point.colour,point.opacity,point.size point colour, opacity, and size for scatterplot. Note these parameters are uniform for all datapoints (e.g., cannot have separate colour by another categorical field)
#' @param add.distribution Add density plot per variable to the leading diagonal
#' @param best.fit,best.fit.ci,best.fit.type Add a line of best fit. Choose from ggplot preset types of best fit: "lm" (default), "glm", "gam", or "loess".
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_scatter_pairs <- function(df, plt.title = NULL, vars = colnames(df), corr.type = "pearson",
                              colour.negative = "#F8766D", colour.middle = "white", colour.positive = "#00B0F6", 
                              text.colour = "black", text.size = 4,
                              point.colour = "black", point.opacity = 0.3, point.size = 0.5,
                              add.distribution = TRUE, best.fit = FALSE, best.fit.type = "lm", 
                              save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  df.plt <- df %>% 
    select(all_of(vars))
  
  check.class <- sapply(df.plt, function(x) is.numeric(x) || is.integer(x))
  if(any(!check.class)) {
    stop("Fields selected in 'vars' should be numeric. Remove non-numeric fields, or use function plt_pair_combo for mixed numeric/non-numeric analysis.")
  }
  
  corr.type.clean <- tolower(corr.type)
  if (!corr.type.clean %in% c("pearson", "spearman", "kendall")) {
    message("best.fit.type should be one of 'pearson', 'spearman', 'kendall'. Selected 'pearson' as default.")
    corr.type.clean <- "pearson"
  }
  
  
  fn_ggpairs_colors <- function(data, mapping, method=corr.type.clean, use="pairwise", ...){
    
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    
    corr <- cor(x, y, method=corr.type.clean, use=use)
    
    colFn <- pal_range_interpolate(colour.start = colour.negative, colour.middle = colour.middle, colour.end = colour.positive)
    fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
    
    ggally_cor(data = data, 
               mapping = mapping, 
               method = corr.type.clean,
               colour = text.colour, 
               size = text.size,
               ...) + 
      theme_void() +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        panel.background = element_rect(fill=fill),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect("lightgrey")
      )
  }
  
  layer_bestfit <- if(best.fit) {"points"} else {"smooth"}
  best.fit.type.clean <- tolower(best.fit.type)
  if (!best.fit) {
    best.fit.type.clean <- NULL
  } else if (!best.fit.type.clean %in% c("lm", "glm", "gam", "loess")) {
    message("best.fit.type should be one of 'lm', 'glm', 'gam', 'loess'. Selected 'lm' (linear model) as default.")
    best.fit.type.clean <- "lm"
  }
  
  plt <- suppressMessages({
    ggpairs(
      data = df.plt,
      diag = if(add.distribution) list(continous = "densityDiag") else NULL,
      lower = if(best.fit)
        list(continuous = wrap("smooth",
                               method = best.fit.type.clean,
                               color = point.colour, 
                               size = point.size,
                               alpha = point.opacity))
       else 
         list(continuous = wrap("points",
                                color = point.colour, 
                                size = point.size,
                                alpha = point.opacity)),
        
      upper = list(continuous = fn_ggpairs_colors),
      title = plt.title
    ) +
      
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect("lightgrey")
      )
  })
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt) 
  
}

#' @title Pairwise plots and correlation coefficients/tests of differences. Allows for a mix of numeric and non-numeric.
#' For continuous vs continuous, presenting Spearman's correlation coefficient. 
#' For categorical vs continuous, presenting p-value for Wilcoxon test if 2 categories, or Kruskal-Wallis test if > 2 categories. 
# For categorical vs categorical, presenting Chi-squared test p-value. Significance: * < 0.05, ** < 0.01, *** < 0.005
#' 
#' @param df Dataframe being plotted
#' @param plt.title Text for plot title
#' @param vars Variables to be plotted. Default uses all fields in df
#' @param text.colour,text.size Text colour and size for correlation coefficients/statistical tests
#' @param point.colour,point.opacity,point.size point colour, opacity, and size for scatterplot. Note these parameters are uniform for all datapoints (e.g., cannot have separate colour by another categorical field)
#' @param box.colour,box.opacity colour and opacity for box plots
#' @param count.colour colour for count plots
#' @param add.distribution,distribution.colour Add density plot per variable to the leading diagonal, and choose colour
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_pair_combo <- function(df, plt.title = NULL, vars = colnames(df),
                           text.colour = "black", text.size = 4,
                           point.colour = "#00B0F6", point.opacity = 0.3, point.size = 0.5,
                           box.colour = "#00BF7D", box.opacity = 0.5, 
                           count.colour = "#F8766D",
                           add.distribution, distribution.colour = "black",
                           save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  
  df.plt <- df %>% 
    select(all_of(vars))
  
  # function for continuous vs continuous
  fn_ggpairs_colors_corrs <- function(data, mapping, method="spearman", use="pairwise", ...){
    
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    
    corr <- cor(x, y, method=method, use=use)
    
    ggally_cor(data = data, 
               mapping = mapping, 
               colour = text.colour, 
               size = text.size,...) + 
      theme_void() +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect("lightgrey")
      )
  }
  
  
  # function for categorical vs continuous
  fn_ggpairs_colors_kruskal <- function(data, mapping, ...){
    
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    
    # how many categorical variables are there?
    num_categories <- if (!is.numeric(x) && is.numeric(y)) {
      length(unique(x))
    } else if (is.numeric(x) && !is.numeric(y)) {
      length(unique(y))
    } else {
      NA  # where both are numeric or both categorical
    }
    
    # check which variable is categorical vs continuous and run test
    
    # first if 2 categories, run Wilcoxon test
    if (!is.numeric(x) && is.numeric(y) && num_categories == 2) {
      model <- stats::wilcox.test(y ~ as.factor(x))  # Treat x as a factor
    } else if (is.numeric(x) && !is.numeric(y) && num_categories == 2) {
      model <- stats::wilcox.test(x ~ as.factor(y))  # Swap x and y if needed
    # if more than 2 categories, run Kruskal-Wallis test
      } else if (!is.numeric(x) && is.numeric(y) && num_categories > 2) {
      model <- stats::kruskal.test(y ~ as.factor(x))  # Treat x as a factor
    } else if (is.numeric(x) && !is.numeric(y) && num_categories > 2) {
      model <- stats::kruskal.test(x ~ as.factor(y))  # Swap x and y if needed
    } else {
      return(ggally_blank())  # If both are numeric or both categorical, or only one category, skip
    }
    
    p_value <- model$p.value
    
    stars <- ifelse(p_value < 0.005, "***", 
                    ifelse(p_value < 0.01, "**", 
                           ifelse(p_value < 0.05, "*", "")))
    
    label <- if (num_categories == 2) {
      paste0("Wilcoxon p-value: \n", sprintf("%.3f", p_value), stars)
    } else {
      paste0("KW p-value: \n", sprintf("%.3f", p_value), stars)
    }
    
    # Create a ggplot object with the label as text
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = label, size = text.size, colour = text.colour) +
      theme_void() +
      theme(panel.border = element_rect(fill = NA, color = "black"))
  }
  
  
  # function for categorical vs categorical
  fn_ggpairs_colors_chisq <- function(data, mapping, ...){
    
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    
    model <- with(data, stats::chisq.test(as.factor(y), as.factor(x)))
    
    p_value <- model$p.value
    
    stars <- ifelse(p_value < 0.005, "***", 
                    ifelse(p_value < 0.01, "**", 
                           ifelse(p_value < 0.05, "*", "")))
    
    label = paste0("Chi-sq p-value: \n", sprintf("%.3f", p_value), stars)
    
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = label, size = text.size, colour = text.colour) +
      theme_void() +
      theme(panel.border = element_rect(fill = NA, color = "black"))
  }
  
  
  # caption
  caption <- "For continuous vs continuous, presenting Pearson's correlation coefficient. For categorical vs continuous, presenting p-value for Wilcoxon test if 2 categories, or Kruskal-Wallis test if > 2 categories. For categorical vs categorical, presenting Chi-squared test p-value. Significance: * < 0.05, ** < 0.01, *** < 0.005"
  
  # main plot
  plt <- suppressMessages({
    ggpairs(
      data = df.plt,
      diag = if(add.distribution) {
        list(continous ="densityDiag",
             discrete = wrap("barDiag",
                             fill = distribution.colour,
                             alpha = 1
             )
        )
      } else {NULL},
      lower = list(continuous = wrap("smooth", 
                                     color = point.colour, 
                                     size = point.size,
                                     alpha = point.opacity),
                   combo = wrap('box',
                                fill = box.colour, 
                                alpha = box.opacity,
                                outlier.color = box.colour),
                   discrete = wrap('count', 
                                   fill = count.colour)
      ),
      
      
      upper = list(continuous = fn_ggpairs_colors_corrs,
                   combo = fn_ggpairs_colors_kruskal,
                   discrete = fn_ggpairs_colors_chisq
      ),
      title = plt.title
    ) +
      
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect("lightgrey")
      )
  })
  
  print(caption)
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt) 
  
}

#' @title XY heatmap chart
#' 
#' For a pair of categorical variables, generate a grid-style heatmap for a chosen numeric field
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param x.var,y.var Categorical variable to plot on the x- and y-axis.
#' @param fill.var Numeric variable for filling the xy grid
#' @param fill.aggregation How to aggregate the fill.var variable. Choose "sum" (default), "mean", or "median".
#' @param fill.round Rounding for the fill.var labels
#' @param row.grid.var,col.grid.var Select variable for facet grids.
#' @param colour.low,colour.middle,colour.high Color palette to use. Default is red-white-blue
#' @param middle.value What to use as the central value for the colour palette. Choose "median" (default), "mean", or enter a numeric value.
#' @param text.colour,text.size Text colour and size for fill.var values
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_xyheatmap <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                         y.var, y.title = NULL, x.var, x.title = NULL,
                         fill.var, fill.aggregation = "sum", fill.round = 0,
                         row.grid.var = NULL, col.grid.var = NULL,
                         colour.low = "#F8766D", colour.middle = "white", middle.value = "median", colour.high = "#00B0F6", 
                         text.colour = "black", text.size = 4, legend.position = NULL,
                         save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  x.var <- as.name(x.var)
  y.var <- as.name(y.var)
  fill.var <- as.name(fill.var)
  fill.aggregation <- tolower(fill.aggregation)
  
  if(!fill.aggregation %in% c("sum", "mean", "median")) {
    #dlg_message("Aggregation type must be sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'sum', 'mean', or 'median'")
  }
  
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  
  # base dataframe
  df.plt <- df %>% 
    group_by(!!x.var, !!y.var, !!row.grid.var, !!col.grid.var) %>% 
    summarise(N = match.fun(fill.aggregation)(!!fill.var, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(N = round(N, fill.round))
  
  
  # add facet grids
  layer_grid <- facet_grid(
    rows = if (gtools::invalid(row.grid.var)) NULL else vars(!!row.grid.var),
    cols = if (gtools::invalid(col.grid.var)) NULL else vars(!!col.grid.var),
    scales = "free"
  )
  
  # fill colour approach
  midpoint.val <- if (middle.value == "mean") {
    mean(df.plt$N, na.rm = TRUE)
  } else if(middle.value == "median") {
    median(df.plt$N, na.rm = TRUE)
  } else if(is.numeric(middle.value)) {
    middle.value
  } else {
    stop("Set 'middle.value' to be 'mean', 'median', or enter a numeric value")
  }
  
  layer_fill <- scale_fill_gradient2(low = colour.low,
                                     mid = colour.middle,
                                     high = colour.high,
                                     midpoint = midpoint.val)
  
  # adjust the legend
  layer_guide <- guides(colour = guide_legend(override.aes = list(size = 5)))
  
  # Plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot(df.plt, aes(x = !!x.var, y = forcats::fct_rev(!!y.var), 
                            fill = N)) +
    geom_tile(color = "white") +
    geom_text(aes(label = N), color = text.colour, size = text.size) +
    
    layer_grid +
    layer_fill +
    plot_theme +
    layer_guide
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}


# examples ----------------------------------------------------------------

stores_fruit <- readr::read_csv("stores_fruit.csv") %>% 
  mutate(dow = factor(dow, levels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")))


## basic scatterplot ----
plt.scatter.basic <- plt_scatter(df = stores_fruit,
                                 plt.title = "Scatter of price by quantity",
                                 plt.subtitle = NA,
                                 plt.caption = NA,
                                 x.var = "ppu",
                                 x.title = "Price per unit",
                                 x.low = NA,x.high = NA,x.ref.value = NA,
                                 y.var = "quantity",
                                 y.title = "Quantity",
                                 y.low = NA,y.high = NA,y.ref.value = NA,
                                 contour.lines = TRUE,
                                 best.fit = TRUE,
                                 best.fit.type = "lm",  
                                 best.fit.ci = TRUE,
                                 point.opacity = 0.5,
                                 point.color = "quantity",
                                 point.shape = "region",
                                 point.size = 1,
                                 jitter.points = FALSE,
                                 row.grid.var = "product",
                                 row.grid.axis = "free",
                                 legend.position = "bottom",
                                 pal.choice = c("lightblue", "blue"),
                                 save.chart = FALSE,save.filepath = "output/",save.filename = "scatter_chart"
)

## heatmap plot ----
plt.scatter.heatmap <- plt_heatmap(df = stores_fruit %>% filter(product == "apple"),
                                   plt.title = "Heatmap of price by quantity of apples",
                                   plt.subtitle = NA,
                                   plt.caption = paste0("Each point ~ 500 datapoints. Total 6,588 store/date combinations."),
                                   x.var = "ppu",
                                   x.title = "Price per unit",
                                   x.low = NA,x.high = NA,x.ref.value = NA,
                                   y.var = "quantity",
                                   y.title = "Quantity",
                                   y.low = NA,y.high = NA,y.ref.value = NA,
                                   bin.size = 500, # how many datapoints to aggregate into a single point for the heatmap. Needs trial and error to get right.
                                   row.grid.var = NA,row.grid.axis = "free",
                                   col.grid.var = NA,col.grid.axis = "free",
                                   pal.choice = "temperature",
                                   legend.position = "bottom",
                                   save.chart = FALSE,save.filepath = "output/",save.filename = "test_scatter_chart"
)

## pairwise scatter of continuous variables ---- 
library(GGally)
plt.scatter.pairwise <- plt_scatter_pairs(df = stores_fruit %>% filter(product == "banana"),
                                          plt.title = "Cross-correlation plot for price, quantity, and % stock sold of bananas",
                                          vars <- c("ppu", "quantity", "pc_sold"),
                                          point.colour = "blue",
                                          point.opacity = 0.3,
                                          point.size = 0.5,
                                          corr.type = "spearman",
                                          add.distribution = TRUE,
                                          best.fit = TRUE,
                                          best.fit.type = "lm",
                                          save.chart = FALSE, # decide if and where to save your charts
                                          save.filepath = "output/",
                                          save.filename = "scatter_pairwise"
)

## pairwise with mix of continous and categorical ----
library(GGally)
plt.scatter.pairwise.all <- plt_pair_combo(df = stores_fruit %>% filter(product == "banana"),
                                            plt.title = "Cross-plots for bananas",
                                            vars = c("region", "dow", "ppu", "quantity", "pc_sold"),
                                            text.size = 4,
                                            point.colour = "blue", point.opacity = 0.25, point.size = 0.5,
                                            box.colour = "lightblue", box.opacity = 0.5,
                                            count.colour = "purple",
                                            add.distribution = TRUE, distribution.colour = "black",
                                            save.chart = FALSE, save.filepath = "output/", save.filename = "test_paired_combo"
)


## XY heatmap ----
plt.xyheatmap <- plt_xyheatmap(df = stores_fruit %>% filter(dow == "SAT"),
                               plt.title = "Heatmap of mean Saturday quantity sold by product and store",
                               plt.subtitle = NA,
                               plt.caption = NA,
                               x.var = "product",
                               x.title = "Product",
                               y.var = "store",
                               y.title = "store",
                               fill.var = "quantity",
                               fill.aggregation = "mean",
                               fill.round = 0,
                               text.size = 3,
                               row.grid.var = "region",
                               middle.value = "median",
                               legend.position = "top",
                               save.chart = FALSE,save.filepath = "output/",save.filename = "xyheatmap_chart"
)
