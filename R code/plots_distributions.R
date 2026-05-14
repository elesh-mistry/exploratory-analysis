
# functions ---------------------------------------------------------------

#' @title Box plot
#' 
#' Generate box plots. 
#' Line = median, 'X' = mean, Box = inter-quartile range, Whisker = 1.5 x IQR, Dot = outlier.
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var numeric Variable to plot on the y-axis.
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param x.var Categorical variable to plot on the x-axis.
#' @param outliers.show Show outliers for datapoints outside the whiskers of the box plot. Default is FALSE.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param orientation Display the boxes as "vertical" (default) or "horizontal"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_box <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                    y.var, y.low = NA, y.high = NA, y.title = NULL, y.ref.value = NA,
                    x.var, x.title = NULL,
                    outliers.show = FALSE,
                    row.grid.var = NA, row.grid.axis = "free", 
                    col.grid.var = NA, col.grid.axis = "free",
                    pal.choice = NULL, legend.position = NULL, orientation = "vertical",
                    save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  df.plt <- df %>% 
    select(!!y.var, !!x.var, !!row.grid.var, !!col.grid.var)
  
  
  # add reference lines
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
  
  # orientate vertical or horizontal
  orientation.clean <- tolower(orientation)
  orientation.flip <- orientation.clean == "horizontal"
  layer_flip <- if (orientation.flip) {coord_flip()} else NULL 
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  layer_fill <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_fill_manual(values = pal.choice)
  }
  
  
  # plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot(df.plt, aes(x = if (orientation.flip) forcats::fct_rev(!!x.var) else !!x.var,
                            y = !!y.var, 
                            color = !!x.var, fill = !!x.var)) +
    geom_boxplot(position = "identity", alpha = 0.5, linewidth = 0.75, outliers = outliers.show) +
    stat_summary(data = df.plt, aes(x = !!x.var, y = !!y.var), 
                 fun = mean, geom = "point", shape = 4, size = 3, color = "black", alpha = 0.25, stroke = 1.25) +
  
    layer_color +
    layer_fill +
    scale_y_continuous(labels = comma) +
    
    coord_cartesian(ylim = c(y.low, y.high)) +
    
    layer_grid +
    layer_hline +
    layer_flip +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}


#' @title Means plot
#' 
#' Generate plots of means. 
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var numeric Variable to plot on the y-axis.
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param y.whiskers either "ci" for 95% confidence interval using standard errors (default) or "min-max" for min and max
#' @param x.var Categorical variable to plot on the x-axis.
#' @param point.size,whisker.size size of point-and-whiskers plotted
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param orientation Display the boxes as "vertical" (default) or "horizontal"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_means <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                      y.var, y.low = NA, y.high = NA, y.title = NULL, y.ref.value = NA, y.whiskers = "ci",
                      x.var, x.title = NULL,
                      point.size = 3, whisker.size = 0.5, 
                      row.grid.var = NA, row.grid.axis = "free", 
                      col.grid.var = NA, col.grid.axis = "free",
                      pal.choice = NULL, legend.position = NULL, orientation = "vertical",
                      save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  # pick which variables for the interval
  y.whiskers.clean <- y.whiskers %>% tolower() %>% gsub("[^a-z0-9]", "", .)
  
  if(y.whiskers.clean == "minmax") {
    error.low <- as.name("Min")
    error.high <- as.name("Max")
  } else if (y.whiskers.clean == "ci") {
    error.low <- as.name("CI_Lower")
    error.high <- as.name("CI_Upper")
  } else {
    stop("y.whiskers must be either 'ci' (for 95% confidence interval using standard errors) or 'min-max' (for min and max).")
  }
  
  df.plt <- df %>% 
    filter(!is.na(!!y.var)) %>% 
    group_by(!!x.var, !!row.grid.var, !!col.grid.var) %>% 
    summarise(Mean = mean(!!y.var),
              Min = min(!!y.var),
              Max = max(!!y.var),
              N = n(),
              SD = sd(!!y.var, na.rm = TRUE),
              SE = SD / sqrt(N),
              t_crit = qt(0.975, df = N - 1),
              CI_Lower = Mean - t_crit * SE,
              CI_Upper = Mean + t_crit * SE
    ) %>% 
    ungroup()
  
  
  # add reference lines
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
  
  # orientate vertical or horizontal
  orientation.clean <- tolower(orientation)
  orientation.flip <- orientation.clean == "horizontal"
  
  layer_flip <- if (orientation.flip) {coord_flip()} else NULL 
  
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot(df.plt, aes(x = if (orientation.flip) forcats::fct_rev(!!x.var) else !!x.var,
                            y = Mean, 
                            color = !!x.var, fill = !!x.var)) +
    geom_point(size = point.size) +
    geom_errorbar(aes(ymin = !!error.low, ymax = !!error.high), width = whisker.size) +
    scale_fill_manual(values = pal.choice) +
    scale_color_manual(values = pal.choice) +
    scale_y_continuous(labels = comma) +
    
    coord_cartesian(ylim = c(y.low, y.high)) +
    
    layer_grid +
    layer_hline +
    layer_flip +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)

    return(plt)
}

#' @title Violin plot
#' 
#' Generate violin plots. 
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var numeric Variable to plot on the y-axis.
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param x.var Categorical variable to plot on the x-axis.
#' @param violin.var A categorical variable for overlapping violin plots. If It's the same as x.var (done as default), it will prevent an overlap.
#' @param show.average,average.width,average.type Add a bar to show the average. Set bar width using average.width. Choose "median" (default) or "mean" for average type.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param orientation Display the boxes as "vertical" (default) or "horizontal"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_violin <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                       y.var, y.low = NA, y.high = NA, y.title = NULL, y.ref.value = NA, 
                       x.var, x.title = NULL,
                       violin.var = x.var, show.average = TRUE, average.width = 0.2, average.type = "median",
                       row.grid.var = NA, row.grid.axis = "free", 
                       col.grid.var = NA, col.grid.axis = "free",
                       pal.choice = NULL, legend.position = NULL, orientation = "vertical",
                       save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  violin.var <- as.name(violin.var)
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  df.plt <- df %>% 
    select(!!y.var, !!x.var, !!violin.var, !!row.grid.var, !!col.grid.var)
  
  
  # add reference lines
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
  
  # orientate vertical or horizontal
  orientation.clean <- tolower(orientation)
  orientation.flip <- orientation.clean == "horizontal"
  
  layer_flip <- if (orientation.flip) {coord_flip()} else NULL
  
  # add average bar
  layer_average <- if (show.average | tolower(show.average) == "yes") {
    stat_summary(fun = average.type, geom = "crossbar", aes(color = !!violin.var), width = average.width)
  } else {NULL}
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  layer_fill <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_fill_manual(values = pal.choice)
  }
  
  # plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot(df.plt, aes(x = if (orientation.flip) forcats::fct_rev(!!x.var) else !!x.var,
                            y = !!y.var, 
                            color = !!violin.var, fill = !!violin.var)) +
    geom_violin(trim = FALSE, scale = "width", position = "identity", alpha = 0.1, linewidth = 0.75) +
    layer_average +
    
    layer_color +
    layer_fill +
    scale_y_continuous(labels = comma) +
    
    coord_cartesian(ylim = c(y.low, y.high)) +
    
    layer_grid +
    layer_hline +
    layer_flip +
    
    plot_theme 
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}


#' @title Ridge plot
#' 
#' Generate ridgeline plots (multiple rows of density plots) using package ggridges. 
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,label.title,value.title Text for plot title, subtitle, caption and axis titles.
#' @param fill.style choose "gradient" to fill on the value.var (default) or "category" to fill on a categorical variable
#' @param fill.var if fill.style = "category", it will be filled using this variable
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_ridge <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                     label.var, label.title = NULL,
                     value.var, value.title = NULL, 
                     fill.style = "gradient", fill.var,
                     pal.choice = NULL, legend.position = NULL, 
                     save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  label.var <- as.name(label.var)
  value.var <- as.name(value.var)
  fill.var <- as.name(fill.var)
  
  fill.style <- tolower(fill.style)
  if (fill.style == "category") {
    layer_plt <- geom_density_ridges(aes(fill = !!fill.var))
    
    layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
      scale_fill_manual(values = pal.choice)
    }
    
  } else if (fill.style == "gradient") {
    layer_plt <- geom_density_ridges_gradient(aes(fill = after_stat(x)),
                                              scale = 3)
    
    layer_color <- if(gtools::invalid(pal.choice) | length(pal.choice) < 3) {
        scale_fill_gradientn(colors = scales::hue_pal()(3))
      } else {
        scale_fill_gradientn(colors = pal.choice[1:3])
      }
  }
  
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = value.title, 
                              y.title = label.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  plt <- ggplot(data = df, aes(x = !!value.var,
                               y = forcats::fct_rev(!!label.var))) +
    
    layer_plt +
    layer_color +
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}

#' @title Histogram plot
#' 
#' Generate histograms. 
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,value.title Text for plot title, subtitle, caption and axis titles.
#' @param value.var numeric Variable to plot the density of.
#' @param value.low,value.high. Fixed axes range for the x-axis. Default is no limits.
#' @param value.ref.value Add a reference line intersecting the x-axis. Default is no line.
#' @param bar.width set the bar widths to be "auto" which uses Sturge's Rule, or choose a numeric value
#' @param normal.show set to TRUE to overlay with a line representing a normal distribution equivalent, based on the value.var mean and standard deviation. Does not work with facet grids.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if value.low & value.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_histogram <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                          value.var, value.low = NA, value.high = NA, value.title = NULL, value.ref.value = NA,
                          bar.width = "auto", normal.show = FALSE,
                          row.grid.var = NA, row.grid.axis = "free", 
                          col.grid.var = NA, col.grid.axis = "free",
                          pal.choice = NULL,
                          save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  value.var.sym  <- as.name(value.var)
  value.var.str  <- value.var
  
  row.grid.var <- if (gtools::invalid(row.grid.var)) NULL else as.name(row.grid.var)
  col.grid.var <- if (gtools::invalid(col.grid.var)) NULL else as.name(col.grid.var)
  
  df.plt <- df %>%
    select(!!value.var.sym, !!row.grid.var, !!col.grid.var)
  
  y.title <- "Density (%)"
  x.title <- value.title
  
  # Bins per facet group, then take the median
  group_vars <- c(
    if (!is.null(row.grid.var)) as.character(row.grid.var),
    if (!is.null(col.grid.var)) as.character(col.grid.var)
  )
  
  if (length(group_vars) == 0) {
    # No facets – use total n
    n <- nrow(df.plt)
    sturge_bins <- ceiling(1 + log2(n))
  } else {
    group_ns <- df.plt %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(n = n(), .groups = "drop") %>%
      pull(n)
    sturge_bins <- ceiling(1 + log2(median(group_ns)))
  }
  
  if (is.numeric(bar.width)) {
    binwidth <- bar.width
    bins     <- NULL
  } else {
    bins     <- sturge_bins
    binwidth <- NULL
  }
  
  # Colour palette
  pal.choice.clean <- if (gtools::invalid(pal.choice)) {
    c("#00B0F6", "#F8766D")
  } else {
    pal.choice
  }
  
  # Normal curve overlay 
  layer_normal <- if (normal.show) {
    if (length(group_vars) == 0) {
      # Single panel: stat_function is fine
      stat_function(
        fun       = dnorm,
        args      = list(mean = mean(df.plt[[value.var.str]], na.rm = TRUE),
                         sd   = sd(df.plt[[value.var.str]],   na.rm = TRUE)),
        color     = pal.choice.clean[2],
        linewidth = 1
      )
    } else {
      # Multiple panels: not working, so set to NULL
      message("Normal distribution overlay does not function accurately for facet grids and has been removed for this function.")
      NULL
    }
  } else {
    NULL
  }
  
  # Reference line
  layer_vline <- if (!gtools::invalid(value.ref.value)) {
    geom_vline(xintercept = value.ref.value, color = "black", linetype = "dashed")
  } else {
    NULL
  }
  
  # Facet grid
  layer_scales_option <- if (is.null(row.grid.var) & is.null(col.grid.var)) {
    NULL
  } else if (is.null(row.grid.var)) {
    col.grid.axis
  } else if (is.null(col.grid.var)) {
    row.grid.axis
  } else if (row.grid.axis == col.grid.axis) {
    row.grid.axis
  } else if (row.grid.axis == "free") {
    "free_y"
  } else {
    "free_x"
  }
  
  layer_grid <- facet_grid(
    rows   = if (is.null(row.grid.var)) NULL else vars(!!row.grid.var),
    cols   = if (is.null(col.grid.var)) NULL else vars(!!col.grid.var),
    scales = layer_scales_option
  )
  
  # Plot
  plot_theme <- plot_theme_fn(
    plt.title    = plt.title,
    plt.subtitle = plt.subtitle,
    x.title      = x.title,
    y.title      = y.title,
    plt.caption  = plt.caption,
    legend.position = "none"
  )
  
  plt <- ggplot(df.plt, aes(x = !!value.var.sym)) +
    
    geom_histogram(
      aes(y = after_stat(count / sum(count))),
      color    = pal.choice.clean[1],
      fill     = pal.choice.clean[1],
      alpha    = 0.5,
      binwidth = binwidth,
      bins     = bins
    ) +
    
    layer_normal +
    
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::percent) +
    
    coord_cartesian(xlim = c(value.low, value.high)) +
    
    layer_grid +
    layer_vline +
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}


#' @title Bar plot
#' 
#' Generate bar plots. Includes options for stacked plots.
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,label.title,count.title Text for plot title, subtitle, caption and axis titles.
#' @param count.var numeric Variable to plot on the y-axis.
#' @param count.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param count.type Choose either "numbers" for raw counts (default), or "percentages" for % share shown as stacked sum to 100%
#' @param stack.var For stacked bar charts, categorical variable to stack by (NULL as default).
#' @param labels.show Show labels on bars. Recommend not showing labels for complex charts involving lots of stacking, facet grids, etc., as alignment often fails.
#' @param labels.position 0 for the base, 0.5 for middle, 1 for the end. Will need manual tweaking if complex plot
#' @param labels.fill,labels.size colour and size of labels
#' @param sorting place in "ascending", "descending", or "original" (default) order. Choosing ascending or descending will override factor variable sorting.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default. If stack = NA, colours are applied to label.var.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param orientation Display the boxes as "vertical" (default) or "horizontal"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_bar <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                   label.var, label.title = NULL,
                   count.var, count.title = NULL, count.ref.value, count.type = "numbers", 
                   stack.var = NULL,
                   labels.show = FALSE, labels.position = 1, labels.fill = "lightgrey", labels.size = 3,
                   sorting = "original",
                   row.grid.var = NA, row.grid.axis = "free", 
                   col.grid.var = NA, col.grid.axis = "free",
                   pal.choice = NULL, legend.position = NULL, orientation = "vertical",
                   save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  label.var <- as.name(label.var)
  count.var <- as.name(count.var)
  stack.var <- if (gtools::invalid(stack.var)) {NULL} else {as.name(stack.var)}
  color.var <- if (is.null(stack.var)) {label.var} else {stack.var}
  
  
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  count.type <- tolower(count.type)
  y.position <- if (count.type %in% c("number", "numbers")) {
    "stack"
  } else if (count.type %in% c("percentage", "percentages")) {
    "fill"
  }
  y.column <- if (count.type %in% c("number", "numbers")) {
    as.name("N")
  } else if (count.type %in% c("percentage", "percentages")) {
    as.name("PC")
  }
 y_scale <- if (count.type %in% c("number", "numbers")) {
    scale_y_continuous(labels = comma)
  } else if (count.type %in% c("percentage", "percentages")) {
    scale_y_continuous(labels = scales::percent)
  }
  
 
  df.plt <- df %>% 
    group_by(!!label.var, !!stack.var, !!row.grid.var, !!col.grid.var) %>% 
    summarise(N = sum(!!count.var, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(!!row.grid.var, !!col.grid.var) %>% 
    mutate(PC = N / sum(N)) %>% 
    ungroup()
  
  
  # sort ascending / descending if needed
  # not done using aes(x = reorder(...)) because needs to be robust to stacking, facet grids, etc.
  sorting <- tolower(sorting)
  
  if (sorting %in% c("ascending", "descending")) {

    order_df <- df.plt %>%
      group_by(!!label.var) %>%
      summarise(grand_total = sum(N, na.rm = TRUE), .groups = "drop")
    
    if (sorting == "ascending") {
      order_df <- order_df %>% arrange(grand_total)
    } else if (sorting == "descending") {
      order_df <- order_df %>% arrange(desc(grand_total))
    }
    
    new_levels <- order_df %>% pull(!!label.var) %>% as.character()
    
    df.plt <- df.plt %>%
      mutate(
        !!label.var := as.character(!!label.var),
        !!label.var := factor(!!label.var, levels = new_levels)
      )
  }
  
  
  # add reference lines
  layer_hline <- if (!gtools::invalid(count.ref.value)) geom_hline(yintercept = count.ref.value, color = "black", linetype = "dashed") else NULL
  
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
  
  # add labels
  layer_label <- if (labels.show == TRUE & !is.na(labels.fill)) {
    
      if (y.position == "stack") {
        geom_label(aes(label = N), 
                   position = position_stack(vjust = labels.position),
                   color = "black",
                   fill = labels.fill, 
                   size = labels.size)
      } else if (y.position == "fill") {
        geom_label(aes(label = N), 
                   position = position_fill(vjust = labels.position),
                   color = "black",
                   fill = labels.fill, 
                   size = labels.size)
      }
    
    } else if (labels.show == TRUE & is.na(labels.fill)) {
      
      if (y.position == "stack") {
        geom_text(aes(label = N), 
                   position = position_stack(vjust = labels.position),
                   color = "black",
                   size = labels.size)
      } else if (y.position == "fill") {
        geom_text(aes(label = N), 
                   position = position_fill(vjust = labels.position),
                   color = "black",
                   size = labels.size)
      
      }
    } else {NULL}
  
  
  # change orientation from column to bar
  
  orientation.clean <- tolower(orientation)
  orientation.flip <- orientation.clean == "horizontal"
  
  layer_flip <- if (orientation.flip) {coord_flip()} else NULL 
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  layer_fill <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_fill_manual(values = pal.choice)
  }
  
  # Plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = label.title, 
                              y.title = count.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot(df.plt, aes(x = if (orientation.flip) forcats::fct_rev(!!label.var) else !!label.var,
                            y = !!y.column, color = !!color.var, fill = !!color.var)) +
    
    geom_bar(stat = "identity", position = y.position) +
    
    y_scale +
    layer_color +
    layer_fill +
    layer_grid +
    layer_hline +
    layer_label +
    layer_flip +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}

#' @title Funnel plot
#' 
#' Generate funnel plots for count variables using packages FunnelPlotR and MASS if negative binomial regression needed.
#' The Poisson regression will check for overdispersion and if overdispersed, switches to use a negative binomial regression.
#' Note that two plots are produced: the first looks at how well the Poisson regression fits the data, and the second is the funnel plot itself. Also review regression results in table res.reg.
#' @param df Dataframe being plotted
#' @param plt.title Title for the funnel plot
#' @param y.var The dependent variable
#' @param reg.vars The regressors
#' @param group.var How to group the datapoints
#' @param sr.method Either "CQC" (default) or "SHMI". See documentation for FunnelPlotR for more
#' @param pal.choice Color palette to use. Recommend using the default colours.
plt_funnel <- function(df, plt.title = NULL,
                       y.var, reg.vars, group.var,
                       sr.method = "CQC", pal.choice = "default", ...) {
  
  # run Poisson regression to generate fitted values
  df.reg <- df %>%
    drop_na(!!sym(y.var), !!sym(group.var), all_of(reg.vars))
  
  mod.base <- glm(reformulate(reg.vars, y.var),
                  family = "poisson",
                  data = df.reg)
  
  # check for overdispersion, and run negative binomial if overdispersed
  test.dispersion <- round(dispersiontest(mod.base, trafo = NULL)$estimate, 3)
  test.over <- round(dispersiontest(mod.base, trafo = 0)$p.value, 5)
  model_choice = ifelse(test.over >= 0.05, 1, 2)
  
  if (model_choice == 1) {
    # Quasi-poisson model
    message("No overdispersion. Quasi-poisson model used for fitted values.")
    model <<- glm(reformulate(reg.vars, y.var), 
                  family = "quasipoisson", 
                  data = df.reg)
    cov <- vcovHC(model, type="HC0")
    std.error <- sqrt(diag(cov))
    res.reg <<- tidy(model) %>%
      select(-c(statistic, std.error, p.value)) %>%
      cbind(std.error) %>%
      mutate(p.value = 2 * pnorm(abs(estimate/std.error), lower.tail=FALSE)
             ,exp.estimate = exp(estimate)
             ,exp.estimate.low = exp(estimate - 2*std.error)
             ,exp.estimate.high = exp(estimate + 2*std.error)
             ,sig.level = case_when(p.value < 0.01 ~ "1% level"
                                    ,p.value < 0.05 ~ "5% level"
                                    ,p.value < 0.1 ~ "10% level"
                                    ,TRUE ~ "Not significant")
      ) %>%
      mutate_if(is.numeric, ~round(.,3))
    
  } else if (model_choice == 2) {
    # Negative binomial model
    message("Overdispersion present. Negative binomial model used for fitted values.")
    model <<- glm.nb(reformulate(reg.vars, y.var), 
                     data = df.reg)
    cov <- vcovHC(model, type="HC0")
    std.error <- sqrt(diag(cov))
    res.reg <<- tidy(model) %>%
      select(-c('statistic', 'std.error', 'p.value')) %>%
      cbind(std.error) %>%
      mutate(p.value = 2 * pnorm(abs(estimate/std.error), lower.tail=FALSE)
             ,exp.estimate = exp(estimate)
             ,exp.estimate.low = exp(estimate - 2*std.error)
             ,exp.estimate.high = exp(estimate + 2*std.error)
             ,sig.level = case_when(p.value < 0.01 ~ "1% level"
                                    ,p.value < 0.05 ~ "5% level"
                                    ,p.value < 0.1 ~ "10% level"
                                    ,TRUE ~ "Not significant")
      ) %>%
      mutate_if(is.numeric, ~round(.,3))
    
  } else {
    stop("No valid model selected.")
  }
  
  # run diagnostics on model fit
  gap <- nrow(df.reg) - length(model$residuals)
  actual <- df.reg[[y.var]] %>% {if (gap > 0) tail(.,-gap) else .}
  pred <- model$fitted.values
  resids <- model$residuals
  stdresids <- rstandard(model)
  cook <- cooks.distance(model)
  
  test.df <- df.reg %>% 
    {if (gap > 0) tail(.,-gap) else .} %>% 
    cbind(actual, pred, resids, cook) %>% 
    mutate(row_number = row_number())
  
  plt.test.scatter.resid <- ggplot(test.df, aes(x = pred, y = resids)) +
    geom_point(color = "black", shape = 1, size = 2, alpha = 1, stroke = 0, show.legend = FALSE) +
    labs(x = "Fitted", y = "Residuals",
         title = "Residuals vs fitted values") +
    theme_minimal()
  
  plt.test.scatter.cook <- ggplot(test.df, aes(x = row_number, y = cook)) +
    geom_point(color = "black", shape = 1, size = 2, alpha = 1, stroke = 0, show.legend = FALSE) +
    labs(x = "Obs", y = "Cook's Distance",
         title = "Cook Distances") +
    theme_minimal()
  
  plt.test.scatter.fit <- ggplot(test.df, aes(x = actual)) +
    geom_smooth(aes(y = pred, color = "Fitted"), method = "lm", se = TRUE, linewidth = 1.25) +
    geom_point(aes(y = pred), color = "black", shape = 17, size = 1, alpha = 0.25, stroke = 0, show.legend = FALSE) +
    geom_line(aes(y = actual, color = "45-degree"), linetype = "dashed", linewidth = 1.1, show.legend = TRUE) +
    scale_color_manual(values = c("Fitted" = "black", "45-degree" = "red")) +
    labs(x = "Actual", y = "Fitted", color = "Model",
         title = "Actual vs Fitted") +
    theme_minimal()
  
  # print diagnostics
  plt.test.all <- plot_grid(plt.test.scatter.fit,
                            plt.test.scatter.resid,
                            plt.test.scatter.cook,
                            nrow = 2,
                            rel_heights = c(1/2, 1/2)
  )
  print(plt.test.all)
  
  # produce funnel plot
  
  if(length(pal.choice) >= 4) {
    pal.color <- pal.choice[1:4]
  } else if (length(pal.choice) >= 2) {
    message("Require at least 4 colours in pal.choice. Using default colours.")
    pal.color <- c("#FF7F0EFF", "#1F77B4FF", "#9467BDFF", "#2CA02CFF")
  } else if (pal.choice == "default") {
    pal.color <- c("#FF7F0EFF", "#1F77B4FF", "#9467BDFF", "#2CA02CFF")
  } else {
    message("Require at least 4 colours in pal.choice. Using default colours.")
    pal.color <- c("#FF7F0EFF", "#1F77B4FF", "#9467BDFF", "#2CA02CFF")
  }
  
  
  if (model_choice == 1) {
    
    plt <- funnel_plot(test.df, numerator = !!sym(y.var), denominator = pred, group = !!sym(group.var),
                       title = plt.title, draw_unadjusted = TRUE, draw_adjusted = FALSE, sr_method = sr.method,
                       plot_cols = pal.color
    )
    
  } else if (model_choice == 2) {
    
    plt <- funnel_plot(test.df, numerator = !!sym(y.var), denominator = pred, group = !!sym(group.var),
                       title = plt.title, draw_unadjusted = FALSE, draw_adjusted = TRUE, sr_method = sr.method,
                       plot_cols = pal.color
    )
    
  }
  
  print(plt)
  
  return(plt)
}

# examples ----------------------------------------------------------------

stores_fruit <- readr::read_csv("stores_fruit.csv") %>% 
  mutate(dow = factor(dow, levels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")))

## box plot ----
plt.box <- plt_box(df = stores_fruit,
                   plt.title = "Box plot of quantity of fruit sold by day of the week",
                   plt.subtitle = NULL,
                   plt.caption = "Line = median, 'X' = mean, Box = inter-quartile range, Whisker = 1.5 x IQR.",
                   y.var = "quantity",
                   y.low = NA, y.high = NA,
                   y.title = "Quantity",
                   y.ref.value = NA,
                   x.var = "product",
                   x.title = "Product",
                   outliers.show = TRUE,
                   row.grid.var = "dow", row.grid.axis = "fixed",
                   col.grid.var = NA, col.grid.axis = "free",
                   orientation = "vertical",
                   pal.choice = c("blue", "gold", "red"),
                   legend.position = "none",
                   save.chart = FALSE, save.filepath = "output/", save.filename = "box_plot"
)

## just means ----
plt.means <- plt_means(df = stores_fruit,
                       plt.title = "Mean price of fruit sold by day of the week",
                       plt.subtitle = NULL,
                       plt.caption = "Point = mean average. Whiskers = min and max.",
                       y.var = "ppu",
                       y.low = NA, y.high = NA,
                       y.title = "Price per unit",
                       y.ref.value = NA,
                       y.whiskers = "min-max",
                       x.var = "product", 
                       x.title = "Product",
                       point.size = 3,
                       whisker.size = 0.25,   
                       row.grid.var = "dow", row.grid.axis = "fixed",
                       col.grid.var = NA, col.grid.axis = "free",
                       orientation = "horizontal",
                       pal.choice = c("blue", "gold", "red"),
                       legend.position = "top",
                       save.chart = FALSE, save.filepath = "output/", save.filename = "means_chart"
)

## violin plot ----
plt.violin <- plt_violin(df = stores_fruit %>% filter(store %in% c("A001", "A002", "A003")),
                         plt.title = "Box plot of quantity of fruit sold by day of the week",
                         plt.subtitle = NULL,
                         plt.caption = "Bar = mean",
                         y.var = "quantity",
                         y.low = NA, y.high = NA,
                         y.title = "Quantity",
                         y.ref.value = NA,
                         x.var = "product",
                         x.title = "Product / Day of week",
                         violin.var = "store",
                         show.average = TRUE, 
                         average.type = "mean",
                         average.width = 0.1,
                         col.grid.var = "dow", col.grid.axis = "fixed",
                         row.grid.var = NA, row.grid.axis = "free",
                         orientation = "vertical",
                         pal.choice = NULL,
                         legend.position = "top",
                         save.chart = FALSE, save.filepath = "output/", save.filename = "violin_plot"
)

## ridgeline ----

library(ggridges)

price_levels <- stores_fruit %>% 
  filter(product == "apple") %>%
  group_by(store) %>% 
  summarise(ppu = mean(ppu)) %>% 
  ungroup() %>% 
  arrange(-ppu) %>% 
  mutate(store = forcats::fct_inorder(store)) %>% 
  pull(store)

plt.ridge <- plt_ridge(df = stores_fruit %>% 
                            filter(product == "apple") %>% 
                            mutate(store = factor(store, levels = price_levels)),
                       plt.title = "Distributions of apple prices by store",
                       plt.subtitle = NA,
                       plt.caption = "Providers arranges by mean price, high to low.",
                      
                       label.var = "store",
                       label.title = "Store Code",
                       value.var = "ppu",
                       value.title = "Price per unit",
                       fill.style = "gradient", 
                       fill.var = NA,
                      
                       pal.choice = NULL,
                       legend.position = "none",
                       save.chart = FALSE,
                       save.filepath = "output/",
                       save.filename = "ridge_chart"
)


## histogram ----
plt.histogram <- plt_histogram(df = stores_fruit,
                               plt.title = "Price distribution across all stores",
                               plt.subtitle = NA,
                               plt.caption = NA,
                               value.var = "ppu",
                               value.low = NA, 
                               value.high = NA,
                               value.title = "Price per unit",
                               value.ref.value = NA,
                               bar.width = 0.1,
                               normal.show = FALSE,
                               row.grid.var = "product", row.grid.axis = "fixed", 
                               col.grid.var = NA, col.grid.axis = NA, 
                               pal.choice = NULL,
                               save.chart = FALSE,
                               save.filepath = "output/",
                               save.filename = "histogram"
)

## bar plot ----
plt.bar <- plt_bar(df = stores_fruit %>% filter(date == as.Date("2021-04-01")),
                   plt.title = "Sales on 2024-04-01",
                   plt.subtitle = NULL,
                   plt.caption = "Bars sorted in descending order for grand total sales",
                   label.var = "store",
                   label.title = "Store",
                   stack.var = "product",
                   count.var = "quantity",
                   count.title = "Quantity",
                   count.ref.value = NA,
                   count.type = "numbers",
                   row.grid.var = NA, row.grid.axis = NA,
                   col.grid.var = NA, col.grid.axis = NA,
                   orientation = "horizontal",
                   sorting = "descending",
                   pal.choice = c("blue", "gold", "red"),
                   legend.position = "top",
                   labels.show = FALSE,
                   labels.fill = "lightgrey",
                   labels.position = 0.5,
                   labels.size = 2,
                   save.chart = FALSE,
                   save.filepath = "output/",
                   save.filename = "bar_chart"
)

## funnel plots ----

library(conflicted)
library(FunnelPlotR)
library(AER)
conflicts_prefer(base::as.Date())
library(MASS)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
)
library(cowplot)
library(broom)

# note this is not a practical example of how to use a funnel plot
# for more on its uses, see https://nhs-r-community.github.io/FunnelPlotR/
plt.funnel <- plt_funnel(df = stores_fruit %>% 
                           mutate(store = factor(store)),
                        
                        plt.title = "Funnel plot of fruit sales per store",
                        
                        y.var = "quantity",
                        reg.vars = c("product", "dow", "ppu"),
                        group.var = "store",
                        
                        sr.method = "CQC", # either "CQC" or "SHMI". Type ?FunnelPlotR in the console for more.
                        pal.choice = "default" # choose colour palette. Can use the package default colours by typing "default"
)
