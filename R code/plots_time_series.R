
# functions ---------------------------------------------------------------

#' @title Line chart
#' 
#' Generate time-series line charts. Includes options for filling regions and rolling averages.
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var Variable to plot on the y-axis.
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.aggregation How to aggregate y.var. Choose "sum", "mean", or "median".
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param x.var Variable to plot on the x-axis.
#' @param x.axis.frequency Frequency within the x-axis, to prevent label cluttering. Choose "daily", "weekly", "monthly", "quarterly", "yearly".
#' @param lines.var Show a line for each category under this selected variable. Default is NA (i.e. a single line)
#' @param line.width Width of the line. Default = 1.
#' @param fill.var Shade in regions of the plot based on a chosen binary field. Default is NA (no fill).
#' @param roll.window To plot a rolling average, enter a numeric for number of rolling periods desired. Default is NA (no rolling window line plotted)
#' @param roll.line.width Width of rolling window line. Default is 1.5.
#' @param roll.line.type Type of rolling window line. Choose in-built ggplot line types, "dotted" as default.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_line <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                    y.var, y.low = NA, y.high = NA, y.title = NULL, y.aggregation = "sum", y.ref.value = NA,
                    x.var, x.title = NULL, x.axis.frequency = "daily",
                    lines.var = NA, line.width = 1, fill.var = NA, 
                    roll.window = NA, roll.line.width = 1.5, roll.line.type = "dotted",
                    row.grid.var = NA, row.grid.axis = "free", 
                    col.grid.var = NA, col.grid.axis = "free",
                    pal.choice = NULL, legend.position = NULL,
                    save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  lines.var <- if (gtools::invalid(lines.var)) {as.name("All")} else {as.name(lines.var)}
  df$All <- if(lines.var == "All") {"All"}
  fill.var <- if (gtools::invalid(fill.var)) {NULL} else {as.name(fill.var)}
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  y.aggregation <- tolower(y.aggregation)
  
  if(!y.aggregation %in% c("sum", "mean", "median")) {
    #dlg_message("Aggregation type must be sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'sum', 'mean', or 'median'")
  }
  
  x.axis.frequency <- tolower(x.axis.frequency)
  x.axis.frequency <- ifelse(x.axis.frequency == "annually", "yearly", x.axis.frequency)
  if(!x.axis.frequency %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
    #dlg_message("Aggregation type must be daily, weekly, monthly, quarterly or yearly", type = "ok")
    stop("Aggregation type must be 'daily', 'weekly', 'monthly', 'quarterly' or 'yearly'")
  }
  
  # base plot
  df.plt <- df %>% 
    group_by(!!lines.var,!!row.grid.var, !!col.grid.var, !!x.var) %>% 
    summarise(N = match.fun(y.aggregation)(!!y.var, na.rm = TRUE)) %>%
    ungroup()
  
  # add rolling mean
  if (gtools::invalid(roll.window)) {
    
    layer_rollmean <- NULL
    
  } else {
    
    df.plt <- df.plt %>% 
      group_by(!!lines.var,!!row.grid.var) %>% 
      mutate(`Rolling Mean` = zoo::rollmean(N, k = roll.window, fill = NA, align= 'right')) %>% 
      ungroup()
    
    layer_rollmean <- geom_line(data = df.plt, aes(x = .data[[x.var]], y = `Rolling Mean`, color = .data[[lines.var]]), 
                                linewidth = roll.line.width, alpha = 1, linetype = roll.line.type, inherit.aes = FALSE)
  }
  
  # add date fills
  if (!gtools::invalid(fill.var)) {
    
    df.fill <- df %>% 
      select(!!x.var, !!fill.var) %>% 
      distinct() %>% 
      arrange(!!x.var)%>%
      mutate(
        next_x = dplyr::lead(!!x.var),
        prev_x = dplyr::lag(!!x.var),
        xmin = if_else(!is.na(prev_x),
                       !!x.var - (as.numeric(!!x.var - prev_x) / 2),
                       !!x.var - (as.numeric(next_x - !!x.var) / 2)),
        xmax = if_else(!is.na(next_x),
                       !!x.var + (as.numeric(next_x - !!x.var) / 2),
                       !!x.var + (as.numeric(!!x.var - prev_x) / 2))
      )
        
    
    if (nrow(df.fill) > nrow(df %>% distinct(!!x.var))) {
      
      #dlg_message("There is more than one fill category per date. Fill will not function correctly.", type = "ok")
      layer_fill <- NULL
      
    } else {
      
      df.plt <- df.plt %>% 
        left_join(df.fill, by = c(as.character(x.var)))
      
      layer_fill <- geom_rect(data = df.plt[df.plt[[fill.var]] == 1,],
                              aes(xmin = xmin, 
                                  xmax = xmax, 
                                  ymin = -Inf, 
                                  ymax = Inf,
                                  fill = "lightgrey"), 
                              alpha = 0.3, show.legend = FALSE)
    }
  } else {
    layer_fill <- NULL
  }
  
  # format the date
  date_scale <- if(x.axis.frequency == "daily") {
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "weekly") {
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "monthly") {
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "quarterly") {
    scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "yearly") {
    scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")
  }
  
  # add facet grid
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
  
  # add reference line
  layer_hline <- if (gtools::invalid(y.ref.value)) {NULL} else {
    geom_hline(yintercept = y.ref.value, color = "black", linetype = "dashed")
  } 
  
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  
  
  # create the plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  plt <- ggplot() +
    geom_line(data = df.plt, aes(x = .data[[x.var]], y = N, color = .data[[lines.var]]), 
              linewidth = line.width, linetype = "solid", alpha = 0.5) +
    
    scale_fill_manual(values = "lightgrey") +
    
    scale_y_continuous(labels = comma) +
    layer_color +
    coord_cartesian(ylim = c(y.low,y.high)) +
    
    layer_fill +
    layer_rollmean +
    layer_hline + 
    layer_grid + 
    
    date_scale +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                                 plot = plt)
  
  return(plt)  
}

#' @title Box-plot as time-series
#' 
#' Generate a series of box plots over time to see how distribution changes over time
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,date.title Text for plot title, subtitle, caption and axis titles.
#' @param y.var Variable to plot on the y-axis.
#' @param y.aggregation.level date variable to aggregate on
#' @param date.aggregation choose month, quarter, year, or financial year
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.aggregation option to aggregate daily to smooth numbers and speed up processing. Choose "None", "Sum", "Mean", or "Median"
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param outliers.show Show outliers for datapoints outside the whiskers of the box plot. Default is FALSE.
#' @param row.grid.var,row.grid.axis,col.grid.var,col.grid.axis. Select variable for facet grids, and if the axes should be "free" or "fixed". Default is "free", overridden if y.low & y.high are set.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_box_ts <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                          y.var, y.low = NA, y.high = NA, y.title = NULL, y.aggregation = "sum", y.ref.value = NA,
                          y.aggregation.level, date.aggregation = "month", date.title = NULL,
                          outliers.show = FALSE,
                          row.grid.var = NA, row.grid.axis = "free", 
                          col.grid.var = NA, col.grid.axis = "free",
                          pal.choice = NULL, legend.position = NULL,
                          save.chart = FALSE, save.filepath, save.filename,...
) {
  
  df$All <- "All"
  y.var <- as.name(y.var)
  y.aggregation.level <- as.name(y.aggregation.level)
  row.grid.var <- if (gtools::invalid(row.grid.var)) {NULL} else {as.name(row.grid.var)}
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  
  y.aggregation <- tolower(y.aggregation)
  if(!y.aggregation %in% c("none", "sum", "mean", "median")) {
    #dlg_message("Aggregation type must be none, sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'none', 'sum', 'mean', or 'median'")
  }
  
  date.aggregation <- tolower(date.aggregation)
  if(!date.aggregation %in% c("month", "quarter", "year", "financial year")) {
    #dlg_message("Aggregation type must be 'month', 'quarter', 'year', or 'financial year'", type = "ok")
    stop("Aggregation type must be 'month', 'quarter', 'year', or 'financial year'")
  }
  
  
  # base plot
  if(y.aggregation == "none") {
    
    df.plt <- df %>% 
      select(All, !!row.grid.var, !!col.grid.var, !!y.aggregation.level, !!y.var) %>% 
      rename(N = y.var)
  } else {
    df.plt <- df %>% 
      select(All, !!row.grid.var, !!col.grid.var, !!y.aggregation.level, !!y.var) %>% 
      group_by(All, !!row.grid.var, !!col.grid.var, !!y.aggregation.level) %>% 
      summarise(N = match.fun(y.aggregation)(!!y.var, na.rm = TRUE)) %>%
      ungroup()
  }
  
  
  if (date.aggregation == "month") {
    
    df.plt <- df.plt %>% 
      mutate(Period = ceiling_date(!!y.aggregation.level, "month") - days(1),
             Period = as.factor(Period))
    
  } else if (date.aggregation == "quarter") {
    
    df.plt <- df.plt %>% 
      mutate(Period = ceiling_date(!!y.aggregation.level, "quarter") - days(1),
             Period = as.factor(Period))
    
  } else if (date.aggregation == "year") {
    
    df.plt <- df.plt %>% 
      mutate(Period = ceiling_date(!!y.aggregation.level, "year") - days(1),
             Period = as.factor(Period))
    
  } else if (date.aggregation == "financial year") {
    
    df.plt <- df.plt %>% 
      mutate(Period = if_else(month(!!y.aggregation.level) > 3,
                              make_date(year(!!y.aggregation.level) + 1, 3, 31),
                              make_date(year(!!y.aggregation.level), 3, 31)),
             Period = as.factor(Period))
    
  }
  
  
  # add facet grid
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
  
  # add reference line
  layer_hline <- if (gtools::invalid(y.ref.value)) {NULL} else {
    geom_hline(yintercept = y.ref.value, color = "black", linetype = "dashed")
  } 
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  layer_fill <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_fill_manual(values = pal.choice)
  }
  
  # create the plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = date.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  plt <- ggplot() +
    geom_boxplot(data = df.plt, aes(x = Period, y = N, 
                                    color = if(!gtools::invalid(row.grid.var)) .data[[row.grid.var]] else NULL, 
                                    fill = if(!gtools::invalid(row.grid.var)) .data[[row.grid.var]] else NULL), 
                 position = "identity", linewidth = 0.5, alpha = 0.25,
                 outliers = outliers.show, outlier.size = 0.5, outlier.stroke = 0.25) +
    stat_summary(data = df.plt, aes(x = Period, y = N), 
                 fun = mean, geom = "point", shape = 4, size = 3, color = "black", alpha = 0.25, stroke = 1.25) +
    
    layer_color +
    layer_fill +
    scale_y_continuous(labels = comma) +
    
    scale_x_discrete(labels = function(x) format(as.Date(x, format = "%Y-%m-%d"), "%b-%y")) +
    
    coord_cartesian(ylim = c(y.low,y.high)) +
    
    layer_hline + 
    layer_grid + 
    
    plot_theme +
    
    theme(legend.title = element_blank())
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}


#' @title Time-series seasonal decomposition plot
#' 
#' Generate charts showing a seasonal decomposition of a variable using moving averages, from stats package
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var Variable to plot on the y-axis.
#' @param y.aggregation How to aggregate y.var. Choose "sum", "mean", or "median".
#' @param x.var Variable to plot on the x-axis.
#' @param x.axis.frequency Frequency within the x-axis, to prevent label cluttering. Choose "daily", "weekly", "monthly", "quarterly", "yearly".
#' @param row.grid.axis whether the decomposition components should have "fixed" or "free" axes
#' @param decomp.freq natural frequency of the data in days. E.g., anything with weekly cycles should receive value of 7
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_seasonal <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                            y.var, y.title = NULL, y.aggregation = "sum", row.grid.axis = "free",
                            x.var, x.title = NULL, x.axis.frequency = "daily", decomp.freq,
                            pal.choice = NULL,
                            save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  
  y.aggregation <- tolower(y.aggregation)
  if(!y.aggregation %in% c("sum", "mean", "median")) {
    #dlg_message("Aggregation type must be sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'sum', 'mean', or 'median'")
  }
  
  x.axis.frequency <- tolower(x.axis.frequency)
  x.axis.frequency <- ifelse(x.axis.frequency == "annually", "yearly", x.axis.frequency)
  if(!x.axis.frequency %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
    #dlg_message("Aggregation type must be daily, weekly, monthly, quarterly or yearly", type = "ok")
    stop("Aggregation type must be 'daily', 'weekly', 'monthly', 'quarterly' or 'yearly'")
  }
  
  # base table
  df.plt <- df %>% 
    select(!!x.var, !!y.var) %>% 
    group_by(!!x.var) %>% 
    summarise(N = match.fun(y.aggregation)(!!y.var, na.rm = TRUE)) %>%
    ungroup()
  
  # base plot
  decomp.vector <- stats::ts(df.plt$N, frequency = decomp.freq)
  frequency(decomp.vector)
  decomp <- stats::decompose(decomp.vector)
  df.decomp <- data.frame(df.plt[[x.var]]
                          ,unlist(decomp$x)
                          ,unlist(decomp$seasonal)
                          ,unlist(decomp$trend)
                          ,unlist(decomp$random))
  df.decomp <- df.decomp %>% 
    mutate(across(where(is.ts), as.numeric)) %>%
    rename_with(~c("Date", "Original", "Seasonal", "Trend", "Random")) %>%
    pivot_longer(cols = -c(Date), names_to = "Variable", values_to = "Values")
  
  # format the date
  date_scale <- if(x.axis.frequency == "daily") {
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "weekly") {
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "monthly") {
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "quarterly") {
    scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "yearly") {
    scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")
  }
  
  # facet grid y-axis
  layer_grid <- if (row.grid.axis == "free") {
    
    facet_grid(Variable ~ ., scales = "free")
    
  } else if (row.grid.axis == "fixed") {
    
    facet_grid(Variable ~ ., scales = "free_x")
    
  }
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(values = pal.choice)
  }
  
  # create the plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = "none")
  
  
  plt <- ggplot(df.decomp, aes(x = Date)) +
    geom_line(aes(y = Values, colour = Variable)) +
    layer_grid +
    geom_hline(yintercept = 0, linetype = "dotted") +
    layer_color +    
    scale_y_continuous(labels = comma) +
    date_scale +
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}


#' @title Time-series structural break plot
#' 
#' Generate time-series plots with Bai-Perron breakpoints, regressing only on the index
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var Variable to plot on the y-axis.
#' @param y.low,y.high. Fixed axes range for the y-axis. Default is no limits.
#' @param y.aggregation How to aggregate y.var. Choose "sum", "mean", or "median".
#' @param y.ref.value Add a reference line intersecting the y-axis. Default is no line.
#' @param x.var Variable to plot on the x-axis.
#' @param x.axis.frequency Frequency within the x-axis, to prevent label cluttering. Choose "daily", "weekly", "monthly", "quarterly", "yearly".
#' @param line.width Width of the line. Default = 1.
#' @param fill.var Shade in regions of the plot based on a chosen binary field. Default is NA (no fill).
#' @param pal.choice Color palette to use. Default is R's default.
#' @param legend.position Position of the legend, using ggplot positions "top", "bottom", "left", "right", or "none"
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_bai_perron <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                           y.var, y.low = NA, y.high = NA, y.title = NULL, y.aggregation = "sum", y.ref.value = NA,
                           x.var, x.title = NULL, x.axis.frequency = "daily",
                           line.width = 1, fill.var = NA, 
                           pal.choice = NULL, legend.position = NULL,
                           save.chart = FALSE, save.filepath, save.filename,...
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  fill.var <- if (gtools::invalid(fill.var)) {NULL} else {as.name(fill.var)}
  y.aggregation <- tolower(y.aggregation)
  
  if(!y.aggregation %in% c("sum", "mean", "median")) {
    #dlg_message("Aggregation type must be sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'sum', 'mean', or 'median'")
  }
  
  x.axis.frequency <- tolower(x.axis.frequency)
  x.axis.frequency <- ifelse(x.axis.frequency == "annually", "yearly", x.axis.frequency)
  if(!x.axis.frequency %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
    #dlg_message("Aggregation type must be daily, weekly, monthly, quarterly or yearly", type = "ok")
    stop("Aggregation type must be 'daily', 'weekly', 'monthly', 'quarterly' or 'yearly'")
  }
  
  # base plot
  df.plt <- df %>% 
    select(!!x.var, !!y.var) %>% 
    group_by(!!x.var) %>% 
    summarise(N = match.fun(y.aggregation)(!!y.var, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(index = row_number())
  
  
  # Fit Bai-Perron breakpoints model
  bp_model <- strucchange::breakpoints(N ~ index, data = df.plt)
  break_dates <- df.plt[[x.var]][bp_model$breakpoints]
  df.plt$fitted <- fitted(bp_model)
  
  
  # add date fills
  if (!gtools::invalid(fill.var)) {
    
    df.fill <- df %>% 
      select(!!x.var, !!fill.var) %>% 
      distinct() %>% 
      arrange(!!x.var)%>%
      mutate(
        next_x = dplyr::lead(!!x.var),
        prev_x = dplyr::lag(!!x.var),
        xmin = if_else(!is.na(prev_x),
                       !!x.var - (as.numeric(!!x.var - prev_x) / 2),
                       !!x.var - (as.numeric(next_x - !!x.var) / 2)),
        xmax = if_else(!is.na(next_x),
                       !!x.var + (as.numeric(next_x - !!x.var) / 2),
                       !!x.var + (as.numeric(!!x.var - prev_x) / 2))
      )
    
    
    if (nrow(df.fill) > nrow(df %>% distinct(!!x.var))) {
      
      #dlg_message("There is more than one fill category per date. Fill will not function correctly.", type = "ok")
      layer_fill <- NULL
      
    } else {
      
      df.plt <- df.plt %>% 
        left_join(df.fill, by = c(as.character(x.var)))
      
      layer_fill <- geom_rect(data = df.plt[df.plt[[fill.var]] == 1,],
                              aes(xmin = xmin, 
                                  xmax = xmax, 
                                  ymin = -Inf, 
                                  ymax = Inf,
                                  fill = "lightgrey"), 
                              alpha = 0.3, show.legend = FALSE)
    }
  } else {
    layer_fill <- NULL
  }
  
  
  # format the date
  date_scale <- if(x.axis.frequency == "daily") {
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "weekly") {
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "monthly") {
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "quarterly") {
    scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "yearly") {
    scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")
  }
  
  
  # add reference line
  layer_hline <- if (gtools::invalid(y.ref.value)) {NULL} else {
    geom_hline(yintercept = y.ref.value, color = "black", linetype = "dashed")
  } 
  
  # colour palette
  layer_color <- if(gtools::invalid(pal.choice)) {NULL} else {
    scale_color_manual(
      name = NULL,
      values = c(
        "N" = pal.choice[1],
        "Fitted" = pal.choice[2],
        "Break dates" = pal.choice[3]
      )
    )
  }
  
  # create the plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = legend.position)
  
  
  plt <- ggplot() +
    geom_line(data = df.plt, aes(x = .data[[x.var]], y = N, colour = "N"), 
              linewidth = line.width, linetype = "solid", alpha = 0.5) +
    geom_line(data = df.plt, aes(x = .data[[x.var]], y = fitted, colour = "Fitted"), 
              linewidth = line.width, linetype = "solid", alpha = 0.5) +
    geom_vline(aes(colour = "Break dates", xintercept = break_dates),
               linetype = "dashed", linewidth = 0.75*line.width) +
    
    layer_color +
    scale_y_continuous(labels = comma) +
    
    coord_cartesian(ylim = c(y.low,y.high)) +
    
    layer_hline + 
    date_scale +
    
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}


#' @title SPC
#' 
#' Generate basic SPC chart using R package NHSRplotthedots
#' @param df Dataframe being plotted
#' @param plt.title,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.var Variable to plot on the y-axis.
#' @param y.aggregation How to aggregate y.var. Choose "sum", "mean", or "median".
#' @param x.var Variable to plot on the x-axis.
#' @param x.axis.frequency Frequency within the x-axis, to prevent label cluttering. Choose "daily", "weekly", "monthly", "quarterly", "yearly".
#' @param col.grid.var Select variable for column facet grid.
#' @param improvement.direction Choose which direction is good performance from "neutral" (default), "increase", "decrease"
#' @param target numeric target. NULL as default.
#' @param point.size Size of the points plotted. start at 3 and adjust down if there are lots of datapoints.
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_spc <- function(df, plt.title = NULL, 
                    y.var, y.title = NULL, y.aggregation = "sum",
                    x.var, x.title = NULL, x.axis.frequency = "daily",
                    col.grid.var = NA,
                    improvement.direction = "neutral", target = NULL, point.size = 3, 
                    save.chart = FALSE, save.filepath, save.filename,...
                            
) {
  
  y.var <- as.name(y.var)
  x.var <- as.name(x.var)
  col.grid.var <- if (gtools::invalid(col.grid.var)) {NULL} else {as.name(col.grid.var)}
  target <- if (gtools::invalid(target)) {NULL} else {target}
  y.aggregation <- tolower(y.aggregation)
  
  if(!y.aggregation %in% c("sum", "mean", "median")) {
    #dlg_message("Aggregation type must be sum, mean, or median", type = "ok")
    stop("Aggregation type must be 'sum', 'mean', or 'median'")
  }
  
  x.axis.frequency <- tolower(x.axis.frequency)
  x.axis.frequency <- ifelse(x.axis.frequency == "annually", "yearly", x.axis.frequency)
  if(!x.axis.frequency %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
    #dlg_message("Aggregation type must be daily, weekly, monthly, quarterly or yearly", type = "ok")
    stop("Aggregation type must be 'daily', 'weekly', 'monthly', 'quarterly' or 'yearly'")
  }
  
  improvement.direction <- tolower(improvement.direction)
  improvement.direction <- case_when(
    improvement.direction %in% c("decrease", "decreasing") ~ "decrease",
    improvement.direction %in% c("increase", "increasing") ~ "increase",
    TRUE ~ improvement.direction
  )
  if(!improvement.direction %in% c("decrease", "increase", "neutral")) {
    #dlg_message("Improvement direction must be decrease, increase, or neutral", type = "ok")
    stop("Improvement direction must be 'decrease', 'increase', or 'neutral'")
  }
  
  # base plot
  df.plt <- df %>% 
    group_by(!!x.var, !!col.grid.var) %>% 
    summarise(N = match.fun(y.aggregation)(!!y.var, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(Facet_Dummy = "")
  
  
  # format the date
  date_scale <- case_when(
    x.axis.frequency == "daily" ~ "%d-%b-%Y",
    x.axis.frequency == "weekly" ~ "%d-%b-%Y",
    x.axis.frequency == "monthly" ~ "%b-%Y",
    x.axis.frequency == "quarterly" ~ "%b-%Y",
    x.axis.frequency == "yearly" ~ "%b-%Y"
  )
  
  x.axis.frequency.spc <- case_when(
    x.axis.frequency == "daily" ~ "1 day",
    x.axis.frequency == "weekly" ~ "1 week",
    x.axis.frequency == "monthly" ~ "1 month",
    x.axis.frequency == "quarterly" ~ "3 months",
    x.axis.frequency == "yearly" ~ "1 year"
  )
  
  facet.col <- if (is.null(col.grid.var)) {as.name("Facet_Dummy")} else {col.grid.var}
  
  plt <- df.plt %>% 
    NHSRplotthedots::ptd_spc(
      value_field = N,
      date_field = .data[[x.var]],
      facet_field = .data[[facet.col]],
      improvement_direction = improvement.direction,
      target = target
    ) %>% 
    NHSRplotthedots::ptd_create_ggplot(
      main_title = plt.title,
      x_axis_label = x.title,
      y_axis_label = y.title,
      point_size = point.size,
      x_axis_breaks = x.axis.frequency.spc,
      x_axis_date_format = date_scale
    )
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)  
}

#' @title Theograph (aka Priestley duration) plots
#' 
#' Generate theographs
#' @param df Dataframe being plotted
#' @param plt.title,plt.subtitle,plt.caption,y.title,x.title. Text for plot title, subtitle, caption and axis titles.
#' @param y.group Group observations, which will be used to produce row-wise grids
#' @param x.start,x.end. The start and end date of the event. Use the same date if occurred within a day.
#' @param x.axis.frequency Frequency within the x-axis, to prevent label cluttering. Choose "daily", "weekly", "monthly", "quarterly", "yearly".
#' @param color.var Categorical variable to apply colour to events.
#' @param pal.choice Color palette to use. Default is R's default.
#' @param label.event Label for each event. If set to NULL, will automatically select x.start
#' @param label.show Print label.event on the plot. Default = FALSE.
#' @param line.width Width of the line. Default = 10.
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
plt_duration <- function(df, plt.title = NULL, plt.subtitle = NULL, plt.caption = NULL,
                        y.group, y.title = NULL,
                        x.start, x.end, x.title = NULL, x.axis.frequency = "daily",
                        color.var = NULL, pal.choice = NULL,
                        label.event, label.show = FALSE, line.width = 10,
                        save.chart = FALSE, save.filepath, save.filename, ...
) {
  
  # set x axis frequency
  x.axis.frequency <- tolower(x.axis.frequency)
  x.axis.frequency <- ifelse(x.axis.frequency == "annually", "yearly", x.axis.frequency)
  if(!x.axis.frequency %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
    #dlg_message("Aggregation type must be daily, weekly, monthly, quarterly or yearly", type = "ok")
    stop("Aggregation type must be 'daily', 'weekly', 'monthly', 'quarterly' or 'yearly'")
  }
  
  date_scale <- if(x.axis.frequency == "daily") {
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "weekly") {
    scale_x_datetime(date_breaks = "1 week", date_labels = "%d-%b-%Y")
  } else if (x.axis.frequency == "monthly") {
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "quarterly") {
    scale_x_datetime(date_breaks = "3 months", date_labels = "%b-%Y")
  } else if (x.axis.frequency == "yearly") {
    scale_x_datetime(date_breaks = "1 year", date_labels = "%b-%Y")
  }
  
  # dummy for group = null
  y.group.clean <- ifelse(gtools::invalid(y.group), "dummy_null", y.group)
  
  # dummy label
  label.clean <- ifelse(gtools::invalid(label.event), x.start, label.event)
  
  
  # set colours
  pal.clean <- if (any(gtools::invalid(pal.choice))) {
    scales::hue_pal()(7)
  } else {
    pal.choice
  }
  
  if(!gtools::invalid(color.var)) {
    
    color.var <- as.name(color.var)
    color.join <- df %>% 
      distinct(!!color.var)
    color.vec <- pal.clean[1:nrow(color.join)]
    color.join <- color.join %>% cbind(color.vec)
    
    df.plt <- df %>% 
      left_join(color.join, by = as.character(color.var))
  
  } else {
    
    color.vec <- pal.clean[1]
    df.plt <- df %>% 
      mutate(color.vec = color.vec)
    
  }
  
  # create the plot
  plot_theme <- plot_theme_fn(plt.title = plt.title, 
                              plt.subtitle = plt.subtitle, 
                              x.title = x.title, 
                              y.title = y.title, 
                              plt.caption = plt.caption,
                              legend.position = "bottom")
  
  plt_base <- vistime::gg_vistime(data = df.plt,
                                 col.event = label.clean,
                                 show_labels = label.show,
                                 col.start = x.start,
                                 col.end = x.end,
                                 col.group = y.group.clean,
                                 col.color = "color.vec",
                                 linewidth = line.width
  )
  
  plt <- plt_base +
    
    date_scale + 
    plot_theme
  
  if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                         plot = plt)
  
  return(plt)
}


# examples ----------------------------------------------------------------

stores_fruit <- readr::read_csv("stores_fruit.csv") %>% 
  mutate(dow = factor(dow, levels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")))

## standard line charts ----
plt.line.basic <- plt_line(df = stores_fruit %>% mutate(wkend = ifelse(dow %in% c("SAT", "SUN"), 1, 0)),
                           plt.title = "Daily revenue by product and region",
                           plt.subtitle = NA,
                           plt.caption = NA,
                           x.var = "date",
                           x.title = "Date",
                           x.axis.frequency = "weekly",
                           y.var = "value",
                           y.low = NA, y.high = NA,
                           y.title = "Daily sales",
                           y.aggregation = "Sum",
                           line.width = 0.5,
                           lines.var = "region",
                           y.ref.value = NA, 
                           roll.window = 7,
                           roll.line.width = 1, 
                           roll.line.type = "dashed",
                           fill.var = "wkend",
                           row.grid.var = "product",
                           row.grid.axis = "free",
                           legend.position = "bottom", 
                           save.chart = FALSE, save.filepath = "output/", save.filename = "line_chart"
)


## box plots over time to see distribution ----
plt.box.ts <- plt_box_ts(df = stores_fruit,
                         plt.title = "Daily sales distribution by product and month",
                         plt.subtitle = NA,
                         plt.caption = "Line = median, 'X' = mean, Box = inter-quartile range, Whisker = 1.5 x IQR, Dot = outlier.",
                         date.aggregation = "month",
                         date.title = "Month ending",
                         y.var = "quantity",
                         y.low = NA, y.high = NA,
                         y.title = "Daily sales",
                         y.aggregation = "none",
                         y.aggregation.level = "date",
                         outliers.show = TRUE,
                         row.grid.var = "product",
                         row.grid.axis = "free",
                         legend.position = "right",
                         pal.choice = c("blue", "gold", "red"),
                         save.chart = FALSE, save.filepath = "output/", save.filename = "ts_box_chart"
)


## seasonal decomposition ----
plt.seasonal.decomp <- plt_seasonal(df = stores_fruit %>% filter(region == "B"),
                                    plt.title = "Seasonal decomposition of daily revenue for Region B",
                                    plt.caption = "Seasonal frequency set to 7 days",
                                    x.var = "date",
                                    x.axis.frequency = "weekly",
                                    x.title = "Date",
                                    y.var = "value",
                                    y.title = "Daily sales",
                                    y.aggregation = "sum",
                                    row.grid.axis = "free",
                                    decomp.freq = 7,
                                    save.chart = FALSE, save.filepath = "output/", save.filename = "seasonal_chart"
)


## structural break test ----
library(strucchange)
plt_line_sb <- plt_bai_perron(df = stores_fruit %>% filter(store == "A011"),
                              plt.title = "Structural breaks in sales for store A011",
                              plt.caption = "Using Bai-Perron breakpoints, regressing on index only",
                              x.var = "date",
                              x.title = "Date",
                              x.axis.frequency = "weekly",
                              y.var = "value",
                              y.title = "Daily sales",
                              y.aggregation = "Sum", 
                              line.width = 1,
                              save.chart = FALSE, save.filepath = "output/", save.filename = "line_sb"
)

## SPC chart ----
library(NHSRplotthedots)
plt_line_spc <- plt_spc(df = stores_fruit %>% filter(store == "A011"),
                        plt.title = "Daily sales for store A011 SPC chart",
                        x.var = "date",
                        x.title = "Date",
                        x.axis.frequency = "monthly",
                        y.var = "value",
                        y.title = "Daily sales",
                        y.aggregation = "Sum", 
                        col.grid.var = "product",
                        improvement.direction = "neutral",
                        target = NULL,
                        point.size = 1,
                        save.chart = FALSE, save.filepath = "output/", save.filename = "spc"
)

## Priestley duration plot ----
library(vistime)

events_example <- data.frame(
  event_id   = c("AE001", "NEL001", "GP001", "OP001", "OP002", "EL001", "OP003", "GP002"),
  event_type = c("ae", "nel_ip", "gp", "op", "op", "el_ip", "op", "gp"),
  location   = c("A", "A", "G", "B", "B", "B", "B", "G"),
  start      = as.Date(c("2021-04-01", "2021-04-01", "2021-05-08", "2021-05-17",
                         "2021-07-31", "2021-09-12", "2021-09-22", "2021-11-05")),
  end        = as.Date(c("2021-04-01", "2021-04-14", "2021-05-08", "2021-05-17",
                         "2021-07-31", "2021-09-14", "2021-09-22", "2021-11-05")),
  stringsAsFactors = FALSE
)

plt.duration <- plt_duration(df = events_example,
                                plt.title = "Patient events in 2021",
                                x.start = "start",
                                x.end = "end",
                                x.title = "Activity dates",
                                x.axis.frequency = "monthly",
                                y.group = "event_type",
                                y.title = "Point of delivery",
                                color.var = "location",
                                label.event = "event_id",
                                label.show = TRUE,
                                line.width = 5,
                                save.chart = FALSE, save.filepath = "output/", save.filename = "duration"
)
