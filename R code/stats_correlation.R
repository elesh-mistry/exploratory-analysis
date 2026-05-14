
# functions ---------------------------------------------------------------

#' @title Pairwise correlation coefficient 
#' 
#' Generate a table of pearson or spearman correlation coefficients. Also an option for a plot of the coefficients.
#' For a visualisation with both scatterplots and coefficients, use the function plt_scatter_pairs
# Significance: * < 0.05, ** < 0.01, *** < 0.005
#' 
#' @param df Dataframe being plotted
#' @param vars Variables to be plotted. Default uses all fields in df
#' @param corr.type Choose which type of correlation coefficients to present, from "pearson" (default) or "spearman". Distance correlation unavailable - use function stat_cor_distance.
#' @param create.plot Choose whether to print a plot of coefficients
#' @param plt.title Text for plot title
#' @param colour.negative,colour.middle,colour.positive Background colours for the correlation coefficient grid, running from -1 to 0 to +1. Default is red-white-blue.
#' @param text.colour,text.size Text colour and size for correlation coefficients
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
stat_cor <- function(df, vars = colnames(df), corr.type = "pearson", 
                     create.plot = FALSE, plt.title = paste0("Pairwise ", corr.type, " correlation plot"), 
                     colour.negative  = "#F8766D", colour.middle = "white", colour.positive = "#00B0F6",
                     text.colour = "black", text.size = 4,
                     save.chart = FALSE, save.filepath, save.filename) {
  
  df.plt <- df %>% 
    select(all_of(vars))
  
  check.class <- sapply(df.plt, function(x) is.numeric(x) || is.integer(x))
  if(any(!check.class)) {
    stop("Fields selected in 'vars' should be numeric. Remove non-numeric fields, or use function plt_pair_combo for mixed numeric/non-numeric analysis.")
  }
  
  # Compute correlation matrix and p-values
  corr.type.lower <- tolower(corr.type)
  
  if(!corr.type.lower %in% c("pearson", "spearman")) {
    stop("corr.type must be either 'pearson' or 'spearman'")
  }
  
  corr_res <- Hmisc::rcorr(as.matrix(df.plt), 
                           type = corr.type.lower)

  # create table of results
  res <- corr_res$r %>%
    as.data.frame() %>%
    rownames_to_column("field1") %>%
    pivot_longer(-field1, names_to = "field2", values_to = "coefficient") %>%
    left_join(
      corr_res$P %>%
        as.data.frame() %>%
        rownames_to_column("field1") %>%
        pivot_longer(-field1, names_to = "field2", values_to = "p_value"),
      by = c("field1", "field2")
    )
  
  res_return <- res %>%
    filter(field1 < field2)
  
  # produce plot
  if (create.plot) {
    
    res_plt <- res %>% 
      mutate(
        stars = case_when(
          p_value < 0.005 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        plt_text = ifelse(is.na(coefficient),NA, paste0(round(coefficient, 3), stars)),
        # to ensure correct sorting in the chart
        field1 = factor(field1, levels = vars),
        field2 = factor(field2, levels = vars),
        row_id = as.numeric(field1),
        col_id = as.numeric(field2),
        coefficient = ifelse(row_id < col_id, coefficient, NA_real_),
        plt_text = ifelse(row_id < col_id, plt_text, NA_real_)
      )
    
    plt <- ggplot(res_plt, aes(x = field1, y = forcats::fct_rev(field2), fill = coefficient)) +
      geom_tile(color = "white") +
      geom_text(aes(label = plt_text), color = text.colour, size = text.size, na.rm = TRUE) +
      scale_fill_gradient2(low = colour.negative,
                           mid = colour.middle,
                           high = colour.positive,
                           limits = c(-1,1),
                           na.value = "white") +
      labs(
        title = paste0("Pairwise ", corr.type, " correlation plot"),
        x = NULL,
        y = NULL,
        caption = "Significance: * < 0.05, ** < 0.01, *** < 0.005"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom",
      )
    
    print(plt)
    
    if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                           plot = plt)
  }
  
  return(res_return)
}


#' @title Pairwise distance correlation coefficients
#' 
#' Generate a table of distance correlation coefficients. Also an option for a plot of the coefficients.
# Significance: * < 0.05, ** < 0.01, *** < 0.005
#' 
#' @param df Dataframe being plotted
#' @param vars Variables to be plotted. Default uses all fields in df
#' @param n.iter number of iterations for producing bootstrap p-values. Default = 10,000, but reduce if working with very large dataframes and limited gains from running parallel
#' @param run.parallel run bootstrapping in parallel. If TRUE, function will use plan() to commit all bar one processor to run the calculation, then revert to the plan() before the function was run
#' @param create.plot Choose whether to print a plot of coefficients
#' @param plt.title Text for plot title
#' @param colour.low,colour.high Background colours for the correlation coefficient grid, running from 0 to +1. Default is white-blue.
#' @param text.colour,text.size Text colour and size for correlation coefficients
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
stat_cor_distance <- function(df, vars = colnames(df), n.iter = 10000, run.parallel = FALSE, 
                              create.plot = FALSE, plt.title = "Pairwise distance correlation plot", 
                              colour.low = "white", colour.high = "#00B0F6", 
                              text.colour = "black", text.size = 4,
                              save.chart = FALSE, save.filepath, save.filename) {

  df.plt <- df %>% 
    select(all_of(vars))
  
  check.class <- sapply(df.plt, function(x) is.numeric(x) || is.integer(x))
  if(any(!check.class)) {
    stop("Fields selected in 'vars' should be numeric. Remove non-numeric fields, or use function plt_pair_combo for mixed numeric/non-numeric analysis.")
  }
  
  # pairs to test
  pairs <- combn(vars, 2, simplify = FALSE)
  
  # produce coefficients and p-values
  if (run.parallel) {
  
    res_distance_correlation <- data.frame(
      field1 = character(),
      field2 = character(),
      coefficient = double(),
      p_value = double(),
      stringsAsFactors = FALSE
    )
    
    plan_original <- plan()
    plan(multisession, workers = parallel::detectCores() - 1)
    
    set.seed(147)
    res_distance_correlation <- furrr::future_map_dfr(pairs, function(pair) {
      
      df.test <- df.plt %>% 
        select(all_of(pair)) %>% 
        filter(complete.cases(.))
      
      dcorr <- energy::dcor.test(df.test[[1]], df.test[[2]], R = n.iter)
      
      tibble(
        var1 = pair[1],
        var2 = pair[2],
        statistic = as.numeric(dcorr$statistic),
        p_value = dcorr$p.value
      )
    },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
    )
    
    plan(plan_original)
    
  } else {
    
    set.seed(147)
    res_distance_correlation <- purrr::map(pairs, function(pair) {
      
      df.test <- df.plt %>% 
        select(all_of(pair)) %>% 
        filter(complete.cases(.))
      
      dcorr <- energy::dcor.test(df.test[[1]], df.test[[2]], R = n.iter)
      
      tibble(
        var1 = pair[1],
        var2 = pair[2],
        statistic = as.numeric(dcorr$statistic),
        p_value = dcorr$p.value
      )
    }
    ) %>% 
      bind_rows %>% 
      rename_with(~c("field1", "field2", "coefficient", "p_value"))
    
  }
  
  # produce plot
  if (create.plot) {
    res_distance_correlation_plt <- expand.grid(
      field1 = vars,
      field2 = vars,
      stringsAsFactors = FALSE
    ) %>% 
      left_join(res_distance_correlation, by = c("field1", "field2")) %>% 
      mutate(
        stars = case_when(
          p_value < 0.005 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        plt_text = ifelse(is.na(coefficient),NA, paste0(round(coefficient, 3), stars)),
        # to ensure correct sorting in the chart
        field1 = factor(field1, levels = vars),
        field2 = factor(field2, levels = vars),
        row_id = as.numeric(field1),
        col_id = as.numeric(field2),
        coefficient = ifelse(row_id < col_id, coefficient, NA_real_),
        plt_text = ifelse(row_id < col_id, plt_text, NA_real_)
      )
    
    plt <- ggplot(res_distance_correlation_plt, aes(x = field1, 
                                                    y = forcats::fct_rev(field2), 
                                                    fill = coefficient)) +
      geom_tile(color = "white") +
      geom_text(aes(label = plt_text), color = text.colour, size = text.size, na.rm = TRUE) +
      scale_fill_gradient(low = colour.low,
                         high = colour.high,
                         limits = c(0,1),
                         na.value = "white") +
      labs(
        title = plt.title,
        x = NULL,
        y = NULL,
        caption = "Significance: * < 0.05, ** < 0.01, *** < 0.005"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom",
      )
    
    print(plt)
    
    if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                          plot = plt)
  }
  
  # return results
  return(res_distance_correlation)
}


#' @title Contingency/Phi coefficients
#' 
#' Generate a table of phi correlation coefficients for binary variables. Also an option for a plot of the coefficients.
#' Significance marked for chosen alpha only.
#' 
#' @param df Dataframe being plotted
#' @param vars Variables to be plotted. Default uses all fields in df
#' @param alpha Set alpha for confidence intervals. Default = 0.05.
#' @param create.plot Choose whether to print a plot of coefficients
#' @param plt.title Text for plot title
#' @param colour.negative,colour.middle,colour.positive Background colours for the correlation coefficient grid, running from -1 to 0 to +1. Default is red-white-blue.
#' @param text.colour,text.size Text colour and size for correlation coefficients
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
stat_cor_phi <- function(df, vars = NULL, alpha = 0.05,
                         create.plot = FALSE,
                         plt.title = "Pairwise Phi correlation plot",
                         colour.negative = "#F8766D", colour.middle = "white", colour.positive = "#00B0F6",
                         text.colour = "black", text.size = 4,
                         save.chart = FALSE, save.filepath, save.filename) {
  
  # validations
  if (is.null(vars)) vars <- colnames(df)
  df.plt <- df %>% select(all_of(vars))
  
  check.class <- sapply(df.plt, function(x) is.factor(x) || is.character(x))
  if (any(!check.class)) {
    stop("All fields selected in 'vars' must be factors or character vectors. ",
         "Remove non-factor fields, or use stat_cor() for numeric analysis.")
  }
  
  df.plt <- df.plt %>% mutate(across(where(is.character), as.factor))
  
  check.binary <- sapply(df.plt, function(x) nlevels(x) == 2)
  if (any(!check.binary)) {
    stop("Phi coefficients require binary variables (exactly 2 levels).")
  }
  
  # calculate coefficients
  pairs <- combn(vars, 2, simplify = FALSE)
  
  res_list <- lapply(pairs, function(pair) {
    f1 <- pair[1]
    f2 <- pair[2]
    
    if (!identical(levels(df.plt[[f1]]), levels(df.plt[[f2]]))) {
      message("Factor levels differ between '", f1, "' and '", f2, "'. ",
              "The lead diagonal of the contingency table may not represent agreement.")
    }
    
    tab <- table(df.plt[[f1]], df.plt[[f2]])
    vec <- statpsych::ci.phi(alpha = alpha,
                             f00 = tab[1, 1], f01 = tab[1, 2],
                             f10 = tab[2, 1], f11 = tab[2, 2])
    
    tibble(
      field1      = f1,
      field2      = f2,
      coefficient = vec[1],   # phi
      lower_cl    = vec[3],   # lower CI
      upper_cl    = vec[4],   # upper CI
      is_sig       = !(lower_cl <= 0 & upper_cl >= 0)
    )
  })
  
  res_return <- bind_rows(res_list)
  
  # plot
  if (create.plot) {
    
    # Build a full grid (both triangles) so the tile matrix renders correctly,
    # mirroring the approach in stat_cor()
    res_full <- bind_rows(
      res_return,
      res_return %>% rename(field1 = field2, field2 = field1)  # mirror
    ) %>%
      mutate(
        sig_label = ifelse(is_sig, "*", ""),
        plt_text  = ifelse(is.na(coefficient), NA_character_,
                           paste0(round(coefficient, 3), sig_label)),
        field1    = factor(field1, levels = vars),
        field2    = factor(field2, levels = vars),
        row_id    = as.numeric(field1),
        col_id    = as.numeric(field2),
        # Keep only the upper triangle for fill & text, leave lower blank
        coefficient = ifelse(row_id < col_id, coefficient, NA_real_),
        plt_text    = ifelse(row_id < col_id, plt_text,    NA_character_)
      )
    
    plt <- ggplot(res_full,
                  aes(x = field1, y = forcats::fct_rev(field2), fill = coefficient)) +
      geom_tile(color = "white") +
      geom_text(aes(label = plt_text), color = text.colour, size = text.size,
                na.rm = TRUE) +
      scale_fill_gradient2(low    = colour.negative,
                           mid    = colour.middle,
                           high   = colour.positive,
                           limits = c(-1, 1),
                           na.value = "white") +
      labs(
        title   = plt.title,
        x       = NULL,
        y       = NULL,
        caption = paste0("* = significant at alpha = ", alpha)
      ) +
      theme_minimal() +
      theme(
        plot.title  = element_text(color = "black", face = "bold",
                                   hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom"
      )
    
    print(plt)
    
    if (save.chart) {
      ggsave(filename = paste0(save.filepath, save.filename, ".png"), plot = plt)
    }
  }
  
  return(res_return)
}


# examples ----------------------------------------------------------------

## dataframes ----
set.seed(147)
vec1 <- rnorm(500, mean = 0, sd = 3)
vec2 <- vec1^2
vec3 <- exp(vec1)
vec4 <- rnorm(500, mean = 1, sd = 3)

df_corr <- data.frame(
  set1 = vec1,
  set1_sq = vec2,
  set1_exp = vec3,
  set2 = vec4
)

vec5 <- sample(1:2, 500, replace = TRUE)
set.seed(148)
vec6 <- sample(1:2, 500, replace = TRUE)
set.seed(149)
vec7 <- sample(1:2, 500, replace = TRUE)

df_binary <- data.frame(
  set1 = vec5,
  set2 = vec6,
  set3 = vec7
) %>% 
  mutate(across(everything(), ~ factor(., levels = c(1, 2))))


## Pearson or Spearman correlation ----
library(Hmisc)
stat.corr <- stat_cor(df = df_corr,
                      vars = colnames(df_corr),
                      corr.type = "spearman",
                      create.plot = TRUE,
                      save.chart = FALSE, save.filepath = "output/", save.filename = "stat_corr_chart"
)

## Distance correlation ----
library(energy)
library(purrr)
library(furrr)
stat.corr.distance <- stat_cor_distance(df = df_corr,
                                        vars = colnames(df_corr),
                                        n.iter = 1000,
                                        run.parallel = FALSE,
                                        create.plot = TRUE,
                                        plt.title = "Distance correlation plot",
                                        save.chart = FALSE, save.filepath = "output/", save.filename = "stat_corrdist_chart"
)


## Phi coefficient ----
library(statpsych)
stat.corr.phi <- stat_cor_phi(df = df_binary,
                              alpha = 0.05,
                              create.plot = TRUE,
                              save.chart = FALSE, save.filepath = "output/", save.filename = "stat_corr_phi")
