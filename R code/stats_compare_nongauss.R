
# functions ---------------------------------------------------------------

#' @title Mann-Whitney-Wilcoxon tests
#' 
#' Generate a vector with Mann-Whitney-Wilcoxon test results
#' 
#' @param vec1 Vector of values to test
#' @param vec2 A second vector to test if comparing two groups. If NULL, do a one-sample test on vec1
#' @param mu Population mean for one-sample test, or population difference between two-sample means
#' @param paired TRUE if two-sample paired data
#' @param var.equal TRUE if two-sample with unequal variances (Brunner-Munzel test)
#' @param alpha Set alpha for confidence intervals. Default = 0.05.
stat_mww <- function(vec1, vec2 = NULL, mu = 0, 
                     paired = FALSE, var.equal = TRUE, 
                     alpha = 0.05) {
  
  if (gtools::invalid(vec2)) vec2 <- NULL
  
  if (var.equal | is.null(vec2)) {
    res_raw <- wilcox.test(x = vec1, 
                           y = vec2,
                           mu = mu,
                           paired = paired,
                           conf.level = 1 - alpha)
    
    res <- tibble(method = res_raw$method,
                  data = res_raw$data.name,
                  mu = mu,
                  paired = paired,
                  var_equal = var.equal,
                  p_value = res_raw$p.value,
                  lowercl = res_raw$conf.int[1],
                  uppercl = res_raw$conf.int[2],
    )
  } else {
    res_raw <- lawstat::brunner.munzel.test(x = vec1,
                                            y = vec2,
                                            alpha = alpha)
    
    res <- tibble(method = "Brunner-Munzel",
                  data = res_raw$data.name,
                  mu = mu,
                  paired = paired,
                  var_equal = var.equal,
                  p_value = res_raw$p.value,
                  lowercl = res_raw$conf.int[1],
                  uppercl = res_raw$conf.int[2],
    )
  }
  
  return(res)
}


#' @title Kolmogorov-Smirnov test
#' 
#' Generate a vector with Kolmogorov-Smirnov test results. Option to plot distributions.
#' 
#' @param vec1 Vector of values to test
#' @param vec2 A second vector to test if comparing two groups. If NULL, do a one-sample test on vec1
#' @param create.plot Choose whether to print a plot of coefficients
#' @param save.chart,save.filepath,save.filename. Choose TRUE/FALSE to save chart. Enter filepath on where to save, and the filename.
stat_ks <- function(vec1, vec2, create.plot = FALSE,
                    save.chart = FALSE, save.filepath, save.filename) {
  
  res_raw <- ks.test(vec1, vec2)
  res_p <- round(res_raw$p.value, 3)
  res_D <- round(res_raw$statistic, 3)
  
  vec1_name <- deparse(substitute(vec1))
  vec2_name <- deparse(substitute(vec2))
  
  res <- tibble(data = paste0(vec1_name, " and ", vec2_name),
                D = res_D,
                p_value = res_p)
  
  if(create.plot) {
    
    df_long <- bind_rows(
      tibble(value = vec1, group = vec1_name),
      tibble(value = vec2, group = vec2_name),
    )
    
    # compute max gap
    x_all <- sort(unique(c(vec1, vec2)))
    
    ecdf1 <- ecdf(vec1)
    ecdf2 <- ecdf(vec2)
    
    diffs <- abs(ecdf1(x_all) - ecdf2(x_all))
    max_index <- which.max(diffs)
    
    x_max  <- x_all[max_index]
    y1_max <- ecdf1(x_max)
    y2_max <- ecdf2(x_max)
    
    
    # Plot
    
    plt <- ggplot(df_long, aes(x = value, colour = group)) +
      stat_ecdf(linewidth = 1) +
      annotate("segment",
               x = x_max, xend = x_max,
               y = y1_max, yend = y2_max,
               linetype = "solid",
               linewidth = 1.5,
               colour = "black") +
      
      labs(
        title = "Empirical Cumulative Distribution Functions",
        subtitle = paste0(
          "KS test: D = ", round(res_D, 3), " (black line)",
          " | p-value = ", res_p
        ),
        x = "Value",
        y = "Cumulative Probability",
        colour = "Group"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(color = "black", hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom",
      )
  
    print(plt)
  
    if (save.chart) ggsave(filename = paste0(save.filepath, save.filename, ".png"), 
                           plot = plt)
      
    }
  
  return(res)
  
}


#' @title Kruskal-Wallis test
#' 
#' Generate a vector with Kruskal-Wallis test results
#' Data needs to be organised in long format
#' 
#' @param df dataframe contain
#' @param value.var field containing the values
#' @param group.var field containing the group labels
stat_kw <- function(df, value.var, group.var) {
  df_name <- rlang::as_name(rlang::ensym(df))
  
  res_raw <- kruskal.test(reformulate(group.var, value.var), data = df)
  
  res <- tibble(data = df_name,
                p_value = res_raw$p.value)
  
  return(res)
}


#' @title Friedman test
#' 
#' Generate a vector with Friedman rank sum test results
#' Data needs to be stored as a matrix in wide format with just the numeric values
#' 
#' @param mat matrix with the values in wide format
stat_friedman <- function(mat, ...) {
  
  check.class <- sapply(mat, function(x) is.numeric(x) || is.integer(x))
  if(!"matrix" %in% class(mat) | any(!check.class)) {
    stop("'mat' should be of class 'matrix' containing only the numeric fields to be tested.")
  }
  
  res_raw <- friedman.test(mat)
  
  res <- tibble(method = res_raw$method,
                data = res_raw$data.name,
                p_value = res_raw$p.value
  )
  
  return(res)
  
}


# examples ----------------------------------------------------------------

## data ----
set.seed(147)
sample1 <- sample.int(10, 150, replace = TRUE)
sample2 <- sample.int(10, 100, replace = TRUE)
sample3 <- sample1 + sample(c(-1, 0, 1), 150, replace = TRUE)

sample2_length <- sample2
length(sample2_length) <- 150
sample_matrix <- cbind(sample1, sample2_length, sample3)
df_long <- bind_rows(
  tibble(value = sample1, group = "vec1"),
  tibble(value = sample2, group = "vec2"),
  tibble(value = sample3, group = "vec3")
)


## Mann-Whitney-Wilcoxon ----
library(lawstat)
stat.mww <- stat_mww(vec1 = sample1,
                     vec2 = sample2,
                     mu = 0,
                     paired = TRUE,
                     var.equal = FALSE,
                     alpha = 0.05)

## Kruskal-Wallis ----
stat.kw <- stat_kw(df = df_long,
                   value.var = "value",
                   group.var = "group")

## Kolmogorov-Smirnov ----
stat.ks <- stat_ks(vec1 = sample1,
                   vec2 = sample2,
                   create.plot = TRUE,
                   save.chart = TRUE,
                   save.filepath = "output/",
                   save.filename = "stat_ks"
)

## Friedman ----
stat.friedman <- stat_friedman(mat = sample_matrix)
