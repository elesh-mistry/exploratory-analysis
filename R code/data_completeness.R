
# functions ---------------------------------------------------------------

#' @title Count of NAs
#' 
#' Sums the number of NA's for each field in a dataframe and return the number and percentage of NAs.
#' @param df dataframe to review
#' @param sum.col which column to sum. Create a dummy count column if needed
na_counts_total <- function(df, sum.col) {
  
  sum.col <- as.name(sum.col)
  
  grand.total <- sum(df[[sum.col]], na.rm = TRUE)
  vec_cols <- colnames(df)
  test.na.count.total <- c()
  
  for (col in vec_cols) {
    
    col <- as.name(col)
    
    df.na <- df %>% 
      filter(is.na(!!col))
    na.count <- sum(df.na[[sum.col]], na.rm = TRUE)
    
    test.na.count <- data.frame(
      Column = as.character(col),
      Count = na.count,
      Total = grand.total
    ) %>% 
      mutate(PC = round(100*Count / Total, 1))
    
    test.na.count.total <- bind_rows(test.na.count.total, test.na.count)
  }
  
  return(test.na.count.total)
}

#' @title Count of NAs, split by another field
#' 
#' Sums the number of NA's for each field in a dataframe, split by another categorical field, 
#' and return the number and percentage of NAs.
#' @param df dataframe to review
#' @param sum.col which column to sum. Create a dummy count column if needed
#' @param field which field to split by
na_counts_split <- function(df, sum.col, field) {
  
  sum.col <- as.name(sum.col)
  field <- as.name(field)
  
  vec_field <- sort(unique(df[[field]]))
  vec_cols <- colnames(df)
  
  test.na.count.split <- c()
  
  for (i in vec_field) {
    
    df.test <- df %>% filter(!!field == i)
    grand.total <- sum(df.test[[sum.col]], na.rm = TRUE) 
    
    for (col in vec_cols) {
      
      col <- as.name(col)
      
      df.na <- df.test %>% 
        filter(is.na(!!col))
      na.count <- sum(df.na[[sum.col]], na.rm = TRUE)
      
      test.na.count <- data.frame(
        Field_Value = as.character(i),
        Column = as.character(col),
        Count = na.count,
        Total = grand.total
      ) %>% 
        mutate(PC = round(100*Count / Total, 1))
      
      test.na.count.split <- bind_rows(test.na.count.split, test.na.count)
    }
  }
  
  return(test.na.count.split)
}

#' @title Distribution of NAs
#' 
#' Three graphical representations of NA's in a dataframe, similar to Python's missingno. 
#' "Matrix" for a tile chart for each observation by field. "Summary" for percentage missing by field. 
#' "Correlation" to see which fields have pairwise commonly missing fields.
#' Function only allows a dataframe with a maximum 1,000,000 datapoints. 
#' This is because the "Matrix" option in particular can take a long time to render otherwise.
#' @param df dataframe to review
#' @param type Choice one of the three types noted above
#' @param title title for the graph
#' @param cluster Change the ordering of the columns in the graph based on Jaccard's distances and hierarchical clustering (0 = present, 1 = NA). If FALSE, retain original order.
na_graph <- function(df, 
                     type = c("matrix", "summary", "correlation"),
                     title = NULL,
                     cluster = TRUE) {
  
  # initial checks
  type <- match.arg(type)
  
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame")
  }
  
  if (nrow(df) * ncol(df) > 1e6) {
    stop(paste0("Dataframe has ", nrow(df)*ncol(df), 
                " datapoints. Maximum allowed is 1000000. Consider sampling your data."))
  }
  
  # binary matrix
  df.mat <- df
  df.mat[] <- lapply(df.mat, function(x) as.integer(!is.na(x)))
  mat <- as.matrix(df.mat)
  
  # cluster similar columns
  col_order <- seq_len(ncol(mat))
  
  if (cluster && ncol(mat) > 1) {
    dist_cols <- dist(t(mat), method = "binary")
    hc <- hclust(dist_cols)
    col_order <- hc$order
  }
  
  mat_ord <- mat[, col_order, drop = FALSE]
  colnames_ord <- colnames(mat)[col_order]
  
  
  # metrics for all visualisation options
  graphics::layout(1)
  row_comp <- rowMeans(mat_ord)
  col_na_pct <- colMeans(mat_ord == 0)
  
  # MATRIX VIEW
  if (type == "matrix") {
    
    if (is.null(title)) title <- "Missing Data Matrix"
    
    graphics::layout(matrix(c(1, 2), ncol = 2), widths = c(4, 1))
    
    # Main
    par(mar = c(6, 4, 4, 1))
    
    image(
      t(mat_ord[nrow(mat_ord):1, ]),
      col = c("white", "black"),
      axes = FALSE,
      main = title
    )
    
    axis(1,
         at = seq(0, 1, length.out = ncol(mat_ord)),
         labels = colnames_ord,
         las = 2,
         cex.axis = 0.7)
    
    # Sparkline
    par(mar = c(5, 1, 4, 3))
    
    plot(
      row_comp,
      seq_along(row_comp),
      type = "l",
      axes = FALSE,
      xlim = c(0, 1),
      xlab = "",
      ylab = ""
    )
    
    mtext("Row completeness", side = 3, line = 1)
  }
  
  # SUMMARY BAR CHART VIEW
  if (type == "summary") {
    
    if (is.null(title)) title <- "Missing Data Summary"
    
    par(mar = c(8, 4, 4, 2))
    
    barplot(
      col_na_pct,
      names.arg = colnames_ord,
      las = 2,
      cex.names = 0.7,
      ylab = "NA %",
      col = "grey",
      main = title
    )
  }
  
  # CORRELATION VIEW
  if (type == "correlation") {
    
    if (is.null(title)) title <- "Missingness Correlation"
    
    cor_mat <- cor(mat_ord == 0)
    
    par(mar = c(6, 6, 4, 2))
    
    # initial set of colours
    image(
      cor_mat,
      axes = FALSE,
      main = title
    )
    
    # greying out the leading diagonal
    n <- ncol(cor_mat)
    
    for (i in seq_len(n)) {
      x <- (i - 1) / (n - 1)
      y <- (i - 1) / (n - 1)
      
      rect(
        x - 0.5/(n - 1),
        y - 0.5/(n - 1),
        x + 0.5/(n - 1),
        y + 0.5/(n - 1),
        col = "grey",
        border = NA
      )
    }
    
    # the rest of the chart
    axis(1,
         at = seq(0, 1, length.out = ncol(cor_mat)),
         labels = colnames(mat_ord),
         las = 2,
         cex.axis = 0.7)
    
    axis(2,
         at = seq(0, 1, length.out = ncol(cor_mat)),
         labels = colnames(mat_ord),
         las = 2,
         cex.axis = 0.7)
  }
  
  invisible(NULL)
}

#' @title label frequency
#' 
#' Sums and returns the 'x' most frequent labels for each character and factor field in a dataframe. 
#' Returns frequency number, percentage, and cumulative percentage.
#' @param df dataframe to review
#' @param sum.col which column to sum. Create a dummy count column if needed
#' @param top.x maximum number of labels to return per field. Default = 10
label_freq <- function(df, sum.col, top.x = 10) {
  
  sum.col <- as.name(sum.col)
  df.test <- df %>% 
    select(where(~ is.character(.) || is.factor(.)), !!sum.col)
  
  vec_cols <- head(colnames(df.test), -1)
  
  test.freq.count <- c()
  for (col in vec_cols) {
    
    col <- as.name(col)
    df.count <- df.test %>% 
      group_by(!!col) %>% 
      summarise(N = sum(!!sum.col)) %>% 
      ungroup() %>% 
      arrange(-N) %>% 
      mutate(PC = round(100*N / sum(N), 1),
             Running_PC = cumsum(PC),
             Column = as.character(col),
             Rank = row_number()
      ) %>% 
      rename(Group = col) %>% 
      select(Column, Group, everything()) %>% 
      head(top.x)
    
    test.freq.count <- bind_rows(test.freq.count, df.count)
  }
  
  return(test.freq.count)
  
}

#' @title Basic stats, total
#' 
#' Returns numeric stats for all numeric fields. Stats returned: 
#' No. of Observations, NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, Median, Sum, 
#' Std Error of Mean, Lower CL Mean, Upper CL Mean, Variance, Std Dev, Skewness, and Kurtosis.
#' Can take longer for large dataframes, but no upper limit set. 
#' If you are unlikely to use all of the stats above, you can simply use summary(df) 
#' instead. This will return NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, and Median.
#' @param df dataframe to review
basic_stats_total <- function(df) {
  
  df.test <- df %>% 
    select(where(is.numeric))
  
  test.basic.stats <- basicStats(df.test) %>% 
    rownames_to_column("stats")
  
  return(test.basic.stats)
  
}


#' @title Basic stats, split by field
#' 
#' Returns numeric stats for all numeric fields, split by categorical field. Stats returned: 
#' No. of Observations, NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, Median, Sum, 
#' Std Error of Mean, Lower CL Mean, Upper CL Mean, Variance, Std Dev, Skewness, and Kurtosis.
#' Can take longer for large dataframes, but no upper limit set. 
#' If you are unlikely to use all of the stats above, you can simply use summary(df) 
#' instead. This will return NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, and Median.
#' @param df dataframe to review
basic_stats_split <- function(df, field) {
  
  field <- as.name(field)
  df <- df %>% 
    select(where(is.numeric), !!field) %>% 
    filter(!is.na(!!field))
  
  vec_field <- sort(unique(df[[field]]))
  
  test.basic.stats.split <- c()
  for (i in vec_field) {
    
    i <- as.name(i)
    
    df.test <- suppressWarnings(df %>% filter(!!field == i) %>% select(-c(!!field)))
    
    test.basic.stats <- basicStats(df.test) %>% 
      rownames_to_column("stats") %>% 
      mutate(Field_Value = as.character(i)) %>% 
      select(Field_Value, everything())
    
    test.basic.stats.split <- bind_rows(test.basic.stats.split, test.basic.stats)
  }
  
  return(test.basic.stats.split)
  
}  

#' @title Field alignment
#' 
#' Useful as a check before joining two dataframes. Compares the field names of two dataframes 
#' and returns fields in common, if common fields are the same class, and fields only in one dataframe.
#' @param df1 first dataframe
#' @param df2 second dataframe
fields_alignment <- function(df1, df2) {
  
  vec_col_df1 <- colnames(df1)
  vec_col_df2 <- colnames(df2)
  
  # Field Alignment
  cat("\nFields in common:\n")
  common_fields <- intersect(vec_col_df1, vec_col_df2)
  print(common_fields)
  
  cat("\nIn df1 but not df2:\n")
  print(setdiff(vec_col_df1, vec_col_df2))
  
  cat("\nIn df2 but not df1:\n")
  print(setdiff(vec_col_df2, vec_col_df1))
  
  # Class comparison
  if (length(common_fields) == 0) {
    cat("\nNo fields in common — skipping class comparison.\n")
    return(invisible(NULL))
  }
  
  class_df <- data.frame(
    field      = common_fields,
    class_df1  = sapply(common_fields, function(x) paste(class(df1[[x]]), collapse = ", ")),
    class_df2  = sapply(common_fields, function(x) paste(class(df2[[x]]), collapse = ", ")),
    match      = NA,   # populate later
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  
  class_df$match <- class_df$class_df1 == class_df$class_df2
  
  cat("\nClass mismatches:\n")
  mismatches <- class_df[!class_df$match, , drop = FALSE]
  
  if (nrow(mismatches) == 0) {
    cat("All classes match for fields in common")
  } else {
    print(mismatches)
  }
}

#' @title Label alignment
#' 
#' 	Useful as a check before joining two dataframes. For the same categorical field in 
#' two dataframes, compares unique values and returns those in common and those in only one dataframe.
#' @param df1 first dataframe
#' @param df2 second dataframe
#' @param field field to compare
label_alignment <- function(df1, df2, field) {
  vec.1 <- unique(df1[[field]]) %>% as.character()
  vec.2 <- unique(df2[[field]]) %>% as.character()
  
  ## Alignment
  cat("\nValues in common:\n")
  common_fields <- intersect(vec.1, vec.2)
  print(common_fields)
  
  cat("\nIn df1 but not df2:\n")
  print(setdiff(vec.1, vec.2))
  
  cat("\nIn df2 but not df1:\n")
  print(setdiff(vec.2, vec.1))
}

#' @title counting duplicates
#' 
#' Generates a dataframe containing all duplicates. Can specify which fields to check across, 
#' or for all fields use test.duplicated.rows = colnames(df) which is the default
#' @param df dataframe to review
#' @param duplicated_across_cols specify which fields to check across
duplicates_count <- function(df, duplicated_across_cols = colnames(df)) {
  test.duplicated.rows <- df %>%
    group_by(across(all_of(duplicated_across_cols))) %>%
    filter(n() > 1) %>%
    ungroup()
  
  num.rows <- nrow(test.duplicated.rows)
  total.rows <- nrow(df)
  pc.rows <- paste0(round(100*num.rows / total.rows, 1), "%")
  
  if(num.rows == 0) {
    msg <- "No duplicated rows"
    message(msg)
    
  } else {
    
    msg <- paste0("There are ", prettyNum(num.rows, big.mark = ","), " (", pc.rows, " of total)",
                  " duplicate rows. Review these under the dataframe 'test.duplicated.rows'")
    message(msg)
    
    test.duplicated.rows <<- test.duplicated.rows
  }
}

#' @title filter outliers
#' 
#' Returns a dataframe containing outliers for a numerical field in a dataframe. 
#' User can select either standard deviation (sd) or standard error (se) and the 
#' number of deviations from the mean.
#' @param df dataframe to review
#' @param outlier.col field in that dataframe to review
#' @param dev.type deviation type. Default = "sd"
#' @param num.dev number of deviations. Default = 2
outliers <- function(df, outlier.col, dev.type = "sd", num.dev = 2) {
  
  outlier.col <- as.name(outlier.col)
  vec_outlier <- df[[outlier.col]]
  
  
  if (dev.type == "sd") {
    
    outlier_mean <- mean(vec_outlier, na.rm = TRUE)
    outlier_stdev <- stdev(vec_outlier, na.rm = TRUE)
    outlier_upper <- outlier_mean + num.dev*outlier_stdev
    outlier_lower <- outlier_mean - num.dev*outlier_stdev
    
    
  } else if (dev.type == "se") {
    
    test.stat <- t.test(vec_outlier)
    
    outlier_upper <- test.stat[["conf.int"]][2]
    outlier_lower <- test.stat[["conf.int"]][1]
    
  } else {
    
    stop("dev.type should be either sd (standard deviation) or se (standard error)")  
    
  }
  
  df.test <- df %>% 
    filter(!!outlier.col <= outlier_lower
           | !!outlier.col >= outlier_upper)
  
  prop.outlier = round(100*nrow(df.test)/nrow(df), 1)
  
  message(paste0("Mean: ", signif(mean(vec_outlier, na.rm = TRUE), 5), 
                 ", Lower limit: ", signif(outlier_lower, 5),
                 ", Upper limit: ", signif(outlier_upper),5,
                 ", % rows which are outliers: ", prop.outlier, "%"
  ))
  
  return(df.test)
  
}


# example tables ----------------------------------------------------------

library(scales)
library(tidyverse)
options(scipen=999)

stores_fruit <- readr::read_csv("stores_fruit.csv")
stores_latlong <- readr::read_csv("stores_latlong.csv")


# NA counts for all fields in total ----
test.na.count.all <- na_counts_total(df = stores_fruit %>% mutate(count = 1),
                                    sum.col = "count")

# NA count for all fields, split by a field ----
test.na.count.split <- na_counts_split(df = stores_fruit %>% mutate(count = 1),
                                       field = "product",
                                       sum.col = "count")

# NA counts in various chart formats ----
na_graph(df = stores_fruit, type = "matrix")
na_graph(df = stores_fruit, type = "summary")
na_graph(df = stores_fruit, type = "correlation")


# categorical labels frequency table ----
test.label.freq <- label_freq(df = stores_fruit %>% mutate(count = 1),
                              sum.col = "count",
                              top.x = 10)

# numerical fields basic stats in total ----
library(fBasics)
test.basic.stats.all <- basic_stats_total(df = stores_fruit)

test.basic.stats.split <- basic_stats_split(df = stores_fruit,
                                            field = "product")

# test for outliers on a numeric column ----
test.outliers <- outliers(df = stores_fruit %>% filter(product == "apple"),
                          outlier.col = "ppu",
                          dev.type = "sd",
                          num.dev = 2)


# common fields test ----
fields_alignment(df1 = stores_fruit,
                 df2 = stores_latlong)

# common labels test ----
label_alignment(df1 = stores_fruit,
                df2 = stores_latlong,
                field = "store")

# count duplicates ----
duplicates_count(df = stores_fruit,
                 duplicated_across_cols = c("store", "product", "date"))

duplicates_count(df = stores_fruit,
                 duplicated_across_cols = colnames(stores_fruit))
