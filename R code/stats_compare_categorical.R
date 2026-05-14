
# functions ---------------------------------------------------------------

#' @title Chi-square / Fisher's test
#' 
#' Generate a vector with chi-squared test results. 
#' For small samples (any E < 5 and/or N < 60) in 2x2 tables, function will automatically attempt Fisher's test.
#' Data needs to be stored as a contingency table of class matrix of actual values
#' 
#' @param mat contingency table of class matrix
stat_chisq <- function(mat) {
  
  check.class <- sapply(mat, function(x) is.numeric(x) || is.integer(x))
  if(!"matrix" %in% class(mat) | any(!check.class)) {
    stop("'mat' should be of class 'matrix' containing only the numeric fields to be tested.")
  }
  
  expected <- suppressWarnings(chisq.test(mat)$expected)
  flag_low_expected <- any(expected <= 5)
  total <- sum(mat)
  
  if(flag_low_expected | total < 60) {
    
    fisher_attempt <- tryCatch(
      fisher.test(mat),
      error = function(e) NULL
    )
    
    if(!is.null(fisher_attempt)) {
      res_raw <- fisher_attempt
      method <- "Fisher"
    } else {
      res_raw <- chisq.test(mat)
      method <- "ChiSq (Fisher failed)"
    }
    
  } else {
    res_raw <- chisq.test(mat)
    method <- "ChiSq"
  }
  
  res <- tibble(method = method,
                data = res_raw$data.name,
                p_value = res_raw$p.value
  )
  
  return(res)
  
}


#' @title McNemar's test
#' 
#' Generate a vector with McNemar's test results. 
#' For small samples (b + c < 35) in 2x2 tables, function will automatically attempt a binomial exact test.
#' Data needs to be stored as a contingency table of class matrix of actual values
#' 
#' @param mat contingency table of class matrix
stat_mcnemar <- function(mat) {
  
  check.class <- sapply(mat, function(x) is.numeric(x) || is.integer(x))
  if(!"matrix" %in% class(mat) | any(!check.class)) {
    stop("'mat' should be of class 'matrix' containing only the numeric fields to be tested.")
  }
  
  n10 <- mat[2,1]
  n01 <- mat[1,2]
  N <- n10 + n01
  
  if(N < 25) {
    
    method = "Binomial Exact"
    p_value = 2*pbinom(n01, N, 0.5)
    
  } else {
    
    res_raw <- mcnemar.test(mat)
    method <- "McNemar"
    p_value <- res_raw$p.value
  }
  
  res <- tibble(method = method,
                p_value = p_value
  )
  
  return(res)
  
}

# examples ----------------------------------------------------------------

## Chi-squared / Fisher's exact tests ----
# Fisher selected automatically if any E <= 5 or N <= 60. 
# Note Fisher not applicable if both dimensions >2 groups
sample_matrix <- matrix(c(25, 36, 23, 51, 37, 19),
                        nrow = 3)

stat.chisq <- stat_chisq(mat = sample_matrix)


## McNemar's / Binomial exact test ----
sample_matrix <- matrix(c(15, 16, 33, 31),
                        nrow = 2)

stat.mcnemar <- stat_mcnemar(mat = sample_matrix)

