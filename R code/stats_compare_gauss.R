
# functions ---------------------------------------------------------------


#' @title T-tests
#' 
#' Generate a vector with t-test results
#' 
#' @param vec1 Vector of values to test
#' @param vec2 A second vector to test if comparing two groups. If NULL, do a one-sample test on vec1
#' @param mu Population mean for one-sample test, or population difference between two-sample means
#' @param paired TRUE if two-sample paired data
#' @param var.equal TRUE if two-sample with unequal variances (Welch's t test)
#' @param alpha Set alpha for confidence intervals. Default = 0.05.
stat_t_test <- function(vec1, vec2 = NULL, mu = 0, 
                        paired = FALSE, var.equal = TRUE, 
                        alpha = 0.05) {
  
  if (gtools::invalid(vec2)) vec2 <- NULL
  
  res_raw <- t.test(x = vec1, 
                    y = vec2,
                    mu = mu,
                    paired = paired,
                    var.equal = var.equal,
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
  
  return(res)
  
}


#' @title One-way ANOVA test
#' 
#' Generate a vector with one-way ANOVA test results
#' Data needs to be organised in long format
#' 
#' @param df dataframe contain
#' @param value.var field containing the values
#' @param group.var field containing the group labels
stat_anova_oneway <- function(df, value.var, group.var) {
  
  df_name <- rlang::as_name(rlang::ensym(df))
  
  res_raw <- summary(aov(reformulate(group.var, value.var), data = df))
  
  res <- tibble(data = df_name,
                p_value = res_raw[[1]]$`Pr(>F)`[1])
  
  return(res)
  
}


#' @title Repeated measures ANOVA test
#' 
#' Generate a vector with repeated measures ANOVA test results
#' Data needs to be organised in long format
#' 
#' @param df dataframe contain
#' @param value.var field containing the values
#' @param group.var field containing the group labels
stat_anova_rm <- function(df, value.var, group.var) {
  
  df_name <- rlang::as_name(rlang::ensym(df))
  
  model <- lm(reformulate(group.var, value.var), data = df)
  res_raw <- car::Anova(model, idata = df, idesign = ~s)
  
  res <- tibble(data = df_name,
                p_value = res_raw[1,4]
  )
  
  return(res)
  
}

# examples ----------------------------------------------------------------

## T-test ----
set.seed(147)
stat.t.test <- stat_t_test(vec1 = rnorm(150, mean = 10, sd = 3),
                           vec2 = rnorm(100, mean = 11, sd = 4),
                           mu = 0,
                           paired = FALSE,
                           var.equal = FALSE,
                           alpha = 0.05
)

## One-way ANOVA ----
# needs pre-processing into a long format
set.seed(147)
vec1 <- rnorm(150, mean = 10, sd = 3)
vec2 <- rnorm(100, mean = 11, sd = 4)
vec3 <- rnorm(200, mean = 10, sd = 5)

df_anova <- bind_rows(
  tibble(value = vec1, group = "vec1"),
  tibble(value = vec2, group = "vec2"),
  tibble(value = vec3, group = "vec3")
)

stat.anova.oneway <- stat_anova_oneway(df = df_anova,
                                       value.var = "value",
                                       group.var = "group")


## Repeated measures ANOVA ----
# needs pre-processing into a long format to work effectively
set.seed(147)
vec1 <- rnorm(150, mean = 10, sd = 3)
vec2 <- rnorm(150, mean = 11, sd = 4)
vec3 <- rnorm(150, mean = 10, sd = 5)

df_anova <- bind_rows(
  tibble(value = vec1, group = "vec1"),
  tibble(value = vec2, group = "vec2"),
  tibble(value = vec3, group = "vec3")
)

stat.anova.rm <- stat_anova_rm(df = df_anova,
                               value.var = "value",
                               group.var = "group")
