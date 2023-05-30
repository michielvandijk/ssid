#' Function to fix outliers using interquartile range
#'
#' This function uses the interquartile range to determine outliers and replace them with the median value.
#'
#' @param x Vector with data containing outliers
#' @param r Range. Default is 1.5
#' @param d Direction where outliers will be replaced. u for upper values, l for lower values, ul for both.
#'
#' @return
#' @export
#'
#' @examples
#' fix_outlier_iqr(rnorm(40, mean = 0, sd = 1))
fix_outlier_iqr <- function(x, r = 1.5, d = "ul"){
  q1 <- quantile(x, .25)
  q3 <- quantile(x, .75)
  m <- median((x))
  iqr <- IQR(x)
  if(d == "l") {
    x[which(x < q1-r*IQR(x))] <- m
  } else if(d == "u") {
    x[which(x > q3+r*IQR(x))] <- m
  } else {
    x[which(x < q1-r*IQR(x) | x > q3+r*IQR(x))] <- m
  }
  return(x)
}

#' Function to fix outliers using quantile approach
#'
#' This function uses quantiles to determine outliers and replace them with the median value.
#'
#' @param x Vector with data containing outliers
#' @param q_lower Lower quantile range. Default is 0.025.
#' @param q_upper Upper quantile range. Default is 0.975.
#' @param d Direction where outliers will be replaced. u for upper values, l for lower values, ul for both.
#'
#' @return
#' @export
#'
#' @examples
#' fix_outlier_quantile(rnorm(40, mean = 0, sd = 1), d = "ul")
fix_outlier_quantile <- function(x, q_lower = 0.025, q_upper = 0.975, d = "ul"){
  q_l <- quantile(x, q_lower)
  q_u <- quantile(x, q_upper)
  m <- median((x))
  if(d == "l") {
    x[which(x < q_l)] <- m
  } else if(d == "u") {
    x[which(x > q_u)] <- m
  } else {
    x[which(x < q_l | x > q_u)] <- m
  }
  return(x)
}
