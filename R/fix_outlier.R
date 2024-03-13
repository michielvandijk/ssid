#' Function to fix outliers using interquartile range
#'
#' This function uses the interquartile range to determine outliers and replace them with the median or a value based on the interquartile range.
#'
#' @param x Vector with data containing outliers
#' @param r Values to replace the outliers. Either m for median (default) or q for 0.25 quantile range - f * IQR (lower) and/or 0.75 quantile range + f * IQR (upper).
#' @param f factor to determine outliers. Default is 1.5. Outliers are measured as values that are f * IQR above or below
#' the .25 and .75 quantile range, respectively.
#' @param d Direction where outliers will be replaced. u for upper values, l for lower values, ul for both.
#'
#' @return
#' @export
#'
#' @examples
#' fix_outlier_iqr(rnorm(40, mean = 0, sd = 1))
fix_outlier_iqr <- function(x, r = "m", f = 1.5, d = "ul"){
  q1 <- quantile(x, .25)
  q3 <- quantile(x, .75)
  m <- median((x))
  iqr <- IQR(x)
  if(r == "m") {
    if(d == "l") {
      x[which(x < q1-f*IQR(x))] <- m
    } else if(d == "u") {
      x[which(x > q3+f*IQR(x))] <- m
    } else {
      x[which(x < q1-f*IQR(x) | x > q3+f*IQR(x))] <- m
    }
  } else if(r == "q"){
    if(d == "l") {
      x[which(x < q1-f*IQR(x))] <- q1-f*IQR(x)
    } else if(d == "u") {
      x[which(x > q3+f*IQR(x))] <- q3+f*IQR(x)
    } else {
      x[which(x < q1-f*IQR(x))] <- q1-f*IQR(x)
      x[which(x > q3+f*IQR(x))] <- q3+f*IQR(x)
    }
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
