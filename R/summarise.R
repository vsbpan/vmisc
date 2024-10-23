#' @title Summarise vector
#' @description Handy function to summaries a vector of numeric values
#' @param x A vector or matrix of numeric values
#' @param interval The density interval used to find the upper and lower intervals. Default is 0.95.
#' @param na.rm If \code{TRUE} (default is \code{FALSE}), missing values will be removed.
#' @return a vector of numeric values summarising the vector \code{x}.
summarise_vec <- function(x, interval = 0.95, na.rm = FALSE){
  x <- c(x)
  if(na.rm){
    x <- x[!is.na(x)]
  }
  lower.prob <- (1-interval)/2
  upper.prob <- 1 - lower.prob
  if(any(is.na(x))){
    out <- c("mean" = NA_real_,
             "median" = NA_real_,
             "sd" = NA_real_,
             "lower" = NA_real_,
             "upper" = NA_real_)
  } else {
    out <- c("mean" = mean(x),
             "median" = stats::median(x),
             "sd" = stats::sd(x),
             "lower" = unname(stats::quantile(x, prob = lower.prob)),
             "upper" = unname(stats::quantile(x, prob = upper.prob)))
  }
  return(out)
}
