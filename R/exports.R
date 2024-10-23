# Export rbind.fill function from plyr
rbind.fill <- function(...){
  plyr::rbind.fill(...)
}


#' @title Other handy exported functions
#' @description
#' Essentially a wrapper for certain useful functions exported form other packages.
#' @param ... additional arguments passed to the exported function
#' @param test.na,unit,est.prob,x See \code{philentropy::KL()}.
#' @return see documentation of exported functions.
#' @rdname exported_other
#' @export
ad.test <- function(...) {
  kSamples::ad.test(...)
}

#' @rdname exported_other
#' @export
hessian <- function(...) {
  numDeriv::hessian(...)
}

#' @rdname exported_other
#' @export
KL <- function(x, test.na = TRUE, unit = "log", est.prob = NULL) {
  philentropy::KL(x, test.na = test.na, unit = unit, est.prob = est.prob)
}

#' @title Summary statistics of x
#' @description
#' Summary statistics of x calculated using \code{Rfast} or \code{DescTools}. For \code{Kurt()}, \code{DescTools} returns the excess Kurtosis, but \code{herbivar::Kurt()} returns the raw kurtosis.
#' @param x a numeric vector for which the summary statistic is calculated
#' @param na.rm if \code{TRUE} (default is \code{FALSE}), the missing values are removed
#' @param fast if \code{TRUE}, \code{Rfast} is used to do the calculation. It's much faster but handles fewer edge cases.
#' @param unbiased If \code{TRUE} (default), the unbiased Gini coefficient is calculated.
#' @param method,weights \code{method} and \code{weights} argument passed to \code{DescTools}.
#' @param ... additional arguments passed to \code{DescTools}
#' @return a numeric value
#' @rdname exported_summary_stat
#' @export
Gini<-function(x, na.rm = FALSE, fast = TRUE, unbiased = TRUE, ...) {
  if(any(is.na(x))){
    if(na.rm){
      x <- x[!is.na(x)]
    } else {
      return(NA_real_)
    }
  }
  if(any(x < 0)){
    warning("Vector contains negative one or more values. Interpert with caution.")
  }
  if(fast){
    if(unbiased){
      n <- length(x)
      return(n / (n-1) * Rfast::ginis(as.matrix(x)))
    } else {
      return(Rfast::ginis(as.matrix(x)))
    }
  } else {
    return(DescTools::Gini(x, ...))
  }
}

#' @rdname exported_summary_stat
#' @export
Skew <- function(x, weights = NULL, na.rm = FALSE, method = 1, fast = TRUE, ...) {
  if(method == 1 && fast){
    if(na.rm){
      x <- x[!is.na(x)]
    }
    Rfast::skew(x)
  } else {
    DescTools::Skew(x, weights = weights, na.rm = na.rm, method = method, ...)
  }
}

#' @rdname exported_summary_stat
#' @export
Kurt <- function(x, weights = NULL, na.rm = FALSE, method = 1, fast = TRUE, ...) {
  if(method == 1 && fast){
    if(na.rm){
      x <- x[!is.na(x)]
    }
    Rfast::kurt(x)
  } else {
    DescTools::Kurt(x, weights = weights, na.rm = na.rm, method = method, ...) + 3
  }
}
