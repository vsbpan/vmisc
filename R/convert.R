#' @title Convert NaN to NA
#' @description Convert NaN to NA
#' @param x a vector
#' @return a vector
NaN_to_NA <- function(x){
  ifelse(is.nan(x), NA, x)
}

#' @title Convert NULL value to NA vector of length 'len'
#' @description Convert NULL value to NA vector of length 'len'
#' @param x a vector which may be NULL
#' @param len number of NA to return when value is NULL
#' @param error_NA if TRUE, if the vector contains errors, do the same thing as if the value of x is NULL.
#' @return a vector
null_to_NA <- function(x, len = 1, error_NA = FALSE){
  f <- function(x){
    if(is.null(x)){
      return(rep(NA, len))
    } else {
      x
    }
  }

  if(error_NA){
    tryCatch({
      f(x)
    }, error = function(e){
      return(rep(NA, len))
    })
  } else {
    f(x)
  }
}
