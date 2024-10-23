# Convert Nan to NA
NaN_to_NA <- function(x){
  ifelse(is.nan(x), NA, x)
}


# Convert NULL value to NA vector of length 'len'
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
