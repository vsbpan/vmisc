#' @title Compute the mean of distribution via simulation
#' @description Compute the mean of distribution by finding the sample mean of 10,000 draws from the distribution
#' @param x a 'distr' object
#' @return an atomic numeric vector
distr_mu <- function(x){
  warnifnot(is.distr(x))
  dist_name <- x$name
  params <- x$params
  av_fun <- distr_fun(x)
  if(av_fun$r){
    # Simulation based mean. Good enough for now. Need to add analytical and integration methods.
    mu <- mean(do.call(paste0("r", dist_name), c(list("n" = 10000), params)))
  } else {
    stop(sprintf("Need r%s() to simulate mu. Analytical methods not available.", dist_name))
  }
  return(mu)
}

#' @title Check whether density function, random number generation, cumulative distribution function, and quantile function are available for the specific distribution.
#' @description Compute the mean of distribution by finding the sample mean of 10,000 draws from the distribution
#' @param x a 'distr' object or a character string of the name of the distribution.
#' @return a list of atomic boolean
distr_fun <- function(x){
  if(is.distr(x) || !is.character(x)){
    dist_name <- x$name
  } else {
    dist_name <- x
  }
  o <- list(
    "d" = has_function(paste0("d",dist_name)),
    "r" = has_function(paste0("r",dist_name)),
    "p" = has_function(paste0("p",dist_name)),
    "q" = has_function(paste0("q",dist_name))
  )
  return(o)
}

