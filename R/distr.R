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

