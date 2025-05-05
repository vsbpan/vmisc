#' @title Compute the mean of distribution via simulation
#' @description Compute the mean of distribution by finding the sample mean of 10,000 draws from the distribution
#' @param distr a 'distr' object
#' @return an atomic numeric vector
distr_mu <- function(distr){
  warnifnot(is.distr(distr))
  assert_distr(distr)
  dist_name <- distr$name
  param <- distr$param
  av_fun <- distr_fun(distr)
  if(av_fun$r){
    # Simulation based mean. Good enough for now. Need to add analytical and integration methods.
    mu <- mean(do.call(paste0("r", dist_name), c(list("n" = 10000), param)))
  } else {
    cli::cli_abort("Need {.fn r{dist_name}} to simulate mu. Analytical methods not available.")
  }
  return(mu)
}

#' @title Check whether density function, random number generation, cumulative distribution function, and quantile function are available for the specific distribution.
#' @description  Check whether density function, random number generation, cumulative distribution function, and quantile function are available for the specific distribution.
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


distr_fit <- function(x, name, as_distr = TRUE, ...){
  res <- fitdistrplus::fitdist(x, name, ...)
  if(as_distr){
    res <- distr_make(res$distname, as.list(res$estimate), res$vcov)
  }
  return(res)
}


distr_make <- function(name, param = NULL, vcv = NULL){
  res <- list("name" = name,
              "param" = param,
              "vcv" = vcv)
  class(res) <- c("distr","list")
  res
}

rdistr <- function(n, distr, boot = FALSE, ...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("r", distr$name),
    c(
      list("n" = n),
      distr$param,
      list(...)
    )
  )
}

ddistr <- function(x, distr, boot = FALSE,...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("d", distr$name),
    c(
      list("x" = x),
      distr$param,
      list(...)
    )
  )
}

pdistr <- function(q, distr, boot = FALSE,...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("p", distr$name),
    c(
      list("q" = q),
      distr$param,
      list(...)
    )
  )
}

qdistr <- function(p, distr, boot = FALSE, ...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("q", distr$name),
    c(
      list("p" = p),
      distr$param,
      list(...)
    )
  )
}

# Not very fast for large number of bootstraps. But okay for now.
distr_boot <- function(distr){
  assert_distr(distr)
  if(!is.null(distr$vcv)){
    distr$param <- as.list(
      mvnfast::rmvn(1, mu = do.call("c", distr$param),
                    sigma = distr$vcv, kpnames = TRUE)
    )
  } else {
    cli::cli_abort("Sorry, no variance-covariance matrix to perform bootstrapping.")
  }
  distr
}

