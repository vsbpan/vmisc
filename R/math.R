#' @title Multivariate delta method
#' @description Approximate the variance covariance matrix of transformed random variables via first order Taylor approximation
#' @param x the expectation of x
#' @param vcv the variance covariance matrix
#' @param trans a list of functions of the transformation
#' @return a variance covariance matrix
FOSM2 <- function(x, vcv, trans){
  stopifnot(is.list(trans))
  stopifnot(length(x) == length(trans))
  x_vec <- x
  vcv <- as.matrix(vcv)
  deriv_vec <- lapply(seq_along(x), function(i){
    transi <- trans[[i]]
    x <- x_vec[i]

    stopifnot(is.function(transi))
    fun.string <- deparse1(transi)
    variable_name <- gsub(".*function \\(|\\).*", "", fun.string)
    fun.string <- gsub(variable_name, "x", fun.string)
    fun.string <- gsub(".*\\{| |\\}.*| ", "", fun.string)
    fun.string <- gsub("function\\(x\\)", "", fun.string)
    if (fun.string == "plogis(x)") {
      deriv.expression <- expression(1/x + 1/(1 - x))
    }
    else {
      deriv.expression <- stats::D(parse(text = fun.string),
                                   "x")
    }
    eval(deriv.expression)
  }) %>%
    do.call("c", .)

  J2 <- tcrossprod(deriv_vec)
  stopifnot(
    identical(dim(J2), dim(vcv))
  )

  var.trans <- J2 * vcv
  return(var.trans)
}


#' @title Compute raw moment
#' @description compute the pth raw moment
#' @param x a vector of samples
#' @param p an atomic numeric vector of the pth moment
#' @return an atomic vector
moment <- function(x, p){
  assert_atomic_type(p, "numeric")
  mean(x^p)
}
