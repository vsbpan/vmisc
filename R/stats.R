#' @title Ordered statistics
#' @description Find the nth ordered statistics
#' @param x the vector
#' @param n the nth order
#' @return an atomic numeric vector
ordered_stat <- function(x, n){
  x <- Rfast::Sort(x, na.last = NA)

  if(n %% 1 > 0){
    stop("n must be a non-zero integer")
  }

  if(n > 0){
    x[n]
  } else {
    x[length(x) + n]
  }
}

# compute pooled sd
pooled_SD <- function(v, n){
  stopifnot(length(v) == length(n))
  res <- rep(NA_real_, length(v))
  for(i in seq_along(v)){
    res[i] <- v[i] * (n[i] - 1)
  }
  sqrt(sum(res) / (sum(n) - length(n)))
}

#' @title Generate uncertainty intervals for binary data using conjugate prior
#' @description Generate uncertainty intervals for binary data using conjugate prior (beta distribution)
#' @param alpha The number of successes
#' @param beta The number of failure
#' @param interval A numeric value indicating the intervals for upper and lower bounds (default to 0.95).
#' @param prior A numeric vector of the alpha and beta value of the conjugate prior. Default is the Bayes-Laplace prior (Tuyl et al. 2008) \eqn{\alpha = 1, \beta = 1}. Other popular choices include 'neutral' prior (Kerman 2011) \eqn{\alpha = 1/3, \beta = 1/3}, and the Jeffreys prior \eqn{\alpha = 0.5, \beta = 0.5}.
#' @references
#' Kerman, J. 2011. Neutral noninformative and informative conjugate beta and gamma prior distributions. Electronic Journal of Statistics 5:1450–1470.
#' Tuyl, F., R. Gerlach, and K. Mengersen. 2008. A Comparison of Bayes-Laplace, Jeffreys, and Other Priors: The Case of Zero Events. The American Statistician 62:40–44.
#' @return a numeric vector
#' @rdname beta_conjugate
beta_conjugate <- function(alpha, beta, interval = 0.95, prior = c(1,1)){
  a <- alpha + prior[1]
  b <- beta + prior[2]

  lower_prob <- (1 - interval) / 2
  upper_prob <- interval + lower_prob

  out <- data.frame("estimate" = a / (a+b),
                    "std" = sqrt(
                      (a * b) / ((a + b)^2 * (a + b + 1))
                    ),
                    "lower" = stats::qbeta(lower_prob, shape1 = a, shape2 = b),
                    "upper" = stats::qbeta(upper_prob, shape1 = a, shape2 = b)
  )
  return(out)
}

#' @rdname beta_conjugate
beta_conjugate2 <- function(a, interval = 0.95, prior = rep(1, length(a))){
  a0 <- sum(a + prior)
  a_ <- a / a0
  lower_prob <- (1 - interval) / 2
  upper_prob <- interval + lower_prob
  out <- data.frame("estimate" = (a + prior) / a0,
                    "std" = sqrt((a_ * (1 - a_)) / (a0 + 1))
  )
  # No unique quantile function exists for dirichlet

  return(out)
}



#' @title  Calculate Coefficient of Variation (CV)
#' @description calculate the CV for a vector of data
#' @param  x
#'  A vector of numeric values
#' @param  method
#'  A character value indicating the type of CV estimator. Valid options are "standard", "Sokal", "Breunig", and "Bao".
#' @param  na.rm
#'  A logical value indicating whether to ignore \code{NA} values in the \code{x} vector
#' @return
#'  A numeric value
#' @details
#'  The method option selects one of four CV estimators investigated by Yang et al. (2020).
#'
#'  **standard**: This is the conventional CV estimator:
#'  \deqn{CV_1 = \frac{\sigma}{\mu}}
#'
#'  **Sokal**: This assumes the sample is normally distributed and corrects for the bias using the sample size as in Sokal and Rohlf (1995):
#'  \deqn{CV_2 = CV_1 + \frac{CV_1}{4N}}
#'
#'  **Breunig**: This implements Breunig's (2001) method:
#'  \deqn{CV_3 = CV_1 \sqrt{1- \frac{CV_1}{N}(3CV_1 -2\gamma_1)}}
#'  \eqn{\gamma_1} is Pearson's measure of skewness
#'
#'  **Bao**: This implements Bao's (2009) method:
#'  \deqn{CV_4 = CV_1 + \frac{CV_1^3}{N}+ \frac{CV_1}{4N}+ \frac{CV_1^2\gamma_1}{2N} + \frac{CV_1\gamma_2}{8N}}
#'  \eqn{\gamma_2} is Pearson's measure of kurtosis
#'
#' @references
#'
#' Bao, Y. 2009. Finite-Sample Moments of the Coefficient of Variation. Econometric Theory 25:291–297.
#'
#' Breunig, R. 2001. An almost unbiased estimator of the coefficient of variation. Economics Letters 70:15–19.
#'
#' Sokal, R. R., and J. F. Rohlf. 1995. Biometry: The Principles and Practices of Statistics in Biological Research. Third edition. Freeman, New York.
#'
#' Yang, J., J. Lu, Y. Chen, E. Yan, J. Hu, X. Wang, and G. Shen. 2020. Large Underestimation of Intraspecific Trait Variation and Its Improvements. Frontiers in Plant Science 11.
#'
#' @export
cv<-function(x, method = c("standard","Sokal","Breunig","Bao"),na.rm = FALSE){
  if(isTRUE(any(x < 0))) {
    warning("Data contain non-positive values")
  }
  if(na.rm){
    x <- x[!is.na(x)]
  }

  method <- match.arg(method)

  cv1 <- stats::sd(x)/mean(x)
  if(method[1] == "standard") {
    return(cv1)
  } else
    if(method[1] == "Sokal") {
      cv2<- cv1 + cv1 / (4 * length(x))
      return(cv2)
    } else
      if(method[1] == "Breunig") {
        cv3 <- cv1*sqrt(1-cv1/length(x)*(3*cv1 - 2 * Skew(x)))
        return(cv3)
      } else
        if(method[1] == "Bao"){
          N <- length(x)
          cv4 <- cv1 + cv1^3/N + cv1/(4*N) + cv1^2*Skew(x)/(2*N) + cv1*Kurt(x)/(8*N)
          return(cv4)
        }
}

#' @title  Calculate Aggregation Coefficient
#' @description calculate the aggregation \eqn{J} for a given mean and variance or vector of data
#' @param x a vector of numeric data
#' @param mean mean of data. Ignored if \code{x} is supplied
#' @param var variance of data. Ignored if \code{x} is supplied
#' @param na.rm a logical value indicating whether to drop NA values
#' @return
#'  A numeric value
#' @details
#' The aggregation coefficient \eqn{J} is calculated as
#'  \deqn{J = \frac{\sigma^2 - \mu}{\mu^2}}
#'  \eqn{J = 0} corresponds to a possion distribution (random)
#'  \eqn{J < 0} corresponds to a more even distribution (underdispersed)
#'  \eqn{J > 0} corresponds to a more aggregated distribution (overdispersed)
#'  @export

J.index <- function(x=NULL,mean=NULL,var=NULL, na.rm = FALSE){
  if(any(x < 0)) {
    warning("Data contain non-positive values")
  }
  if(!is.null(x)){
    var <- var(x, na.rm = na.rm)
    mean <- mean(x,na.rm = na.rm)
  }
  (var - mean) / (mean)^2
}


#' @title  Calculate Coefficient of Dispersion (CD)
#' @description calculate CD for a given mean and variance or vector of data
#' @param x a vector of numeric data
#' @param robust if \code{TRUE}, a more robust for of CD is calculated. Default is \code{FALSE}
#' @param na.rm a logical value indicating whether to drop \code{NA} values
#' @return
#'  A numeric value
#' @details
#' The coefficient of dispersion \eqn{CD}, a.k.a. variance mean ratio, is calculated as
#'  \deqn{CD = \frac{\sigma^2}{\mu}}
#' The robust version is calculated as
#' \deqn{CD = \frac{1}{n} \frac{\sum_i^n |m - x_i|}{m}}
#' where \eqn{m} is the median of the data
#' @export
cd <- function(x, robust = FALSE, na.rm = FALSE){
  if(any(x < 0)) {
    warning("Data contain non-positive values")
  }
  if(robust){
    m <- stats::median(x, na.rm = na.rm)
    mean(abs(x - m),na.rm = na.rm) / m
  } else {
    stats::var(x,na.rm = na.rm) / mean(x, na.rm = na.rm)
  }
}


#' @title Calculate Standard Error of The Mean (SE)
#' @description calculate standard error for a vector of data
#' @param x a vector of numeric data
#' @param na.rm a logical value indicating whether to drop \code{NA} values
#' @return
#'  A numeric value
#' @details
#' The standard error of the mean \eqn{SE}, is calculated as
#'  \deqn{SE(X) = \sqrt{\frac{\matbb{Var}[X]}{n}}}
#' @export
se <- function(x, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
  }
  stats::sd(x)/sqrt(length(x))
}


#' @title Hoover Index
#' @description Calculate the Hoover index of a vector of numeric data
#' @param x a vector of numeric data
#' @param na.rm a logical value indicating whether to drop \code{NA} values
#' @details
#' The Hoover index can be thought of the percentage of the total x that would have to be reallocated to make all individuals within a population have equal values of x. It is calculated as such:
#' \deqn{H(X) = \frac{\sum_i^n |x_i - E[X]|}{2 \sum_i^n x_i}}
#'
#' @return A numeric value
#' @export
Hoover <- function(x, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
  }
  sum(abs(x - mean(x))) / (2 * sum(x))
}

#' @title Lorenz Asymmetry Coefficient
#' @description Calculate Lorenz Asymmetry Coefficient (also known as \eqn{S}) for a vector of data. The statistic identifies which quantile of \code{x} contribute most to the total inequality.
#' @param x a vector of non-negative numeric values
#' @param n a vector of frequencies the same length as \code{x}. If \code{NULL} (default), the frequencies are assumed to be 1 (as in a vector of untabulated raw data).
#' @param na.rm a logical value indicating whether to drop \code{NA} values
#' @param interval a logical value indicating whether to return an interval when there are x values exactly equal to the mean. If \code{FALSE} (default), the midpoint is returned.
#' @note Part of the code for this function is adapted from Achim Zeileis's \code{ineq::Lasym()} version 0.2-13.
#' @return a single numeric value if \eqn{x_i \neq \hat\mu} for all values of \code{x} or if \code{interval = FALSE}. Otherwise, a vector of length two is returned.
#' @details
#' The Lorenz Asymmetry Coefficient \eqn{S} is a complementary statistic to the Gini coefficient that describes the Lorenz curve. Whereas the Gini coefficient is a measure of inequality, the asymmetry coefficient is is a measure of which half of the \code{x} quantile contribute most to the inequality. If \eqn{S = 1}, the Lorenz curve is symmetrical. If \eqn{S < 1}, the point where the Lorenz curve is parallel to the line of equality is below the axis of symmetry. This means that the lower half of the population contribute most to the inequality. If \eqn{S > 1}, the point where the Lorenz curve is parallelt to the line of equality is above the axis of symmetry. This means that the upper half of the population contribute most to the inequality. See example plot.
#'
#'
#' For a vector of ordered non-negative value \eqn{(x_1, x_2, ..., x_m, x_{m+1}, ..., x_n)}, the sample Lorenz Asymmetry is defined as:
#' \deqn{S = F(\hat\mu) + L(\hat\mu)}
#'
#' where
#' \deqn{\delta = \frac{\hat\mu - x_m}{x_{m+1} - x_m},}
#'
#' \deqn{F(\hat\mu) = \frac{m + \delta}{n},}
#'
#' \deqn{L(\hat\mu) = \frac{L_m + \delta x_{m+1}}{L_m}.}
#'
#' \eqn{\hat\mu} is the mean of \eqn{x}, \eqn{n} is the sample size, \eqn{m} is the number of elements where \eqn{x_m < \hat\mu}, and \eqn{L_i = \sum_{j=1}^i x_j}.
#'
#'
#' If one or more values of \eqn{x_i = \hat\mu}, the closed interval is calculated instead:
#'
#' \deqn{[ \frac{m}{n} + \frac{L_m}{L_n}, \frac{m + a}{n} + \frac{L_{m+a}}{L_n} ],}
#' where \eqn{a} is the number of elements of the vector \eqn{x} that satisfies \eqn{x_i = \hat\mu}.
#'
#' @references
#' Damgaard, C., and J. Weiner. 2000. Describing Inequality in Plant Size or Fecundity. Ecology 81:1139–1142.
#'
#' @examples
#' x <- rbeta(1000,1,0.2)
#' hist(x)
#' plot(lorenz_curve(x))
#' abline(a=1,b=-1) # Line of symmetry
#' lac(x) # S = 0.48
#'
#' x2 <- rallo(1000,a=3)
#' hist(x2)
#' plot(lorenz_curve(x2))
#' abline(a=1,b=-1) # Line of symmetry
#' lac(x2) # S = 1.24
#'
#' @export
lac <- function(x, n = NULL, interval = FALSE, na.rm = FALSE) {
  if(!is.numeric(x)){
    stop("x must be a numeric vector")
  }
  if(na.rm){
    x <- x[!is.na(x)]
  }
  if(any(is.na(x)) || any(x < 0)){
    return(NA_real_)
  }
  if(!is.null(n)){
    x <- rep(x,n)
  }
  x <- sort(x)
  mu <- mean(x)
  xlow <- x < mu
  xeq <- x == mu
  m <- sum(xlow)
  n <- length(x)
  Lm <- sum(x[xlow])
  Ln <- sum(x)


  if (any(xeq)) {
    a <- sum(xeq)
    Lma <- sum(x[xlow | xeq])
    s <- c(m/n + Lm/Ln, (m + a)/n + Lma/Ln)
    if (!interval)
      s <- mean(s) #hmmm
  } else {
    xm <- max(x[xlow])
    xm1 <- min(x[!xlow])
    delta <- (mu - xm)/(xm1 - xm)
    s <- (m + delta)/n + (Lm + delta * xm1)/Ln
  }
  return(s)
}


#' @title Generalized Mean
#' @description Calculate generalized mean for a vector \code{x}. If \eqn{p = 0}, the function returns the geometric mean. If \eqn{p = -1}, the function returns the harmonic mean. If \eqn{p = 1} or \eqn{p = 2}, the arithmetic mean or the root mean square (quadratic mean) is returned. If \eqn{p = \infty} or \eqn{p = -\infty}, the function returns the maximum or minimum respectively. For other values of \eqn{p}, the power mean is returned. For large values of \eqn{|p|}, the function can be numerically unstable.
#' @param x a vector of numeric data
#' @param p a numeric value of length 1 choosing the type of mean computed.
#' @param na.rm if \code{TRUE} (default is \code{FALSE}), the missing values are removed.
#' @return a numeric value of length 1
gen_mean <- function(x, p, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
  }

  if(p == Inf){
    m <- max(x)
  } else {
    if(p == -Inf){
      m <- min(x)
    } else {
      if(p == 0){
        m <- exp(
          1 / length(x) * sum(log(x))
        )
      } else {
        m <- (1 / length(x) * sum(x^p))^(1 / p)
      }
    }
  }
  return(m)
}

