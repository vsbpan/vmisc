#' @title Likelihood Ratio Test (LRT)
#' @description Perform likelihood ratio test.
#' @param model_candidate,model_ref objects for which the log likelihood can be extracted via \code{stats::logLik()}. 'model_ref' must have more degrees of freedom than 'model_candidate' for the function to not throw an error. 'model_candidate' should be nested within 'model_ref' for the test to be valid.
#' @return a data.frame of the LRT
LRT <- function(model_candidate, model_ref){
  temp <- as.list(match.call())
  can_name <- as.character(deparse(temp$model_candidate))
  null_name <- as.character(deparse(temp$model_ref))

  can_ll <- stats::logLik(model_candidate)
  null_ll <- stats::logLik(model_ref)
  can_df <-attr(can_ll,"df")
  null_df <-attr(null_ll,"df")

  LLR <- 2 * (as.numeric(can_ll) - as.numeric(null_ll))
  df <- can_df - null_df
  stopifnot(df > 0)

  P <- stats::pchisq(LLR, df = df, lower.tail = FALSE)

  cat("\r--------- Models --------- \n")
  print(data.frame("loglik" = c(can_ll, null_ll),
                   "df" = c(can_df, null_df), row.names = c(can_name, null_name)))

  cat("\n \r--------- Likelihood Ratio Test --------- \n")
  print(round(data.frame( "Chisq" = LLR, "df" = df,"P" = P, row.names = ""), digits = 4))

  if(LLR < 0){
    warning(paste0(
      "'", can_name, "' is not nested within '",
      null_name, "' or log-likelihood estimation is unstable. Test is not valid."
    ))
  }
  invisible(data.frame( "Chisq" = LLR, "df" = df,"P" = P))
}


#' @title Corrected Akaike's An Information Criterion (AICc)
#' @description Calculate AICc taking into account small sample size bias
#' @param object The object from which log likelihood can be extracted
#' @param ... additional fitted model objects.
#' @param k the penalty on model complexity. Default is 2.
#' @details
#' The formula for AICc is given by:
#' \deqn{AIC_c = -2\ln{\mathcal{L}(x)} + k  d \frac{n}{n - d - 1}}
#' \eqn{d} is the number of parameters, \eqn{k} is the penalty, \eqn{n} is the sample size, and \eqn{\mathcal{L}(x)} is the likelihood.
#'
#' @return if just one object is provided, a numeric value of AICc is returned. If multiple objects are provided, a data.frame with rows corresponding to the objects and columns representing the number of parameters in the model and the AICc is returned.
AICc <- function (object, ..., k = 2)
{
  dots <- as.list(match.call())[-1]
  dots <- dots[!names(dots) == "k"]
  if (length(dots) == 1) {
    logLik <- logLik(object)
    n <- attributes(logLik)$nobs
    if (is.null(n)) {
      n <- stats::nobs(object)
    }
    df <- attributes(logLik)$df
    aicc <- as.numeric(logLik) * -2 + k * df * (n/(n - df -
                                                     1))
    if (n <= (df + 1)) {
      warning("Sample size too small.")
    }
    return(aicc)
  }
  else {
    out <- do.call("rbind", lapply(unname(dots), function(x) {
      loglik <- logLik(eval(x))
      n <- attributes(loglik)$nobs
      if (is.null(n)) {
        n <- stats::nobs(eval(x))
      }
      df <- attributes(loglik)$df
      aicc <- as.numeric(loglik) * -2 + k * df * (n/(n -
                                                       df - 1))
      if (n <= (df + 1)) {
        warning("Sample size too small.")
      }
      return(data.frame(model = deparse(x), AICc = aicc,
                        df = df))
    }))
    out$dAICc <- out$AICc - min(out$AICc)
    out <- out[order(out$AICc), ]
    return(out)
  }
}
