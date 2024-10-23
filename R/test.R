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
