#' @title Change the text color depending on rstudio theme dark mode or light mode
#' @description
#' stolen from `tidyverse`
#' @param x a character string
#' @return a character string
text_col <- function(x){
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  theme <- rstudioapi::getThemeInfo()
  if (isTRUE(theme$dark))
    crayon::white(x)
  else crayon::black(x)
}

#' @title Warn if condition fails
#' @description
#' Same as `stopifnot()` but warns instead
#' @param x a condition to be tested
#' @return NULL
warnifnot <- function(x){
  if(!isTRUE(x)){
    test <- deparse1(substitute(x))
    test <- crayon::blue(test)
    value <- crayon::yellow(TRUE)
    cli::cli_alert_warning("{.code {test}} is not {value}")
  }
}

#' @title Suppress duplicate warnings or messages
#' @description Collapse duplicate messages.
#' @param expr an expression
#' @return output of expr
suppressExtraMessages <- function(expr){
  .f <- function(){
    expr
  }
  .f <- purrr::as_mapper(.f)
  fewer <- function(z) {
    z <- trimws(z)
    counts <- table(z)
    counts <- counts[match(names(counts), z)]
    paste0(names(counts), ifelse(counts > 1, cli::col_blue(sprintf(" (%d times)", counts)), ""))
  }
  out <- function(...) {
    res <- capture_output(.f(...))
    msgs <- fewer(res$messages)
    warns <- fewer(res$warnings)
    res <- res$result
    for (m in msgs) message(m)
    for (w in warns) warning(w, call. = FALSE)
    return(res)
  }
  return(out())
}



