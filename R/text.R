#' @title Capitalize the first letter
#' @description Capitalize the first letter
#' @param x a character string
#' @return a character string
capitalize <- function(x){
  gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", x, perl = TRUE)
}

#' @title Perform `gsub()` by element
#' @description Perform `gsub()` by element
#' @param pattern see `?gsub()`
#' @param replacement see `?gsub()`
#' @param x see `?gsub()`
#' @param ignore.case see `?gsub()`
#' @param perl see `?gsub()`
#' @param fixed see `?gsub()`
#' @param useBytes see `?gsub()`
#' @return a character string
gsub_element_wise <- function(pattern, replacement, x,
                              ignore.case = FALSE, perl = FALSE,
                              fixed = FALSE, useBytes = FALSE){
  n <- length(x)
  if(n > 1){
    if(length(replacement) == 1){
      replacement <- rep(replacement, n)
    }
    if(length(pattern) == 1){
      pattern <- rep(pattern, n)
    }
  }

  lapply(seq_along(x), function(i){
    gsub(pattern[i], replacement[i], x[i])
  }) %>%
    do.call("c",.)
}
