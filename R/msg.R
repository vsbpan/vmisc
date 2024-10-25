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

warnifnot <- function(x){
  if(!isTRUE(x)){
    test <- deparse1(substitute(x))
    test <- crayon::blue(test)
    cli::cli_alert_warning("{.code {test}} is not TRUE")
  }
}


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

capture_output <- utils::getFromNamespace("capture_output", "purrr")

