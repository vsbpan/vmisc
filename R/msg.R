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
