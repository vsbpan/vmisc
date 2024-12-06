#' @title Fancy ggplot scales
#' @description variously stolen from other places
#' @param l character string
#' @param name The name of the scale. Used as the axis or legend title. If `waiver()`, the default, the name of the scale is taken from the first mapping used for that aesthetic. If NULL, the legend title will be omitted.
#' @param axis "x" log the x-axis; "y" log the y-axis
#' @param ... additional arguments
#' @return an expression
#' @rdname fancy_scale
fancy_linear <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = FALSE)
  # return this as an expression
  parse(text=l)
}

#' @rdname fancy_scale
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # e+00 becomes 1
  l <- gsub("e\\+00", "", l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove prefactor 1
  l <- gsub("'1'e", "10^", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove plus
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}

#' @rdname fancy_scale
fancy_scientificb <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove prefactor 1
  l <- gsub("'1'e", "10^", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove plus
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}

#' @rdname fancy_scale
scale_x_ta <- function(name = ggplot2::waiver(), ...){
  ggplot2::scale_x_continuous(
    name,
    breaks = c(-pi, -pi / 2, 0, pi / 2, pi),
    labels = c(expression(-pi), expression(-pi/2), 0, expression(pi/2), expression(pi)),
    ...)
}

#' @rdname fancy_scale
scale_xy_log <- function(name = ggplot2::waiver(), axis = c("x", "y")){
  axis <- match.arg(axis, several.ok = TRUE)
  if("x" %in% axis){
    resx <- list(
      ggplot2::scale_x_log10(label = fancy_scientific)
    )
  } else {
    resx <- NULL
  }


  if("y" %in% axis){
    resy <- list(
      ggplot2::scale_y_log10(label = fancy_scientific)
    )
  } else {
    resy <- NULL
  }
  return(c(resx, resy))
}
