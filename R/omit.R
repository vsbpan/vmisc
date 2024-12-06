#' @export
#' @rdname omit_zero
omit_zero.list <- function(x){
  purrr::keep(x, function(z){
    sum(z) != 0
  })
}

#' @export
#' @rdname omit_zero
omit_zero.default <- function(x){
  x[!x==0]
}
