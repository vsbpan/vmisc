#' @title Returns TRUE if the string has only numbers
#' @description Returns TRUE if the string has only numbers
#' @param x a vector of character string
#' @return a logical vector the same length as x
is.numbers_only <- function(x){
  !grepl("\\D", x)
}

#' @title Check if value is odd
#' @description Check if value is odd
#' @param x a vector of numeric values
#' @return a logical vector the same length as x
is.odd <- function(x){
  x %% 2 == 1
}

#' @title Check if value is between range
#' @description Check if value is between range
#' @param x a vector of numeric values
#' @param range a numeric vector of length 2 indicating the range
#' @param inclusive should the bound be included (default is FALSE)?
#' @return a logical vector the same length as x
is.between <- function(x, range, inclusive = FALSE){
  if(inclusive){
    return(
      x >= range[1] & x <= range[2]
    )
  } else {
    return(
      x > range[1] & x < range[2]
    )
  }
}

#' @title Check if range of vector contains zero
#' @description Check if range of vector contains zero
#' @param ... additional numeric vectors
#' @param return_star if TRUE, return '*' or "" in place of TRUE or FALSE
#' @return a logical or logical vector the same length as ...
overlaps_zero <- function(..., return_star = FALSE){
  l <- list(...)
  o <- do.call("c",lapply(l, function(x){var(sign(x)) == 0}))
  if(return_star){
    o <- ifelse(o, "*", "")
  }
  return(o)
}

#' @title Check if brmsfit is multivariate
#' @description Check if brmsfit is multivariate
#' @param model a model of class 'brmsfit'
#' @return a boolean
is.multi_response <- function(model){
  insight::get_family(model)$family %in% c("multinomial", "categorical")
}

#' @title Check if is null, else, return a vector of is.na(x)
#' @description a safer version of is.na(x) that also deals with NULL
#' @param x the thing to check
#' @return a logical vector
is.null_na <- function(x){
  if(is.null(x)){
    return(TRUE)
  } else {
    return(is.na(x))
  }
}

#' @title Vectorized isTRUE() or isFALSE()
#' @description Vectorized isTRUE() or isFALSE()
#' @param x a vector
#' @return a logical vector
#' @rdname isTRUE_vectorized
isTRUE_elementwise <- function(x){
  vapply(x, isTRUE, FUN.VALUE = logical(1))
}

#' @rdname isTRUE_vectorized
isFALSE_elementwise <- function(x){
  vapply(x, isFALSE, FUN.VALUE = logical(1))
}

#' @title Assert atomic type
#' @description Throws error if supplied vector is not atomic or is not of expected object type
#' @param x a vector to check
#' @param type the object class to check. Must have `is.CLASS()` to work.
#' @param null_as_is if TRUE, return NULL if x is NULL
#' @param NA_as_is if TRUE, return NA, if x is NA
#' @return x
assert_atomic_type <- function(x, type,
                               null_as_is = FALSE, NA_as_is = TRUE){

  if(is.null(x)){
    if(null_as_is){
      return(NULL)
    } else {
      cli::cli_abort(
        {"{.arg {deparse1(substitute(x))}} of type {.cls {class(x)}} is not {.cls {type}}"}
      )
    }
  }

  if(any(is.na(x))){
    if(NA_as_is){
      return(NA)
    } else {
      cli::cli_abort(
        {"{.arg {deparse1(substitute(x))}} contains {sum(is.na(x))} NA{?s}!"}
      )
    }
  }

  if(length(x) > 1){
    if(length(unique(x)) > 1){
      cli::cli_abort(
        {"{.arg {deparse1(substitute(x))}} is not an atomic {.cls {type}} vector or an identical vector. {length(unique(x))}  unique values found."}
      )
    } else {
      x <- unique(x)
    }
  }

  test <- function(x){
    do.call(paste0("is.",type), list(x))
  }

  if(!test(x)){
    cli::cli_abort(
      {"{.arg {deparse1(substitute(x))}} of type {.cls {class(x)}} is not {.cls {type}}"}
    )
  }

  return(x)
}

#' @title Check and expand directory path
#' @description Throws an error if directory does not exist and run `file.path(x)`
#' @param x the directory path
#' @return a character string
assert_dir <- function(x){
  if(!isTRUE(dir.exists(x))){
    cli::cli_abort("There is no exsting director {.path {x}} for {.arg {as.character(substitute(x))}}.")
  }
  file.path(x)
}

#' @title RequireNamespace() with better error message
#' @description RequireNamespace() with better error message
#' @param x the package
#' @return NULL
assert_package <- function(x){
  if(!requireNamespace(x, quietly = TRUE)){
    cli::cli_abort("This function requires the {.pkg {x}} package, but loading failed.")
  }
}

#' @title Various type checks
#' @description Various type checks
#' @param x the object
#' @return TRUE or FALSE
#' @rdname various_type_check
is.distr <- function(x){
  inherits(x, "distr")
}

#' @rdname various_type_check
is.error <- function(x){
  inherits(x, "error") | inherits(x, "try-error")
}

#' @title Check whether function exists and is function
#' @description Check whether function exists and is function.
#' @param x the object
#' @return TRUE or FALSE
has_function <- function(x){
  stopifnot(length(x) == 1)
  is.function(get0(x))
}


assert_distr <- function(x, vcv = FALSE){
  nms <- c("name", "param")
  if(vcv){
    nms <- c(nms, "vcv")
  }
  if(!all(nms %in% names(x))){
    miss <- nms[!nms %in% names(x)]
    cli::cli_abort("Missing {length(miss)} item{?s} {miss} in {.cls distr} object.")
  }
}



