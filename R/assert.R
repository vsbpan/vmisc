# Returns TRUE if the string has only numbers
is.numbers_only <- function(x){
  !grepl("\\D", x)
}

# Returns TRUE if is odd
is.odd <- function(x){
  x %% 2 == 1
}

# Is between range?
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

overlaps_zero <- function(..., return_star = FALSE){
  l <- list(...)
  o <- do.call("c",lapply(l, function(x){var(sign(x)) == 0}))
  if(return_star){
    o <- ifelse(o, "*", "")
  }
  return(o)
}

# Check if brmsfit is multivariate
is.multi_response <- function(model){
  insight::get_family(model)$family %in% c("multinomial", "categorical")
}

# Check if is null, else, return a vector of is.na(x)
is.null_na <- function(x){
  if(is.null(x)){
    return(TRUE)
  } else {
    return(is.na(x))
  }
}

# vectorized isTRUE
isTRUE_elementwise <- function(x){
  vapply(x, isTRUE, FUN.VALUE = logical(1))
}

# Vectorized isFALSE
isFALSE_elementwise <- function(x){
  vapply(x, isFALSE, FUN.VALUE = logical(1))
}


# Assert atomic type
assert_atomic_type <- function(x, type = c("logical", "character", "numeric", "list", "integer"),
                               null_as_is = FALSE, NA_as_is = TRUE){
  type <- match.arg(type)

  if(is.null(x)){
    if(null_as_is){
      return(NULL)
    } else {
      stop(sprintf("`%s` of type '%s' is not '%s'!", deparse1(substitute(x)), class(x), type))
    }
  }

  if(any(is.na(x))){
    if(NA_as_is){
      return(NA)
    } else {
      stop(sprintf("%s contains %s NA%s!",
                   deparse1(substitute(x)),
                   sum(is.na(x)),
                   ifelse(sum(is.na(x)) > 1, "s", "")
      ))
    }
  }

  if(length(x) > 1){
    if(length(unique(x)) > 1){
      stop(sprintf("%s of is not an atomic %s vector or an identical vector. %s unique values found. ", deparse1(substitute(x)), type, length(unique(x))))
    } else {
      x <- unique(x)
    }
  }

  test <- switch(type,
                 "logical" = is.logical,
                 "character" = is.character,
                 "list" = is.list,
                 "numeric" = is.numeric,
                 "integer" = is.integer)

  if(!test(x)){
    stop(sprintf("`%s` of type '%s' is not '%s'!", deparse1(substitute(x)), class(x), type))
  }

  return(x)
}


assert_dir <- function(x){
  if(!isTRUE(dir.exists(x))){
    stop(paste0("There is no existing directory '", x,"' for ", as.character(substitute(x)), "."))
  }
  file.path(x)
}


assert_package <- function(x){
  if(!requireNamespace(x, quietly = TRUE)){
    stop(sprintf("This function requires the '%s' package, but loading failed.", x))
  }
}

is.distr <- function(x){
  inherits(x, "distr")
}

is.error <- function(x){
  inherits(x, "error") | inherits(x, "try-error")
}

has_function <- function(x){
  stopifnot(length(x) == 1)
  is.function(get0(x))
}

