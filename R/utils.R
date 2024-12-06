#' @title Bind matrices together into an array along the z dimension
#' @description Bind matrices together into an array along the z dimension
#' @param ... matrices of the same width and height
#' @return a three dimensional array
zbind <- function(...){
  mcall <- as.list(match.call(expand.dots = TRUE))[-1]

  array(c(...),
        dim = c(
          dim(eval(mcall[[1]])),
          length(mcall)
        ), dimnames = c(
          dimnames(eval(mcall[[1]])),
          list(
            #do.call("c",lapply(mcall,deparse1))
          )
        ))
}

#' @title Reverse name and value
#' @description Reverse name and value
#' @param x a named vector
#' @return a named character vector
reverse_names <- function(x){
  val <- x
  nms <- names(x)
  names(nms) <- val
  return(nms)
}

#' @title Same thing as `setNames()`
#' @description Same thing as `setNames()`
#' @param x an object
#' @param name the name
#' @return a named vector
append_name <- function(x, name){
  names(x) <- name
  x
}


#' @title Length of unique elements
#' @description Find the length of unique elements
#' @param x a vector
#' @return a numeric value
unique_len <- function(x){
  length(unique(x))
}


#' @title Bind a list of vector into data.frame
#' @description Bind a list of vector into a data.frame and keep track of list names and vector names.
#' @param x a list of vectors
#' @param margin the direction for which the vectors will be bound. 1 indicates rows (default) and 2 indicates columns.
#' @param keep_row_names if \code{TRUE} (default), the row names will be kept. Otherwise, they will be set to \code{NULL}.
#' @param row_names_as_col if a character string is provided, the row name will be added to the data.frame as the first column with that name. If \code{TRUE}, "rownames" will be used as the column name. Otherwise, the no column is added (default is \code{FALSE}).
#' @return a data.frame
bind_vec <- function(x,margin = 1L, keep_row_names = TRUE, row_names_as_col = FALSE){
  out <- as.data.frame(do.call("cbind",x))
  names(out) <- names(x)
  if(as.numeric(margin) == 1){
    out <- as.data.frame(t(out))
  }
  if(isTRUE(row_names_as_col)){
    row_names_as_col <- "rownames"
  }

  if(is.character(row_names_as_col)){
    out <- cbind("v" = rownames(out), out)
    names(out)[1] <- row_names_as_col
  }

  if(!keep_row_names){
    rownames(out) <- NULL
  }

  return(out)
}


#' @title Evenly spaced sequences between the range of a vector
#' @description Generate evenly spaced sequences between the range of a vector \code{x} using \code{seq()}
#' @param x the vector
#' @param length.out the length of output vector
#' @param by number: increment of the sequence.
#' @param na.rm if \code{TRUE}, remove NA.
#' @return a numeric vector
seq_interval <- function(x, length.out = 300, by = NULL, na.rm = FALSE){
  if(!missing(by)){
    length.out <- NULL
  }

  if(!is.null(length.out)){
    if(!is.null(by) && missing(by)){
      stop("Only one of 'length.out' or 'by' should be supplied and the other set to NULL.")
    }
    o <- seq(min(x, na.rm = na.rm),max(x, na.rm = na.rm), length.out = length.out)
  } else {
    if(!is.null(by)){
      o <- seq(min(x, na.rm = na.rm),max(x, na.rm = na.rm), by = by)
    } else {
      stop("Must supply 'length.out' or 'by'.")
    }
  }
  return(o)
}


#' @title Find each empty list element then drop them
#' @description Find each empty list element then drop them
#' @param x list of a bunch of nested lists
#' @return a list
drop_empty_list <- function(x){
  f <- function(x){
    depth <- purrr::pluck_depth(x) - 2
    for(i in 0:depth){
      x <- purrr::map_depth(x, .depth = i, .f = purrr::discard, .p = rlang::is_empty)
    }
    return(x)
  }
  f(f(x))
}

#' @title Turn named matrix into a named vector
#' @description Turn named matrix into a named vector
#' @param x matrix
#' @return a named vector
flatten_mat_name <- function(x){
  cn <- colnames(x)
  rn <- rownames(x)
  if(is.null(cn)){
    cn <- seq_len(ncol(x))
  }
  if(is.null(rn)){
    rn <- seq_len(nrow(x))
  }

  stats::setNames(c(x),
           paste(rep(cn, each = nrow(x)), rn, sep="__")
  )
}



#' @title Reshape an array to long format
#' @description Reshape an array to long format where each row correspond to the index of a value in the nth dimension. The value is stored in the 'val' column.
#' @param x the array
#' @param drop if \code{TRUE}, default is \code{FALSE}, dimensions with length one is removed.
#' @return a data.frame with \code{prod(dim(x))} number of rows and \code{length(dim(x)) + 1} number of columns.
melt <- function (x, drop = FALSE){
  x.dim <- dim(x)
  l <- lapply(
    seq.int(length(x.dim)),
    function(i, name, dim_n){
      temp <- name[[i]]
      if(is.null(temp)){
        temp <- seq.int(dim_n[[i]])
      }
      return(temp)
    },
    name = dimnames(x),
    dim_n = x.dim
  )
  names(l) <- paste0("dim", seq_along(l))



  d <- do.call("expand.grid", l)
  if (drop) {
    d <- d[, x.dim > 1]
  }
  d$val <- c(x)
  return(d)
}


#' @title Reshape an array in long format to an array
#' @description Reshape an array in the long format where each row correspond to the index of a value in the nth dimension and the value is stored in the 'val' column into an nth dimensional array.
#' @param x a matrix or data.frame with named columns. The value of each cell is stored as a 'val' column.
#' @return an array
unmelt <- function(x){
  nm <- colnames(x)

  if(!("val" %in% nm)){
    stop("Expects 'val' as a column in 'x'.")
  }

  nm <- nm[nm != "val"]

  array(
    x[do.call("order", as.list(x[rev(nm)])), "val"],
    dim = Rfast::colMaxs(as.matrix(x[,nm]), value = TRUE)
  )
}


#' @title Set rowname as the first column
#' @description Set rowname as the first column
#' @param x matrix or data.frame
#' @param rn name of the rowname column
#' @return a matrix or data.frame
keep_rowname <- function(x, rn = "rn"){
  res <- cbind("rn" = rownames(x), x)
  colnames(res)[1] <- rn
  res
}


#' @title Convert formula as function
#' @description Convert formula as function
#' @param x the formula
#' @return a function
#' @export
as.function.formula <- function(x) {
  cmd <- as.character(x)[3]
  exp <- parse(text = cmd)
  function(...) eval(exp, list(...))
}
