#' @title Format seconds to readable format
#' @description
#' Format seconds to readable format
#' @param x seconds
#' @return a character string
#' @rdname sec_format
# Format seconds into nice H:S:M format
hms_runtime <- function(x){
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  s <- floor(x - h * 3600 - m * 60)
  cat(sprintf("\nRuntime %02d:%02d:%02d\n", h,m,s))
}

# Format seconds into nice H:M:S format
#' @rdname sec_format
hms_format <- function(x){
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  s <- floor(x - h * 3600 - m * 60)
  return(sprintf("%02d:%02d:%02d", h,m,s))
}

# Format seconds into nice H:M format
#' @rdname sec_format
hm_format <- function(x){
  h <- floor(x / 3600)
  m <- round((x - h * 3600) / 60)
  return(sprintf("%02d:%02d", h,m))
}

#' @title Return stars based on p value.
#' @description
#' Return stars based on p value like in `summary.lm()`
#' @param x a vector of p values
#' @return a character vector of stars
stars_pval <- function(x){
  if(is.null(x)){
    return("")
  }
  x <- as.numeric(x)
  x[is.na(x)] <- 1
  stars <- c("***", "**", "*", ".","")
  var <- c(0, 0.001, 0.01, 0.05, 0.10, 1)
  i <- findInterval(x, var, left.open = TRUE, rightmost.closed = TRUE)
  stars[i]
}

#' @title Left align text
#' @description Left align text
#' @param x a vector of character strings
#' @return a vector of character strings
left_align_text <- function(x){
  n <- max(nchar(x))
  vapply(x, function(z) paste0(z, paste0(rep(" ", n - nchar(z)), collapse = "")), character(1))
}


#' @title Find the significant figures
#' @description Find the significant figures
#' @param x a vector of numeric values
#' @param digits an atomic integer indicating the number of significant figures.
#' @return a character vector
sigfig <- function(x, digits = 2){
  if(x == 0){
    return(paste0("0.",paste0(rep("0", digits), collapse = "")))
  }

  round_to <- digits - ceiling(log10(abs(x)))

  value <- round(x, round_to)

  ## accuracy is expected to be (e.g.) 0.01 rather than, say, 3
  accuracy_arg <- 1 / 10^round_to

  ## Construct a labeller within the function run
  labeller <- scales::label_number(accuracy = accuracy_arg)

  label <- labeller(value)

  return(label)
}

#' @title Find object size in pretty format
#' @description Find object size in pretty format
#' @param x the object
#' @return pretty object size
pretty_size <- function(x){
  prettyunits::pretty_bytes(utils::object.size(x))
}
