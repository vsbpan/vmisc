# Format seconds into nice H:S:M format
hms_runtime <- function(x){
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  s <- floor(x - h * 3600 - m * 60)
  cat(sprintf("\nRuntime %02d:%02d:%02d\n", h,m,s))
}

# Format seconds into nice H:M:S format
hms_format <- function(x){
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  s <- floor(x - h * 3600 - m * 60)
  return(sprintf("%02d:%02d:%02d", h,m,s))
}

# Format seconds into nice H:M format
hm_format <- function(x){
  h <- floor(x / 3600)
  m <- round((x - h * 3600) / 60)
  return(sprintf("%02d:%02d", h,m))
}

# Return starts based on p value
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

# Left align text
left_align_text <- function(x){
  n <- max(nchar(x))
  vapply(x, function(z) paste0(z, paste0(rep(" ", n - nchar(z)), collapse = "")), character(1))
}


# Wrapper for formatC()
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


