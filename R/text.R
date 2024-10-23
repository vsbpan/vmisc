# Capitalize the first letter
capitalize <- function(x){
  gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", x, perl = TRUE)
}

# Perform gsub() by element
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
