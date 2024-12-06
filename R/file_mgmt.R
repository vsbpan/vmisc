#' @title Check if path is relative to current working directory
#' @description Check if path is relative to current working directory
#' @param x vector of paths
#' @return a logical vector the same length as x
is_relative_path <- function(x){
  !grepl(getwd(), x)
}

#' @title Append working directory to the paths
#' @description Append working directory to the paths
#' @param x vector of paths
#' @return a character vector the same length as x
abs_path <- function(x){
  if(is_relative_path(x)){
    paste(getwd(),x, sep = "/")
  } else {
    x
  }
}

#' @title Create directory if it doesn't exists
#' @description Create directory if it doesn't exists
#' @param root_path path to where the directory should be located
#' @param dir_name name of the directory
#' @return NULL
init_dir <- function(root_path, dir_name){
  stopifnot(length(dir_name) == 1)
  stopifnot(length(root_path) == 1)
  dest_dir <- paste(root_path, dir_name, sep = "/")
  dest_dir <- gsub("//", "/", dest_dir)


  if(dest_dir %in% list.dirs(root_path)){
    message("Directory already exists. Function terminated.")
    return(invisible(NULL))
  } else {
    message(sprintf("Creating directory '%s' ...", dir_name))
    dir.create(dest_dir, showWarnings = TRUE)
    message("Done!")
    return(invisible(NULL))
  }
}

#' @title Move files with `file.rename()` with a progress bar
#' @description Move files with `file.rename()` with a progress bar
#' @param from_dir path where the files should be looked for
#' @param to_dir path to where the files should be moved
#' @param file_names name of files to be moved
#' @param pb if TRUE, show a progress bar
#' @return a logical vector of whether moving each file succeeded
move_files <- function (from_dir, to_dir, file_names, pb = FALSE){
  to_dir <- assert_dir(to_dir)
  from_dir <- assert_dir(from_dir)
  n <- length(file_names)
  if(pb){
    a <- cli::cli_progress_along(seq_len(n),
                                 format = "Moving item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
  } else {
    a <- seq_len(n)
  }
  for (i in a) {
    file.rename(paste(from_dir, file_names[i], sep = "/"),
                paste(to_dir, file_names[i], sep = "/"))
  }
}
