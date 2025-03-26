#' @title lapply with multi-session parallel evaluation using doParallel backend
#' @description
#' Same deal as `lapply()` but allows for parallel computing, progress bar, and fake package compatability handling.
#' @param x a vector
#' @param FUN a function to be evaluated on each element of x
#' @param cores number of parallel sessions used for function evaluation of vector elements.
#' @param inorder If TRUE (default), each function evaluation of vector elements must be in the same order as they are supplied
#' @param export_fun_only If TRUE (default), only functions in the global environment are exported to parallel sessions
#' @param fake_pkg_root_path the path of the fake package
#' @param ... additional arguments passed to FUN
#' @return a list
pb_par_lapply <- function(x, FUN,
                          cores = 1,
                          ...,
                          inorder = TRUE,
                          export_fun_only = TRUE,
                          fake_pkg_root_path = getwd()){

  has_clust <- inherits(cores, "cluster")

  if(!has_clust && (is.null(cores) || is.na(cores) || cores <= 1 || isFALSE(cores))){
    if(is.list(x)){
      indf <- function(x,i){
        x[[i]]
      }
    } else {
      indf <- function(x,i) {
        x[i]
      }
    }

    n <- length(x)

    out <- lapply(cli::cli_progress_along(seq_along(x),
                                          format = "Processing item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"), FUN = function(i){
                                            FUN(indf(x, i), ...)
                                          })

  } else {

    if(!has_clust){
      message(sprintf("\nInitializing %s parallel workers. . .", cores))

      cl <- parallel::makeCluster(cores, outfile = "")
      doSNOW::registerDoSNOW(cl)
    }

    # Remove fake_pkg package from list. foreach::`%dopar%` calls library(package) as some point, which would give an error
    pkg <- .packages()
    fake_pkgs <- fake_pkg(fake_pkg_root_path)
    pkg <- pkg[!pkg %in% fake_pkgs]
    fake_pkg_path <- path.package(fake_pkgs)
    load_fake_pkg <- load_all2


    environment(FUN) <- environment()
    indices <- seq_along(x)

    if(export_fun_only){
      # Export only functions in the global environment (faster)
      export <- do.call("c",lapply(ls(globalenv()),
                                   function(x){
                                     switch(is.function(get(x)), x, NULL)
                                   }))
    } else {
      # Otherwise export other global variables as well
      export <- ls(globalenv())
    }

    cur_env <- environment()

    cli::cli_progress_bar(format = "Processing item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                          total = length(x), .envir = cur_env)

    out <- tryCatch(foreach::foreach(
      i = x, # Passing large list directly as elements to avoid memory overflow
      .export = export,
      .combine = c,
      .verbose = FALSE,
      .inorder = inorder,
      .options.snow = list(
        progress = function(n){
          cli::cli_progress_update(1, .envir = cur_env)
        }
      ),
      .final = function(x){
        if(!has_clust){
          message("\nClosing parallel workers. . .")
          parallel::stopCluster(cl)
          has_clust <- TRUE
        }
        invisible(x)
      },
      .packages = pkg
    ) %dopar% {
      load_fake_pkg(path = fake_pkg_path,
                      export_all = TRUE,
                      quiet = TRUE)
      list(FUN(i, ...))
    }, error = function(e){

      if(!has_clust){
        message(e)
        message("\nClosing parallel workers. . .")
        parallel::stopCluster(cl)
      }
    })
  }

  on.exit({
    cli::cli_process_done()
    cli::cli_progress_done()
  })

  return(out)
}


#' @title Timeout wrapper
#' @description
#' Evaluate an expression within a limited time. If timed out, throws an error.
#' @param expr an expression to be evaluated
#' @param substitute If true (default), substitute the expression
#' @param envir In what environment to evaluate the expression? Default to parent environment.
#' @param timeout How many seconds to wait before timing out
#' @param cpu,elapsed double (of length one). Set a limit on the total or elapsed CPU time in seconds, respectively.
#' @param ... additional arguments not passed to anything at the moment.
#' @return the output of the expression
with_timeout <- function(expr, substitute = TRUE, envir = parent.frame(), timeout,
                         cpu = timeout, elapsed = timeout, ...){
  if (substitute)
    expr <- substitute(expr)
  if (!is.environment(envir))
    stop("Argument 'envir' is not a list: ", class(envir)[1L])
  stopifnot(elapsed > 0)
  setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch({
    eval(expr, envir = envir, enclos = baseenv())
  }, error = function(ex) {
    stop(sprintf("\nError in %s: %s (%s)",
                 paste0(deparse(expr,nlines = -1), collapse = "\n"),
                 ex$message,
                 hms_format(timeout)), call. = FALSE)
  })
}

#' @title lapply with append_name()
#' @description
#' lapply() but with names(z) <- x
#' @param x a vector
#' @param FUN a function to be applied to each element of the vector x
#' @param ... additional arguments
#' @return a list
lapply_name <- function(x, FUN, ...){
  z <- lapply(x, FUN, ...)
  names(z) <- x
  z
}


dist2 <- function(l, FUN, is_symmetric = TRUE, cores = 1){
  FUN <- match.fun(FUN)
  n <- length(l)


  if(is_symmetric){
    grid <- expand.grid("a" = seq_along(l), "b" = seq_along(l)) %>%
      dplyr::mutate(
        key = paste(pmin(a, b), pmax(a, b), sep = "-"),
        dup = duplicated(key) | a == b,
        i = cumsum(!dup)
      ) %>%
      as.data.frame()

    n2 <- choose(n,2)
    res <- pb_par_lapply(seq_len(nrow(grid)), function(i, grid){
      if(grid[i, "dup"]){
        return(NA_real_)
      } else {
        FUN(
          l[[grid[i,1]]],l[[grid[i,2]]]
        )
      }
    }, grid = grid, cores = cores) %>%
      unlist() %>%
      matrix(nrow = n,
             ncol = n,
             dimnames = list(
               names(l),
               names(l)
             ))

    res <- as.matrix(as.dist(res))
  } else {
    grid <- expand.grid(seq_along(l), seq_along(l))
    n2 <- n^2
    res <- pb_par_lapply(seq_len(nrow(grid)), function(i, grid){
      FUN(
        l[[grid[i,1]]],l[[grid[i,2]]]
      )
    }, grid = grid, cores = cores) %>%
      unlist() %>%
      matrix(nrow = n,
             ncol = n,
             dimnames = list(
               names(l),
               names(l)
             ))
  }

  res <- stats::as.dist(res, diag = TRUE, upper = TRUE)
  return(res)
}


#' @title Conditional map to each nest element of a nested list
#' @description
#' Apply a function .f to each nested element of a list x that satisfy the condition .p(.x).
#' @param x a vector
#' @param .f a function to be evaluated on each element of x if .p is TRUE
#' @param .p a condition to test on each element of x
#' @param .f2 a function to be evaluated on each element of x if .p is FALSE. Default to identity
#' @return the output of the expression
map_if_depth <- function(x, .f, .p, .f2 = identity){
  depth <- purrr::pluck_depth(x) - 1
  f2 <- function(.x, .p){
    if(.p(.x)){
      .f(.x)
    } else {
      .f2(.x)
    }
  }
  for(i in 0:depth){
    x <- purrr::map_depth(x, .depth = i, .f = f2, .p = .p)
  }
  return(x)
}

