#' @title Compute the mean of distribution via simulation
#' @description Compute the mean of distribution by finding the sample mean of 10,000 draws from the distribution
#' @param distr a 'distr' object
#' @return an atomic numeric vector
distr_mu <- function(distr){
  warnifnot(is.distr(distr))
  assert_distr(distr)
  dist_name <- distr$name
  param <- distr$param
  av_fun <- distr_fun(distr)
  if(av_fun$r){
    # Simulation based mean. Good enough for now. Need to add analytical and integration methods.
    mu <- mean(do.call(paste0("r", dist_name), c(list("n" = 10000), param)))
  } else {
    cli::cli_abort("Need {.fn r{dist_name}} to simulate mu. Analytical methods not available.")
  }
  return(mu)
}

#' @title Check whether density function, random number generation, cumulative distribution function, and quantile function are available for the specific distribution.
#' @description  Check whether density function, random number generation, cumulative distribution function, and quantile function are available for the specific distribution.
#' @param x a 'distr' object or a character string of the name of the distribution.
#' @return a list of atomic boolean
distr_fun <- function(x){
  if(is.distr(x) || !is.character(x)){
    dist_name <- x$name
  } else {
    dist_name <- x
  }
  o <- list(
    "d" = has_function(paste0("d",dist_name)),
    "r" = has_function(paste0("r",dist_name)),
    "p" = has_function(paste0("p",dist_name)),
    "q" = has_function(paste0("q",dist_name))
  )
  return(o)
}


distr_fit <- function(x, name, as_distr = TRUE, start = NULL, fix.arg = NULL, ...){
  if(!is.numeric(x)){
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }

  if(is.null(start)){
    start <- auto_start(x, name = name, start = start)
    start <- start[!names(start) %in% names(fix.arg)]
  }

  res <- fitdistrplus::fitdist(as.vector(x, mode = "numeric"), name, start = start, fix.arg = fix.arg, ...)
  if(as_distr){
    if(!is.null(res$fix.arg)){
      res$estimate <- c(res$estimate, res$fix.arg)
      n <- ncol(res$vcov)
      n_new <- length(res$estimate)
      res$vcov <- cbind(res$vcov, matrix(rep(0, (n_new - n) * n), nrow = n, ncol = (n_new - n)))
      res$vcov <- rbind(res$vcov, matrix(rep(0, n_new * (n_new - n)), nrow = n_new - n, ncol = n_new))
      rownames(res$vcov) <- colnames(res$vcov) <- names(res$estimate)
      fanams <- names(res$fix.arg)
    } else {
      fanams <- NULL
    }

    res <- distr_make(res$distname, as.list(res$estimate), res$vcov)
    attr(res, "fix.arg") <- fanams
  }
  return(res)
}


distr_make <- function(name, param = NULL, vcv = NULL){
  res <- list("name" = name,
              "param" = param,
              "vcv" = vcv)
  class(res) <- c("distr","list")
  res
}

rdistr <- function(n, distr, boot = FALSE, ...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("r", distr$name),
    c(
      list("n" = n),
      distr$param,
      list(...)
    )
  )
}

ddistr <- function(x, distr, boot = FALSE,...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("d", distr$name),
    c(
      list("x" = x),
      distr$param,
      list(...)
    )
  )
}

pdistr <- function(q, distr, boot = FALSE,...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("p", distr$name),
    c(
      list("q" = q),
      distr$param,
      list(...)
    )
  )
}

qdistr <- function(p, distr, boot = FALSE, ...){
  assert_distr(distr)
  if(isTRUE(boot)){
    distr <- distr_boot(distr)
  }
  do.call(
    paste0("q", distr$name),
    c(
      list("p" = p),
      distr$param,
      list(...)
    )
  )
}

# Not very fast for large number of bootstraps. But okay for now.
distr_boot <- function(distr){
  assert_distr(distr)
  if(!is.null(distr$vcv)){
    nms <- names(distr$param)
    fix.arg <- attr(distr, "fix.arg")
    fix.arg.val <- distr$param[fix.arg]
    boot_nms <- nms[!nms %in% fix.arg]

    distr$param <- as.list(
      mvnfast::rmvn(1, mu = do.call("c", distr$param)[boot_nms],
                    sigma = distr$vcv[boot_nms,boot_nms, drop = FALSE], kpnames = TRUE)
    )
    distr$param <- c(distr$param, fix.arg.val)
    names(distr$param) <- nms # Somtimes mvnfast::rmvn() fails to pass the names
    attr(distr, "boot") <- TRUE
  } else {
    cli::cli_abort("Sorry, no variance-covariance matrix to perform bootstrapping.")
  }
  distr
}


.prep_multi_fit_args <- function(dist, fix.arg){
  if(length(dist) == 1 && is.character(dist) && isFALSE(any(sapply(fix.arg, function(x){any(is.character(x))})))){
    start <- NULL

    fix.arg.final <- list(fix.arg)
    names(fix.arg.final) <- dist

  } else if(!is.character(dist) && is.list(dist)){
    start <- dist
    dist <- names(dist)
    if(!is.null(fix.arg)){
      if(!is.list(fix.arg) || is.null(names(fix.arg))){
        cli::cli_abort("{.arg fix.arg} must be a named list with fixed argument as values and distribution as names.")
      }
      if(any(!names(fix.arg) %in% names(dist))){
        msnames <- names(fix.arg)[!names(fix.arg) %in% names(dist)]
        cli::cli_warn("Cannot find {msnames} in the list of distributions provided by {.arg dist}")
      }
      fix.arg.final <- vector(mode = "list", length = length(start))
      names(fix.arg.final) <- names(start)
      for (i in seq_along(start)){
        a <- names(start)[i]
        fix.arg.nms <- unlist(fix.arg[[a]], FALSE, FALSE)
        if(is.null(fix.arg.nms)){
          next
        } else {
          arg.nms.full <- names(start[[a]])
          if(is.null(start[[i]][fix.arg.nms]) ||
             is.null(unlist(start[[i]][fix.arg.nms])) ||
             any(!fix.arg.nms %in% arg.nms.full)){
            if(is.null(is.null(start[[i]][fix.arg.nms]))){
              problem_args <- fix.arg.nms
            } else {
              problem_args <- fix.arg.nms[!fix.arg.nms %in% arg.nms.full]
              problem_args <- c(problem_args, fix.arg.nms[do.call("c", lapply(start[[i]][fix.arg.nms], is.null))])

            }
            cli::cli_abort("Cannot find the value to fix at for the {.arg {unique(problem_args)}} argument{?s} of the distribution {.val {a}}")
          }
          fix.arg.final[[i]] <- start[[a]][arg.nms.full %in% fix.arg.nms]
          start[[a]] <- start[[a]][!arg.nms.full %in% fix.arg.nms]
        }
      }

    }
  } else {
    start <- NULL
    if(!is.null(fix.arg)){
      cli::cli_warn("Ignored {.arg fix.arg} because there is no specified value to fix at.")
    }
    fix.arg.final <- NULL
  }
  return(
    list(
      "dist" = dist,
      "start" = start,
      "fix.arg" = fix.arg.final
    )
  )
}

distr_multi_fit <- function(x, dist, fix.arg = NULL, as_distr = FALSE, ...){
  if(!is.numeric(x)){
    cli::cli_abort("{.arg x} of class {.cls {class(x)}} is not the expected {.cls numeric}.")
  }
  x <- x[!is.na(x)]

  args <- .prep_multi_fit_args(dist = dist, fix.arg = fix.arg)

  l <- lapply(args$dist, function(d){

    fit_args <- list("x" = x,
                     "name" = d,
                     as_distr = as_distr,
                     ...)

    if(!is.null(start)){
      fit_args <- c(fit_args, list("start" = args$start[[d]], "fix.arg" = args$fix.arg[[d]]))
    }


    fit <- do.call(
      "distr_fit",
      fit_args
    )
    fit
  })
  names(l) <- args$dist
  return(l)
}

# fixed.arg should be a named list of character vectors of which arguments to fix at the starting value
distr_comp <- function(x, dist = c("gamma", "lnorm"), criterion = "AICc", fix.arg = NULL, ...){
  if(!is.numeric(x)){
    cli::cli_abort("{.arg x} of class {.cls {class(x)}} is not the expected {.cls numeric}.")
  }
  x <- x[!is.na(x)]
  n <- length(x)

  l <- distr_multi_fit(x, dist = dist, fix.arg = fix.arg, as_distr = FALSE, ...)

  l <- lapply(l, function(fit){
    res <- logLik(fit)
    class(res) <- "logLik"
    attr(res, "nall") <- n
    attr(res, "nobs") <- n
    attr(res, "df") <- length(fit$estimate)
    res
  })

  out <- lapply(
    seq_along(l), function(i){
      cri <- do.call(criterion, list(l[[i]]))
      df <- attributes(l[[i]])$df
      data.frame(model = names(l)[i], cri = cri, df = df)
    }
  ) %>%
    do.call("rbind", .)

  out$dcri <- out$cri - min(out$cri)
  out <- out[order(out$cri), ]
  names(out)[2] <- criterion
  names(out)[4] <- paste0("d",criterion)

  return(out)
}


distr_ks_test <- function(x, name, simulate = 1, simplify = TRUE,
                          test.args = list(
                            alternative = c("two.sided", "less", "greater"),
                            exact = NULL, simulate.p.value = FALSE, B = 2000
                          ), ...){
  fit <- distr_fit(x = x, name = name, ...)
  env <- sys.frame(sys.parent())

  test.args <- vmisc::append_default_args(test.args)

  if(!isFALSE(simulate)){
    assert_atomic_type(simulate, "numeric", null_as_is = FALSE, NA_as_is = FALSE)
    res <- lapply(seq_len(simulate), function(i){
      res <- do.call("ks.test", c(
        list(
          "x" = x,
          "y" = rdistr(length(x), fit, boot = FALSE)
        ),
        test.args
      ), envir = env)
      res$method <- paste0(res$method, sprintf(" (boot %s)", i))
      res
    })
  } else {
    res <- list(
      do.call("ks.test", c(
        list(
          "x" = x,
          "y" = paste0("p", name)
        ),
        fit$param,
        test.args
      ), envir = env)
    )
  }

  dataname <- deparse(substitute(x))
  res <- lapply(res, function(x){
    x$data.name <- dataname
    x
  })

  if(simplify){
    out <- res[[1]]

    stat <- do.call("c", lapply(res, function(x){x$statistic}))
    p <- do.call("c", lapply(res, function(x){x$p.value}))
    new_p <- fisher_method(p)

    out$statistic <- mean(stat)
    names(out$statistic) <- unique(names(stat))
    stat <- unname(stat)
    out$p.value <- new_p$P

    out$fisher_method <- new_p
    out$sim <- cbind(
      "statistic" = stat,
      "p" = p
    )

    out$method <- gsub("\\(boot 1\\)",sprintf("(combined %s boot)", length(res)), out$method)

    res <- out
  }

  return(res)
}


distr_pp <- function(x, dist, fix.arg = NULL, fit_args = NULL, ...){
  l <- do.call("distr_multi_fit", c(list(
    x = x,
    dist = dist,
    fix.arg = fix.arg,
    as_distr = FALSE
  ), fit_args), envir = sys.frame(sys.parent()))

  fitdistrplus::ppcomp(l, ...)
}

distr_qq <- function(x, dist, fix.arg = NULL, fit_args = NULL, ...){
  l <- do.call("distr_multi_fit", c(list(
    x = x,
    dist = dist,
    fix.arg = fix.arg,
    as_distr = FALSE
  ), fit_args), envir = sys.frame(sys.parent()))

  fitdistrplus::qqcomp(l, ...)
}

distr_cdf <- function(x, dist, fix.arg = NULL, fit_args = NULL, ...){
  l <- do.call("distr_multi_fit", c(list(
    x = x,
    dist = dist,
    fix.arg = fix.arg,
    as_distr = FALSE
  ), fit_args), envir = sys.frame(sys.parent()))

  fitdistrplus::cdfcomp(l, ...)
}

auto_start_env <- new.env()

auto_start <- function(x, name, start){
  if(!is.null(start)){
    return(start)
  }
  start <- get0(name, envir = auto_start_env)

  if(!is.null(start)){
    if(!is.list(start)){
      cli::cli_warn("Auto-start object of class {.cls {class(start)}} is not {.cls list} for distribution {.val {name}}.")
      start <- NULL
    }
    if(!is.null(start) && is.null(names(start))){
      cli::cli_warn("Missing argument name for distribution {.val {name}}.")
      start <- NULL
    }
    if(!is.null(start)){
      is_fun <- sapply(start, function(x){is.function(x)})
      is_num <- sapply(start, function(x){is.numeric(x)})

      if(!all(is_fun | is_num)){
        invalid <- class(
          sapply(start, class)[sapply(start, function(x){is.function(x) | is.numeric(x)})]
        )
        cli::cli_warn("Auto-start list elements must be a function or numeric, but the invalid class{?es} {.cls {invalid}} found for distribution {.val {name}}.")
        start <- NULL
      }
    }

    if(!is.null(start) && any(is_fun)){
      for(i in which(is_fun)){
        start[[i]] <- start[[i]](x)
      }
    }
  }


  if(is.null(start)){
    start <- fitdistrplus:::startargdefault(x, name)
  }
  return(start)
}

auto_start_register <- function(name, ...){
  l <- list(...)
  assign(name, l, envir = auto_start_env)
  cli::cli_alert_success("Successfully registered auto-start arguments for the {.val {name}} distribution: {.var  {names(l)}}")
  invisible(NULL)
}


