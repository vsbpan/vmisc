#' @export
loghist.default <- function(x,
                    nclass = function(x){ceiling(log2(length(x)) + 1)},
                    by = NULL,
                    log.p = TRUE,
                    log.x = TRUE,
                    scale = FALSE,
                    delta = 1,
                    phi = 1,
                    geom = c("line", "col", "point"),
                    linewidth = 1,
                    distr_list = NULL,
                    distr_draw_args = list(
                      linewidth = linewidth,
                      linetype = "dashed",
                      delta = delta,
                      phi = phi,
                      scale = scale,
                      boot = FALSE
                    ),
                    hist_args = NULL,
                    show_legend = TRUE,
                    discrete = FALSE,
                    ...){

  if(!missing(by)){
    nclass <- NULL
  }

  if(!is.null(nclass)){
    if(!is.null(by) && missing(by)){
      cli::cli_abort("Only one of {.arg nclass} or {.arg by} should be supplied and the other set to {.val NULL}")
    }
    breaks <- nclass
  } else {
    if(!is.null(by)){
      if(scale){
        x1 <- x / mean(x, na.rm = TRUE)^phi
      } else {
        x1 <- x
      }
      if(log.x){
        if(any(x1 == 0)){
          cli::cli_warn("Dropped {sum(stats::na.omit(x1) == 0)} zero value{?s}.")
          x1 <- omit_zero(x1)
        }
        x1 <- log(x1)
      }

      breaks <- seq_interval(x1, by = by, na.rm = TRUE)
      breaks <- c(min(breaks, na.rm = TRUE) - by, breaks, max(breaks, na.rm = TRUE) + by)
    } else {
      cli::cli_abort("Must supply {.arg nclass} or {.arg by} to set the breaks.")
    }
  }

  d <- do.call("calc_hist", c(
    list(
      "x" = x,
      "breaks" = breaks,
      "log.x" = log.x,
      "log.p" = log.p,
      "scale" = scale,
      "delta" = delta,
      "phi" = phi,
      "discrete" = discrete
    ),
    hist_args
  ))

  if(!log.p && length(geom) == 3){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  if(scale){
    x_lab <- tex("$x / \\langle x \\rangle^\\phi$")
    y_lab <- tex("$x^\\Delta P(x)$")
  } else {
    x_lab <- tex("$x$")
    y_lab <- tex("$P(x)$")
  }

  g <- d %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = p)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(x = x_lab, y = y_lab)

  if(log.x){
    g <- g + ggplot2::scale_x_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(log.p){
    g <- g + ggplot2::scale_y_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(geom == "line"){
    g <- g + ggplot2::geom_line(linewidth = linewidth, ...)
  }

  if(geom == "col"){
    g <- g + ggplot2::geom_col(...)
  }

  if(geom == "point"){
    g <- g + ggplot2::geom_point(...)
  }

  if(!is.null(distr_list)){
    g <- do.call("distr_draw",
                 c(
                   list("g" = g,
                        "distr_list" = distr_list,
                        "x" = unique(d$x),
                        "discrete" = discrete
                        ),
                   distr_draw_args
                 ))
  }

  if(!isTRUE(show_legend)){
    g <- g + ggplot2::theme(legend.position = "none")
  }

  return(g)
}

#' @export
loghist.list <- function(x,
                         nclass = 50,
                         by = NULL,
                         log.p = TRUE,
                         log.x = TRUE,
                         scale = FALSE,
                         delta = 1,
                         phi = 1,
                         geom = c("line", "col", "point"),
                         linewidth = 1,
                         distr_list = NULL,
                         distr_draw_args = list(
                           linewidth = linewidth,
                           linetype = "dashed",
                           delta = delta,
                           phi = phi,
                           scale = scale,
                           boot = FALSE
                         ),
                         hist_args = NULL,
                         show_legend = TRUE,
                         discrete = FALSE,
                         ...){

  if(!missing(by)){
    nclass <- NULL
  }

  if(!is.null(nclass)){
    if(!is.null(by) && missing(by)){
      cli::cli_abort("Only one of {.arg nclass} or {.arg by} should be supplied and the other set to {.val NULL}")
    }
    breaks <- nclass
  } else {
    if(!is.null(by)){
      x1 <- unlist(x, TRUE, FALSE)
      if(log.x){
        if(any(x1 == 0)){
          cli::cli_warn("Dropped {sum(stats::na.omit(x1) == 0)} zero value{?s}.")
        }
      }
      if(scale){
        x1 <- range(unlist(lapply(x, function(z){
          z <- omit_zero(z)
          z / mean(z, na.rm = TRUE)^phi
          }), TRUE, FALSE), na.rm = TRUE)
      }
      if(log.x){
        x1 <- log(omit_zero(x1))
      }

      breaks <- seq_interval(x1, by = by, na.rm = TRUE)
      breaks <- c(min(breaks, na.rm = TRUE) - by, breaks, max(breaks, na.rm = TRUE) + by)
    } else {
      cli::cli_abort("Must supply {.arg nclass} or {.arg by} to set the breaks.")
    }
  }

  nms <- names(x)
  if(is.null(nms)){
    nms <- seq_along(x)
  }

  d <- lapply(seq_along(x), function(i){
    do.call("calc_hist", c(
      list(
        "x" = x[[i]],
        "breaks" = breaks,
        "log.x" = log.x,
        "log.p" = log.p,
        "scale" = scale,
        "delta" = delta,
        "phi" = phi,
        "discrete" = discrete
      ),
      hist_args
    )) %>%
      cbind("group" = as.factor(nms[i]))
  }) %>%
    do.call("rbind", .)

  if(!log.p && length(geom) == 3){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  if(scale){
    x_lab <- tex("$x / \\langle x \\rangle^\\phi$")
    y_lab <- tex("$x^\\Delta P(x)$")
  } else {
    x_lab <- tex("$x$")
    y_lab <- tex("$P(x)$")
  }

  g <- d %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = p)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(x = x_lab, y = y_lab)

  if(log.x){
    g <- g + ggplot2::scale_x_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(log.p){
    g <- g + ggplot2::scale_y_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(geom == "line"){
    g <- g + ggplot2::geom_line(ggplot2::aes(color = group, group = group),
                                linewidth = linewidth,
                                ...)
  }

  if(geom == "col"){
    g <- g + ggplot2::geom_col(ggplot2::aes(group = group, fill = group),
                               position = "dodge",
                               ...)
  }

  if(geom == "point"){
    g <- g + ggplot2::geom_point(ggplot2::aes(color = group, group = group), ...)
  }

  if(!is.null(distr_list)){
    g <- do.call("distr_draw",
                 c(
                   list("g" = g,
                        "distr_list" = distr_list,
                        "x" = unique(d$x),
                        "discrete" = discrete),
                   distr_draw_args
                 ))
  }

  if(!isTRUE(show_legend)){
    g <- g + ggplot2::theme(legend.position = "none")
  }

  return(g)
}


calc_hist <- function(x, breaks,
                      log.x, log.p,
                      scale,
                      delta,
                      phi,
                      discrete,
                      ...){
  x <- x[!is.na(x)]

  if(log.x){
    x <- omit_zero(x)
    if(scale){
      mu <- mean(x)
      x <- x / mu^phi
      p <- hist(log(x), plot = FALSE, breaks = breaks, ...)
      if(isTRUE(discrete)){
        p$density <- p$counts / sum(p$counts) * exp(p$mids)^(delta-1)
      }
      d <- data.frame(
        "x" = exp(p$mids),
        "p" = p$density * exp(p$mids)^(delta-1)
      )
      attr(d, "scaling") <- list(
        "mu" = mu,
        "phi" = phi,
        "delta" = delta
      )
    } else {
      p <- hist(log(x), plot = FALSE, breaks = breaks, ...)
      if(isTRUE(discrete)){
        p$density <- p$counts / sum(p$counts)  * exp(p$mids)
      }
      d <- data.frame(
        "x" = exp(p$mids),
        "p" = p$density / exp(p$mids)
      )
    }

  } else {
    if(scale){
      mu <- mean(x)
      x <- x / mu^phi
      p <- hist(x, plot = FALSE, breaks = breaks, ...)
      if(isTRUE(discrete)){
        p$density <- p$counts / sum(p$counts)
      }
      d <- data.frame(
        "x" = p$mids,
        "p" = p$density * p$mids^delta
      )
      attr(d, "scaling") <- list(
        "mu" = mu,
        "phi" = phi,
        "delta" = delta
      )
    } else {
      p <- hist(x, plot = FALSE, breaks = breaks, ...)
      if(isTRUE(discrete)){
        p$density <- p$counts / sum(p$counts)
      }
      d <- data.frame(
        "x" = p$mids,
        "p" = p$density
      )
    }
  }

  if(log.p){
    d <- d[d$p > 0,]
  }
  return(d)
}

distr_draw <- function(g, distr_list, x, discrete = FALSE,
                       linewidth = 1, linetype = "dashed", scale = FALSE,
                       boot = FALSE,
                       delta, phi, ...){
  if(is.distr(distr_list)){
    distr_list <- list(distr_list)
  }
  if(is.numeric(boot)){
    distr_list <- rep(distr_list, boot)
  }

  n <- length(distr_list)
  den_data <- vector(mode = "list", length = n)


  for (i in seq_len(n)){
    dist_name <- distr_list[[i]]$name

    if(scale){
      mu <- distr_mu(distr_list[[i]])
      x1 <- x * mu^phi
    } else {
      x1 <- x
    }
    if(isTRUE(discrete)){
      x1 <- round(x1)
    }

    if(is.numeric(boot)){
      den <- ddistr(x1, distr_list[[i]], boot = TRUE)
    } else {
      den <- ddistr(x1, distr_list[[i]], boot = FALSE)
    }

    if(scale){
      den <- den * x1^delta
    }

    den[!is.finite(den)] <- NA_real_


    den_data[[i]] <- data.frame(
      "x" = x,
      "p" = den,
      "dist" = dist_name,
      "distID" = paste0(dist_name,"-",i)
    )
  }

  g <- g +
    ggplot2::geom_line(
      data = do.call("rbind", den_data),
      ggplot2::aes(color = dist, x = x, y = p, group = distID),
      linewidth = linewidth,
      linetype = linetype,
      ...
    ) +
    ggplot2::theme(legend.position = "top")
  return(g)
}


