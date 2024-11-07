#' @export
loghist.default <- function(x,
                    nclass = function(x){ceiling(log2(length(x)) + 1)},
                    by = NULL,
                    log.p = FALSE,
                    log.x = TRUE,
                    scale = FALSE,
                    delta = 1,
                    phi = 1,
                    geom = c("line", "col"),
                    linewidth = 1,
                    distr_list = NULL,
                    draw_distr_args = list(
                      linewidth = linewidth,
                      linetype = "dashed",
                      delta = delta,
                      phi = phi,
                      scale = scale
                    ),
                    hist_args = NULL,
                    ...){

  if(!missing(by)){
    nclass <- NULL
  }

  if(!is.null(nclass)){
    if(!is.null(by) && missing(by)){
      stop("Only one of 'nclass' or 'by' should be supplied and the other set to NULL.")
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
        x1 <- log(x1)
      }

      breaks <- seq_interval(x1, by = by, na.rm = TRUE)
      breaks <- c(min(breaks) - by, breaks, max(breaks) + by)
    } else {
      stop("Must supply 'nclass' or 'by' to set the breaks.")
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
      "phi" = phi
    ),
    hist_args
  ))

  if(!log.p && length(geom) == 2){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  if(scale){
    x_lab <- tex("x / \\langle x \\rangle^\\phi")
    y_lab <- tex("x^\\Delta P(x)")
  } else {
    x_lab <- tex("x")
    y_lab <- tex("P(x)")
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

  if(!is.null(distr_list)){
    g <- do.call("draw_distr",
                 c(
                   list("g" = g,
                        "distr_list" = distr_list,
                        "x" = unique(d$x)),
                   draw_distr_args
                 ))
  }

  return(g)
}

#' @export
loghist.list <- function(x,
                         nclass = 50,
                         by = NULL,
                         log.p = FALSE,
                         log.x = TRUE,
                         scale = FALSE,
                         delta = 1,
                         phi = 1,
                         geom = c("line", "col"),
                         linewidth = 1,
                         distr_list = NULL,
                         draw_distr_args = list(
                           linewidth = linewidth,
                           linetype = "dashed",
                           delta = delta,
                           phi = phi,
                           scale = scale
                         ),
                         hist_args = NULL,
                         ...){

  if(!missing(by)){
    nclass <- NULL
  }

  if(!is.null(nclass)){
    if(!is.null(by) && missing(by)){
      stop("Only one of 'nclass' or 'by' should be supplied and the other set to NULL.")
    }
    breaks <- nclass
  } else {
    if(!is.null(by)){
      if(scale){
        x1 <- range(unlist(lapply(x, function(z){z / mean(z, na.rm = TRUE)^phi}), TRUE, FALSE), na.rm = TRUE)
      } else {
        x1 <- range(unlist(x, TRUE, FALSE), na.rm = TRUE)
      }
      if(log.x){
        x1 <- log(x1)
      }

      breaks <- seq_interval(x1, by = by, na.rm = TRUE)
      breaks <- c(min(breaks) - by, breaks, max(breaks) + by)
    } else {
      stop("Must supply 'nclass' or 'by' to set the breaks.")
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
        "phi" = phi
      ),
      hist_args
    )) %>%
      cbind("group" = as.factor(nms[i]))
  }) %>%
    do.call("rbind", .)

  if(!log.p && length(geom) == 2){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  if(scale){
    x_lab <- tex("x / \\langle x \\rangle^\\phi")
    y_lab <- tex("x^\\Delta P(x)")
  } else {
    x_lab <- tex("x")
    y_lab <- tex("P(x)")
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
    g <- g + ggplot2::geom_line(aes(color = group, group = group),
                                linewidth = linewidth,
                                ...)
  }

  if(geom == "col"){
    g <- g + ggplot2::geom_col(aes(group = group, fill = group),
                               position = "dodge",
                               ...)
  }

  if(!is.null(distr_list)){
    g <- do.call("draw_distr",
                 c(
                   list("g" = g,
                        "distr_list" = distr_list,
                        "x" = unique(d$x)),
                   draw_distr_args
                 ))
  }

  return(g)
}


calc_hist <- function(x, breaks,
                      log.x, log.p,
                      scale,
                      delta,
                      phi,
                      ...){
  x <- x[!is.na(x)]

  if(log.x){
    if(scale){
      mu <- mean(x)
      x <- x / mu^phi
      p <- hist(log(x), plot = FALSE, breaks = breaks, ...)
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

draw_distr <- function(g, distr_list, x, linewidth = 1, linetype = "dashed", scale = FALSE, delta, phi, ...){
  if(is.distr(distr_list)){
    distr_list <- list(distr_list)
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

    den <- do.call(
      paste0("d",dist_name),
      c(
        list(x1),
        distr_list[[i]]$params
      )
    )

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


