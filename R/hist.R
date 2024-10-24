loghist.default <- function(x,
                    nclass = function(x){ceiling(log2(length(x)) + 1)},
                    by = NULL,
                    log.p = FALSE,
                    log.x = TRUE,
                    geom = c("line", "col"),
                    linewidth = 1,
                    distr_list = NULL,
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
      breaks <- seq_interval(if(log.x){log(x)} else {x}, by = by, na.rm = TRUE)
      breaks <- c(breaks - by, breaks, breaks + by)
    } else {
      stop("Must supply 'nclass' or 'by' to set the breaks.")
    }
  }

  d <- calc_hist(x, breaks = breaks, log.x = log.x, log.p = log.p, ...)

  if(!log.p && length(geom) == 2){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  g <- d %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = p)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(x = "x", y = "P(x)")

  if(log.x){
    g <- g + ggplot2::scale_x_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(log.p){
    g <- g + ggplot2::scale_y_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(geom == "line"){
    g <- g + ggplot2::geom_line(linewidth = linewidth)
  }

  if(geom == "col"){
    g <- g + ggplot2::geom_col()
  }

  if(!is.null(distr_list)){
    g <- draw_distr(g, distr_list = distr_list, x = unique(d$x), log.x = log.x,
                    linewidth = linewidth, linetype = "dashed")
  }

  return(g)
}


loghist.list <- function(x,
                            nclass = 50,
                            by = NULL,
                            log.p = FALSE,
                            log.x = TRUE,
                            geom = c("line", "col"),
                            linewidth = 1,
                            distr_list = NULL,
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
      if(log.x){
        f <- log
      } else {
        f <- identity
      }
      breaks <- seq_interval(f(range(unlist(x, TRUE, FALSE))), by = by, na.rm = TRUE)
      breaks <- c(breaks - by, breaks, breaks + by)
    } else {
      stop("Must supply 'nclass' or 'by' to set the breaks.")
    }
  }

  nms <- names(x)
  if(is.null(nms)){
    nms <- seq_along(x)
  }

  d <- lapply(seq_along(x), function(i){
    calc_hist(x[[i]], breaks = breaks, log.x = log.x, log.p = log.p, ...) %>%
      cbind("group" = nms[i])
  }) %>%
    do.call("rbind", .)

  if(!log.p && length(geom) == 2){
    geom <- "col"
  } else {
    geom <- match.arg(geom)
  }

  g <- d %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = p)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(x = "x", y = "P(x)")

  if(log.x){
    g <- g + ggplot2::scale_x_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(log.p){
    g <- g + ggplot2::scale_y_continuous(trans = "log10", labels = fancy_scientificb)
  }

  if(geom == "line"){
    g <- g + ggplot2::geom_line(aes(color = group, group = group), linewidth = linewidth)
  }

  if(geom == "col"){
    g <- g + ggplot2::geom_col(aes(group = group, fill = group), position = "dodge")
  }

  if(!is.null(distr_list)){
    g <- draw_distr(g, distr_list = distr_list, x = unique(d$x), log.x = log.x,
                    linewidth = linewidth, linetype = "dashed")
  }

  return(g)
}


calc_hist <- function(x, breaks, log.x, log.p, ...){
  if(log.x){
    p <- hist(log(x), plot = FALSE, breaks = breaks, ...)
    d <- data.frame(
      "x" = exp(p$mids),
      "p" = p$density
    )
  } else {
    p <- hist(x, plot = FALSE, breaks = breaks, ...)
    d <- data.frame(
      "x" = p$mids,
      "p" = p$density
    )
  }

  if(log.p){
    d <- d[d$p > 0,]
  }
  return(d)
}

draw_distr <- function(g, distr_list, x, log.x, linewidth = 1, linetype = "dashed", ...){
  if(is.distr(distr_list)){
    distr_list <- list(distr_list)
  }

  n <- length(distr_list)
  den_data <- vector(mode = "list", length = n)

  for (i in seq_len(n)){
    dist_name <- distr_list[[i]]$name

    den <- do.call(
      paste0("d",dist_name),
      c(
        list(x),
        distr_list[[i]]$params
      )
    )

    den[!is.finite(den)] <- NA_real_

    if(log.x){
      p <- den * x
    } else {
      p <- den
    }

    den_data[[i]] <- data.frame(
      "x" = x,
      "p" = p,
      "dist" = dist_name
    )
  }

  g <- g +
    ggplot2::geom_line(
      data = do.call("rbind", den_data),
      ggplot2::aes(color = dist, x = x, y = p, group = dist),
      linewidth = linewidth,
      linetype = linetype,
      ...
    ) +
    ggplot2::theme(legend.position = "top")
  return(g)
}


