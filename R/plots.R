#' @title Rank size plot
#' @description Generate a rank size plot
#' @param x a vector of numeric values greater than 0
#' @param omit_zero If TRUE (default), remove 0 from x.
#' @param add a logical value indicating whether to overlay points upon an existing plot
#' @param plot a logical value indicating whether to plot the results.
#' @param ... additional arguments passed to \code{points()} or \code{plot()}
#' @return a data.frame of the rank and log values
plot_rank_size <- function(x, omit_zero = TRUE, add = FALSE, plot = TRUE, ...){
  if(omit_zero){
    x <- omit_zero(x)
  }

  stopifnot(all(x > 0))
  #x <- x/sum(x)
  r <- log(length(x) - rank(x))
  v <- log(x)


  if (plot) {
    if (add) {
      graphics::points(v ~ r, ...)
    }
    else {
      plot(v ~ r, ylab = expression(ln(x)),
           xlab = expression(ln(rank(x))), ...)
    }
  }
  invisible(data.frame(log.rank = r, log.val  = v))
}


#' @title Survival Plot
#' @description Generate a log-log survival plot of positive only sample. Can be useful for visualizing the tail of the distribution.
#' @param x a vector of numeric values greater than 0
#' @param add a logical value indicating whether to overlay points upon an existing plot
#' @param plot a logical value indicating whether to plot the results. Set to \code{FALSE} to just getting the empirical cumulative distribution function data.frame.
#' @param ... additional arguments passed to \code{points()} or \code{plot()}
#' @return a data.frame of the empirical cumulative distribution function
#' @examples
#' x <- rlnorm(10000,1,1)
#' plot_survival(x)
#'
#' x2 <- rlnorm(10000,1,1.1)
#' plot_survival(x2, add = TRUE, col = "green", pch = 19)
#'
#' @references Aban, I. B., M. M. Meerschaert, and A. K. Panorska. 2006. Parameter Estimation for the Truncated Pareto Distribution. Journal of the American Statistical Association 101:270–277.
plot_survival <- function(x, add = FALSE, plot = TRUE, ...){
  stopifnot(all(x > 0))
  x2 <- sort(x)
  vals <- unique(x2)
  rval <- cumsum(tabulate(match(x2,vals)))/length(x2)

  if(plot){
    if(add){
      graphics::points(log(1-rval)~log(vals), ...)
    } else {
      plot(log(1-rval)~log(vals),
           xlab = expression(ln(x)),
           ylab = expression(ln(P(X>x))),
           ...)
    }

  }
  invisible(data.frame("ecdf" = rval, "x" = vals))
}


#' @title Get Biplot or Triplot
#' @description Generate biplot or triplot using ggplot2.
#' @param x an object that is supported by \code{vegan::scores()}
#' @param choices a vector of length 2 defining the axes to plot
#' @param scaling scaling argument passed to \code{vegan::scores()}
#' @param display a vector of characters defining what to plot
#' @param ellipse a character string passed to the \code{type} argument of \code{ggplot2::stat_ellipse()}. If \code{NA}, no ellipse is plotted.
#' @param group a vector with the same length and order as the sites data used to fit the model that is used to color the site points.
#' @param group2 a vector with the same length and order as the sites data used to fit the model that is used to determine the shape of the site points
#' @param sites_alpha the transparency of the sites plotted as points
#' @param sites_size the size pf the site points
#' @param group_as_aes if \code{TRUE} (default is TRUE), \code{group} is passed as an aesthetics via \code{ggplot2::aes()}.
#' @param species_color the color of the species labels
#' @param species_alpha the transparency of the species plotted as text
#' @return a ggplot object
ggbiplot <- function (x, choices = c(1, 2), scaling = 2, display = c("sites",
                                                                     "species", "biplot", "centroids"),
                      ellipse = NA, group = NULL,
                      group2 = NULL, sites_alpha = 1, sites_size = 2, group_as_aes = TRUE,
                      species_color = "violetred", species_alpha = 1)
{
  display <- match.arg(display, several.ok = TRUE)
  s <- vegan::scores(x, choices = choices, scaling = scaling)
  name <- names(s)
  if (is.null(name)) {
    name <- "sites"
    s <- list(s)
  }
  s <- lapply(seq_along(name), function(i, s, name) {
    x <- as.data.frame(s[[i]])
    x <- cbind(x, a = rownames(x))
    names(x)[length(x)] <- name[i]
    x
  }, s = s, name = name)
  names(s) <- name
  dim1 <- names(s$sites)[1]
  dim2 <- names(s$sites)[2]
  s$dummy <- data.frame(1, 2)
  names(s$dummy) <- c(dim1, dim2)
  egv <- vegan::eigenvals(x)
  prec_var <- signif((egv/sum(egv) * 100)[choices], digits = 2)
  g <- s$dummy %>% ggplot2::ggplot(ggplot2::aes_string(paste0("x = ",
                                                              dim1), paste0("y = ", dim2))) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                        linetype = "dashed", color = "grey", size = 1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                        linetype = "dashed", color = "grey", size = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = paste0(dim1, " (", prec_var[1], "%)"),
                  y = paste0(dim2, " (", prec_var[2], "%)"))
  if ("sites" %in% display) {
    if (!is.null(group)) {
      if (!is.null(group2)) {
        s$sites <- cbind(s$sites, group = group, group2 = group2)
        if (group_as_aes) {
          g <- g + ggplot2::geom_point(data = s$sites,
                                       ggplot2::aes(color = group, shape = group2),
                                       alpha = sites_alpha, size = sites_size)
        }
        else {
          g <- g + ggplot2::geom_point(data = s$sites,
                                       ggplot2::aes(shape = group2), color = group,
                                       alpha = sites_alpha, size = sites_size)
        }
      }
      else {
        s$sites <- cbind(s$sites, group = group)
        if (group_as_aes) {
          g <- g + ggplot2::geom_point(data = s$sites,
                                       ggplot2::aes(color = group), alpha = sites_alpha,
                                       size = sites_size)
        }
        else {
          g <- g + ggplot2::geom_point(data = s$sites,
                                       color = group, alpha = sites_alpha, size = sites_size)
        }
      }
    }
    else {
      g <- g + ggplot2::geom_point(data = s$sites, color = "deepskyblue",
                                   alpha = sites_alpha)
    }
  }
  if (!is.na(ellipse)) {
    if (!is.null(group)) {
      s$sites <- cbind(s$sites, z = group)
      g <- g + ggplot2::stat_ellipse(data = s$sites, ggplot2::aes(color = z),linetype = 4, size = 1)
    }
    else {
      g <- g + ggplot2::stat_ellipse(data = s$sites, color = "navy",
                                     linetype = 4, size = 1)
    }
  }
  if ("species" %in% display && !is.null(s$species)) {
    g <- g + ggplot2::geom_text(data = s$species, ggplot2::aes(label = species),
                                color = species_color, alpha = species_alpha)
  }
  if ("biplot" %in% display && !is.null(s$biplot)) {
    s$biplot$x <- 0
    s$biplot$y <- 0
    s$biplot$xend <- s$biplot[, dim1]
    s$biplot$yend <- s$biplot[, dim2]
    g <- g + ggplot2::geom_text(data = s$biplot, ggplot2::aes(label = biplot),
                                color = "black") +
      ggplot2::geom_segment(data = s$biplot,
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2,"cm")),
                            color = "black", size = 1)
  }
  if ("centroids" %in% display && !is.null(s$centroids)) {
    g <- g + ggplot2::geom_text(data = s$centroids, ggplot2::aes(label = centroids),
                                color = "darkolivegreen")
  }
  return(g)
}

#' @title Plot raw moments against the first moment
#' @description Plot raw moments against the first moment. Useful for testing the prediction of scale collapse (i.e., when the slope of the relationship between the pth raw moment and the first raw moment is p).
#' @param data_list a list of numeric sample values
#' @param choice a vector of pth moments
#' @param center whether to compute the central moment instead of the raw moments (default is FALSE)
#' @param plot a logical value indicating whether to plot the results.
#' @param rr.digits,coef.digits number of digits to display
#' @param method the method of fitting ("MA": major axis, "OLS": ordinary least squares, "SMA": standardized major axis, "RMA": range major axis)
#' @return a data.frame which is used to generate the ggplot
plot_moments <- function(data_list, choice = c(2:4),
                         plot = TRUE,
                         center = FALSE,
                         rr.digits = 3,
                         coef.digits = 3,
                         method = "MA"){
  choice <- c(1, choice)

  out <- lapply(
    data_list,
    function(x){
      res <- vapply(choice, FUN = function(p){
        moment(x, p)
      }, FUN.VALUE = numeric(1))
      names(res)<- c("x", paste0(choice[-1]))
      res
    }
  ) %>%
    bind_vec() %>%
    tidyr::gather(
      key = k, value = val, -x
    ) %>%
    dplyr::mutate(
      k = as.numeric(k)
    )

  if(isTRUE(plot)){
    out2 <- out


    if(center){
      y_lab <- tex("\\langle x^k \\rangle - \\langle x \\rangle^k")
      out2 <- out2 %>%
        dplyr::mutate(
          val = val - x^k
        )
    } else {
      y_lab <- tex("\\langle x^k \\rangle")
    }

    g <- out2 %>%
      ggplot2::ggplot(ggplot2::aes(x = x,
                                   y = val,
                                   color = as.factor(k),
                                   group = as.factor(k))) +
      ggplot2::geom_point() +
      geom_lineeq(rr.digits = rr.digits, method = method, coef.digits = coef.digits) +
      scale_xy_log() +
      ggplot2::labs(x = tex("\\langle x \\rangle"),
           y = y_lab,
           color = "k") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_viridis_d()


    suppressWarnings(print(g))
  }
  invisible(out)
}


#' @title Plot gradient field
#' @description Plot gradient field
#' @param data a data with x, y, z
#' @param aggregate bin size to aggregate data
#' @param colours colors of gradient field
#' @return a ggplot
gggradfield <- function(data, aggregate = 2,
                           colours = c("#B2182B", "#E68469", "#D9E9F1", "#ACD2E5",
                                       "#539DC8", "#3C8ABE", "#2E78B5")){
  foo <- function(data,aggregate,colours){
    rast <- raster::rasterFromXYZ(data)
    names(rast) <- "z"
    raster::projection(rast) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    quiv <- raster::aggregate(rast, aggregate)
    terr <- raster::terrain(quiv, opt = c('slope', 'aspect'))
    quiv$u <- terr$slope[] * sin(terr$aspect[])
    quiv$v <- terr$slope[] * cos(terr$aspect[])
    quiv_df <- as.data.frame(quiv, xy = TRUE)
    rast_df <- as.data.frame(rast, xy = TRUE)
    nms <- colnames(data)
    g <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y, fill = z)) +
      ggplot2::geom_raster(data = rast_df, na.rm = TRUE) +
      ggquiver ::geom_quiver(data = quiv_df, ggplot2::aes(u = u, v = v), vecsize = 1.5) +
      ggplot2::scale_fill_gradientn(colours = colours, na.value = "transparent") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = nms[1], y = nms[2], fill = nms[3])
    g
  }

  environment(foo) <- asNamespace("raster")
  g <- foo(data,aggregate,colours)

  return(g)
}
