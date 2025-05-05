#' @title Add fitted line and equation geom layer
#' @description a wrapper for `ggpmisc::stat_ma_line()` and `ggpmisc::stat_ma_eq()`
#' @param x a ggplot object
#' @param method the method of fitting ("MA": major axis, "OLS": ordinary least squares, "SMA": standardized major axis, "RMA": range major axis)
#' @param rr.digits,coef.digits the number of digits to display
#' @param labels a vector of what to label. ("eq", "R2", "P", "n")
#' @return a ggplot object
geom_lineeq <- function(x, method = "MA", rr.digits = 3, coef.digits = 3, labels = c("eq", "R2")){
  list(
    ggpmisc::stat_ma_line(method = method),
    ggpmisc::stat_ma_eq(eq.with.lhs = "italic(hat(y))~`=`~",
                        ggpmisc::use_label(labels),
                        rr.digits = rr.digits,
                        coef.digits = coef.digits,
                        coef.keep.zeros = TRUE,
                        method = method)
  )
}

#' @title Latex label
#' @description a wrapper for `latex2exp::Tex()`
#' @param x the latex command without the `$`.
#' @param italic if TRUE italicize the output
#' @param ... additional arguments passed to `latex2exp::Tex()`
#' @return a plotmath expression
tex <- function(x, italic = TRUE, ...){
  latex2exp::TeX(sprintf("%s", x), italic = italic, ...)
}


#' @title Generate marginal effect data frame for plotting
#' @description Generate marginal effect data frame for plotting
#' @param model the model object
#' @param terms the terms for which to compute the marginal effects. Has the same behavior as in `sjPlot::plot_model()`
#' @param n specifies the density of points along `terms`
#' @param ci the interval of upper and lower bounds
#' @return a data.frame
marginal_effects <- function(model, terms, n = 300, ci = 0.95, ...){

  fam <- insight::get_family(model)
  if(!inherits(fam,"family")){
    linkinv <- identity
  } else {
    linkinv <- insight::link_inverse(model)
  }


  new_data <- prepare_newdata(model, terms, n)
  terms <- gsub("\\[.*", "", terms)

  if(inherits(model, "clm")){
    pred <- suppressWarnings(predict(model, newdata = new_data, se = TRUE))

    new_data <- cbind(new_data,
                      "yhat" = pred$fit,
                      "se" = pred$se.fit)

    new_data <- new_data %>%
      dplyr::group_by(dplyr::all_of(terms)) %>%
      dplyr::summarise_all(function(x){
        if(is.numeric(x)){
          mean(x)
        } else {
          NA
        }
      })

    new_data <- new_data %>%
      tidyr::gather(value = "yhat", key = "cat", paste("yhat",levels(model$y), sep = ".")) %>%
      dplyr::mutate(cat = factor(gsub(".*\\.","",cat), levels = levels(model$y), ordered = TRUE))
    names(new_data)[names(new_data) == "cat"] <- insight::find_response(model)

  } else {
    pred <- suppressWarnings(predict(model, newdata = new_data, se = TRUE, ...))

    new_data <- cbind(new_data,
                      "yhat_link" = pred$fit,
                      "se" = pred$se.fit)
    new_data$lower_link <- new_data$yhat_link + stats::qnorm((1 - ci)/2) * new_data$se
    new_data$upper_link <- new_data$yhat_link + stats::qnorm((1 - ci)/2, lower.tail = FALSE) * new_data$se

    new_data <- new_data %>%
      dplyr::mutate(se = se^2) %>%
      dplyr::group_by(dplyr::across(terms)) %>%
      dplyr::summarise_all(function(x){
        if(is.numeric(x)){
          mean(x)
        } else {
          NA
        }
      }) %>%
      dplyr::mutate(se = sqrt(se))

    resp_inv <- insight::get_transformation(model)$inverse

    new_data$lower <- resp_inv(linkinv(new_data$lower_link))
    new_data$upper <- resp_inv(linkinv(new_data$upper_link))
    new_data$yhat <- resp_inv(linkinv(new_data$yhat_link))
  }

  return(new_data)
}


#' @title Find the size of device using windows x11
#' @description Uses `x11()` to interactively get (with dragging) the desired window size. Only usable for windows devices.
#' @param expr an expression to be evaluated
#' @param units the units of the window dimensions
#' @return NULL
dev.isize <- function(expr, units = c("in", "cm", "px")){
  units <- match.arg(units)
  grDevices::x11()

  if(ggplot2::is.ggplot(expr)){
    eval(print(expr))
  } else{
    eval(expr)
  }

  cli::cli_text("Press {.kbd Esc} to exit.")
  while(TRUE){
    cdev <- grDevices::dev.size(units = "in")
    cdev <- round(cdev, digits = 2)
    Sys.sleep(1)
    cat(sprintf("Width (%s): %s, Height (%s): %s               ", units, cdev[1], units, cdev[2]), "\r")
  }
  invisible(NULL)
}
