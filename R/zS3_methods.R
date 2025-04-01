# Generic methods
#' @title Generate posterior draws from model
#' @description
#' Generate posterior draws from model.
#' @param x the model object
#' @param newdata a data.frame of newdata for the model to predict from
#' @param ndraws a positive integer indicating the number of draws from the model
#' @param re_formula if NA, ignore random effects.
#' @param scale return the predictions on the link scale ("link") or response scale ("response"). Default is "response".
#' @param ... additional arguments
#' @param unconditional if TRUE then the smoothing parameter uncertainty corrected covariance matrix is used, when available, otherwise the covariance matrix conditional on the estimated smoothing parameters is used.
#' @param resp passed to `brms::prepare_predictions()`
#' @param dpar passed to `rstantools::posterior_epred()`
#' @param nlpar passed to `rstantools::posterior_epred()`
#' @param draw_ids passed to `brms::prepare_predictions()`
#' @param sort passed to `rstantools::posterior_epred()`
#' @return a matrix of ndraws X nobs
#' @rdname posterior_epred
posterior_epred <- function(x,
                            newdata = NULL,
                            ndraws,
                            re_formula = NA,
                            scale = c("response", "link"),
                            ...){
  UseMethod("posterior_epred")
}

#' @title Create simulated residuals
#' @description see `?DHARMa::simulateResiduals()`.
#' @param x the model object
#' @param plot if TRUE, plot the simulated residuals
#' @param asFactor if TRUE, bin the residuals.
#' @param integer is the response an integer?
#' @param ... additional arguments
#' @rdname simulateResiduals
simulateResiduals <- function(x, plot = FALSE, asFactor, ...){
  UseMethod("simulateResiduals")
}

#' @title Drop the element if it is completely zero
#' @description Drop the element if it is completely zero
#' @param x a vector
#' @param ... additional arguments
#' @rdname omit_zero
omit_zero <- function(x, ...){
  UseMethod("omit_zero")
}

loghist <- function(x,
                    nclass,
                    by = NULL,
                    log.p = TRUE,
                    log.x = TRUE,
                    scale = FALSE,
                    delta = 1,
                    phi = 1,
                    geom = c("line", "col", "point"),
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
                    show_legend = TRUE,
                    ...){
  UseMethod("loghist")
}

registerS3method(genname = "vcov",
                 class = "nls",
                 method = vcov.nls)

registerS3method(genname = "as.function",
                 class = "formula",
                 method = as.function.formula)

registerS3method(genname = "omit_zero",
                 class = "default",
                 method = omit_zero.default)

registerS3method(genname = "omit_zero",
                 class = "list",
                 method = omit_zero.list)

registerS3method(genname = "posterior_epred",
                 class = "gam",
                 method = posterior_epred.gam)

registerS3method(genname = "posterior_epred",
                 class = "brmsfit",
                 method = posterior_epred.brmsfit)

registerS3method(genname = "posterior_epred",
                 class = "nls",
                 method = posterior_epred.nls)

registerS3method(genname = "simulateResiduals",
                 class = "brmsfit",
                 method = simulateResiduals.brmsfit)

registerS3method(genname = "simulateResiduals",
                 class = "nls",
                 method = simulateResiduals.nls)

registerS3method(genname = "simulateResiduals",
                 class = "default",
                 method = simulateResiduals.default)

registerS3method(genname = "loghist",
                 class = "default",
                 method = loghist.default)

registerS3method(genname = "loghist",
                 class = "list",
                 method = loghist.list)



