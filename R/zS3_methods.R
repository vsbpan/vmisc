# Generic methods
posterior_epred <- function(x,
                            newdata = NULL,
                            ndraws,
                            re_formula = NA,
                            scale = c("response", "link"),
                            ...){
  UseMethod("posterior_epred")
}

simulateResiduals <- function(x, plot = FALSE, asFactor, ...){
  UseMethod("simulateResiduals")
}

omit_zero <- function(x, ...){
  UseMethod("omit_zero")
}

loghist <- function(x,
                    nclass,
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



