# Generic methods
posterior_epred <- function(x, ...){
  UseMethod("posterior_epred")
}

simulateResiduals <- function(x, ...){
  UseMethod("simulateResiduals")
}

omit_zero <- function(x, ...){
  UseMethod("omit_zero")
}

loghist <- function(x, ...){
  UseMethod("loghist")
}

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



