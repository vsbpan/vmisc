simulateResiduals.brmsfit <- function(x, integer = NULL, plot = FALSE, asFactor = FALSE, ...) {
  stopifnot(brms::is.brmsfit(x))
  if(is.null(integer)){
    integer <- switch(insight::get_family(x)$type,
                      "real" = FALSE,
                      "int" = TRUE)
  }

  if(insight::get_family(x)$family %in% c("multinomial")){
    resp <- brms::posterior_predict(x, ndraws = 1000)
    y <- brms::get_y(x)
    pred <- colMeans(brms::posterior_epred(x, ndraws = 1000, re.form = NA))

    out <- list()
    for (i in seq_len(ncol(y))){
      out[[i]] <- DHARMa::createDHARMa(
        simulatedResponse = t(resp[,,i]),
        observedResponse = y[,i],
        fittedPredictedResponse = pred[,i],
        integerResponse = FALSE)

      if (isTRUE(plot)) {
        plot(out[[i]], asFactor = asFactor, title = dimnames(resp)[[3]][i], ...)
      }
    }

    return(invisible(out))
  }

  if(insight::get_family(x)$family %in% c("categorical")){
    levels <- unique(brms::get_y(x))
    y <- apply(levels,1,function(x){as.numeric(brms::get_y(x) == x)})
    resp <- brms::posterior_predict(x, ndraws = 1000)
    resp <- array(do.call("c", lapply(levels,function(x){as.numeric(resp == x)})),
                  dim = c(1000, nrow(y), length(levels)))
    pred <- colMeans(brms::posterior_epred(x, ndraws = 1000, re.form = NA))

    out <- list()
    for (i in seq_len(ncol(y))){
      out[[i]] <- DHARMa::createDHARMa(
        simulatedResponse = t(resp[,,i]),
        observedResponse = y[,i],
        fittedPredictedResponse = pred[,i],
        integerResponse = FALSE)

      if(isTRUE(plot)) {
        plot(out[[i]], asFactor = asFactor, title = dimnames(resp)[[3]][i], ...)
      }
    }
    return(invisible(out))
  }

  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(x, ndraws = 1000)),
    observedResponse = brms::get_y(x),
    fittedPredictedResponse = colMeans(brms::posterior_epred(x, ndraws = 1000, re.form = NA)),
    integerResponse = integer)

  if(isTRUE(plot)) {
    plot(dharma.obj, asFactor = asFactor, ...)
  }

    invisible(dharma.obj)
}


simulateResiduals.nls <- function(x, asFactor = FALSE, plot = FALSE, integer = FALSE, ...) {

  pred <- predict(x)

  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = replicate(500, stats::rnorm(length(pred), pred, stats::sigma(x))),
    observedResponse = insight::get_response(x),
    fittedPredictedResponse = pred,
    integerResponse = integer)

  if (isTRUE(plot)) {
    plot(dharma.obj, asFactor = asFactor, ...)
  }

  invisible(dharma.obj)
}

simulateResiduals.default <- function(x, plot = FALSE, asFactor = NULL, ...){
  dharma.obj <- DHARMa::simulateResiduals(x, ...)

  if(isTRUE(plot)){
    plot(dharma.obj, asFactor = asFactor, ...)
  }

  invisible(dharma.obj)
}

