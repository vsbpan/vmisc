simulateResiduals.brmsfit <- function(model, integer = NULL, plot = FALSE, asFactor = FALSE, ...) {
  stopifnot(brms::is.brmsfit(model))
  if(is.null(integer)){
    integer <- switch(insight::get_family(model)$type,
                      "real" = FALSE,
                      "int" = TRUE)
  }

  if(insight::get_family(model)$family %in% c("multinomial")){
    resp <- brms::posterior_predict(model, ndraws = 1000)
    y <- brms::get_y(model)
    pred <- colMeans(brms::posterior_epred(model, ndraws = 1000, re.form = NA))

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

  if(insight::get_family(model)$family %in% c("categorical")){
    levels <- unique(brms::get_y(model))
    y <- apply(levels,1,function(x){as.numeric(brms::get_y(model) == x)})
    resp <- brms::posterior_predict(model, ndraws = 1000)
    resp <- array(do.call("c", lapply(levels,function(x){as.numeric(resp == x)})),
                  dim = c(1000, nrow(y), length(levels)))
    pred <- colMeans(brms::posterior_epred(model, ndraws = 1000, re.form = NA))

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
    simulatedResponse = t(brms::posterior_predict(model, ndraws = 1000)),
    observedResponse = brms::get_y(model),
    fittedPredictedResponse = colMeans(brms::posterior_epred(model, ndraws = 1000, re.form = NA)),
    integerResponse = integer)

  if(isTRUE(plot)) {
    plot(dharma.obj, asFactor = asFactor, ...)
  }

    invisible(dharma.obj)
}


simulateResiduals.nls <- function(model, asFactor = FALSE, plot = FALSE, integer = FALSE, ...) {

  pred <- predict(model)

  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = replicate(500, stats::rnorm(length(pred), pred, stats::sigma(m))),
    observedResponse = insight::get_response(x),
    fittedPredictedResponse = pred,
    integerResponse = integer)

  if (isTRUE(plot)) {
    plot(dharma.obj, asFactor = asFactor, ...)
  }

  invisible(dharma.obj)
}

simulateResiduals.default <- function(model, ...){
  DHARMa::simulateResiduals(model, ...)
}

