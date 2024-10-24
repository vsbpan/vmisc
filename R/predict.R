# Get posterior draws on the response or link scale based on specified terms.
# `terms` has the same behavior as in `sjPlot::plot_model()`
# `n` specifies the number of draws.
# `newdata2` passes extra data not specified in `terms`.
# `along_n` specifies the density of points along `terms`
# `transFUN` passes a function used to transform the posterior draws. Defaults to identity.
spaghetti <- function(model, terms, n = 100, along_n = 300, newdata2 = NULL,
                          transFUN = NULL, scale = c("response", "link")){
  if(is.null(transFUN)){
    f <- function(x) {
      x
    }
  } else {
    f <- match.fun(transFUN)
  }

  predictors <- prepare_newdata(model, terms = terms, n = along_n)

  if(!is.null(newdata2)){
    predictors <- cbind(predictors, newdata2)
  }


  epred <- posterior_epred(model,
                           newdata = predictors,
                           ndraws = n,
                           re_formula = NA,
                           scale = scale)

  foo <- function(x){
    nd <- cbind(
      predictors,
      as.data.frame(t(x))
    )
    nd %>%
      tidyr::gather(key = draw, value = val, -names(predictors)) %>%
      dplyr::mutate(val = f(val))
  }
  if(length(dim(epred)) == 2){
    foo(epred) %>% cbind("resp" = insight::find_response(model))
  } else {
    lapply(
      seq_len(dim(epred)[3]), function(i){
        cbind(foo(epred[,,i]), "resp" = dimnames(epred)[[3]][i])
      }
    ) %>%
      do.call("rbind", .)
  }
}


# Takes a model and specified predictor terms and format newdata used for predictions.
# Covariates not specified in terms are set to:
## numeric: the mean
## otherwise: set to unique entries.
prepare_newdata <- function(model, terms = NULL, n = 300){

  predictor_frame <- insight::get_data(model)
  yname <- insight::find_response(model)
  predictor_frame <- predictor_frame[,!names(predictor_frame) %in% yname, drop = FALSE]

  rand_names <- insight::find_random(model)$random
  var_names <- names(predictor_frame)

  v <- lapply(terms, function(z){
    switch(as.character(grepl("\\[", z) && grepl("\\[", z)),
           "TRUE" = eval(parse(text = sprintf("c(%s)", gsub(".*\\[|\\]","", z)))),
           "FALSE" = NULL)
  })
  terms <- gsub("\\[.*","",terms)

  if(length(n) == 1){
    n <- rep(n, length(terms))
  }


  new_data <- expand.grid(lapply(seq_along(predictor_frame), function(i, d, n, v){
    x <- d[, i]

    if(names(d)[i] %in% terms){
      j <- which(terms %in% names(d)[i])

      if(is.null(v[[j]])){
        if(is.numeric(x)){
          x <- seq_interval(stats::na.omit(x), n[j])
        } else {
          x <- as.vector(na.omit(unique(x)))
        }
      } else {
        x <- v[[j]]
      }
      return(x)
    } else {
      if(is.numeric(x)){
        x <- mean(x, na.rm = TRUE)
      } else {
        if(names(d)[i] %in% rand_names){
          x <- "foooooooooooooooo"
        } else {
          x <- as.vector(na.omit(unique(x)))
        }
      }
      return(x)
    }
  },
  d = as.data.frame(predictor_frame),
  n = n,
  v = v))

  names(new_data) <- names(predictor_frame)

  return(new_data)
}


# Posterior prediction like method for `gam` objects.
posterior_epred.gam <- function(object, newdata = NULL,
                                ndraws = 100,
                                unconditional = TRUE,
                                re_formula = NA,
                                scale = c("response", "link"), ...){
  design_matrix <- stats::predict(object,
                           newdata = newdata,
                           type = "lpmatrix",
                           unconditional = unconditional)
  vcov_mat <- stats::vcov(object, unconditional = unconditional)
  coefs <- mvnfast::rmvn(ndraws, mu = stats::coef(object),
                         sigma = vcov_mat, kpnames = TRUE)
  linpred <- tcrossprod(design_matrix, coefs)

  if(match.arg(scale) == "response"){
    preds <- as.matrix(stats::family(object)$linkinv(linpred))
  } else {
    preds <- linpred
  }
  preds <- t(preds)
  rownames(preds) <- paste0("draw_", seq_len(nrow(preds)))
  return(preds)
}


# Poterior prediction method for `brmsfit`.
posterior_epred.brmsfit <- function (object, newdata = NULL, re_formula = NULL, re.form = NULL,
                                     resp = NULL, dpar = NULL, nlpar = NULL, ndraws = NULL, draw_ids = NULL,
                                     sort = FALSE, scale = c("response", "link"), ...) {
  cl <- match.call()
  if ("re.form" %in% names(cl) && !missing(re.form)) {
    re_formula <- re.form
  }
  object <- brms::restructure(object)
  prep <- brms::prepare_predictions(object, newdata = newdata, re_formula = re_formula,
                                    resp = resp, ndraws = ndraws, draw_ids = draw_ids, check_response = FALSE,
                                    ...)



  scale <- match.arg(scale)
  if(scale == "link" && is.multi_response(object)){
    # Set the number of trials to 1 such that the result is in proportions.
    prep$data$trials <- rep(1, length(prep$data$trials))
    res <- rstantools::posterior_epred(prep, dpar = dpar, nlpar = nlpar, sort = sort,
                                       scale = "response", summary = FALSE)
    # Back transform to link scale
    res <- insight::link_function(object)(res)
  } else {
    res <- rstantools::posterior_epred(prep, dpar = dpar, nlpar = nlpar, sort = sort,
                                       scale = scale, summary = FALSE)
  }

  rownames(res) <- paste0("draw_", seq_len(nrow(res)))

  return(res)
}
