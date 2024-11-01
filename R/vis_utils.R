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


tex <- function(x, italic = TRUE, ...){
  latex2exp::TeX(sprintf("$%s$", x), italic = italic, ...)
}


# Generate marginal effect data frame for plotting
marginal_effects <- function(model, terms, n = 300, ci = 0.95){

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
    pred <- suppressWarnings(predict(model, newdata = new_data, se = TRUE, type = "link"))

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



