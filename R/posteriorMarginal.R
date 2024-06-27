#' Generate Predictive Marginal Effect
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' The function returns the average marginal effect for each category, across levels of xvar, all other variables observed.
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @param category_groups {list of categories to group together}

#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export

posterior_pme = function(model = regressionModels$envWater,
                         xvar = "authM3",
                         mvar = "latino",
                         xrange =  c(0, 1),
                         mrange =  c(0, 1)){
  data =  model$data
  cols_to_average <- setdiff(names(data)[-1], c(xvar, mvar))

  data_grid <- data %>%
    select(all_of(cols_to_average)) %>%
    summarize(across(everything(), mean)) %>%
    expand_grid(
      !!xvar := xrange,  # Use correct syntax to find min/max
      !!mvar := mrange) %>% add_epred_draws(model)
  t1 =
    data_grid %>%
    filter(!!sym(xvar) == 1) %>%
    subset(select = ".epred")

  t2 = data_grid %>%
    filter(!!sym(xvar) == 0) %>%
    subset(select = ".epred")

  dat = data_grid %>%
    filter(!!sym(xvar) == 1)

  dat$me = t1$.epred - t2$.epred

  if (model$family[[1]] == "categorical" |
      model$family[[1]] == "cumulative"){
    plot = dat %>%
    subset(select = c("latino", ".category", "me"))  %>%
    group_by(latino, .category)
  }
  if (model$family[[1]] == "bernoulli" |
      model$family[[1]] == "gaussian"){
    plot = dat %>%
      subset(select = c("latino",  "me"))  %>%
      group_by(latino)
  }

  plot = plot %>%
      summarize(
      mean = mean(me),
      lower = quantile(me, 0.025),
      upper = quantile(me, 0.975)
    )
  return(plot)
}
