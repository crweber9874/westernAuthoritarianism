#' Generate Average Marginal Effects
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' The function returns the average marginal effect for each category, across levels of xvar, all other variables observed.
#' @param data {dataframe used in brms model}
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @param category_groups {list of categories to group together}
#' @example #Not Run
#'          ## posterior_marginal()
#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export
#'
#
posterior_ame <- function(xvar = "authM3",
                         mvar = "latino",
                         xval = c(0, 1),
                         mval = c(0, 1),
                         model = ideology,
                         category_groups =
                           list(Oppose = c(1, 2),
                                Neutral = 3,
                                Support = c(3, 4)),
                         ndraw = 100) {
  formula     =    model$formula
  regex       =    names(model$data)
  modelData   =    model$data
  # Filter data frame to exclude, xvar, mvar, and all variables in the model formula.
  means       =   modelData[, setdiff(names(modelData), c(xvar, mvar))][, -1]
  # names       =      modelData[, (names(modelData) !=  xvar &
  #                                   names(modelData) != mvar)
  #                              &
  #                                names(modelData) %in% regex][, -1] %>% names() %>% as.character()
  dat  = means %>% expand_grid(xvar = xval, mvar = mval)
  # #For xvar in dat, rename to xvar
  names(dat)[names(dat) == "xvar"] <- xvar
  names(dat)[names(dat) == "mvar"] <- mvar
  dat = dat %>% add_epred_draws(model, ndraws = ndraw)
  if (model$family[[1]] == "categorical" |
      model$family[[1]] == "cumulative") {
    dat = pivot_wider(dat, names_from = .category, values_from = .epred)
    for (group_name in names(category_groups)) {
      dat[[group_name]] <- rowSums(dat[, as.character(category_groups[[group_name]]), drop = FALSE])
      datME = dat %>%
        filter(!!sym(xvar) == min(xval)) %>%
        as.data.frame() %>%
        select(-contains(names(category_groups))) %>%
        select(-contains(c(xvar))) %>%
        select(-matches("^[0-9]"))
      for (categories in names(category_groups)) {
        datLo = dat %>%
          filter(!!sym(xvar) == min(xval)) %>%
          as.data.frame() %>%
          select(contains(categories))
        datHigh = dat %>%
          filter(!!sym(xvar) == max(xval)) %>%
          as.data.frame() %>%
          select(contains(categories))
        me = datHigh - datLo
        datME[[categories]] = me
      }
    }
  }
  if(model$family[[1]] == "bernoulli") {
    print("This is a binary model")
    datME = dat %>%
      filter(!!sym(xvar) == min(xval)) %>%
      as.data.frame() %>%
      select(-contains(c(xvar))) %>%
      rename(prob = .epred)
    datLo = dat %>%
      filter(!!sym(xvar) == min(xval)) %>%
      as.data.frame() %>%
      rename(prob = .epred) %>%
      select(prob)
    datHigh = dat %>%
      filter(!!sym(xvar) == max(xval)) %>%
      as.data.frame() %>%
      rename(prob = .epred) %>%
      select(prob)
    me = datHigh$prob - datLo$prob
    datME$prob = me
  }
  return(datME)
}

#' Generate Predictive Marginal Effect
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' The function returns the average marginal effect for each category, across levels of xvar, all other variables observed.
#' @param data {dataframe used in brms model}
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @param category_groups {list of categories to group together}

#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export
posterior_pe <- function(xvar = "authM3",
                                mvar = "latino",
                                xval = c(0, 1),
                                mval = c(0, 1),
                                model = ideology,
                                category_groups =
                                  list(Oppose = c(1, 2),
                                       Neutral = 3,
                                       Support = c(3, 4)),
                                ndraw = 1000) {
  formula     =    model$formula
  regex       =    names(model$data)
  modelData   =    model$data
  # Filter data frame to exclude, xvar, mvar, and all variables in the model formula.
  means       =   modelData[, setdiff(names(modelData), c(xvar, mvar))][, -1]
  # names       =      modelData[, (names(modelData) !=  xvar &
  #                                   names(modelData) != mvar)
  #                              &
  #                                names(modelData) %in% regex][, -1] %>% names() %>% as.character()
  dat  = means %>% expand_grid(xvar = xval, mvar = mval)
  # #For xvar in dat, rename to xvar
  names(dat)[names(dat) == "xvar"] <- xvar
  names(dat)[names(dat) == "mvar"] <- mvar
  dat = dat %>% add_epred_draws(model, ndraws = ndraw)
  if (model$family[[1]] == "categorical" |
      model$family[[1]] == "cumulative") {
    dat = pivot_wider(dat, names_from = .category, values_from = .epred)
    for (group_name in names(category_groups)) {
      dat[[group_name]] <- rowSums(dat[, as.character(category_groups[[group_name]]), drop = FALSE])
      datME = dat %>%
        filter(!!sym(xvar) == min(xval)) %>%
        as.data.frame() %>%
        select(-contains(names(category_groups))) %>%
        select(-contains(c(xvar))) %>%
        select(-matches("^[0-9]"))
      for (categories in names(category_groups)) {
        datLo = dat %>%
          filter(!!sym(xvar) == min(xval)) %>%
          as.data.frame() %>%
          select(contains(categories))
        datHigh = dat %>%
          filter(!!sym(xvar) == max(xval)) %>%
          as.data.frame() %>%
          select(contains(categories))
        me = datHigh - datLo
        datME[[categories]] = me
      }
    }
  }
  if(model$family[[1]] == "bernoulli") {
    print("This is a binary model")
    datME = dat %>%
      filter(!!sym(xvar) == min(xval)) %>%
      as.data.frame() %>%
      select(-contains(c(xvar))) %>%
      rename(prob = .epred)
    datLo = dat %>%
      filter(!!sym(xvar) == min(xval)) %>%
      as.data.frame() %>%
      rename(prob = .epred) %>%
      select(prob)
    datHigh = dat %>%
      filter(!!sym(xvar) == max(xval)) %>%
      as.data.frame() %>%
      rename(prob = .epred) %>%
      select(prob)
    me = datHigh$prob - datLo$prob
    datME$prob = me
  }
  return(datME)
}
