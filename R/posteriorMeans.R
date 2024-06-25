#' Generate Posterior Predictions with 2 way interactions
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' @param data {dataframe used in brms model}
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @param category_groups {list of categories to group together}
#' @example #Not Run
#'          ## posterior_means()
#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export
#'
#

posterior_means <- function(data = dataActive,
                           xvar = "authM3",
                           mvar = "latino",
                           xval = seq(0, 1, 0.1),
                           mval = c(0, 1),
                           model = ideology,
                           category_groups =
                             list(Oppose = c(1, 2), Neutral = 3,
                                  Support = c(3, 4))) {
  formula     =    model$formula
  regex       =    names(model$data)
  # Filter data frame to exclude, xvar, mvar, and all variables in the model formula.
  means       =   data[, setdiff(names(data), c(xvar, mvar))]
  means       =   means[, (names(means) %in% regex)][,-1] %>% colMeans(na.rm = TRUE)
  names   =    data[, (names(data) !=  xvar & names(data) != mvar)
                    &  names(data) %in% regex][,-1] %>% names() %>% as.character()
  dat = cbind(means) %>% t() %>% as.data.frame() %>% expand_grid(xvar = xval, mvar = mval)
  #For xvar in dat, rename to xvar
   names(dat)[names(dat) == "xvar"] <- xvar
   names(dat)[names(dat) == "mvar"] <- mvar
   dat = dat %>% add_epred_draws(model)
   if(model$family[[1]] == "categorical" | model$family[[1]] == "cumulative"){
     print("This is an multiple category ordinal or multinomial model.
           The categories will be grouped together according to category_groups")
     dat = pivot_wider(dat, names_from = .category, values_from = .epred)
     for (group_name in names(category_groups)) {
       dat[[group_name]] <- rowSums(dat[, as.character(category_groups[[group_name]]), drop = FALSE])
     }
   }
   else{
     print("This is a binary model")
     dat <- dat %>%
       rename(prob = .epred)
   }
  return(dat)
}

