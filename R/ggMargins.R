#' Generate plot of point estimates from BRMS object
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
#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export

ggMargins_categorical <- function(data = plotData,
                      size_x = 10,
                      size_y = 10,
                      size_title = 12,
                      title = "Marginal Effect",
                      xtitle = "Authoritarianism",
                      ytitle = "Probability",
                      xlimits = c(0, 1),
                      ylimits = c(-1, 1),
                      mvar = "latino",
                      ...) {

  if (!(".category" %in% names(data))) {
    warning("This function is only for cumulative and categorical models")
    return(NULL)
  }
  else{
      plot <-
      ggplot2::ggplot(data)  +
      ggplot2::aes(
        x = .category,
        y = mean,
        ymin = lower,
        ymax = upper,
        group = !!sym(mvar),
        color = !!sym(mvar)
      ) +
      ggplot2::geom_point(size = 2, position = position_dodge(width = 0.5), alpha = 0.75) +
      ggplot2::geom_errorbar(width = 0.15, position = position_dodge(width = 0.5), alpha = 0.75, colour = "black") +
      ggplot2::ggtitle(title) +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous(ytitle, limits = ylimits) + # Corrected limits to ylimits
        ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +    # Cleaner base theme
        ggplot2::scale_color_manual("", values = c("#457B9D", "#E63946")) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = size_title, face = "bold", hjust = 0, vjust = 0, colour = "#3C3C3C"),
          axis.text = ggplot2::element_text(size = size_x, colour = "#535353", face = "bold"),  # Simplified axis text
          axis.title = ggplot2::element_text(size = size_title, colour = "#535353", face = "bold"),
          axis.title.y = ggplot2::element_text(vjust = 1.5),  # Adjust vertical position
          axis.ticks = ggplot2::element_blank(),
          strip.text.x = ggplot2::element_text(size = size_x),
          panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", size = 0.25),
          legend.position = "none"
        )
      return(plot)
       }

}

#' @export
ggMargins_nc <- function(data = plotData,
                                  size_x = 10,
                                  size_y = 10,
                                  size_title = 12,
                                  title = "Marginal Effect",
                                  xtitle = "Authoritarianism",
                                  ytitle = "Probability",
                                  xlimits = c(0, 1),
                                  ylimits = c(-1, 1),
                                  mvar = "latino",
                                  ...) {

  if ((".category" %in% names(data))) {
    warning("This function is NOT for cumulative and categorical models")
    return(NULL)
  }
  else{
    plot <-
      ggplot2::ggplot(data)  +
      ggplot2::aes(
        x = as.factor(!!sym(mvar)),
        y = mean,
        ymin = lower,
        ymax = upper,
        group = !!sym(mvar)
      ) +
      ggplot2::geom_point(size = 2, position = position_dodge(width = 0.5), alpha = 0.75) +
      ggplot2::geom_errorbar(width = 0.15, position = position_dodge(width = 0.5), alpha = 0.75, colour = "black") +
      ggplot2::ggtitle(title) +
      ggplot2::scale_x_discrete("") + # Corrected limits to ylimits
      ggplot2::scale_y_continuous(ytitle, limits = ylimits) + # Corrected limits to ylimits
      ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +    # Cleaner base theme
      ggplot2::scale_color_manual("", values = c("#457B9D", "#E63946")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = size_title, face = "bold", hjust = 0, vjust = 0, colour = "#3C3C3C"),
        axis.text = ggplot2::element_text(size = size_x, colour = "#535353", face = "bold"),  # Simplified axis text
        axis.title = ggplot2::element_text(size = size_title, colour = "#535353", face = "bold"),
        axis.title.y = ggplot2::element_text(vjust = 1.5),  # Adjust vertical position
        axis.ticks = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = size_x),
        panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", size = 0.25),
        legend.position = "none"
      )
    return(plot)
  }

}

