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


ggPoint <- function(data = plotData,
                    model = model,
                    size_x = 10,
                    size_y = 10,
                    size_title = 12,
                    title = "Authoritarianism \nand Party Identification",
                    xtitle = "Authoritarianism",
                    caption = "Data come from the 2020 Western Survey",
                    ytitle = "Probability",
                    xlimits = c(0, 1),
                    ylimits = c(0, 1),
                    facet_order = "default", # New parameter for facet order

                    ...) {
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(
    x = authM3,
    group = as.factor(latino),
    color = as.factor(latino),
    y = mean,
    ymin = lower,
    ymax = upper
  ))

  if (model$family[[1]] == "cumulative" || model$family[[1]] == "categorical") {
    # Apply facet ordering based on user input
    if (facet_order == "default") {
      plot <- plot + ggplot2::facet_wrap(~forcats::fct_rev(.category), ...)
    } else if (facet_order == "reverse") {
      plot <- plot + ggplot2::facet_wrap(~.category, ...)
    } else {
      # Assuming facet_order is a character vector specifying a custom order
      plot <- plot + ggplot2::facet_wrap(~factor(.category, levels = facet_order), ...)
    }
  }

  plot <- plot +
    ggplot2::geom_ribbon(alpha = 0.3, color = "lightgrey") +
    ggplot2::geom_line() +
    ggplot2::ggtitle(title) +
    ggplot2::scale_y_continuous(ytitle, limits = ylimits) +
    ggplot2::scale_x_continuous(xtitle, breaks = seq(xlimits[1], xlimits[2], by = 0.5)) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical") +
    ggplot2::scale_color_manual("", values = c("#457B9D", "#E63946")) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::labs(caption = caption) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, vjust = 0, colour = "#3C3C3C", size = size_title),
      axis.text.x = ggplot2::element_text(size = size_x, colour = "#535353", face = "bold"),
      axis.text.y = ggplot2::element_text(size = size_y, colour = "#535353", face = "bold"),
      axis.title = ggplot2::element_text(size = size_title, colour = "#535353", face = "bold"),
      axis.title.y = ggplot2::element_text(size = size_x, colour = "#535353", face = "bold", vjust = 1.5),
      axis.ticks = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = size_x),
      panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", size = .25),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      legend.position = "bottom",
      legend.box = "vertical"
    )
  return(plot)
}
