
#' Common ggplot theme
#'
#' @param
#'
#' @return persistent ggplot theme
#' @export
#'
#'
#' export
mytheme <- function(...){
  ggtheme =
  theme(
  plot.title =  ggplot2::element_text(face = "bold", hjust = 0, vjust = 0, colour = "#3C3C3C", size = 20),
  axis.text.x = ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
  axis.text.y = ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
  axis.title =  ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, colour = "#535353", face = "bold", vjust = 1.5),
  axis.ticks = ggplot2::element_blank(),
  strip.text.x = ggplot2::element_text(size = 16),
  panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", size = .25),
  panel.background = ggplot2::element_rect(fill = "white"),
  legend.text = ggplot2::element_text(size = 14),
  legend.title = ggplot2::element_text(size = 16) )
return(ggtheme)
  }



