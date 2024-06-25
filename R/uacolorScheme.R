
#' @export
ua_scale_color_qualitative <- function(){
  scale_color_manual(values = ua_red,
                     ua_blue,
                     ua_chili,
                     ua_bloom,
                     ua_sky,
                     ua_midnight)
}

ua_scale_color_sequential <- function(low_color = ua_red,
                                      high_color = ua_blue){
  scale_fill_gradient(low_color = ua_red, high_color = ua_blue)
}

ua_scale_color_diverging <- function(low_color = ua_red,
                                     mid_color = ua_sky,
                                     high_color = ua_blue){
  scale_fill_gradient2(low = low_color, mid = mid_color,  high_color = high_color)
}

