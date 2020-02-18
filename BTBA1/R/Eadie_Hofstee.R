#' Draw an Eadie-Hofstee graph (and compute the y-axis intercept)
#'
#' @param vel Velocity
#' @param sub Substarate concentration 
Eadie_Hofstee <- function(vel, sub){
  ggplot2::ggplot(mapping = ggplot2::aes(
    x = vel/sub,
    y = vel)
    )+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm",
                fullrange = TRUE)+
    ggplot2::scale_x_continuous(expand=c(0,0), limits=c(0, base::max(vel/sub))) +
    ggplot2::scale_y_continuous(expand=c(0,0), limits=c(0, base::max(vel)+20))+
    ggplot2::ggtitle("Eadie-Hostee-Plot")
(base::summary(
  stats::lm(vel/sub~vel)
  ))    
}

