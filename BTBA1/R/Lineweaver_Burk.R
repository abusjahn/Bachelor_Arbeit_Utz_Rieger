#' Plot a Lineweaver-Burk diagram and compute the ordinate intercept
#'
#' @param sub list
#' @param vel list
Lineweaver_Burk <- function(sub, vel){
  ggplot2::ggplot(mapping = ggplot2::aes(
    x = 1/sub,
    y = 1/vel
  ))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(
      method = "lm",
      fullrange = TRUE
    )+
    ggplot2::scale_x_continuous(expand=c(0,0), limits=c(0, max(1/sub+ 1))) +
    ggplot2::scale_y_continuous(expand=c(0,0), limits=c(0, max(1/vel + .01))) +
    ggplot2::ggtitle("Lineweaver-Burk-Plot")
#  Velo <-1/vel
#  Subs <- 1/sub
#  stats::coefficients(
#    stats::lm(Velo~Subs)
#  )
}

