#' Create a non-linear Regression for a Michaelis-Menten type Enzyme with a little knowlege of its Km and Vmax
#'
#' @param sub list
#' @param vel list
#' @param V_max float
#' @param K_m float
non_linear_MM <- function(sub, vel, V_max, K_m){
  ggplot2::ggplot(mapping = ggplot2::aes(x=sub, y=vel))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "nls",
                method.args = base::list(formula = y ~ Vmax * x / (Km + x),
                                   start = base::list(Vmax = V_max, Km = K_m)))
}
