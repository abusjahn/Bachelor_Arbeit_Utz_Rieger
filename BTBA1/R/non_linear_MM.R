#' Create a non-linear Regression for a Michaelis-Menten type Enzyme with a little knowlege of its Km and Vmax
#'
#' @param sub list
#' @param vel list
#' @param V_max_start float
#' @param K_m_start float
non_linear_MM <- function(sub, velo, V_max_start , K_m_start){ # ich würde gerne die Argumente V_max_start und K_m_start auf die Variablen K_m und V_max übertragen
  fit <- stats::nls(velo ~ (V_max * sub) / (K_m + sub),
              start = c(V_max_start = V_max , K_m_start = K_m)) %>%  # hier sind die Variablen als Starwerte füe das Modell
              base::summary() %>%
              base::print()
  ggplot2::ggplot(mapping = ggplot2::aes(x = sub, y = velo))+
    ggplot2::geom_point()+
    ggplot2::stat_function(fun = function(sub){ (stats::coef(fit)[[1]] * sub) / (stats::coef(fit)[[2]] + sub)})
}

