#' concentration evaluation
#'
#' @param abs_P absorption of a unknown sample
#' @param abs_std absorption of calibrationstandards
#' @param conc_std concentration of calibrationstandards
#'
#' @return float
#'
#' @export
conc_eval <- function(abs_P, abs_std, conc_std){
  (LinMod <- stats::lm(
                       conc_std ~ abs_std)
  ) %>%
    base::summary() %>%
    pander::pander() %>%
    base::print()
  base::print(
              abs_P * LinMod$coefficients[2] + LinMod$coefficients[1]
  )
}
