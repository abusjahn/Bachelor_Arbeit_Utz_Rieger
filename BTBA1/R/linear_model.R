#' Linear model for concentration evaluation
#'
#' This must be performed in advanvce of usage of the function conc_eval to produce a linear model suiteable
#' for concentration evaluation.
#'
#' @param abs list
#' @param conc list
#'
#' @return tataframe
#' @export
#'
LinMod_CEv <- function(abs, conc) {
  return(
  stats::lm(
    conc ~ abs
  )
)
  return(summary(LinMod_CEv))
}

