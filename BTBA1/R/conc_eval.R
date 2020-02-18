#' Generischer Workflow Konzentrationsbestimmung durch Regression
#'
#' Hiermit kannst du die Ergebnisse deiner Standardreihe (Absorptionsmessung jeder Art) plotten, bewerten und
#' die Konzentration deiner unbekannten Probe bestimmen.
#' Verdünnungsfaktoren sind noch kein Bestandteil der Funktionen
#'
#' @param abs_P float
#'
#' @return float
#'
#' @export
Konzentration <- function(abs_P){
  (abs * LinMod_CEv$coefficients[2] + LinMod_CEv$coefficients[1])
}
