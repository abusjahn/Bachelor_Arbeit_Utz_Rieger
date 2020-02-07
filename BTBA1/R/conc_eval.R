#' Generischer Workflow Konzentrationsbestimmung durch Regression
#'
#' Hiermit kannst du die Ergebnisse deiner Standardreihe (Absrptionsmessung jeder Art) Plotten, bewerten und
#' die Konzentration deinrt Unbekannten Probe bestimmen.
#' Verdünnunsfaktoren sind noch kein Bestandteil der Funktionen
#'
#' @param abs_P float
#'
#' @return float
#'
#' @export
Konzentration <- function(abs_P){
  (abs * LinMod_CEv$coefficients[2] + LinMod_CEv$coefficients[1])
}
