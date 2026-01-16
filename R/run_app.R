#' Start de DrawsNeeded Shiny App
#'
#' @export
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable
run_app <- function() {
  appDir <- system.file("shiny", "drawsneeded", package = "drawsneeded")
  if (appDir == "") {
    stop("De Shiny app kon niet gevonden worden in het package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
