#' runExplorer
#'
#' Starts interactive UI for data exploration
#' @import plotly
#' @export
runExplorer <- function(type = 'demo') {

  #if(type == 'category'){
    appDir <- system.file("shiny", type, package = "tSpace")
    if (appDir == "") {
      stop("Could not find directory. Try re-installing `tSpace`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")


}
