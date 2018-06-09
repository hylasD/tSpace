#' runExplorer
#'
#' Starts interactive UI for data exploration
#'
#' @export
runExplorer <- function() {

  #if(type == 'category'){
    appDir <- system.file("shiny", "plot", package = "tSpace")
    if (appDir == "") {
      stop("Could not find directory. Try re-installing `tSpace`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")


}
