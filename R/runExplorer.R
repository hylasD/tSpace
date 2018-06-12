#' runExplorer
#'
#' Starts interactive UI for data exploration. It allows upload of a custom
#' csv file, up to 1.2 Gb, however large data sets will load slowly and
#' all changes within application will be slow. It includes T cell FACS data from
#' the pre-print (doi: https://doi.org/10.1101/336313)
#' @import plotly
#' @export
runExplorer <- function() {

  #if(type == 'category'){
    appDir <- system.file("shiny", 'demo', package = "tSpace")
    if (appDir == "") {
      stop("Could not find directory. Try re-installing `tSpace`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")


}
