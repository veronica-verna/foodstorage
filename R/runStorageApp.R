#' function for loading the storage-app 
#' 
#' @return This function is the only exported function of the foodstorage package. The package was built for analyzing a Food Co-ops' consumption. The results are represented in the shiny 'storage-app' which you can run by this function!
#' 
#'
#' @export
runStorageApp <- function(version, display.mode = "normal", port = 3838) {
  # locate all the shiny app examples that exist
  validApps <- list.files(system.file("shiny-apps", package = "foodstorage"))

  validAppsMsg <-
    paste0(
      "Valid apps are: '",
      paste(validApps, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(version) || !nzchar(version) ||
      !version %in% validApps) {
    stop(
      'Please run `runStorageApp()` with a valid app as an argument.\n',
      validAppsMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-apps", version, package = "foodstorage")
  shiny::runApp(appDir, display.mode = display.mode, port = port)
}