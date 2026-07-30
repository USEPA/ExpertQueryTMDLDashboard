#' Launch the TMDL Dashboard Shiny app
#' @export
run_app <- function() {
  app_dir <- normalizePath(file.path(getwd(), "launcher"), winslash = "/", mustWork = TRUE)
  
  options(TMDLDash.app_dir = app_dir)
  message("startup: using app_dir = ", app_dir)
  
  shiny::shinyAppDir(app_dir)
}
