#' Launch the TMDL Dashboard Shiny app
#' @export
run_app <- function() {
  # Preferred development location: source tree launcher/
  candidates <- c(
    file.path(getwd(), "launcher"),
    system.file("launcher", package = "TMDLDash")
  )
  
  app_dir <- candidates[dir.exists(candidates)][1]
  
  if (is.na(app_dir) || !nzchar(app_dir)) {
    stop(
      "Could not find the app directory.\n",
      "Checked:\n  - ", shQuote(file.path(getwd(), "launcher")), "\n",
      "  - ", shQuote(system.file("launcher", package = "TMDLDash")), "\n",
      call. = FALSE
    )
  }
  
  # Make the path discoverable by the app’s code
  options(TMDLDash.app_dir = app_dir)
  message("startup: using app_dir = ", app_dir)
  
  # Return a Shiny app object from that directory
  shiny::shinyAppDir(app_dir)
}