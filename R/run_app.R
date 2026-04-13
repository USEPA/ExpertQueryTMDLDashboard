#' #' Launch the TMDL Dashboard Shiny app
#' #' @export
#' run_app <- function() {
#'   # 1) Installed package location
#'   app_dir <- system.file("app", package = "TMDLDash")
#'   
#'   # 2) Fallbacks for development (load_all / source tree)
#'   if (!nzchar(app_dir) || !dir.exists(app_dir)) {
#'     pkg_path <- tryCatch(getNamespaceInfo("TMDLDash", "path"), error = function(e) "")
#'     candidates <- c(
#'       file.path(pkg_path, "inst", "app"),
#'       file.path(getwd(), "inst", "app")
#'     )
#'     app_dir <- candidates[dir.exists(candidates)][1]
#'     if (is.na(app_dir)) app_dir <- ""
#'   }
#'   
#'   if (!nzchar(app_dir) || !dir.exists(app_dir)) {
#'     stop(
#'       "Could not find the app directory.\n",
#'       "Checked:\n  - ", shQuote(system.file("app", package = "TMDLDash")), "\n",
#'       "  - inst/app under: ", shQuote(getNamespaceInfo("TMDLDash", "path")), "\n",
#'       "  - inst/app under: ", shQuote(getwd()), "\n",
#'       call. = FALSE
#'     )
#'   }
#'   
#'   # Make the path discoverable by the app’s code
#'   options(TMDLDash.app_dir = app_dir)
#'   message("startup: using app_dir = ", app_dir)
#'   
#'   # Return a Shiny app object from that directory
#'   shiny::shinyAppDir(app_dir)
#' }

#' Launch the TMDL Dashboard Shiny app
#' @export
run_app <- function() {
  # 1) Installed package location
  app_dir <- system.file("app", package = "TMDLDash")
  
  # 2) Fallbacks for development (load_all / source tree)
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    pkg_path <- tryCatch(getNamespaceInfo("TMDLDash", "path"), error = function(e) "")
    candidates <- c(
      file.path(pkg_path, "inst", "app"),
      file.path(getwd(), "inst", "app")
    )
    app_dir <- candidates[dir.exists(candidates)][1]
    if (is.na(app_dir)) app_dir <- ""
  }
  
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "Could not find the app directory.\n",
      "Checked:\n  - ", shQuote(system.file("app", package = "TMDLDash")), "\n",
      "  - inst/app under: ", shQuote(getNamespaceInfo("TMDLDash", "path")), "\n",
      "  - inst/app under: ", shQuote(getwd()), "\n",
      call. = FALSE
    )
  }
  
  # Make the path discoverable by the app’s code
  options(TMDLDash.app_dir = app_dir)
  message("startup: using app_dir = ", app_dir)
  
  # Return a Shiny app object from that directory
  shiny::shinyAppDir(app_dir)
}