local({
  # If the package is installed, run from the installed app dir (package mode)
  pkg_app <- system.file("app", package = "TMDLDash")
  if (nzchar(pkg_app) && dir.exists(pkg_app)) {
    message("Running in PACKAGE mode")
    # Source app.R and temporarily setwd() to that directory so relative paths work
    source(file.path(pkg_app, "app.R"), local = TRUE, chdir = TRUE)$value
  } else {
    message("Running in PLAIN SHINY mode")
    # Make helpers in R/ available in plain mode (if you use files under R/)
    rdir <- file.path(getwd(), "R")
    if (dir.exists(rdir)) {
      rfiles <- list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE)
      lapply(rfiles, sys.source, envir = environment())
    }
    # Source app from launcher and temporarily setwd() there so relative paths work
    source(file.path("launcher", "app.R"), local = TRUE, chdir = TRUE)$value
  }
})
