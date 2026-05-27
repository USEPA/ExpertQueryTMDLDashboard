local({
  # Package mode?
  if (nzchar(system.file(package = "TMDLDash"))) {
    message("Running in PACKAGE mode")
    source(system.file("app", "app.R", package = "TMDLDash"), local = TRUE)$value
  } else {
    message("Running in PLAIN SHINY mode")
    # Make helpers in R/ available in plain mode
    rdir <- file.path(getwd(), "R")
    if (dir.exists(rdir)) {
      rfiles <- list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE)
      lapply(rfiles, sys.source, envir = environment())
    }
    # Run the app directly from the repo
    source(file.path("inst", "app", "app.R"), local = TRUE)$value
  }
})