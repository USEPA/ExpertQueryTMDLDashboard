# app.R (repo root)

local({
  # Make any helpers in R/ available when running from source
  rdir <- file.path(getwd(), "R")
  if (dir.exists(rdir)) {
    rfiles <- list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE)
    lapply(rfiles, sys.source, envir = environment())
  }
  # Run the actual app (your real app lives in inst/app/app.R)
  source(file.path("inst", "app", "app.R"), local = TRUE)$value
})