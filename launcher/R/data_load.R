# launcher/R/data_load.R

EQ_cache <- new.env(parent = emptyenv())

load_eq_data <- function() {
  p <- file.path("data", "EQ_data.RData")
  if (!file.exists(p)) {
    stop("EQ_data.RData not found at: ", p)
  }
  
  tmp <- new.env(parent = emptyenv())
  objs <- load(p, envir = tmp)
  
  message("Loaded EQ_data.RData from: ", p)
  message("Objects loaded: ", paste(objs, collapse = ", "))
  
  list2env(as.list(tmp), envir = EQ_cache)
  EQ_cache$loaded <- TRUE
}

load_rmd_data <- function() {
  p <- file.path("data", "RMD_data.RData")
  if (!file.exists(p)) {
    stop("RMD_data.RData not found at: ", p)
  }
  
  tmp <- new.env(parent = emptyenv())
  objs <- load(p, envir = tmp)
  
  message("Loaded RMD_data.RData from: ", p)
  message("Objects loaded: ", paste(objs, collapse = ", "))
  
  list2env(as.list(tmp), envir = EQ_cache)
}

load_all_app_data <- function() {
  load_eq_data()
  load_rmd_data()
}