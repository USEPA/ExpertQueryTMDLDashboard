library(TMDLDash)

message("Launcher starting at: ", Sys.time())
app <- tryCatch(
  TMDLDash::run_app(),
  error = function(e) {
    message("run_app() failed: ", conditionMessage(e))
    traceback(2)
    stop(e)
  }
)
message("Launcher handing app to Shiny at: ", Sys.time())
app