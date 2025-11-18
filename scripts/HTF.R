# load libraries and data
library(dplyr)

load("EQ_data.RData")

# HTF states
htf.states <- c("AR", "IA", "IL", "IN", "KY", "LA", "MN", "MO", "MS", "OH", "TN", "WI")

# filter filt.df (clean TMDL data set) for HTF states and NUTRIENTS group
htf.tmdl <- filt.df %>%
  dplyr::filter(state %in% htf.states &
                  pollutantGroup %in% c("NUTRIENTS", "ALGAL GROWTH"))

# count HTF tmdls
n.htf.tmdls <- htf.tmdl %>%
  dplyr::n_distinct()

# create list of pollutants in NUTRIENTS group with Expert Query function
nutrient.polls <- rExpertQuery::EQ_DomainValues("param_name") %>%
  dplyr::filter(context %in% c("NUTRIENTS", "ALGAL GROWTH")) %>%
  dplyr::arrange(context, name) %>%
  dplyr::select(name) %>%
  dplyr::mutate(name = paste(name, collapse = "\n")) %>%
  dplyr::distinct() %>%
  dplyr::pull()

# print statement for HTF report
print(cat("The count for the ", format(Sys.Date(), "%Y"), " HTF RTC TMDLs for nutrients and algal growth in these ",
             "12 states was: ", formatC(n.htf.tmdls, big.mark = ","), " (as of ", update.tmdls, ").", "\n",
             "\n", "This was calculated using the 'NUTRIENTS' and 'ALGAL GROWTH' pollutant groups for the 12 HTF states. ",
             "At the time of the 2025 report pull (", format(as.Date(update.tmdls, "%B %d, %Y"), "%B %Y"), ")", 
             " these pollutant groups included: ", "\n","\n", nutrient.polls, sep = ""))

