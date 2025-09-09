# check for rExpertQuery in library, install if not found

if(!"rExpertQuery" %in% installed.packages()) {
  
  if(!"remotes"%in%installed.packages()){
    install.packages("remotes")
  }
  
  remotes::install_github("USEPA/rExpertQuery", ref = "develop", dependencies = TRUE, force = TRUE)
}

# open required packages (R Studio should prompt you to install others if not installed already as 
# they are available on CRAN)
library(rExpertQuery)
library(dplyr)
library(jsonlite)
library(lubridate)
library(httr2)


# get update date for printed message
# base url
base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/"

# specify nat downloads
nat.url <- "national-downloads/"

# get latest json
latest.json <- jsonlite::fromJSON(paste0(base.url, nat.url, "latest.json"))

# identify folder number
folder.num <- latest.json$julian

# get tupdate info for all profiles
update.dates <- jsonlite::fromJSON(paste0(base.url, nat.url, folder.num, "/ready.json"))

# open update details
update.df <- update.dates$details

# get and format update date for tmdl profile
update.tmdls <- update.df %>%
  dplyr::filter(name == "attains_app.profile_tmdl") %>%
  dplyr::select(last_refresh_end_time) %>%
  dplyr::pull() %>%
  lubridate::as_datetime() %>%
  lubridate::with_tz(tx = "US/Eastern") %>%
  format("%B %d, %Y")


# download Expert Query National Profile for TMDLs
nat.tmdl <- rExpertQuery::EQ_NationalExtract("tmdl")

# select required columns and remove duplicates
clean.tmdl <- nat.tmdl %>%
  # drop object ID and remove true dups
  dplyr::select(-objectId) %>%
  dplyr::distinct() %>%
  # required cols
  dplyr::select(region, state, fiscalYearEstablished, pollutant, pollutantGroup,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  # remove dups that may result from multiple addressed params, etc.
  dplyr::distinct() %>%
  # format fiscal year as numeric
  dplyr::mutate(fiscalYearEstablished = suppressWarnings(as.numeric(fiscalYearEstablished)))

# count tmdls
n.tmdls <- clean.tmdl %>%
  dplyr::n_distinct()

# print statement for HTF report
print(paste("The TMDL count date from Expeert Query last refreshed on  ", update.tmdls, 
      " was ", formatC(n.tmdls, big.mark = ",")))

