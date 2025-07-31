library(rExpertQuery)
library(dplyr)
library(jsonlite)
library(lubridate)
library(httr2)


# check resposne before downloading
base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/"

nat.url <- "national-downloads/"

latest.json <- jsonlite::fromJSON(paste0(base.url, nat.url, "latest.json"))

folder.num <- latest.json$julian

file <- "tmdl.csv"

url <- paste0(base.url, nat.url, folder.num, "/", file, ".zip")

check.api <- httr2::request(url) %>%
  httr2::req_perform() %>%
  httr2::resp_status()

# if response is not OK, do not continue (retain older data)
if(check.api != 200) {
  print("Expert Query webs services are not working, TMDL data not updated. Contact ATTAINS@epa.gov for help.")
}

# if response is OK, update data
if(check.api == 200) {
orig.df <- rExpertQuery::EQ_NationalExtract("tmdl")

# start by filtering to necessary cols
filt.df <- orig.df %>%
  dplyr::filter(!is.na(pollutant),
                pollutant != "") %>%
  dplyr::select(region, state, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  dplyr::distinct() %>%
  dplyr::mutate(fiscalYearEstablished = as.numeric(fiscalYearEstablished)) %>%
  dplyr::group_by(actionId, assessmentUnitId, pollutant) %>%
  dplyr::mutate(addressedParameters = paste(sort(unique(addressedParameter)), collapse = "; ")) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

# create df of parameters
parameters <- filt.df %>%
  dplyr::select(addressedParameter, addressedParameters) %>%
  distinct() %>%
  dplyr::arrange(addressedParameter)

# create df for counting by action id/assessment unit/pollutant
act.df <- filt.df %>%
  dplyr::select(-addressedParameter) %>%
  dplyr::distinct()

# create df for counting by assessment unit/pollutant
wb.df <- orig.df %>%
  dplyr::filter(!is.na(pollutant),
                pollutant != "") %>%
  dplyr::select(region, state, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  dplyr::distinct() %>%
  dplyr::group_by(pollutant, assessmentUnitId, actionId) %>%
  dplyr::mutate(addressedParameter = paste0(actionId, ": ", paste(addressedParameter, collapse = "; "))) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::group_by(pollutant, assessmentUnitId) %>%
  dplyr::mutate(addressedParameters = paste(sort(unique(addressedParameter)), collapse = "; "),
                actionIds = paste(unique(actionId), collapse = "\n"),
                actionNames = paste(unique(actionName), collapse = "\n"),
                planSummaryLinks = paste(unique(planSummaryLink), collapse = "\n")) %>%
  dplyr::ungroup() %>%                
  dplyr::select(-addressedParameter, -actionId, -actionName, -planSummaryLink) %>%
  distinct()

rm(orig.df)

# create df of states and regions
states_regions <- filt.df %>%
  dplyr::select(state, region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(region, state)

# create df of pollutants and groups
pollutants_groups <- df %>%
  dplyr::select(pollutant, pollutantGroup) %>%
  dplyr::distinct() %>%
  dplyr::arrange(pollutantGroup, pollutant)

# find max year
max_year <- as.numeric(format(Sys.Date(), "%Y"))

# Create a list of years
years_list <- seq(1995, as.numeric(format(Sys.Date(), "%Y")))

# Create list of pollutant group categories
categories <- unique(pollutants_groups$pollutantGroup)

# create df of action names by state and region
actions <- df %>%
  dplyr::select(actionName, state, region) %>%
  dplyr::distinct()

# create df of assessment unit names by state and region
aus <- df %>%
  dplyr::select(assessmentUnitName, state, region) %>%
  dplyr::distinct()


# get update date

update.base <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/national-downloads/"

update.dates <- jsonlite::fromJSON(paste0(update.base, folder.num, "/ready.json"))

update.df <- update.dates$details

update.tmdls <- update.df %>%
  dplyr::filter(name == "attains_app.profile_tmdl") %>%
  dplyr::select(last_refresh_end_time) %>%
  dplyr::pull() %>%
  as.Date() %>%
  lubridate::with_tz(tx = "US/Eastern") %>%
  format("%B %d, %Y at %I:%M %Z")

# create .RData file

save(act.df, wb.df, states_regions, pollutants_groups, parameters, max_year, years_list, categories,
     update.tmdls, file = "EQ_data.RData")
}

