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

# number of original records
orig.n <- dim(orig.df)[1]

# number of records with no pollutant
orig.nopoll.n <- dim(orig.df %>%
  dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
    dplyr::distinct() %>%
  dplyr::filter(is.na(pollutant) |
                pollutant == ""))[1]

# number of records with no assessment unit id
orig.noauid.n <- dim(orig.df %>%
                       dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                                     actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
                       dplyr::distinct() %>%
                       dplyr::filter(is.na(assessmentUnitId) |
                                       assessmentUnitId == ""))[1]

# number of records with no assessment unit id or pollutant
orig.noauidpoll.n <- dim(orig.df %>%
                       dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                                     actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
                       dplyr::distinct() %>%
                       dplyr::filter(is.na(assessmentUnitId) |
                                       assessmentUnitId == ""|
                                     is.na(pollutant) |
                                       pollutant == ""))[1]

# number of duplicate records
# dups only df
orig.dups <- orig.df %>%
  dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(dup.count = dplyr::n()) %>%
  dplyr::filter(dup.count > 1)

# max and min number of repeats
max.dups <- orig.dups %>%
  dplyr::ungroup() %>%
  dplyr::select(dup.count) %>%
  dplyr::distinct() %>%
  dplyr::slice_max(dup.count) %>%
  dplyr::pull()

min.dups <- orig.dups %>%
  dplyr::ungroup() %>%
  dplyr::select(dup.count) %>%
  dplyr::distinct() %>%
  dplyr::slice_min(dup.count) %>%
  dplyr::pull()

# number of duplicate records
orig.dups.n <- dim(orig.dups)[1]


# number of distinct records
orig.distinct.n <- dim(orig.df %>%
                         dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                                       actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
                          dplyr::distinct())[1]

# number of dups removed
orig.dups.removed <- orig.dups.n - orig.distinct.n

# start by filtering to necessary cols
filt.df <- orig.df %>%
  dplyr::filter(!is.na(pollutant),
                pollutant != "",
                !is.na(assessmentUnitId),
                assessmentUnitId != "") %>%
  dplyr::select(region, state, actionAgency, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter,  
                actionId, actionName, assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  dplyr::distinct() %>%
  dplyr::mutate(fiscalYearEstablished = as.numeric(fiscalYearEstablished)) %>%
  dplyr::group_by(actionId, assessmentUnitId, pollutant) %>%
  dplyr::mutate(addressedParameters = paste(sort(unique(addressedParameter)), collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()

# create df of parameters
parameters <-filt.df %>%
  dplyr::select(addressedParameter, addressedParameters) %>%
  distinct() %>%
  dplyr::arrange(addressedParameter)

# create df of pollutants, pollutant groups and addressed parameters
addparameters_filter_poll <- orig.df %>%
  dplyr::select(actionId, pollutant, pollutantGroup, addressedParameter) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(pollutant),
                pollutant != "",
                !is.na(addressedParameter),
                addressedParameter != "") %>%
  dplyr::group_by(actionId) %>%
  dplyr::mutate(addressedParameters = paste(sort(unique(addressedParameter)), collapse = "; ")) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::select(-actionId) %>%
  dplyr::distinct() %>%
  dplyr::arrange(addressedParameter)

# create df to filter addressed params by pollutant Group
addparameters_filter_pg <- orig.df %>%
  dplyr::select(pollutant, pollutantGroup, addressedParameter) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(pollutant),
                pollutant != "",
                !is.na(addressedParameter),
                addressedParameter != "") %>%
  dplyr::distinct() %>%
  dplyr::arrange(addressedParameter)
  

# remove intermediat objects
rm(orig.df)

# create df of states and regions
states_regions <- filt.df %>%
  dplyr::select(state, region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(region, state)

# create df of pollutants and groups
pollutants_groups <- filt.df %>%
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
actions <- filt.df %>%
  dplyr::select(actionName, state, region) %>%
  dplyr::distinct()

# create list of actionAgency values
act_agencies <- filt.df %>%
  dplyr::select(actionAgency) %>%
  dplyr::distinct() %>%
  dplyr::pull()

# create df of assessment unit names by state and region
aus <- filt.df %>%
  dplyr::select(assessmentUnitName, state, region) %>%
  dplyr::distinct()

# reorder filt df for use in app
filt.df <- filt.df %>%
  dplyr::select(region, state, fiscalYearEstablished, pollutant,
                pollutantGroup, addressedParameters, actionId, actionName,
                assessmentUnitId, assessmentUnitName, planSummaryLink) %>%
  dplyr::distinct()

# number of filt.df records
filt.df.n <- dim(filt.df)[1]


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

rm(update.base, update.dates, update.df, aus, actions)

# create .RData files

save(filt.df, states_regions, addparameters_filter_poll, addparameters_filter_pg,
     pollutants_groups, parameters, max_year, years_list, categories, update.tmdls,
     act_agencies,
     file = "app/data/EQ_data.RData")

save(update.tmdls, orig.distinct.n, orig.dups.n, orig.dups.removed, orig.n,
     orig.noauid.n, orig.nopoll.n, orig.noauidpoll.n, max.dups, min.dups, filt.df.n,
     file = "www/RMD_data.RData")
}

