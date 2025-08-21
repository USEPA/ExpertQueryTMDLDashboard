load("EQ_data.RData")

# this section results in a data set which can be used to count tmdls by unique combination of
# assessment unit/pollutant/action id

# this is the tmdl data set with each row representing a unique combination of assessment unit/pollutant/action id
# all duplicates have been removed, all records where pollutant or assessment unit is NA or null have been removed
clean.tmdl.actionid <- filt.df 

# count tmdls
n.tmdls.action.id <- clean.tmdl.actionid %>%
  dplyr::n_distinct()

# print statement for HTF report
print(paste0("The TMDL count (unqiue combinations of assessment unit/pollutant/action id) ",
"from Expert Query was last refreshed on ", update.tmdls, 
            " was ", formatC(n.tmdls.action.id, big.mark = ","), "."))

# this section results in a data set which can be used to count tmdls by unique combination of
# assessment unit/pollutant

# this is the tmdl data set with each row representing a unique combination of assessment unit/pollutant/action id
# all duplicates have been removed, all records where pollutant or assessment unit is NA or null have been removed
clean.tmdl.wbpoll <- filt.df %>%
  dplyr::select(region, state, pollutant, pollutantGroup, assessmentUnitId, 
                assessmentUnitName) %>%
  dplyr::distinct() 

# count tmdls
n.tmdls.wbpoll <- clean.tmdl.wbpoll %>%
  dplyr::n_distinct()

# print statement for HTF report
print(paste0("The TMDL count ( unqiue combinations of assessment unit/pollutant/action id) ",
             "from Expert Query was last refreshed on  ", update.tmdls, 
             " was ", formatC(n.tmdls.wbpoll, big.mark = ",")))

