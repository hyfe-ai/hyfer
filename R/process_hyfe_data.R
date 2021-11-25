#' Process a `hyfe_data` object for plotting/analysis
#'
#' This function converts a raw `hyfe_data` object into a polished `hyfe` object.
#' This function is usually the first step after downloading data.
#'
#' @param hyfe_data A standard `hyfe_data` object downloaded
#' from the Research Dashboard (for external partners) or from `hyferdrive` (internal analysts).
#' See full details and examples in the [package vignette](https://hyfe-ai.github.io/hyfer/#hyfedata).
#' @param by_user Should data be processed in bulk, or for each user separately?
#' Processing in bulk (`by_user = FALSE`, the default) can be quicker and makes summary plots easier
#' (e.g., grand total monitoring hours across the cohort). Processing for each user separately (`by_user = TRUE`)
#' can take more time at this stage, but opens up more possibilities for plotting and is necessary for unbiased metrics
#' in which each user is given equal weight (e.g., mean cough rate across the cohort).
#' @param verbose Print status updates?
#'
#' @return A `hyfe` object, which is a list with several named slots.
#' See full details and examples in the [package vignette](https://hyfe-ai.github.io/hyfer/#hyfe_object).
#'
#' @export
#'
process_hyfe_data <- function(hyfe_data,
                              by_user = FALSE,
                              verbose=TRUE){

  if(FALSE){
    data(hyfe_data)
    by_user = FALSE
    verbose=TRUE
  }

  ho <- hyfe_data

  # Stage timezone
  tz <- unique(hyfe_data$cohort_settings$timezone) ; head(tz)
  if(is.null(tz[1])){tz <- 'UTC'}
  if(is.na(tz[1])){tz <- 'UTC'}
  if(length(tz)>1){tz <- 'UTC'}
  tz

  # Stage results
  coughs <- NULL

  # Format times in sounds
  if(verbose){message('Formatting cough times ...')}
  sounds <- hyfe_data$sounds
  if(!is.null(sounds)){
    nrow(sounds)
    coughs <- sounds %>% filter(is_cough==TRUE)
    nrow(coughs)
    if(nrow(coughs)>0){
      cough_times <- format_hyfe_time(coughs$timestamp)
      coughs <- data.frame(coughs, cough_times[,-1])
    }
  }
  head(coughs)
  ho$coughs <- coughs


  # Cough bouts


  # Expand sessions
  #if(verbose){message('Expanding sessions')}
  #hyfe_time <- expand_sessions(hyfe_data,
  #                             unit = 'hour',
  #                             verbose=verbose)$timetable
  #hyfe_data$timetable <- hyfe_time


  # Create timetables all-together or by user?
  if(!by_user){
    # Lump all users together
    hyfe_tables <- hyfe_timetables(hyfe_data,
                                   verbose=verbose)
    ho$hours <- hyfe_tables$hours
    ho$days <- hyfe_tables$days
    ho$weeks <- hyfe_tables$weeks

  }else{
    # Loop through each user
    uids <-unique(hyfe_data$sessions$uid) ; uids
    ts_start <- min(hyfe_data$sessions$start)
    ts_stop <- max(hyfe_data$sessions$stop)

    hyfe_tables <- list()
    i=1
    for(i in 1:length(uids)){
      uidi <- uids[i]
      uid_data <- filter_to_user(uid=uidi, hyfe_data)

      if(nrow(uid_data$sessions[uid_data$sessions$duration > 0,]) > 0){
        if(verbose){message('--- building summary tables for user ',i,' out of ',length(uids),' : ',uidi)}
        user_tables <- hyfe_timetables(uid_data,
                                       timestamp_start = ts_start,
                                       timestamp_stop = ts_stop,
                                       verbose=FALSE)
        user_tables$id_key <- hyfe_data$id_key %>% filter(uid == uidi)
        #user_tables$hours
        #user_tables$hours %>% nrow %>% print
        #user_tables$hours %>% tail
        #user_tables$days %>% tail
        #user_tables$weeks %>% as.data.frame %>% tail
        hyfe_tables[[length(hyfe_tables)+1]] <- user_tables

        ho$user_summaries <- hyfe_tables
      }
    }
  }

  if(verbose){message('Finished!')}
  return(ho)

}
