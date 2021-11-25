#' Expand sessions
#'
#' Expand a sessions dataframe (one row per session) into a dataframe with one row per person-date,
#' with time recorded on that date. This function handles the messiness of
#' getting the amount of time recorded for a specific person on a specific day
#' (since a session start on day 1 and ending on day 3 means that all of day 2 was recorded)
#' @param hyfe_data A standard `hyfe_data` object downloaded
#' from the Research Dashboard (for external partners) or from `hyferdrive` (internal analysts).
#' See full details and examples in the [package vignette](https://hyfe-ai.github.io/hyfer/#hyfedata).
#' @param unit Day, hour (not yet implemented)
#' @param tz desc
#' @param verbose Print status updates?
#' @return A list with `timetable` (a dataframe with one row per person-date or person-hour)
#' and `series` (a dataframe in which each row is a timestamp, and each column is a UID)
#' @export
#'
expand_sessions <- function(hyfe_data,
                            unit = 'hour',
                            timestamp_start = NULL,
                            timestamp_stop = NULL,
                            tz = NULL,
                            create_table = TRUE,
                            create_series = FALSE,
                            inactive_value = 0,
                            verbose = FALSE){
  start_proc <- Sys.time()

  if(FALSE){
    # for debugging
    data(hyfe_data)
    unit  = 'hour'
    create_table = TRUE
    create_series = FALSE
    inactive_value = 0
    verbose=TRUE
  }

  if(verbose){message('--- staging time series . . .')}

  sessions <- hyfe_data$sessions ; head(sessions)
  if(is.null(tz)){
    tz <- hyfe_data$cohort_settings$timezone ; head(tz)
  }
  if(length(tz)>1){tz <- 'UTC'}

  if(nrow(sessions) < 1){
    return(data.frame())
  } else {

    head(sessions)

    # Format times
    sessions <- sessions %>% arrange(start)

    if(!is.null(timestamp_start)){
      ts_start <- timestamp_start
    }else{
      ts1 <- sessions$start %>% min()
      ts1 <- format_hyfe_time(ts1, timezone=tz)
      ts1 <- ts1$date_floor[1]
      ts1 <- as.numeric(ts1)
      ts_start <- ts1
      #ts_start <-
      #  sessions$start %>% min() %>%
      #  format_hyfe_time(timezone=tz) %>%
      #  dplyr::select(date_floor) %>%
      #  as.numeric()
    }

    if(!is.null(timestamp_stop)){
      ts_stop <- timestamp_stop
    }else{
      ts1 <- sessions$start %>% max()
      ts1 <- format_hyfe_time(ts1, timezone=tz)
      ts1 <- ts1$date_ceiling[1]
      ts1 <- as.numeric(ts1)
      ts_stop <- ts1

      #ts_stop <-
      #sessions$stop %>% max() %>%
      #format_hyfe_time(timezone = tz) %>%
      #dplyr::select(date_ceiling) %>%
      #as.numeric()
    }

    tot_duration <- ts_stop - ts_start
    tot_hours <- tot_duration / 3600
    tot_days <- tot_hours / 24

    # Get basic info
    #start_date <- lubridate::floor_date(sessions_df$start_time[1],unit='day')
    #stop_date <- lubridate::ceiling_date(sessions_df$stop_time[nrow(sessions_df)],unit='day')
    #ts_start <- unclass(start_date)
    #ts_stop <- unclass(stop_date)
    #duration_days <- as.numeric(difftime(stop_date,start_date,unit='days') + 1)
    #duration_hours <- duration_days * 24

    # Stage foundational timetable
    if(unit=='day'){
      ts <- ts_start + (3600*24*(0:ceiling(tot_days)))
    }
    if(unit=='hour'){
      ts <- ts_start + (3600*(0:tot_hours))
    }
    ts
    df <- format_hyfe_time(ts, timezone = tz)
    head(df)

    # Stage timetable result
    if(create_table){
      results <- data.frame()
    }else{
      results <- df
    }

    # Stage timeseries (each row is a second)
    # Each column will be a UID
    ts_df <- data.frame()
    if(create_series){
      ts_series <- ts_start:ts_stop
      ts_df <- data.frame(timestamp=ts_series)
    }

    # Loop through each uid
    uids <- unique(sessions$uid)
    i=1
    for(i in 1:length(uids)){
      uidi <- uids[i]

      if(verbose){message('--- user ',i,' out of ',length(uids),' : ',uidi,' . . .')}

      sessi <- sessions %>% filter(sessions == uidi)

      # Stage results for this uid
      dfi <- df
      dfi$uid <- uidi
      secs <- rep(0,times=nrow(df))

      # Remove 0 sessions
      sessi <- sessi[sessi$duration > 0,]
      if(nrow(sessi)>0){
        # Simplify session data
        sessi <- sessi %>% select(start, stop)
        head(sessi)

        # Collapse sessions into a vector of every timestamp second monitored
        ts_sessions <- apply(sessi,1,function(x){x[1]:x[2]})
        ts_sessions <- unlist(ts_sessions,use.names=FALSE)

        if(create_table){
          # Count up seconds in daily bins
          recut  <- cut(ts_sessions,df$timestamp,labels=FALSE) ; length(recut)
          counts <- table(recut)
          is <- as.numeric(names(counts)) ; is
          secs[is] <- as.numeric(counts) ; secs

          # Store result for timetable
          dfi$session_time <- secs
          results <- rbind(results,dfi)
        }

        if(create_series){
          # Add to timeseries df
          uid_series <- ifelse(ts_series %in% ts_sessions, 1, inactive_value)
          ts_df$new <- uid_series
          names(ts_df)[ncol(ts_df)] <- uidi
        }
      }
    }

    head(results)
    head(ts_df)

    return(list(timetable = results,
                series = ts_df))
  }
}
