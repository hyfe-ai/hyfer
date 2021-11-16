#' Produce summary tables of Hyfe data (hourly, daily, weekly)
#'
#' @param hyfe_data A standard `hyfe_data` object.
#' @param tz desc
#' @param timestamp_start If you want to specify the beginning of the time table, enter timestamp here.
#' @param timestamp_stop Specify end of timetable, if you want.
#' @param verbose Print status updates?
#'
#' @return
#' @export
#'
hyfe_timetables <- function(hyfe_data,
                            tz = NULL,
                            timestamp_start = NULL,
                            timestamp_stop = NULL,
                            verbose=TRUE){

  if(FALSE){
    data(hyfe_data)
    hyfe_data <- uid_data
    verbose=TRUE
    tz = NULL
    timestamp_start = NULL
    timestamp_stop = NULL

    hyfe_data <- uid_data
    lapply(hyfe_data,nrow)
  }

  # Get subset of coughs
  coughs <- hyfe_data$sounds %>% filter(is_cough==TRUE)

  # Get base timetable
  if(verbose){message('Expanding sessions')}
  #if('timetable' %in% names(hyfe_data)){
  #  hyfe_time <- hyfe_data$timetable
  #}else{
    hyfe_time <- expand_sessions(hyfe_data,
                                 unit = 'hour',
                                 tz = tz,
                                 timestamp_start,
                                 timestamp_stop,
                                 verbose=verbose)$timetable
  #}

  # Hourly timestable
  uids <- unique(hyfe_time$uid)
  if(verbose){message('Creating hourly timetable summary ...')}
  head(hyfe_time)
  nrow(hyfe_time)
  tt_base <- hyfe_time %>%
    dplyr::filter(uid==uids[1]) %>%
    select(-session_time, -uid); nrow(tt_base)
  tt_sum  <- hyfe_time %>%
    dplyr::group_by(timestamp) %>%
    dplyr::summarize(n_uid = length(unique(uid[session_time > 0])),
                     session_seconds = sum(session_time),
                     session_hours = session_seconds / 3600,
                     session_days = session_seconds / 86400)
  nrow(tt_sum)
  tt <- left_join(tt_base,tt_sum,by='timestamp')
  head(tt)

  # Peak totals
  if(verbose){message('--- summarizing detections of all explosive sounds ...')}
  tt_peaks <- rep(0,times=nrow(tt))
  sounds <- hyfe_data$sounds
  recut  <- cut(sounds$timestamp,tt$timestamp,labels=FALSE) ; length(recut)
  counts <- table(recut)
  tt_indices <- as.numeric(names(counts)) ; tt_indices
  tt_peaks[tt_indices] <- as.numeric(counts) ; tt_peaks
  tt$peaks <- tt_peaks

  # Cough totals
  if(verbose){message('--- summarizing cough predictions ...')}
  tt_coughs <- rep(0,times=nrow(tt))
  recut  <- cut(coughs$timestamp,tt$timestamp,labels=FALSE) ; length(recut)
  counts <- table(recut)
  tt_indices <- as.numeric(names(counts)) ; tt_indices
  tt_coughs[tt_indices] <- as.numeric(counts) ; tt_coughs
  tt$coughs <- tt_coughs

  # Rate
  tt <- tt %>%  dplyr::mutate(cough_rate = coughs / session_hours)

  # Add cumulative
  head(tt)
  tt <-
    tt %>%
    dplyr::mutate(session_seconds_tot = cumsum(session_seconds),
           session_hours_tot = cumsum(session_hours),
           session_days_tot = cumsum(session_days),
           peaks_tot = cumsum(peaks),
           coughs_tot = cumsum(coughs))

  tt %>% head
  tt %>% tail

  hyfe_hours <- tt
  head(hyfe_hours)
  length(unique(hyfe_hours$date))

  if(verbose){message('Creating daily timetable summary ...')}
  hyfe_days <- hyfe_hours %>%
    group_by(date) %>%
    summarize(tz = unique(tz)[1],
              date_floor = unique(date_floor)[1],
              date_ceiling = unique(date_ceiling)[1],
              year = unique(year)[1],
              week = unique(week)[1],
              yday = unique(yday)[1],
              study_week = unique(study_week)[1],
              study_day = unique(study_day)[1],
              n_uid = max(n_uid)[1],
              session_seconds = sum(session_seconds)[1],
              session_hours = sum(session_seconds) / 3600,
              session_days = sum(session_seconds) / 86400,
              peaks = sum(peaks),
              coughs = sum(coughs)) %>%
    dplyr::mutate(cough_rate = (coughs / session_hours)*24)

  hyfe_days <-
    hyfe_days %>%
    dplyr::mutate(session_seconds_tot = cumsum(session_seconds),
                  session_hours_tot = cumsum(session_hours),
                  session_days_tot = cumsum(session_days),
                  peaks_tot = cumsum(peaks),
                  coughs_tot = cumsum(coughs))

  hyfe_days %>% as.data.frame %>% head
  hyfe_days %>% as.data.frame %>% tail
  hyfe_days %>% select(peaks,coughs) %>% as.data.frame %>% head(100)

  nrow(hyfe_days)
  head(hyfe_days)
  length(unique(hyfe_days$week))

  if(verbose){message('Creating weekly timetable summary ...')}
  hyfe_weeks <- hyfe_hours %>%
    group_by(week) %>%
    summarize(tz = unique(tz)[1],
              date_floor = min(date_floor)[1],
              date_ceiling = max(date_ceiling)[1],
              year = unique(year)[1],
              week = unique(week)[1],
              study_week = unique(study_week)[1],
              n_uid = max(n_uid)[1],
              session_seconds = sum(session_seconds),
              session_hours = sum(session_seconds) / 3600,
              session_days = sum(session_seconds) / 86400,
              peaks = sum(peaks),
              coughs = sum(coughs)) %>%
    dplyr::mutate(cough_rate = (coughs / session_hours)*24*7)

  hyfe_weeks <-
    hyfe_weeks %>%
    dplyr::arrange(study_week) %>%
    dplyr::mutate(session_seconds_tot = cumsum(session_seconds),
                  session_hours_tot = cumsum(session_hours),
                  session_days_tot = cumsum(session_days),
                  peaks_tot = cumsum(peaks),
                  coughs_tot = cumsum(coughs))

  nrow(hyfe_weeks)
  head(hyfe_weeks)

  return(list(hours = hyfe_hours,
              days = hyfe_days,
              weeks = hyfe_weeks))
}
