#' Format and create date/time variables
#'
#' Standard date/time variables for downstream Hyfe functions.
#'
#' @param timestamps A numeric vector of integer timestamps.
#' It is assumed that these timestamps represent seconds since 00:00:00 UTC on January 1, 1970,
#' # which is the standard format of `hyfe_data` objects.
#' @param timezone  A character vector for the timezone, which must match one of the timezones listed in `OlsonNames()`.
#' Most users will retrieve this timezone from `hyfe_data$cohort_settings$timezone`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' format_hyfe_time(1626851363)
#' format_hyfe_time(1626851363, 'UTC')
#' format_hyfe_time(1626851363, 'Africa/Kampala')
#' format_hyfe_time(1626851363, 'America/Chicago')
#'
format_hyfe_time <- function(timestamps,
                             timezone=NULL){

  if(FALSE){
    # for debugging only
    timestamps <- c(1626851363,1626800363)
    timezone = NULL
    timezone = 'America/Chicago'
    timezone = 'Africa/Kampala'
  }

  if(is.null(timezone)){timezone <- 'UTC'}

  dt <- lubridate::as_datetime(timestamps) ; dt
  dt <- lubridate::with_tz(dt, tz=timezone) ; dt

  # Study interval
  dtmin <- min(dt)
  sdiff <- sapply(dt,function(x){lubridate::interval(dtmin,x) %>% lubridate::int_length()})

  result <-
    data.frame(timestamp = timestamps,
               date_time = dt,
               tz = timezone,
               date = lubridate::date(dt),
               date_floor = lubridate::floor_date(dt,'day') %>% unclass %>% as.numeric,
               date_ceiling = lubridate::ceiling_date(dt,'day')  %>% unclass %>% as.numeric,
               year = lubridate::year(dt),
               week = lubridate::week(dt),
               yday = lubridate::yday(dt),
               hour = lubridate::hour(dt),
               study_week = ceiling(sdiff / (86400*7)),
               study_day = ceiling(sdiff / 86400),
               study_hour = ceiling(sdiff / 3600),
               frac_week = sdiff / (86400*7),
               frac_day = sdiff / 86400,
               frac_hour = sdiff / 3600)

  #result$study_week[result$study_week == 0] <- 1
  #result$study_day[result$study_day == 0] <- 1
  #result$study_hour[result$study_hour == 0] <- 1

  result

  if(FALSE){
    format_hyfe_time(1626851363)
    format_hyfe_time(1626851363, 'UTC')
    format_hyfe_time(1626851363, 'Africa/Kampala')
    format_hyfe_time(1626851363, 'America/Chicago')
  }

  return(result)
}

