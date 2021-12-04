#' Synchronize Hyfe detections with a set of labels
#'
#' Comparing Hyfe performance to a groundtruth, such as a set of labeled detections,
#' requires that the two sets of events are synchronized. Even if Hyfe's system time
#' differs from the labeler's clock by a second or two, that offset can complicate
#' and confuse the performance evaluation process.
#'
#' This functions synchronizes Hyfe detections to the set of reference/label times.
#'
#' @param reference_times A vector of numeric timestamps (seconds since 00:00:00 UTC on January 1, 1970).
#' These are the reference times to which the Hyfe detection times will be compared.
#' For example, the timestamps of a labelled sound can serve as a reference time.
#' @param reference_labels A numeric vector of labels the same length as `reference_times`.
#' If `NULL`, the function will assume that all reference times refer to a true cough event.
#' These labels are interpreted using Hyfe's 4-tier labeling system (0= definitely not a cough;
#' 1 = disputable that this is an authentic or audible cough; 2 = definitely an authentic cough, but distant;
#' 3 = an authentic and loud cough).
#' @param hyfe_times A vector of numeric timestamps of Hyfe detections.
#' @param hyfe_predicitons A logical vector of predictions the same length as `hyfe_times`.
#' If `NULL`, the function will assume that all Hyfe times refer to cough predictions.
#' @param filter_to_3 If `TRUE` (the default), the reference times will be filtered to those that are definitely a loud cough.
#' @param toplot If `TRUE`, a diagnostic plot will be shown.
#' @param verbose Print status updates to the Console?
#'
#' @details This function uses a three-stage iterative method for identifying the offset that reduces the total offset error between
#' reference times and Hyfe times. The first stage tests for hour-scale offsets, such as timezone differences.
#' The second stage tests for minute-scale offsets.
#' The final stage tests for half-second scale offsets. These three offsets are summed and returned.
#'
#' @return A numeric estimate of Hyfe's time offset, in seconds, from the reference times. A negative number indicates that Hyfe is *ahead*
#' of the reference times. In most cases, your next step in analysis will be yo add the return of this function to your
#' Hyfe times.
#'
#' Note that this output is almost always a single number,
#' but sometimes two (or more) different offsets can yield an equal reduction in error and therefore be equally viable offsets.
#' In those instances, all viable offsets are returned.
#'
#' @export
#'
synchronize <- function(reference_times,
                        reference_labels = NULL,
                        hyfe_times,
                        hyfe_predictions = NULL,
                        filter_to_3 = TRUE,
                        toplot=TRUE,
                        verbose=TRUE){

  #=============================================================================
  # For debugging only -- not run!

  if(FALSE){
    library(dplyr)

    toplot = TRUE
    verbose = TRUE
    filter_to_3 = TRUE

    # Fabricate reference_times
    start_time <- Sys.time() %>%  as.numeric %>%  round
    intervals <- sample(5:15,size=50,replace=TRUE)
    int_cum <- cumsum(intervals)
    reference_times <- start_time + int_cum
    reference_times

    # Labels
    reference_labels <- sample(1:3,size=length(reference_times),replace=TRUE)
    reference_labels

    # Hyfe_times
    major_offset <- 3600 * 7 # time zone issue
    minor_offset <- -27 # hyfe is 27 seconds behind reference times
    hyfe_times <- reference_times + major_offset + minor_offset

    # Hyfe labels
    hyfe_predictions <- rep(FALSE,length(reference_times))
    hyfe_predictions[reference_labels == 3] <- TRUE
    hyfe_predictions[reference_labels == 2] <- sample(c(FALSE,TRUE),size=length(reference_labels[reference_labels == 2]), replace=TRUE)
    hyfe_predictions[reference_labels == 1] <- sample(c(FALSE,TRUE),size=length(reference_labels[reference_labels == 1]), replace=TRUE)
    hyfe_predictions

    # Test it
    synchronize(reference_times,
                reference_labels,
                hyfe_times,
                hyfe_predictions,
                filter_to_3 = TRUE,
                toplot=TRUE,
                verbose=TRUE)
  }

  #=============================================================================
  # Filter timestamps

  if(is.null(reference_labels)){
    reference_labels <- rep(3,times=length(reference_times))
  }
  if(is.null(hyfe_predictions)){
    hyfe_predictions <- rep(TRUE,times=length(hyfe_times))
  }

  # filter to predicted coughs
  hyfe_times <- hyfe_times[hyfe_predictions == TRUE]

  # filter to loud authentic coughs only?
  if(filter_to_3){
    reference_times <- reference_times[reference_labels == 3]
  }

  if(length(hyfe_times)==0 | length(reference_times) == 0){
    if(verbose){message('Cannot proceed! After filtering, hyfe_times or reference_times is length 0.')}
    best_offset <- NA
  }else{

    #=============================================================================
    # Time zone sync (really, half hour resolution)

    if(toplot){par(mfrow=c(3,1))}
    if(verbose){message('--- calculating time zone offset . . .')}
    tz_offset <- find_time_offset(reference_times,
                                  hyfe_times,
                                  offset_step = 1800,
                                  offset_range = c(-86400, 86400),
                                  plot_title = 'Timezone offset',
                                  toplot=toplot)[1]
    tz_offset
    hyfe_times_tz <- hyfe_times + tz_offset

    #=============================================================================
    # Minute-scale sync

    if(verbose){message('--- calculating minutes offset . . .')}

    minutes_offset <- find_time_offset(reference_times,
                                  hyfe_times_tz,
                                  offset_step = 60,
                                  offset_range = c(-3600, 3600),
                                  plot_title = 'Minute-scale offset',
                                  toplot=toplot)[1]
    minutes_offset
    hyfe_times_minutes <- hyfe_times_tz + minutes_offset

    #=============================================================================
    # Second-scale sync

    if(verbose){message('--- calculating seconds offset . . .')}

    seconds_offset <- find_time_offset(reference_times,
                                       hyfe_times_minutes,
                                       offset_step = .5,
                                       offset_range = c(-60, 60),
                                       plot_title = 'Half-second-scale offset',
                                       toplot=toplot)
    seconds_offset
    if(length(seconds_offset)>1){
      if(verbose){message('WARNING! More than one time offsets is equally viable. Returning all ')}
    }

    if(toplot){par(mfrow=c(1,1))}

    #=============================================================================

    best_offset <- tz_offset + minutes_offset + seconds_offset
    best_offset
  }

  return(best_offset)
}
