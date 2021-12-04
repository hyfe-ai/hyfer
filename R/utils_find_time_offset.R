#' Background function: find the time offset between Hyfe and a reference
#'
#' This function is never called directly by an analyst. It is used in `synchronize()`
#' to determine offset corrections.
#'
#' @param reference_times A vector of numeric timestamps (seconds since 00:00:00 UTC on January 1, 1970).
#' These are the reference times to which the Hyfe detection times will be compared.
#' For example, the timestamps of a labelled sound can serve as a reference time.
#' @param hyfe_times A vector of numeric timestamps of Hyfe detections.
#' @param offset_step The resolution of the offset estimation process; for example, if set to 60,
#' the function will test various offsets at intervals of 60 seconds.
#' @param offset_range The range of potential offsets to evaluate.
#' @param toplot If `TRUE`, a diagnostic plot will be shown.
#' @param verbose Print status updates to the Console?
#'
#' @return A best estimate of Hyfe's time offset, in seconds, from the reference times. The resolution/accuracy of this estimate will depend
#' on the values chosen for `offset_step`.
#'
#' @export
#'
find_time_offset <- function(reference_times,
                             hyfe_times,
                             offset_step,
                             offset_range,
                             plot_title = NULL,
                             toplot=TRUE,
                             verbose=TRUE
                             ){
  #=============================================================================
  if(FALSE){
    library(dplyr)

    # Fabricate reference_times
    start_time <- Sys.time() %>%  as.numeric %>%  round
    intervals <- sample(5:15,size=50,replace=TRUE)
    int_cum <- cumsum(intervals)
    reference_times <- start_time + int_cum
    reference_times

    # Hyfe_times
    major_offset <- 3600 * 7 # time zone issue
    minor_offset <- -27 # hyfe is 27 seconds behind reference times
    hyfe_times <- reference_times + major_offset + minor_offset

    offset_step <- 3600
    offset_range <- c(-86400, 86400)
    plot_title = NULL
    toplot = TRUE
    verbose=TRUE
  }
  #=============================================================================

  reference_times <- reference_times %>% sort
  hyfe_times <- hyfe_times %>% sort

  # get time range of labelled coughs
  #test_begin <- min(c(reference_times, hyfe_times))
  #test_end <- max()

  # filter cough detections to that same time range
  #length(tt)
  #tt <- tt[tt >= test_begin & tt <= test_end]
  #length(tt)

  # Setup potential offsets
  offsets <- seq(offset_range[1], offset_range[2], by = offset_step)
  offsets

  # Function for calculating offset error
  # offseti <- offsets[1]
  offset.error <- function(offseti) {
    ttoff <- hyfe_times + offseti
    nearest <- c()
    for (i in 1:length(reference_times)) {
      cti <- reference_times[i]
      diffs <- cti - ttoff
      whichdiff <- which.min(abs(diffs))
      nearest <- c(nearest, diffs[whichdiff])
    }
    tot.error <- sum(abs(nearest))
    return(tot.error)
  }

  # Determine total time mismatch for each potential offset
  tot.errors <- sapply(offsets, offset.error)
  tot.errors
  min.error <- min(tot.errors)
  best_offset <- offsets[which(tot.errors == min.error)]
  best_offset

  if(best_offset[1] %in% offsets[c(1,length(offsets))]){
    if(verbose){message('Warning! Offset range may not have been adequate to identify the best offset.')}
  }

  # Check out plot
  if(toplot){
    par(mar = c(4.5, 4.5, 3, 0.5))
    plot(tot.errors ~ offsets, type = "l", col = "steelblue3",
         ylab = "Total errors (secs)", xlab = "Offset estimate (Control - Test)",
         main = plot_title)
    abline(v=0,lty=3,col='grey')
  }

  return(best_offset)
}
