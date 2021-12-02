#' Plot cough rate error as a function of monitoring time.
#'
#' How much monitoring is enough to estimate the overall cough rate accurately?
#' This is a difficult question to answer, but this plot can help approximate it.
#' It shows the cough rate estimate based on 1 hour of monitoring, 2 hours, 3 hours, ...,
#' up until the entire cough record. The plot also displays the 'overall' cough rate, using the entire
#' monitoring record, and horizontal lines indicating a range of acceptable error around the 'true' rate.
#'
#' @param hours A dataframe of hourly cough counts. Minimum requirements:
#' A column `coughs` with coughs counted in each hour,
#' and a column `rec` with monitoring time during that hour (0 - 1).
#' For example, you can use `ho$hours` (from a `hyfe` object) or the output of `simulate_cough_rate()`.
#' @param cutoff The amount of monitoring within an hour
#' in order for that hour to be included in cough rate calculations.
#' Default is 0.5 (half hour of monitoring during the hour).
#' @param min_error Minimum acceptable error. Default is 10% (0.1).
#' @param toplot If `TRUE` (the default), a plot will dispaly the result.
#'
#' @return The `hours` dataframe with additional columns:
#' `estimate`, the cough rate estimate up to that hour of monitoring;
#' `error`, the error in that estimate based upon the overall cough rate;
#' `prop_error`, the proportional error.
#' @export
#'
plot_cough_rate_error <- function(hours,
                                  cutoff=0.5,
                                  min_error=.1,
                                  toplot=TRUE){

  # ============================================================================
  # For debugging only -- not run!
  if(FALSE){
    hours <- simulate_cougher(10)
    hours %>%  head
    cutoff = 0.5
    min_error = .1
    toplot=TRUE

    # test it
    demo <- plot_cough_rate_error(hours, min_error = .1)
  }
  # ============================================================================

  hours$rate <- hours$coughs / hours$rec
  overall_rate <- mean(hours$rate[hours$rec >= cutoff])

  # Compute error curve
  rate_estimate <- c()
  for(i in 1:nrow(hours)){
    hrsi <- hours[1:i,]
    hrsi <- hrsi[hrsi$rec >= cutoff,]
    if(nrow(hrsi)==0){
      rate_estimate[i] <- NA
    }else{
      rate_estimate[i] <- mean(hrsi$rate,na.rm=TRUE)
    }
  }
  rate_estimate

  hours$estimate <- rate_estimate
  hours$error <- overall_rate - rate_estimate
  hours$prop_error <- ((overall_rate + abs(hours$error))/overall_rate) - 1
  head(hours)

  last_hour_of_error <- which(hours$estimate <= (1-min_error)*overall_rate |
                                hours$estimate >= (1+min_error)*overall_rate) %>%  max

  message('Monitoring time needed to stabilize below ',(min_error*100),'% error: ',
          last_hour_of_error,
          ' hours (',
          round(last_hour_of_error/24,1),
          ' days)')

  if(toplot){
    par(mar=c(4.2,4.2,.5,.5))
    plot(hours$estimate, type='l', lwd=2.0, xlab='Hours',ylab='Estimate (coughs per hour)')
    abline(h=overall_rate,lty=3)
    abline(h=(1-min_error)*overall_rate,lty=1,col='red',lwd=.5)
    abline(h=(1+min_error)*overall_rate,lty=1,col='red',lwd=.5)
    abline(v=last_hour_of_error, lty=3, col='grey',lwd=1)
  }

  return(hours)
}
