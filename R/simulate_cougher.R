#' Generate a simulated cough timeline for a user
#'
#' Simulate an hourly cough timeseries for an imaginary user.
#'
#' @param rate_mean Mean hourly cough rate
#' @param rate_variance Variance in the hourly cough rate.
#' If left `NULL`, variance will be estimated using a regression model.
#' @param hours Duration of time series, in hours. Default is 720 (about a month).
#' @param start_timestamp Optional start timestamp (seconds since midnight UTC
#' on January 1, 1970). Default is to use your system's current time.
#' @param tz Optional timezone for timestamp. Default is your system timezone.
#' @param distribution The frequency distribution to base the simulation upon.
#' Default is `'nb'`, negative binomial. This is currently the only option.
#' @param format_time If `TRUE` (the default), formatted date/time variables will be returned using `format_hyfe_time()`.
#' It may be useful to change this to `FALSE` if conducting iterative analyses that rely on speed.
#' @param random_seed Set the random number generator seed in order to replicate a cougher.
#'
#' @return A dataframe with a `timestamp` column, a `coughs` column, and a dummy `rec` column in which all rows have 1 full hour of "recording".
#' If `format_time` is `TRUE`, additional date/time variables are provide.d See `format_hyfe_time()`.
#'
#' @export
#'
simulate_cougher <- function(rate_mean,
                             rate_variance = NULL,
                             hours = 24*30,
                             start_timestamp = as.numeric(Sys.time()),
                             tz = Sys.timezone(),
                             distribution = 'nb',
                             format_time = TRUE,
                             random_seed = NULL
                             ){

  # ============================================================================
  # For debugging only -- not run!
  if(FALSE){
    rate_mean = 2
    rate_variance = NULL
    hours = 24*30
    start_timestamp = as.numeric(Sys.time())
    tz = Sys.timezone()
    distribution = 'nb'
    format_time = TRUE
  }
  # ============================================================================

  # Handle NULL variance
  if(is.null(rate_variance)){

    # Regression function based upon Navarra cohort analysis
    predict_var <- function(mu,b1 = 1.339539, intercept = 1.90719){
      var_return <- exp(b1*log(mu) + intercept)
      #plot(var_function(mu) ~ mu, type='l')
      return(var_return)
    }

    rate_variance <- predict_var(rate_mean)
  }

  # Setup timestamp series
  ts <- start_timestamp + (0:(hours-1))*3600
  ttable <- data.frame(timestamp=ts)
  if(format_time){
    ttable <- format_hyfe_time(ts, tz)
  }
  head(ttable)

  coughs <- NA
  if(distribution == 'nb'){
    # Calculate negative-binomial
    nb_size <- (rate_mean^2) / (rate_variance - rate_mean)

    # Simulate coughs
    if(!is.null(random_seed)){set.seed(random_seed)}
    coughs <- rnbinom(n=hours, size = nb_size, mu = rate_mean)
    coughs
  }

  ttable$rec <- 1
  ttable$coughs <- coughs

  #plot(ttable$coughs, type='l')

  return(ttable)
}
