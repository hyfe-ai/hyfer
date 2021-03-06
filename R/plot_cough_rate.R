#' Plot a timeseries of cough rate (Hyfe)
#'
#' @param ho A `hyfe` object, which is generated by `process_hyfe_data()`.
#' See full details and examples in the [package vignette](https://hyfe-ai.github.io/hyfer/#hyfe_object).
#' @param time_unit The time unit by which to plot it.
#' If `hours`, the cough rate will be coughs per hour and there will be a datapoint for each hour;
#' if `days`, the cough rate will be coughs per day and there will be a data point plotted for each day;
#' if `weeks`, the cough rate will be coughs per week and there will be a data point plotted for each week.
#' @param date_min Optionally filter the data to a minimum date.
#' Provide as a character vector of length one, with the format `"YYYY-MM-DD HH:MM:SS"`.
#' @param date_max Optionally filter to a date maximum. Same format as `date_min` above.
#' @param by_user If `FALSE` (the default), a single line will be plotted that pools all users together.
#' If `TRUE`, a line will be plotted for each user separately.
#' Note that this is only possible if the call to `process_hyfe_data()`
#' that created the `hyfe` object used the argument `by_user=TRUE`.
#' @param running_mean If you wish to add a running mean, provide a binning window here in the units chosen.
#' This will only work if you are not plotting `by_yser`.  If `NULL`, no running mean will be plotted.
#' For example, when `time_unit = hours` and `running_mean = 24`,
#' the running mean will average the y variable for a 24-hour period, stepping forward one hour at a time.
#' @param print_plot If `TRUE` (the default), the plot will be printed for you.
#' @param return_plot If `TRUE` (*not* the default), the `ggplot` plot object will be returned.
#' This can be useful if you want to modify/add to the plot (e.g., change axis titles, add a plot title, etc.).
#' @param return_data If `TRUE` (*not* the default), a simple dataframe will be returned
#' that provides you with the exact values used to produce the plot.
#'
#' @return
#' @export
#'
plot_cough_rate <- function(ho,
                            unit = c('days','hours','weeks'),
                            date_min = NULL,
                            date_max = NULL,
                            by_user = FALSE,
                            running_mean = NULL,
                            print_plot = TRUE,
                            return_plot = FALSE,
                            return_data = FALSE){

  if(FALSE){
    # debugging only - not run
    data(hyfe_data)
    ho <- process_hyfe_data(hyfe_data)
    ho_by_user <- process_hyfe_data(hyfe_data, by_user = TRUE)

    by_user <- FALSE
    by_user <- TRUE

    unit <- 'days'

    date_min = '2021-01-01 00:00:00'
    date_max = NULL
    # -- or -- #
    date_min = NULL
    date_max = NULL

    running_mean <- NULL
    #running_mean <- 7

    # Try it
    plot_cough_rate(ho)
    plot_cough_rate(ho_by_user, by_user = TRUE)
    plot_cough_rate(ho, unit = 'hours')
    plot_cough_rate(ho, unit = 'days', running_mean=14)
    plot_cough_rate(ho, unit = 'weeks')
  }


  # Stage safe copies of datasets
  hoi <- ho
  time_unit <- unit[1]

  # Test to see if `ho` is user-separated
  this_by_user <- 'user_summaries' %in% names(hoi)

  # If so, pool data (unless the plot inputs say otherwise)
  if(this_by_user){
    hoi <- pool_user_data(hoi,
                          group_users = !by_user,
                          verbose=TRUE)
  }

  # Source dataset from correct time unit and variable type ====================

  names(ho)

  if(time_unit == 'hours'){
    df <- hoi$hours
    df$x <- df$date_time
    xwidth <- NULL
    ylabel <- 'Coughs per person-hour'
  }

  if(time_unit == 'days'){
    df <- hoi$days
    df$x <- df$date
    xwidth <- 1
    ylabel <- 'Coughs per person-day'
  }

  if(time_unit == 'weeks'){
    df <- hoi$weeks
    df$x <- df$date_floor %>% lubridate::as_datetime() #%>% as.character
    xwidth <- NULL
    ylabel <- 'Coughs per person-week'
  }

  df$y <- df$cough_rate

  # Handle date filters ========================================================

  if(!is.null(date_min)){
    dmin <- lubridate::as_datetime(date_min) %>% as.numeric ; dmin
    df <- df %>% filter(x >= dmin)
  }
  if(!is.null(date_max)){
    dmax <- lubridate::as_datetime(date_max) %>% as.numeric ; dmax
    df <- df %>% filter(x <= dmax)
  }

  # Add uid, if ho was aggregated by user ======================================

  if('uid' %in% names(df)){
    df <- df %>% dplyr::select(x,y,uid)
  }else{
    df <- df %>% dplyr::select(x,y)
  }

  # Build plot =================================================================

  if(by_user & this_by_user){
    # Keep users separate
    p <-ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=uid)) +
      ggplot2::theme(legend.text = ggplot2::element_text(size=4)) +
      ggplot2::geom_point()
  }else{
    # Pool all users
    if(by_user){message('Sorry, cannot plot by user -- `hyfe` object is an aggregation.')}

    # Add running mean
    if(!is.null(running_mean)){
      df$rm <- add_running_mean(x = df$x, y = df$y, window = running_mean)$y
    }

    p <-ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) +
      ggplot2::geom_col(alpha=.5,fill='palevioletred4', width=xwidth)

    if(!is.null(running_mean)){
      p <- p + ggplot2::geom_line(ggplot2::aes(x=x,y=rm),
                                  lwd=1.25,alpha=.7,
                                  col='palevioletred4')
    }
  }

  # add labels
  p <- p +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(ylabel)

  # Return
  return_list <- list()
  if(return_plot){return_list$plot <- p}
  if(return_data){return_list$data <- df}
  if(print_plot){print(p)}
  if(length(return_list)>0){return(return_list)}
}
