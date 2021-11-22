#' Plot Hyfe cough rate
#'
#' @param ho desc
#' @param time_unit desc
#' @param date_min desc
#' @param date_max desc
#' @param by_user desc
#' @param print_plot desc
#' @param return_plot desc
#' @param return_data desc
#' @param verbose desc
#'
#' @return
#' @export
#'
plot_cough_rate <- function(ho,
                            unit = c('days','hours','weeks'),
                            date_min = NULL,
                            date_max = NULL,
                            by_user = FALSE,
                            print_plot = TRUE,
                            return_plot = FALSE,
                            return_data = FALSE,
                            verbose=TRUE){

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

    # Try it
    plot_cough_rate(ho)
    plot_cough_rate(ho_by_user, by_user = TRUE)
    plot_cough_rate(ho, unit = 'hours')
    plot_cough_rate(ho, unit = 'weeks')
  }


  # Stage safe copies of datasets
  hoi <- ho
  plot_type <- type[1]
  time_unit <- unit[1]

  # Test to see if `ho` is user-separated
  this_by_user <- 'user_summaries' %in% names(hoi)

  # If so, pool data (unless the plot inputs say otherwise)
  if(this_by_user){
    hoi <- pool_user_data(hoi,
                          group_users = !by_user,
                          verbose=verbose)
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
    p <-ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) +
      ggplot2::geom_col(alpha=.5,fill='palevioletred4', width=xwidth)
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
