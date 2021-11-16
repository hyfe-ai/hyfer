#' Summarize Hyfe data
#'
#' @param ho desc
#' @param cutoff_hourly desc
#' @param cutoff_daily desc
#'
#' @return
#' @export
#'
hyfe_summarize <- function(ho,
                           cutoff_hourly = 30,
                           cutoff_daily = 4){
  if(FALSE){
    library(hyfer)
    library(dplyr)
    data(hyfe_data)
    ho <- process_hyfe_data(hyfe_data)
    ho <- process_hyfe_data(hyfe_data, by_user = TRUE)

    cutoff_hourly = 30
    cutoff_daily = 4

  }

  summarize_core <- function(ho, cutoff_hourly, cutoff_daily){
    hoi <- ho
    #hoi <- useri

    hours <- hoi$hours
    days <- hoi$days

    if('sessions' %in% names(hoi)){
      users <- hoi$sessions$uid %>% unique %>% length
    }else{
      users <- 1
    }

    # Basic summaries
    summi <-
      data.frame(users = users,
                 seconds = hours$session_seconds %>% as.numeric %>% sum)

    summi$hours <- summi$seconds / 3600
    summi$days <- summi$hours / 24
    summi$years <- summi$days / 365
    summi$sounds = hours$peaks %>% sum
    summi$coughs = hours$coughs %>% sum

    # Rates
    hours <- hours %>% filter(session_seconds/n_uid >= cutoff_hourly*60)
    hours %>% nrow

    days <- days %>% filter(session_hours/n_uid >= cutoff_daily)
    days %>% nrow

    summi$hourly_n = hours %>% nrow
    summi$hourly_rate = hours$cough_rate %>% mean(na.rm=TRUE)
    summi$hourly_var = hours$cough_rate %>% var(na.rm=TRUE)
    summi$hourly_sd = hours$cough_rate %>% sd(na.rm=TRUE)
    summi$hourly_max = hours$cough_rate %>% max(na.rm=TRUE)

    summi$daily_n = days %>% nrow
    summi$daily_rate = days$cough_rate %>% mean(na.rm=TRUE)
    summi$daily_var = days$cough_rate %>% var(na.rm=TRUE)
    summi$daily_sd = days$cough_rate %>% sd(na.rm=TRUE)
    summi$daily_max = days$cough_rate %>% max(na.rm=TRUE)

    summi
    return(summi)
  }

  if('user_summaries' %in% names(ho)){
    # ho by user
    users <- data.frame()
    i=1
    for(i in 1:length(ho$user_summaries)){
      useri <- ho$user_summaries[[i]]
      names(useri)
      summi <- summarize_core(useri,
                              cutoff_hourly = cutoff_hourly,
                              cutoff_daily = cutoff_daily)
      summi
      summi <- data.frame(useri$id_key, summi)
      users <- rbind(users, summi)
    }
    users

    overall <- data.frame(users = nrow(users),
                          seconds = sum(users$seconds),
                          hours = sum(users$hours),
                          days = sum(users$days),
                          years = sum(users$years),
                          sounds = sum(users$sounds),
                          coughs = sum(users$coughs),
                          hourly_n = mean(users$hourly_n,na.rm=TRUE),
                          hourly_rate = mean(users$hourly_rate, na.rm=TRUE),
                          hourly_var = var(users$hourly_rate, na.rm=TRUE),
                          hourly_sd = sd(users$hourly_rate, na.rm=TRUE),
                          hourly_max = max(users$hourly_rate, na.rm=TRUE),
                          daily_n = mean(users$daily_n, na.rm=TRUE),
                          daily_rate = mean(users$daily_rate, na.rm=TRUE),
                          daily_var = var(users$daily_rate, na.rm=TRUE),
                          daily_sd = sd(users$daily_rate, na.rm=TRUE),
                          daily_max = max(users$daily_rate, na.rm=TRUE))
    overall

  }else{
    # Simply ho object
    users <- NULL
    overall <- summarize_core(ho,
                              cutoff_hourly = cutoff_hourly,
                              cutoff_daily = cutoff_daily)
  }

  return(list(overall = overall,
              users = users
              ))

}
