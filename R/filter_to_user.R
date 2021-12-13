#' Filter `hyfe_data` object to a certain user(s).
#'
#' If you want to work with data from only a single user (or subset of users)
#' in a `hyfe_data` object containing data from multiple users,
#' use this function before running `process_hyfe_data()`.
#'
#' @param uid_filter The Firebase user IDs to filter to. If you are using other identifiers
#' (e.g., research alias, email address, or username), use the `hyfe_data$id_key` dataframe
#' to find the `uid`(s) you want to filter by. If `uid` is `NULL`, the default, a random `uid` will be chosen
#' and the data will be filtered according to that random selection. This can be handy if you have an enormous
#' `hyfe_data` object and just want to practice data processing / plotting without
#' dealing with the enormous filesizes and processing times.
#'
#' @param hyfe_data A standard `hyfe_data` object downloaded
#' from the Research Dashboard (for external partners) or from `hyferdrive` (internal analysts).
#' See full details and examples in the [package vignette](https://hyfe-ai.github.io/hyfer/#hyfedata).
#'
#' @return A `hyfe_data` object containing only the data pertaining to the `uid`(s) you filtered by.
#' Same data structure as a raw `hyfe_data` object.
#'
#' @export
#'
filter_to_user <- function(uid_filter=NULL,
                           hyfe_data){

  if(FALSE){
    uid_filter <- '9D7SChvklVa7zya0LdU6YVOi9QV2'
    #uid_filter <- uids
    data(hyfe_data)
  }

  if(is.null(uid_filter)){uid_filter <- sample(hyfe_data$id_key,1)}

  uid_data <- lapply(hyfe_data,function(x){
    if(!is.null(x)){
      if('uid' %in% names(x)){
        x <- x %>% dplyr::filter(uid %in% uid_filter)
      }
    }
    return(x)
  })

  lapply(uid_data,nrow)
  uid_data$sounds$uid %>% table

  return(uid_data)
}
