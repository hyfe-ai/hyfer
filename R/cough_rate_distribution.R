cough_rate_distribution <- function(ho){

  #=============================================================================
  # for debugging only -- not run!
  if(FALSE){
    # debugging only - not run
    data(hyfe_data)
    ho <- process_hyfe_data(hyfe_data)
    ho_by_user <- process_hyfe_data(hyfe_data, by_user = TRUE)

  }
  #=============================================================================

  hoi <- ho # make safe copy of input

  # Test to see if `ho` is user-separated
  this_by_user <- 'user_summaries' %in% names(hoi)
  if(this_by_user){

    hoi <- pool_user_data(hoi,
                          group_users = !by_user,
                          verbose=verbose)
  }


  return_list <- list()

  return(return_list)
}
