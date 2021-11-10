filter_to_user <- function(uid=NULL,
                           hyfe_data){

  if(FALSE){
    uid <- '9D7SChvklVa7zya0LdU6YVOi9QV2'
    data(hyfe_data)
  }

  if(is.null(uid)){uid <- sample(hyfe_data$id_key,1)}

  uid_filter <- uid

  uid_data <- lapply(hyfe_data,function(x){
    if(!is.null(x)){
      x <- x %>% dplyr::filter(uid %in% uid_filter)
    }
    return(x)
  })

  lapply(uid_data,nrow)
  uid_data$sounds$uid %>% table

  return(uid_data)
}
